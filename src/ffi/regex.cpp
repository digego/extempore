/*
 * Copyright (c) 2011, Andrew Sorensen
 *
 * All rights reserved.
 *
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * Neither the name of the authors nor other contributors may be used to endorse
 * or promote products derived from this software without specific prior written
 * permission.
 *
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 */

#include "SchemeFFIRegistry.h"

#include "EXTRegex.h"

#include <string>
#include <string_view>

namespace extemp {

namespace SchemeFFI {

using regex::matchDataFor;

// A pattern pcre2 rejects is a Scheme error carrying pcre2's message and the
// offset it stopped at, so a typo in a compiler regex is loud instead of
// quietly matching nothing.
static regex::PatternPtr cachedPattern(const char* Pattern, pointer Args) {
    auto re = regex::compile(Pattern);
    if (!re) {
        throw ScmRuntimeError(std::string(ffi_current_name()) + ": invalid regex \"" + Pattern +
                                  "\" at offset " + std::to_string(re.error().offset) + ": " +
                                  re.error().message,
                              Args);
    }
    return *re;
}

static int regexExec(const regex::PatternPtr& Re, const regex::MatchDataPtr& MatchData,
                     std::string_view Data) {
    return regex::search(Re, MatchData, Data);
}

static pointer regex_match(scheme* Scheme, pointer Args) {
    const std::string_view data(argString(Scheme, Args, 1));
    auto re(cachedPattern(argString(Scheme, Args, 2), Args));
    auto matchData(matchDataFor(re));
    return (regexExec(re, matchData, data) > 0) ? Scheme->T : Scheme->F;
}

static pointer regex_matched(scheme* Scheme, pointer Args) {
    const std::string_view data(argString(Scheme, Args, 1));
    auto re(cachedPattern(argString(Scheme, Args, 2), Args));
    auto matchData(matchDataFor(re));
    const int rc = regexExec(re, matchData, data);
    pointer list = Scheme->NIL;
    if (rc < 1) {
        return list;
    }
    const PCRE2_SIZE* ovector = pcre2_get_ovector_pointer(matchData.get());
    for (int i = 0; i < rc; ++i) {
        const PCRE2_SIZE start = ovector[2 * size_t(i)];
        const PCRE2_SIZE end = ovector[2 * size_t(i) + 1];
        // cons itself can trigger GC before storing its arguments (new_cell
        // -> try_to_call_gc), and the GC cannot see C-frame locals: BOTH the
        // list and the just-allocated string need protection across the cons
        const EnvInjector injector(Scheme, list);
        pointer s = (start == PCRE2_UNSET)
                        ? mk_string(Scheme, "")
                        : mk_counted_string(Scheme, data.data() + start, int(end - start));
        const EnvInjector injector2(Scheme, s);
        list = cons(Scheme, s, list);
    }
    return reverse_in_place(Scheme, Scheme->NIL, list);
}

static pointer regex_match_all(scheme* Scheme, pointer Args) {
    std::string_view data(argString(Scheme, Args, 1));
    auto re(cachedPattern(argString(Scheme, Args, 2), Args));
    auto matchData(matchDataFor(re));
    pointer list = Scheme->NIL;
    while (!data.empty()) {
        if (regexExec(re, matchData, data) < 1) {
            break;
        }
        const PCRE2_SIZE* ovector = pcre2_get_ovector_pointer(matchData.get());
        const EnvInjector injector(Scheme, list);
        pointer s =
            mk_counted_string(Scheme, data.data() + ovector[0], int(ovector[1] - ovector[0]));
        const EnvInjector injector2(Scheme, s);
        list = cons(Scheme, s, list);
        // a zero-width match leaves the end offset where the search started,
        // so step at least one character or this never terminates
        const size_t advance = (ovector[1] > 0) ? size_t(ovector[1]) : 1;
        if (advance >= data.size()) {
            break;
        }
        data.remove_prefix(advance);
    }
    return reverse_in_place(Scheme, Scheme->NIL, list);
}

static pointer regex_split(scheme* Scheme, pointer Args) {
    std::string_view data(argString(Scheme, Args, 1));
    auto re(cachedPattern(argString(Scheme, Args, 2), Args));
    auto matchData(matchDataFor(re));
    pointer list = Scheme->NIL;
    while (true) {
        if (data.empty() || regexExec(re, matchData, data) < 1) {
            if (!data.empty())  // append remaining chars if any left
            {
                // mk_counted_string and cons can each trigger GC: without
                // protection, `list` and the fresh string (referenced only
                // from this C frame) are collected and the cons links freed
                // cells, corrupting whatever reuses them
                const EnvInjector injector(Scheme, list);
                pointer s = mk_counted_string(Scheme, data.data(), int(data.size()));
                const EnvInjector injector2(Scheme, s);
                list = cons(Scheme, s, list);
            }
            return reverse_in_place(Scheme, Scheme->NIL, list);
        }
        const PCRE2_SIZE* ovector = pcre2_get_ovector_pointer(matchData.get());
        size_t piece = size_t(ovector[0]);
        size_t advance = size_t(ovector[1]);
        if (advance == 0) {
            // zero-width match at the start of what's left: take one character
            // into this piece, so the loop makes progress
            piece = 1;
            advance = 1;
        }
        const EnvInjector injector(Scheme, list);
        // NOLINTNEXTLINE(bugprone-suspicious-stringview-data-usage): the length is passed
        pointer s = mk_counted_string(Scheme, data.data(), int(piece));
        const EnvInjector injector2(Scheme, s);
        list = cons(Scheme, s, list);
        if (advance >= data.size()) {
            return reverse_in_place(Scheme, Scheme->NIL, list);
        }
        data.remove_prefix(advance);
    }
}

static pointer regex_replace(scheme* Scheme, pointer Args) {
    const std::string_view data(argString(Scheme, Args, 1));
    auto re(cachedPattern(argString(Scheme, Args, 2), Args));
    const std::string result = regex::replaceFirst(re, data, argString(Scheme, Args, 3));
    return mk_counted_string(Scheme, result.data(), int(result.size()));
}

std::span<const FFIEntry> regexDefs() {
    static const FFIEntry defs[] = {
        FFI_DEF("regex:match?", regex_match, 2, 0, false),
        FFI_DEF("regex:matched", regex_matched, 2, 0, false),
        FFI_DEF("regex:match-all", regex_match_all, 2, 0, false),
        FFI_DEF("regex:split", regex_split, 2, 0, false),
        FFI_DEF("regex:replace", regex_replace, 3, 0, false),
    };
    return defs;
}

}  // namespace SchemeFFI

}  // namespace extemp
