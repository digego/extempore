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

// pcre2 is width-agnostic: the code unit width has to be picked before the
// header, and the 8-bit library is the one this file uses.
#define PCRE2_CODE_UNIT_WIDTH 8
#include <pcre2.h>

#include <cstdint>
#include <memory>
#include <mutex>
#include <string>
#include <string_view>
#include <unordered_map>

namespace extemp {

namespace SchemeFFI {

// A compiled pattern is read-only during pcre2_match, so one copy can be shared
// across threads; shared_ptr keeps a pattern alive for a call in progress even
// if the cache drops it.
using PcrePtr = std::shared_ptr<pcre2_code>;

// pcre2 writes the match offsets into a match_data block sized from the pattern
// it was made for. That block is per-match state rather than shared, so each
// call below makes its own and reuses it across the iterations of the looping
// bodies.
struct MatchDataDeleter
{
    void operator()(pcre2_match_data* MatchData) const { pcre2_match_data_free(MatchData); }
};

using MatchDataPtr = std::unique_ptr<pcre2_match_data, MatchDataDeleter>;

// A $N group number is parsed as a bounded integer: the replacement string is
// user input, and a long run of digits mustn't overflow on the way to being
// rejected as a group the pattern doesn't have.
static constexpr uint32_t REGEX_MAX_GROUP = 100000;

// A pattern pcre2 rejects is a Scheme error carrying pcre2's message and the
// offset it stopped at, so a typo in a compiler regex is loud instead of
// quietly matching nothing.
static PcrePtr compilePattern(const char* Pattern, pointer Args)
{
    int errorcode = 0;
    PCRE2_SIZE erroroffset = 0;
    pcre2_code* re = pcre2_compile(reinterpret_cast<PCRE2_SPTR>(Pattern), PCRE2_ZERO_TERMINATED, 0,
            &errorcode, &erroroffset, nullptr);
    if (!re) {
        PCRE2_UCHAR message[256];
        pcre2_get_error_message(errorcode, message, sizeof(message));
        throw ScmRuntimeError(std::string(ffi_current_name()) + ": invalid regex \"" + Pattern
                        + "\" at offset " + std::to_string(erroroffset) + ": "
                        + reinterpret_cast<const char*>(message),
                Args);
    }
    return PcrePtr(re, [](pcre2_code* Code) { pcre2_code_free(Code); });
}

// These sit on the compiler's hot path --- every type string goes through
// several --- and pcre2_compile is not cheap, so compiled patterns are cached.
// Plain bounded map rather than an LRU: the working set is the handful of
// patterns in runtime/*.xtm, and dropping the lot on overflow costs one
// recompile each.
static PcrePtr cachedPattern(const char* Pattern, pointer Args)
{
    static constexpr size_t MAX_CACHED_PATTERNS = 512;
    static std::mutex sPatternCacheMutex;
    static std::unordered_map<std::string, PcrePtr> sPatternCache;

    std::string key(Pattern);
    {
        std::lock_guard<std::mutex> lock(sPatternCacheMutex);
        auto it = sPatternCache.find(key);
        if (it != sPatternCache.end()) {
            return it->second;
        }
    }
    PcrePtr re = compilePattern(Pattern, Args);  // compiled outside the lock
    std::lock_guard<std::mutex> lock(sPatternCacheMutex);
    if (sPatternCache.size() >= MAX_CACHED_PATTERNS) {
        sPatternCache.clear();
    }
    sPatternCache.emplace(std::move(key), re);
    return re;
}

static MatchDataPtr matchDataFor(const PcrePtr& Re)
{
    return MatchDataPtr(pcre2_match_data_create_from_pattern(Re.get(), nullptr));
}

// Sizing the match data from the pattern means the ovector always has room for
// every capture, so pcre2_match returns the capture count or a negative code,
// never 0.
static int regexExec(const PcrePtr& Re, const MatchDataPtr& MatchData, std::string_view Data)
{
    return pcre2_match(Re.get(), reinterpret_cast<PCRE2_SPTR>(Data.data()), Data.size(), 0, 0,
            MatchData.get(), nullptr);
}

static pointer regex_match(scheme* Scheme, pointer Args)
{
    std::string_view data(argString(Scheme, Args, 1));
    auto re(cachedPattern(argString(Scheme, Args, 2), Args));
    auto matchData(matchDataFor(re));
    return (regexExec(re, matchData, data) > 0) ? Scheme->T : Scheme->F;
}

static pointer regex_matched(scheme* Scheme, pointer Args)
{
    std::string_view data(argString(Scheme, Args, 1));
    auto re(cachedPattern(argString(Scheme, Args, 2), Args));
    auto matchData(matchDataFor(re));
    int rc = regexExec(re, matchData, data);
    pointer list = Scheme->NIL;
    if (rc < 1) {
        return list;
    }
    const PCRE2_SIZE* ovector = pcre2_get_ovector_pointer(matchData.get());
    for (int i = 0; i < rc; ++i)
    {
        PCRE2_SIZE start = ovector[i * 2];
        PCRE2_SIZE end = ovector[i * 2 + 1];
        // cons itself can trigger GC before storing its arguments (new_cell
        // -> try_to_call_gc), and the GC cannot see C-frame locals: BOTH the
        // list and the just-allocated string need protection across the cons
        EnvInjector injector(Scheme, list);
        pointer s = (start == PCRE2_UNSET)
                ? mk_string(Scheme, "")
                : mk_counted_string(Scheme, data.data() + start, int(end - start));
        EnvInjector injector2(Scheme, s);
        list = cons(Scheme, s, list);
    }
    return reverse_in_place(Scheme, Scheme->NIL, list);
}

static pointer regex_match_all(scheme* Scheme, pointer Args)
{
    std::string_view data(argString(Scheme, Args, 1));
    auto re(cachedPattern(argString(Scheme, Args, 2), Args));
    auto matchData(matchDataFor(re));
    pointer list = Scheme->NIL;
    while (!data.empty()) {
        if (regexExec(re, matchData, data) < 1) {
            break;
        }
        const PCRE2_SIZE* ovector = pcre2_get_ovector_pointer(matchData.get());
        EnvInjector injector(Scheme, list);
        pointer s = mk_counted_string(
                Scheme, data.data() + ovector[0], int(ovector[1] - ovector[0]));
        EnvInjector injector2(Scheme, s);
        list = cons(Scheme, s, list);
        // a zero-width match leaves the end offset where the search started,
        // so step at least one character or this never terminates
        size_t advance = (ovector[1] > 0) ? size_t(ovector[1]) : 1;
        if (advance >= data.size()) {
            break;
        }
        data.remove_prefix(advance);
    }
    return reverse_in_place(Scheme, Scheme->NIL, list);
}

static pointer regex_split(scheme* Scheme, pointer Args)
{
    std::string_view data(argString(Scheme, Args, 1));
    auto re(cachedPattern(argString(Scheme, Args, 2), Args));
    auto matchData(matchDataFor(re));
    pointer list = Scheme->NIL;
    while (true) {
        if (data.empty() || regexExec(re, matchData, data) < 1) {
            if (!data.empty()) // append remaining chars if any left
            {
                // mk_counted_string and cons can each trigger GC: without
                // protection, `list` and the fresh string (referenced only
                // from this C frame) are collected and the cons links freed
                // cells, corrupting whatever reuses them
                EnvInjector injector(Scheme, list);
                pointer s = mk_counted_string(Scheme, data.data(), int(data.size()));
                EnvInjector injector2(Scheme, s);
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
        EnvInjector injector(Scheme, list);
        pointer s = mk_counted_string(Scheme, data.data(), int(piece));
        EnvInjector injector2(Scheme, s);
        list = cons(Scheme, s, list);
        if (advance >= data.size()) {
            return reverse_in_place(Scheme, Scheme->NIL, list);
        }
        data.remove_prefix(advance);
    }
}

// Substitution is hand-rolled rather than handed to pcre2_substitute, which
// treats three things this API has always allowed as errors: a $N naming a
// group the pattern doesn't have, a $N the match didn't fill, and a '$' that
// isn't a group reference at all.
static pointer regex_replace(scheme* Scheme, pointer Args)
{
    std::string_view data(argString(Scheme, Args, 1));
    auto re(cachedPattern(argString(Scheme, Args, 2), Args));
    const char* replace = argString(Scheme, Args, 3);
    auto matchData(matchDataFor(re));
    int rc = regexExec(re, matchData, data);
    if (rc < 1) {
        return mk_counted_string(Scheme, data.data(), int(data.size()));
    }
    const PCRE2_SIZE* ovector = pcre2_get_ovector_pointer(matchData.get());

    std::string result(data.substr(0, ovector[0]));
    for (const char* p = replace; *p;) {
        if (*p != '$') {
            result += *p++;
            continue;
        }
        ++p;
        if (*p < '0' || *p > '9') {
            result += '$';  // a '$' not followed by a digit is a literal
            continue;
        }
        uint32_t group = 0;
        while (*p >= '0' && *p <= '9') {
            if (group <= REGEX_MAX_GROUP) {
                group = group * 10 + uint32_t(*p - '0');
            }
            ++p;
        }
        // a group beyond what the pattern matched, or one this match left
        // unset, substitutes nothing
        if (group < uint32_t(rc) && ovector[group * 2] != PCRE2_UNSET) {
            result.append(data.data() + ovector[group * 2],
                    size_t(ovector[group * 2 + 1] - ovector[group * 2]));
        }
    }
    result.append(data.substr(ovector[1]));
    return mk_counted_string(Scheme, result.data(), int(result.size()));
}

std::span<const FFIEntry> regexDefs()
{
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
