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

#pragma once

// The IR preamble: the text prepended to every xtlang compile so it can
// reference types, functions and globals from earlier compiles and the
// runtime helpers in runtime/bitcode.ll. Everything here is plain text
// processing on LLVM IR, independent of LLVM itself, so SchemeFFI.cpp can
// use it and tests/cpp-unit can exercise it directly.

#include <cctype>
#include <map>
#include <set>
#include <string>
#include <string_view>
#include <unordered_set>
#include <vector>

namespace extemp {
namespace IRPreamble {

// IR text keyed by bare name (no % or @ prefix). std::map (ordered) so the
// generated preamble is byte-deterministic across runs.
struct Entries {
    std::map<std::string, std::string> types;      // "%name = type ..." lines
    std::map<std::string, std::string> funcDecls;  // "declare ..." lines
    // The bitcode.ll helpers (casts, zone-stack thunks, ...): private
    // alwaysinline definitions, emitted into each module that calls them so
    // the optimiser inlines them. Being private they are never JIT symbols,
    // so no module owns a copy that another module could depend on.
    std::map<std::string, std::string> funcDefs;
    std::map<std::string, std::string> globals;  // "@name = external global ..." lines
};

// Insert an entry (first definition wins).
inline void add(std::map<std::string, std::string>& map, std::string name, std::string text) {
    map.emplace(std::move(name), std::move(text));
}

// Call fn(line) for each line of text, without its newline or trailing CR.
template <typename Fn> inline void forEachLine(std::string_view text, Fn fn) {
    size_t pos = 0;
    while (pos < text.size()) {
        size_t end = text.find('\n', pos);
        if (end == std::string_view::npos) {
            end = text.size();
        }
        std::string_view line = text.substr(pos, end - pos);
        if (!line.empty() && line.back() == '\r') {
            line.remove_suffix(1);
        }
        fn(line);
        pos = end + 1;
    }
}

// Call fn with each identifier `text` references: the name after every `@`
// or `%`, in LLVM's unquoted identifier syntax. Locals and labels come
// through too and simply match no preamble entry.
template <typename Fn> inline void forEachIdentifier(std::string_view text, Fn fn) {
    auto isStart = [](char c) {
        return std::isalpha(static_cast<unsigned char>(c)) || c == '_' || c == '$' || c == '.' ||
               c == '-';
    };
    auto isBody = [&](char c) { return isStart(c) || std::isdigit(static_cast<unsigned char>(c)); };
    for (size_t i = 0; i + 1 < text.size(); ++i) {
        if ((text[i] != '@' && text[i] != '%') || !isStart(text[i + 1])) {
            continue;
        }
        size_t j = i + 2;
        while (j < text.size() && isBody(text[j])) {
            ++j;
        }
        fn(text.substr(i + 1, j - i - 1));
        i = j - 1;
    }
}

// The name of the function a "declare ..." or "define ..." line introduces
// (between '@' and '('), or empty.
inline std::string_view functionName(std::string_view line) {
    size_t at = line.find('@');
    if (at == std::string_view::npos) {
        return {};
    }
    size_t paren = line.find('(', at);
    if (paren == std::string_view::npos) {
        return {};
    }
    return line.substr(at + 1, paren - at - 1);
}

// Names an IR string itself defines or declares.
struct OwnNames {
    std::unordered_set<std::string> types;
    std::unordered_set<std::string> funcs;
    std::unordered_set<std::string> globals;
};

inline OwnNames ownNames(std::string_view ir) {
    OwnNames names;
    forEachLine(ir, [&](std::string_view line) {
        if (line.empty()) {
            return;
        }
        if (line[0] == '%') {  // "%Name = type ..."
            size_t eq = line.find(" = type ");
            if (eq != std::string_view::npos) {
                names.types.emplace(line.substr(1, eq - 1));
            }
        } else if (line.substr(0, 7) == "declare" || line.substr(0, 6) == "define") {
            std::string_view name = functionName(line);
            if (!name.empty()) {
                names.funcs.emplace(name);
            }
        } else if (line[0] == '@') {  // "@name = ..."
            size_t eq = line.find(" = ");
            if (eq != std::string_view::npos) {
                names.globals.emplace(line.substr(1, eq - 1));
            }
        }
    });
    return names;
}

// Record every "%name = type ..." line of `ir` (leading whitespace allowed) in
// entries.types. LLVM's module may not preserve forward declarations or opaque
// types, so they are captured from the source text.
inline void captureTypeDefs(Entries& entries, std::string_view ir) {
    forEachLine(ir, [&](std::string_view line) {
        size_t start = line.find_first_not_of(" \t");
        size_t end = line.find_last_not_of(" \t");
        if (start == std::string_view::npos) {
            return;
        }
        std::string_view trimmed = line.substr(start, end - start + 1);
        if (trimmed.size() < 2 || trimmed[0] != '%') {
            return;
        }
        size_t eq = trimmed.find(" = type ");
        if (eq != std::string_view::npos) {
            add(entries.types, std::string(trimmed.substr(1, eq - 1)), std::string(trimmed) + "\n");
        }
    });
}

// Record every "@name = external global ..." line of `ir` in entries.globals.
// LLVM drops an unused external declaration when parsing, so they have to be
// captured from the source text for later compiles to reference.
inline void captureExternalGlobals(Entries& entries, std::string_view ir) {
    forEachLine(ir, [&](std::string_view line) {
        if (line.size() < 2 || line[0] != '@') {
            return;
        }
        size_t ext = line.find(" = external global ");
        if (ext != std::string_view::npos) {
            add(entries.globals, std::string(line.substr(1, ext - 1)), std::string(line) + "\n");
        }
    });
}

// Record the "declare" lines and "define ... { ... }" blocks of a hand-written
// IR file (runtime/bitcode.ll) in entries.funcDecls and entries.funcDefs. A
// definition runs from its "define" line to the next line starting with '}'.
inline void captureDeclaresAndDefines(Entries& entries, std::string_view text) {
    std::string discard;          // where a duplicate definition's body goes
    std::string* body = nullptr;  // the definition being collected, if any
    forEachLine(text, [&](std::string_view line) {
        if (body) {
            *body += line;
            *body += '\n';
            if (!line.empty() && line[0] == '}') {
                body = nullptr;
                discard.clear();
            }
            return;
        }
        const bool isDeclare = line.substr(0, 8) == "declare ";
        const bool isDefine = line.substr(0, 7) == "define ";
        if (!isDeclare && !isDefine) {
            return;
        }
        std::string name(functionName(line));
        if (name.empty()) {
            return;
        }
        if (isDeclare) {
            add(entries.funcDecls, std::move(name), std::string(line) + "\n");
            return;
        }
        auto [it, inserted] = entries.funcDefs.emplace(std::move(name), std::string(line) + "\n");
        body = inserted ? &it->second : &discard;  // first definition wins
    });
}

// The preamble for `ir`: the type definitions, declarations, globals and
// helpers it references, plus whatever those reference in turn, each category
// in name order (types, then declarations, then helper definitions, then
// globals). Names the IR defines or declares itself are left out, since LLVM's
// parser rejects a second declaration of an existing name (an AOT-compiled
// .ll file carries its own bind-lib declarations; a redefinition defines a
// function an earlier compile declared).
inline std::string select(const Entries& entries, std::string_view ir) {
    const OwnNames own = ownNames(ir);
    std::set<std::string> types, funcDecls, funcDefs, globals;
    std::unordered_set<std::string_view> seen;
    std::vector<std::string_view> pending;  // views into ir and the entries' text
    auto consider = [&](std::string_view name) {
        if (seen.insert(name).second) {
            pending.push_back(name);
        }
    };
    forEachIdentifier(ir, consider);
    while (!pending.empty()) {
        const std::string name(pending.back());
        pending.pop_back();
        // Include the entry for `name` from `map` (if any, and not already
        // included), queueing the identifiers its text references. Returns
        // whether the map has the name at all.
        auto include = [&](const std::map<std::string, std::string>& map,
                           std::set<std::string>& chosen,
                           const std::unordered_set<std::string>& ownSet) {
            auto it = map.find(name);
            if (it == map.end()) {
                return false;
            }
            if (!ownSet.count(name) && chosen.insert(name).second) {
                forEachIdentifier(it->second, consider);
            }
            return true;
        };
        include(entries.types, types, own.types);
        if (!include(entries.funcDefs, funcDefs, own.funcs)) {
            include(entries.funcDecls, funcDecls, own.funcs);
        }
        include(entries.globals, globals, own.globals);
    }
    std::string preamble;
    auto emit = [&](const std::map<std::string, std::string>& map,
                    const std::set<std::string>& names) {
        for (const auto& n : names) {
            preamble += map.at(n);
        }
    };
    emit(entries.types, types);
    emit(entries.funcDecls, funcDecls);
    emit(entries.funcDefs, funcDefs);
    emit(entries.globals, globals);
    return preamble;
}

}  // namespace IRPreamble
}  // namespace extemp
