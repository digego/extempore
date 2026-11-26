#pragma once

// Regular expressions, on PCRE2: the one engine behind the Scheme regex:
// functions (src/ffi/regex.cpp), the xtlang ones (rmatch and the rest, below)
// and the --device option.

// pcre2 is width-agnostic: the code unit width has to be picked before the
// header, and the 8-bit library is the one Extempore builds.
#define PCRE2_CODE_UNIT_WIDTH 8
#include <pcre2.h>

#include <cstddef>
#include <cstdint>
#include <expected>
#include <memory>
#include <string>
#include <string_view>

#include "UNIV.h"

namespace extemp::regex {

// A compiled pattern is read-only during a match, so one copy can be shared
// across threads; shared_ptr keeps a pattern alive for a call in progress even
// if the cache drops it.
using PatternPtr = std::shared_ptr<pcre2_code>;

// pcre2 writes the match offsets into a match_data block sized from the pattern
// it was made for. That block is per-match state rather than shared, so each
// caller makes its own and reuses it across the iterations of a loop.
struct MatchDataDeleter {
    void operator()(pcre2_match_data* MatchData) const {
        pcre2_match_data_free(MatchData);
    }
};

using MatchDataPtr = std::unique_ptr<pcre2_match_data, MatchDataDeleter>;

// Why pcre2 rejected a pattern, and where in it.
struct CompileError {
    std::size_t offset;
    std::string message;
};

// The compiled pattern, from a cache of them.
std::expected<PatternPtr, CompileError> compile(const char* Pattern);

MatchDataPtr matchDataFor(const PatternPtr& Re);

// Searches Data from Start for the first match, leaving its offsets in
// MatchData. Sizing the match data from the pattern means the ovector always
// has room for every capture, so this returns the capture count (the whole
// match counting as one) or a negative pcre2 code, never 0.
int search(const PatternPtr& Re, const MatchDataPtr& MatchData, std::string_view Data,
           std::size_t Start = 0);

// Data with its first match replaced, or Data as it is when nothing matches.
// $N in Replacement is the match's Nth group, and any other '$' is itself.
std::string replaceFirst(const PatternPtr& Re, std::string_view Data, const char* Replacement);

}  // namespace extemp::regex

// The xtlang regex functions (libs/core/adt.xtm). A pattern pcre2 rejects
// matches nothing, and says why in a warning.
//
// rmatches gives each match as a malloc'd string. rsplit writes what lies
// either side of the first match into a and b, which the caller makes 2048
// bytes each, and rreplace writes its result into a 4096-byte buffer.
EXPORT bool rmatch(const char* regex, const char* str);
EXPORT int64_t rmatches(const char* regex, const char* str, char** results, int64_t maxnum);
EXPORT bool rsplit(const char* regex, const char* str, char* a, char* b);
EXPORT char* rreplace(const char* regex, const char* str, const char* replacement, char* result);
