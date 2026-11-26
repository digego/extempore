#include "Regex.h"

#include "Log.h"

#include <cstdlib>
#include <cstring>
#include <mutex>
#include <unordered_map>

namespace extemp::regex {

namespace {

// A $N group number is parsed as a bounded integer: the replacement string is
// user input, and a long run of digits mustn't overflow on the way to being
// rejected as a group the pattern doesn't have.
constexpr uint32_t MAX_GROUP = 100000;

std::expected<PatternPtr, CompileError> compileUncached(const char* Pattern) {
    int errorcode = 0;
    PCRE2_SIZE erroroffset = 0;
    pcre2_code* re = pcre2_compile(reinterpret_cast<PCRE2_SPTR>(Pattern), PCRE2_ZERO_TERMINATED, 0,
                                   &errorcode, &erroroffset, nullptr);
    if (!re) {
        PCRE2_UCHAR message[256];
        pcre2_get_error_message(errorcode, message, sizeof(message));
        return std::unexpected(
            CompileError{erroroffset, std::string(reinterpret_cast<const char*>(message))});
    }
    return PatternPtr(re, [](pcre2_code* Code) { pcre2_code_free(Code); });
}

// The pattern for one of the xtlang functions, or null, with a warning, when
// pcre2 rejects it.
PatternPtr xtlangPattern(const char* Function, const char* Pattern) {
    auto re = compile(Pattern);
    if (!re) {
        log::warn("{}: invalid regex \"{}\" at offset {}: {}", Function, Pattern,
                  re.error().offset, re.error().message);
        return nullptr;
    }
    return *re;
}

}  // namespace

// These sit on the compiler's hot path --- every type string goes through
// several --- and pcre2_compile is not cheap, so compiled patterns are cached.
// Plain bounded map rather than an LRU: the working set is the handful of
// patterns in runtime/*.xtm, and dropping the lot on overflow costs one
// recompile each.
std::expected<PatternPtr, CompileError> compile(const char* Pattern) {
    static constexpr size_t MAX_CACHED_PATTERNS = 512;
    static std::mutex sPatternCacheMutex;
    static std::unordered_map<std::string, PatternPtr> sPatternCache;

    std::string key(Pattern);
    {
        const std::lock_guard<std::mutex> lock(sPatternCacheMutex);
        auto it = sPatternCache.find(key);
        if (it != sPatternCache.end()) {
            return it->second;
        }
    }
    auto re = compileUncached(Pattern);  // compiled outside the lock
    if (!re) {
        return re;
    }
    const std::lock_guard<std::mutex> lock(sPatternCacheMutex);
    if (sPatternCache.size() >= MAX_CACHED_PATTERNS) {
        sPatternCache.clear();
    }
    sPatternCache.emplace(std::move(key), *re);
    return re;
}

MatchDataPtr matchDataFor(const PatternPtr& Re) {
    return MatchDataPtr(pcre2_match_data_create_from_pattern(Re.get(), nullptr));
}

int search(const PatternPtr& Re, const MatchDataPtr& MatchData, std::string_view Data,
           std::size_t Start) {
    return pcre2_match(Re.get(), reinterpret_cast<PCRE2_SPTR>(Data.data()), Data.size(), Start, 0,
                       MatchData.get(), nullptr);
}

// Substitution is hand-rolled rather than handed to pcre2_substitute, which
// treats three things this has always allowed as errors: a $N naming a group
// the pattern doesn't have, a $N the match didn't fill, and a '$' that isn't a
// group reference at all.
std::string replaceFirst(const PatternPtr& Re, std::string_view Data, const char* Replacement) {
    auto matchData(matchDataFor(Re));
    const int rc = search(Re, matchData, Data);
    if (rc < 1) {
        return std::string(Data);
    }
    const PCRE2_SIZE* ovector = pcre2_get_ovector_pointer(matchData.get());

    std::string result(Data.substr(0, ovector[0]));
    for (const char* p = Replacement; *p;) {
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
            if (group <= MAX_GROUP) {
                group = group * 10 + uint32_t(*p - '0');
            }
            ++p;
        }
        // a group beyond what the pattern matched, or one this match left
        // unset, substitutes nothing
        const size_t at = 2 * size_t{group};
        if (group < uint32_t(rc) && ovector[at] != PCRE2_UNSET) {
            result.append(Data.data() + ovector[at], size_t(ovector[at + 1] - ovector[at]));
        }
    }
    result.append(Data.substr(ovector[1]));
    return result;
}

}  // namespace extemp::regex

using extemp::regex::matchDataFor;
using extemp::regex::search;

EXPORT bool rmatch(const char* regex, const char* str) {
    const auto re = extemp::regex::xtlangPattern("rmatch", regex);
    if (!re) {
        return false;
    }
    return search(re, matchDataFor(re), str) > 0;
}

EXPORT int64_t rmatches(const char* regex, const char* str, char** results, int64_t maxnum) {
    const auto re = extemp::regex::xtlangPattern("rmatches", regex);
    if (!re) {
        return 0;
    }
    const auto matchData(matchDataFor(re));
    const std::string_view data(str);
    int64_t num = 0;
    for (size_t start = 0; num < maxnum && start <= data.size();) {
        if (search(re, matchData, data, start) < 1) {
            break;
        }
        const PCRE2_SIZE* ovector = pcre2_get_ovector_pointer(matchData.get());
        const std::string_view match = data.substr(ovector[0], ovector[1] - ovector[0]);
        char* copy = static_cast<char*>(malloc(match.size() + 1));
        memcpy(copy, match.data(), match.size());
        copy[match.size()] = '\0';
        results[num++] = copy;
        // an empty match ends where it started, so step past it
        start = match.empty() ? ovector[1] + 1 : ovector[1];
    }
    return num;
}

EXPORT bool rsplit(const char* regex, const char* str, char* a, char* b) {
    constexpr size_t BUFFER_SIZE = 2048;
    const auto re = extemp::regex::xtlangPattern("rsplit", regex);
    if (!re) {
        return false;
    }
    const auto matchData(matchDataFor(re));
    // a pattern with groups in it has never split anything
    if (search(re, matchData, str) != 1) {
        return false;
    }
    const PCRE2_SIZE* ovector = pcre2_get_ovector_pointer(matchData.get());
    const size_t before = ovector[0];
    const size_t after = strlen(str) - ovector[1];
    if (before >= BUFFER_SIZE || after >= BUFFER_SIZE) {
        return false;
    }
    memcpy(a, str, before);
    a[before] = '\0';
    memcpy(b, str + ovector[1], after);
    b[after] = '\0';
    return true;
}

// Always writes a NUL-terminated string, and the original when the result
// wouldn't fit or the pattern is invalid.
EXPORT char* rreplace(const char* regex, const char* str, const char* replacement, char* result) {
    constexpr size_t BUFFER_SIZE = 4096;
    const char* out = str;
    std::string replaced;
    if (const auto re = extemp::regex::xtlangPattern("rreplace", regex)) {
        replaced = extemp::regex::replaceFirst(re, str, replacement);
        if (replaced.length() < BUFFER_SIZE) {
            out = replaced.c_str();
        }
    }
    const size_t n = strnlen(out, BUFFER_SIZE - 1);
    memcpy(result, out, n);
    result[n] = '\0';
    return result;
}
