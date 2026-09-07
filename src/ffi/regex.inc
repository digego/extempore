// ovector holds one int pair per capture (plus one for the whole match), and
// pcre needs a third of it as scratch: 60 ints is 19 usable captures.
static constexpr int REGEX_OVECTOR_SIZE = 60;
static constexpr int REGEX_MAX_CAPTURES = REGEX_OVECTOR_SIZE / 3;

// A compiled pattern is read-only during pcre_exec, so one copy can be shared
// across threads; shared_ptr keeps a pattern alive for a call in progress even
// if the cache drops it.
using PcrePtr = std::shared_ptr<pcre>;

// A pattern pcre rejects gives a null PcrePtr, and the bodies below then report
// no match.
//
// That leniency is a compatibility shim, not a design choice: xtc-bind.xtm's
// generic bind-type path builds "<name>([{<:*)|$" --- an unterminated character
// class --- and depends on the non-match, so raising here fails every generic
// type definition. Once that pattern is fixed this should throw a
// ScmRuntimeError carrying pcre's message, so a typo in a compiler regex is
// loud instead of quietly matching nothing.
static PcrePtr compilePattern(const char* Pattern)
{
    const char* error = nullptr;
    int erroffset = 0;
    pcre* re = pcre_compile(Pattern, 0, &error, &erroffset, nullptr);
    return PcrePtr(re, [](pcre* p) {
        if (p) {
            pcre_free(p);
        }
    });
}

// These sit on the compiler's hot path --- every type string goes through
// several --- and pcre_compile is not cheap, so compiled patterns are cached.
// Plain bounded map rather than an LRU: the working set is the handful of
// patterns in runtime/*.xtm, and dropping the lot on overflow costs one
// recompile each. Patterns that fail to compile are cached as well, so a bad
// one isn't handed to pcre_compile on every call.
static PcrePtr cachedPattern(const char* Pattern)
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
    PcrePtr re = compilePattern(Pattern);  // compiled outside the lock
    std::lock_guard<std::mutex> lock(sPatternCacheMutex);
    if (sPatternCache.size() >= MAX_CACHED_PATTERNS) {
        sPatternCache.clear();
    }
    sPatternCache.emplace(std::move(key), re);
    return re;
}

// pcre_exec returns 0 when ovector was too small to hold every capture; it has
// still filled the whole thing.
static int regexExec(const PcrePtr& Re, std::string_view Data, int* Ovector)
{
    if (!Re) {  // the pattern didn't compile; see compilePattern
        return -1;
    }
    int rc = pcre_exec(Re.get(), nullptr, Data.data(), int(Data.size()), 0, 0, Ovector,
            REGEX_OVECTOR_SIZE);
    return (rc == 0) ? REGEX_MAX_CAPTURES : rc;
}

static pointer regex_match(scheme* Scheme, pointer Args)
{
    std::string_view data(argString(Scheme, Args, 1));
    auto re(cachedPattern(argString(Scheme, Args, 2)));
    int ovector[REGEX_OVECTOR_SIZE];
    return (regexExec(re, data, ovector) > 0) ? Scheme->T : Scheme->F;
}

static pointer regex_matched(scheme* Scheme, pointer Args)
{
    std::string_view data(argString(Scheme, Args, 1));
    auto re(cachedPattern(argString(Scheme, Args, 2)));
    int ovector[REGEX_OVECTOR_SIZE];
    int rc = regexExec(re, data, ovector);
    pointer list = Scheme->NIL;
    if (rc < 1) {
        return list;
    }
    for (int i = 0; i < rc; ++i)
    {
        int start = ovector[i * 2];
        int end = ovector[i * 2 + 1];
        // cons itself can trigger GC before storing its arguments (new_cell
        // -> try_to_call_gc), and the GC cannot see C-frame locals: BOTH the
        // list and the just-allocated string need protection across the cons
        EnvInjector injector(Scheme, list);
        pointer s = (start < 0) ? mk_string(Scheme, "")
                                : mk_counted_string(Scheme, data.data() + start, end - start);
        EnvInjector injector2(Scheme, s);
        list = cons(Scheme, s, list);
    }
    return reverse_in_place(Scheme, Scheme->NIL, list);
}

static pointer regex_match_all(scheme* Scheme, pointer Args)
{
    std::string_view data(argString(Scheme, Args, 1));
    auto re(cachedPattern(argString(Scheme, Args, 2)));
    pointer list = Scheme->NIL;
    while (!data.empty()) {
        int ovector[REGEX_OVECTOR_SIZE];
        if (regexExec(re, data, ovector) < 1) {
            break;
        }
        EnvInjector injector(Scheme, list);
        pointer s = mk_counted_string(Scheme, data.data() + ovector[0], ovector[1] - ovector[0]);
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
    auto re(cachedPattern(argString(Scheme, Args, 2)));
    pointer list = Scheme->NIL;
    while (true) {
        int ovector[REGEX_OVECTOR_SIZE];
        if (data.empty() || regexExec(re, data, ovector) < 1) {
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

static pointer regex_replace(scheme* Scheme, pointer Args)
{
    std::string_view data(argString(Scheme, Args, 1));
    auto re(cachedPattern(argString(Scheme, Args, 2)));
    const char* replace = argString(Scheme, Args, 3);
    int ovector[REGEX_OVECTOR_SIZE];
    int rc = regexExec(re, data, ovector);
    if (rc < 1) {
        return mk_counted_string(Scheme, data.data(), int(data.size()));
    }

    std::string result(data.substr(0, size_t(ovector[0])));
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
        int group = 0;
        while (*p >= '0' && *p <= '9') {
            if (group <= REGEX_MAX_CAPTURES) {
                group = group * 10 + (*p - '0');
            }
            ++p;
        }
        // a group beyond what the pattern matched substitutes nothing; the
        // index used to run off the end of ovector
        if (group < rc && ovector[group * 2] >= 0) {
            result.append(data.data() + ovector[group * 2],
                    size_t(ovector[group * 2 + 1] - ovector[group * 2]));
        }
    }
    result.append(data.substr(size_t(ovector[1])));
    return mk_counted_string(Scheme, result.data(), int(result.size()));
}

#define REGEX_DEFS \
        FFI_DEF("regex:match?", regex_match, 2, 0, false), \
        FFI_DEF("regex:matched", regex_matched, 2, 0, false), \
        FFI_DEF("regex:match-all", regex_match_all, 2, 0, false), \
        FFI_DEF("regex:split", regex_split, 2, 0, false), \
        FFI_DEF("regex:replace", regex_replace, 3, 0, false)
