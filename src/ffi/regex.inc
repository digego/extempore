struct ScopedPcre
{
private:
    pcre* m_re;
public:
    ScopedPcre(const char* pattern) {
        const char *error;
        int erroffset;
        m_re = pcre_compile(pattern, 0, &error, &erroffset, nullptr); // no options, default tables
    }
    ~ScopedPcre() {
        pcre_free(m_re);
    }
    operator const pcre*() { return m_re; }
};

static pointer regex_match(scheme* Scheme, pointer Args)
{
    char* data = string_value(pair_car(Args));
    auto re(ScopedPcre(string_value(pair_cadr(Args))));
    int ovector[60];
    return (pcre_exec(re, NULL, data, strlen(data), 0, 0, ovector, 60) >= 0) ? Scheme->T : Scheme->F;
}

static pointer regex_matched(scheme* Scheme, pointer Args)
{
    char* data = string_value(pair_car(Args));
    auto re(ScopedPcre(string_value(pair_cadr(Args))));
    int ovector[60];
    int rc = pcre_exec(re, NULL, data, strlen(data), 0, 0, ovector, 60);
    pointer list = Scheme->NIL;
    if (rc < 0) {
        return list;
    }
    int* p = ovector;
    for (int i = 0; i < rc; ++i, p += 2)
    {
        if (p[0] == -1) {
            EnvInjector injector(Scheme, list);
            list = cons(Scheme, mk_string(Scheme, ""), list);
        } else {
            int range = p[1] - p[0];
            char* b = (char*) alloca(range + 1);
            memcpy(b, data + p[0], range);
            b[range] = '\0';
            EnvInjector injector(Scheme, list);
            pointer tlist = cons(Scheme, mk_string(Scheme, b), list);
            list = tlist;
        }
    }
    return reverse_in_place(Scheme, Scheme->NIL, list);
}

static pointer regex_match_all(scheme* Scheme, pointer Args)
{
    char* data = string_value(pair_car(Args));
    auto re(ScopedPcre(string_value(pair_cadr(Args))));
    pointer list = Scheme->NIL;
    while(true) {
        int ovector[60];
        int rc = pcre_exec(re, NULL, data, strlen(data), 0, 0, ovector, 60);
        if (rc < 1) {
            return reverse_in_place(Scheme, Scheme->NIL, list);
        }
        int range = ovector[1] - ovector[0];
        char* b = (char*) alloca(range + 1);
        memcpy(b, data + ovector[0], range);
        b[range] = '\0';
        EnvInjector injector(Scheme, list);
        list = cons(Scheme, mk_string(Scheme, b), list);
        data += ovector[1];
    }
    // shouldn't ever reach here!
    return Scheme->NIL;
}

static pointer regex_split(scheme* Scheme, pointer Args)
{
    char* data = string_value(pair_car(Args));
    auto re(ScopedPcre(string_value(pair_cadr(Args))));
    pointer list = Scheme->NIL;
    while (true) {
        int ovector[60];
        int rc = pcre_exec(re, NULL, data, strlen(data), 0, 0, ovector, 60);
        if (rc < 1) {
            if (data[0]) // append remaining chars if any left
            {
                list = cons(Scheme, mk_string(Scheme, data), list);
            }
            return reverse_in_place(Scheme, Scheme->NIL, list);
        }
        char* b = (char*) alloca(ovector[0] + 1);
        memcpy(b, data, ovector[0]);
        b[ovector[0]] = '\0';
        EnvInjector injector(Scheme, list);
        list = cons(Scheme, mk_string(Scheme,b),list);
        data += ovector[1];
    }
    // shouldn't ever reach here!
    return Scheme->NIL;
}

static pointer regex_replace(scheme* Scheme, pointer Args)
{
    char* data = string_value(pair_car(Args));
    auto re(ScopedPcre(string_value(pair_cadr(Args))));
    char* replace = string_value(pair_caddr(Args));
    int ovector[60];
    int rc = pcre_exec(re, NULL, data, strlen(data), 0, 0, ovector, 60);
    if (rc < 1) {
        return mk_string(Scheme, data);
    }
    char* p = strchr(replace, '$');
    if (!p) {
        return mk_string(Scheme, (std::string(data, ovector[0]) + replace + (data + ovector[1])).c_str());
    }
    // add anything from the start
    std::string result(data, ovector[0]);
    result += std::string(replace, p - replace);
    while (p) {
        int pos = strtol(p, &replace, 10);
        p = strchr(replace, '$');
        int len;
        if (p) {
            len = p - replace;
            ++p;
        } else {
            len = strlen(replace);
        }
        int range = (pos > 0) ? ovector[pos * 2 + 1] - ovector[pos * 2] : 0;
        result += std::string(data + ovector[pos * 2], range);
        result += std::string(replace, len);
    }
    // add anything from the end
    result += std::string(data + ovector[1], strlen(data)-ovector[1]);
    return mk_string(Scheme, result.c_str());
}

#define REGEX_DEFS \
        { "regex:match?", &regex_match }, \
        { "regex:matched", &regex_matched }, \
        { "regex:match-all", &regex_match_all }, \
        { "regex:split", &regex_split }, \
        { "regex:replace", &regex_replace }
