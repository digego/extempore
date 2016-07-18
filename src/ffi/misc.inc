template <typename T>
static T* getPtr(pointer Args) {
    return reinterpret_cast<T*>(reinterpret_cast<uintptr_t>(cptr_value(pair_car(Args))) + ivalue(pair_cadr(Args)));
}

static pointer dataGETi64(scheme* Scheme, pointer Args)
{
    return mk_integer(Scheme, *getPtr<int64_t>(Args));
}

static pointer dataGETdouble(scheme* Scheme, pointer Args)
{
    return mk_real(Scheme, *getPtr<double>(Args));
}

static pointer dataGETfloat(scheme* Scheme, pointer Args)
{
    return mk_real(Scheme, *getPtr<float>(Args));
}

static pointer dataSETi64(scheme* Scheme, pointer Args)
{
    *getPtr<int64_t>(Args) = ivalue(pair_caddr(Args));
    return Scheme->T;
}

static pointer dataSETdouble(scheme* Scheme, pointer Args)
{
    *getPtr<double>(Args) = rvalue(pair_caddr(Args));
    return Scheme->T;
}

static pointer dataSETfloat(scheme* Scheme, pointer Args)
{
    *getPtr<float>(Args) = rvalue(pair_caddr(Args));
    return Scheme->T;
}

static pointer cptrToString(scheme* Scheme, pointer Args)
{
    return mk_string(Scheme, reinterpret_cast<char*>(cptr_value(pair_car(Args))));
}

static pointer stringToCptr(scheme* Scheme, pointer Args)
{
    auto cstr(reinterpret_cast<char*>(cptr_value(pair_car(Args))));
    return mk_cptr(Scheme, strdup(cstr));;
}

static char* cstrstrip(char* String)
{
    while (*String == ' ') {
        ++String;
    }
    char* end = String + strlen(String) - 1;
    while (end >= String && *end == ' ') {
        --end;
    }
    *(end + 1) = '\0';
    return String;
}

static pointer stringStrip(scheme* Scheme, pointer Args)
{
    return mk_string(Scheme, cstrstrip(string_value(pair_car(Args))));
}

static pointer stringHash(scheme* Scheme, pointer Args)
{
    return mk_integer(Scheme, string_hash(string_value(pair_car(Args))));
}

static pointer Base64Encode(scheme* Scheme, pointer Args)
{
    auto dat(reinterpret_cast<unsigned char*>(cptr_value(pair_car(Args))));
    size_t datlength = ivalue(pair_cadr(Args));
    size_t lth = 0;
    return mk_string(Scheme, base64_encode(dat, datlength, &lth));
}

static pointer Base64Decode(scheme* Scheme, pointer Args)
{
    char* str = string_value(pair_car(Args));
    size_t lth = 0;
    return mk_string(Scheme, reinterpret_cast<char*>(base64_decode(str, strlen(str), &lth)));
}

static pointer CNameEncode(scheme* Scheme, pointer Args)
{
    char* str = string_value(pair_car(Args));
    size_t lth = 0;
    return mk_string(Scheme, cname_encode(str, strlen(str), &lth));
}

static pointer CNameDecode(scheme* Scheme, pointer Args)
{
    char* str = string_value(pair_car(Args));
    size_t lth = 0;
    return mk_string(Scheme, cname_decode(str, strlen(str), &lth));
}

static pointer stringJoin(scheme* Scheme, pointer Args)
{
    pointer array = pair_car(Args);
    if (unlikely(array == Scheme->NIL)) {
        return mk_string(Scheme, "");
    }
    char* joinstr = string_value(pair_cadr(Args));
    int len = 0;
    unsigned count = 0;
    for (; array != Scheme->NIL; array = pair_cdr(array), ++count)
    {
        len += strlen(string_value(pair_car(array)));
    }
    array = pair_car(Args); // reset array
    len += (count - 1) * strlen(joinstr);
    char* result = reinterpret_cast<char*>(alloca(len + 1));
    result[0] = '\0';
    for (; array != Scheme->NIL; array = pair_cdr(array))
    {
        strcat(result, string_value(pair_car(array)));
        if (pair_cdr(array) != Scheme->NIL) {
            strcat(result, joinstr);
        }
    }
    return mk_string(Scheme, result);
}

static pointer callCPPAtTime(scheme* Scheme, pointer Args)
{
    if (unlikely(!is_cptr(pair_caddr(Args)))) {
        printf("Bad task needs valid CM ptr");
        return Scheme->F;
    }
    auto task_type = ivalue(pair_cadr(Args));
    // is_render_thread_type are for tasks that must be called in the render thread
    // primarily for midi calls to an AU that must occur on the audio thread
    if (task_type == 1) {
        std::cout << "NO RENDER THREAD AVAILABLE FOR CALLBACKS" << std::endl;
    } else {
        bool is_callback = (task_type == 2);
        auto obj(new SchemeObj(Scheme, pair_cadddr(Args), pair_caddddr(Args)));
        if (is_pair(pair_car(Args))) {
            extemp::TaskScheduler::I()->addTask(ivalue(pair_caar(Args)), ivalue(pair_cdar(Args)),
                    reinterpret_cast<extemp::CM*>(cptr_value(pair_caddr(Args))), obj, 0, is_callback);
        } else {
            extemp::TaskScheduler::I()->addTask(ivalue(pair_car(Args)), ivalue(pair_car(Args)) +
                    Scheme->call_default_time, reinterpret_cast<extemp::CM*>(cptr_value(pair_caddr(Args))), obj,
                    0, is_callback);
        }
    }
    return Scheme->T;
}

static pointer getTime(scheme* Scheme, pointer Args)
{
    return mk_integer(Scheme, extemp::UNIV::TIME);
}

static std::string extractCell(scheme* Scheme, pointer Args, bool Full = false, bool StringQuotes = true)
{
    std::stringstream ss;
    UNIV::printSchemeCell(Scheme, ss, Args, Full, StringQuotes);
    auto s(ss.str());
    return s.substr(1, s.length() - 2);
}

static pointer sexprToString(scheme* Scheme, pointer Args)
{
    return mk_string(Scheme, extractCell(Scheme, Args, true).c_str());
}

static pointer print(scheme* Scheme, pointer Args)
{
    if (Args == Scheme->NIL) {
        putchar('\n');
        fflush(stdout);
        return Scheme->T;
    }
    printf("%s\n", extractCell(Scheme, Args).c_str());
    fflush(stdout);
    return Scheme->T;
}

static pointer print_no_new_line(scheme* Scheme, pointer Args)
{
    if (Args == Scheme->NIL) {
        putchar('\n');
        fflush(stdout);
        return Scheme->T;
    }
    printf("%s", extractCell(Scheme, Args, false, false).c_str());
    fflush(stdout);
    return Scheme->T;
}

static pointer printFull(scheme* Scheme, pointer Args)
{
    printf("%s\n", extractCell(Scheme, Args, true).c_str());
    return Scheme->T;
}

static pointer printFullNoQuotes(scheme* Scheme, pointer Args)
{
    printf("%s\n", extractCell(Scheme, Args, true, false).c_str());
    return Scheme->T;
}

static pointer printError(scheme* Scheme, pointer Args)
{
    ascii_error();
    printf("%s\n", extractCell(Scheme, Args, true).c_str());
    ascii_normal();
    fflush(stdout);
    return Scheme->T;
}

static pointer printNotification(scheme* Scheme, pointer Args)
{
    ascii_warning();
    printf("%s\n", extractCell(Scheme, Args).c_str());
    ascii_normal();
    fflush(stdout);
    return Scheme->T;
}

static pointer getClosureEnv(scheme* Scheme, pointer Args)
{
    return closure_env(pair_car(Args));
}

static pointer scmAddForeignFunc(scheme* sc, pointer Args) {
    char* symbol_name = string_value(pair_car(Args));
    foreign_func func = foreign_func(cptr_value(pair_cadr(Args)));
    pointer ffunc = mk_foreign_func(sc, func);
    scheme_define(sc, sc->global_env, mk_symbol(sc, symbol_name), ffunc);
    return ffunc;
}

typedef std::pair<std::string, std::string> entry_type;
static entry_type sDictHistory[2];
static std::unordered_map<std::string, entry_type> sImpCirDict;

static entry_type* getEntry(const char* Key)
{
    if (!strcmp(Key, "current")) {
        return &sDictHistory[0];
    }
    if (!strcmp(Key, "previous")) {
        return &sDictHistory[1];
    }
    return &(sImpCirDict.insert(std::make_pair(Key, entry_type())).first)->second;
}

static pointer impcirGetName(scheme* Scheme, pointer Args)
{
    return mk_string(Scheme, getEntry(string_value(pair_car(Args)))->first.c_str());
}

static pointer impcirGetType(scheme* Scheme, pointer Args)
{
    return mk_string(Scheme, getEntry(string_value(pair_car(Args)))->second.c_str());
}

static pointer impcirAdd(scheme* Scheme, pointer Args)
{
    std::string key(string_value(pair_car(Args)));
    std::string name(string_value(pair_cadr(Args)));
    std::string type(string_value(pair_caddr(Args)));
    auto entry(std::make_pair(name, type));
    auto res(sImpCirDict.insert(std::make_pair(key, entry)));
    if (!res.second) {
        res.first->second = std::move(entry);
    }
    sDictHistory[1] = std::move(sDictHistory[0]);
    sDictHistory[0] = res.first->second;
    return Scheme->T;
}

static pointer string_contains(scheme* Scheme, pointer Args)
{
    char* data = string_value(pair_car(Args));
    char* find = string_value(pair_cadr(Args));
    return strstr(data, find) ? Scheme->T : Scheme->F;
}

#define MISC_DEFS \
        { "cptr:get-i64", &dataGETi64 }, \
        { "cptr:get-double", &dataGETdouble }, \
        { "cptr:get-float", &dataGETfloat }, \
        { "cptr:set-i64", &dataSETi64 }, \
        { "cptr:set-double", &dataSETdouble }, \
        { "cptr:set-float", &dataSETfloat }, \
        { "cptr->string", &cptrToString }, \
        { "cptr:get-string", &cptrToString }, \
        { "string->cptr", &stringToCptr }, \
        { "string-strip", &stringStrip }, \
        { "string-hash", &stringHash }, \
        { "base64-encode", &Base64Encode }, \
        { "base64-decode", &Base64Decode }, \
        { "cname-encode", &CNameEncode }, \
        { "cname-decode", &CNameDecode }, \
        { "string-join", &stringJoin }, \
        { "call-cpp-at-time", &callCPPAtTime }, \
        { "now", &getTime }, \
        { "sexpr->string", &sexprToString }, \
        { "println", &print }, \
        { "print", &print_no_new_line }, \
        { "print-full", &printFull }, \
        { "print-full-nq", &printFullNoQuotes }, \
        { "pprint-error", &printError }, \
        { "print-notification", &printNotification }, \
        { "get-closure-env", &getClosureEnv }, \
        { "mk-ff", &scmAddForeignFunc }, \
        { "impc:ir:getname", &impcirGetName }, \
        { "impc:ir:gettype", &impcirGetType }, \
        { "impc:ir:addtodict", &impcirAdd }, \
        { "string-contains?", &string_contains }
