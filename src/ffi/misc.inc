template <typename T>
static T* getPtr(scheme* Scheme, pointer Args)
{
    return reinterpret_cast<T*>(reinterpret_cast<uintptr_t>(argCptr(Scheme, Args, 1)) +
            argInt(Scheme, Args, 2));
}

static pointer dataGETi64(scheme* Scheme, pointer Args)
{
    return mk_integer(Scheme, *getPtr<int64_t>(Scheme, Args));
}

static pointer dataGETdouble(scheme* Scheme, pointer Args)
{
    return mk_real(Scheme, *getPtr<double>(Scheme, Args));
}

static pointer dataGETfloat(scheme* Scheme, pointer Args)
{
    return mk_real(Scheme, *getPtr<float>(Scheme, Args));
}

static pointer dataSETi64(scheme* Scheme, pointer Args)
{
    *getPtr<int64_t>(Scheme, Args) = argInt(Scheme, Args, 3);
    return Scheme->T;
}

static pointer dataSETdouble(scheme* Scheme, pointer Args)
{
    *getPtr<double>(Scheme, Args) = argReal(Scheme, Args, 3);
    return Scheme->T;
}

static pointer dataSETfloat(scheme* Scheme, pointer Args)
{
    *getPtr<float>(Scheme, Args) = float(argReal(Scheme, Args, 3));
    return Scheme->T;
}

static pointer cptrToString(scheme* Scheme, pointer Args)
{
    auto cstr(reinterpret_cast<const char*>(argCptr(Scheme, Args, 1)));
    if (!cstr) {
        return Scheme->F;
    }
    return mk_string(Scheme, cstr);
}

static pointer stringToCptr(scheme* Scheme, pointer Args)
{
    auto cstr(reinterpret_cast<const char*>(argCptr(Scheme, Args, 1)));
    if (!cstr) {
        return Scheme->F;
    }
    // The copy is deliberately unowned: string->cptr hands the caller a raw
    // pointer that outlives the Scheme string, and freeing it is the caller's
    // job (usually never --- these are compiler-lifetime strings).
    return mk_cptr(Scheme, strdup(cstr));
}

static pointer stringStrip(scheme* Scheme, pointer Args)
{
    pointer arg = argAt(Scheme, Args, 1);
    const char* str;
    if (s7_is_output_port(Scheme->sc, arg)) {
        str = s7_get_output_string(Scheme->sc, arg);
    } else {
        str = string_value(arg);
    }
    if (!str || !*str) return mk_string(Scheme, "");
    while (*str == ' ') ++str;
    const char* end = str + strlen(str) - 1;
    while (end >= str && *end == ' ') --end;
    size_t len = (end >= str) ? static_cast<size_t>(end - str + 1) : 0;
    return mk_counted_string(Scheme, str, static_cast<int>(len));
}

static pointer stringHash(scheme* Scheme, pointer Args)
{
    return mk_integer(Scheme, string_hash(argString(Scheme, Args, 1)));
}

static pointer Base64Encode(scheme* Scheme, pointer Args)
{
    auto dat(reinterpret_cast<unsigned char*>(argCptr(Scheme, Args, 1)));
    size_t datlength = argInt(Scheme, Args, 2);
    size_t lth = 0;
    char* encoded = base64_encode(dat, datlength, &lth);
    if (!encoded) {
        return Scheme->F;
    }
    pointer result = mk_string(Scheme, encoded);
    free(encoded);
    return result;
}

static pointer Base64Decode(scheme* Scheme, pointer Args)
{
    char* str = argString(Scheme, Args, 1);
    size_t lth = 0;
    char* decoded = reinterpret_cast<char*>(base64_decode(str, strlen(str), &lth));
    if (!decoded) {
        return Scheme->F;
    }
    // decoded data is arbitrary bytes: take the returned length rather than
    // stopping at the first NUL
    pointer result = mk_counted_string(Scheme, decoded, int(lth));
    free(decoded);
    return result;
}

static pointer CNameEncode(scheme* Scheme, pointer Args)
{
    char* str = argString(Scheme, Args, 1);
    size_t lth = 0;
    char* encoded = cname_encode(str, strlen(str), &lth);
    if (!encoded) {
        return Scheme->F;
    }
    // cname_encode NUL-fills its trailing padding, and the encoding relies on
    // mk_string stopping there, so this one must not use the returned length
    pointer result = mk_string(Scheme, encoded);
    free(encoded);
    return result;
}

static pointer CNameDecode(scheme* Scheme, pointer Args)
{
    char* str = argString(Scheme, Args, 1);
    size_t lth = 0;
    char* decoded = cname_decode(str, strlen(str), &lth);
    if (!decoded) {
        return Scheme->F;
    }
    pointer result = mk_counted_string(Scheme, decoded, int(lth));
    free(decoded);
    return result;
}

static pointer stringJoin(scheme* Scheme, pointer Args)
{
    pointer array = argAt(Scheme, Args, 1);
    if (unlikely(!is_pair(array))) {
        return mk_string(Scheme, "");
    }
    const char* joinstr = argString(Scheme, Args, 2);
    size_t joinlen = strlen(joinstr);
    size_t len = 0;
    size_t count = 0;
    for (pointer p = array; is_pair(p); p = pair_cdr(p), ++count) {
        len += strlen(string_value(pair_car(p)));
    }
    std::string result;
    result.reserve(len + (count - 1) * joinlen);
    for (pointer p = array; is_pair(p); p = pair_cdr(p)) {
        result += string_value(pair_car(p));
        if (is_pair(pair_cdr(p))) {
            result += joinstr;
        }
    }
    return mk_counted_string(Scheme, result.data(), int(result.size()));
}

static pointer callCPPAtTime(scheme* Scheme, pointer Args)
{
    if (unlikely(!is_cptr(argAt(Scheme, Args, 3)))) {
        printf("Bad task needs valid CM ptr");
        return Scheme->F;
    }
    auto task_type = argInt(Scheme, Args, 2);
    // is_render_thread_type are for tasks that must be called in the render thread
    // primarily for midi calls to an AU that must occur on the audio thread
    if (task_type == 1) {
        std::cout << "NO RENDER THREAD AVAILABLE FOR CALLBACKS" << std::endl;
    } else {
        bool is_callback = (task_type == 2);
        auto obj(new SchemeObj(Scheme, argAt(Scheme, Args, 4), argAt(Scheme, Args, 5)));
        auto cm(reinterpret_cast<extemp::CM*>(argCptr(Scheme, Args, 3)));
        pointer when = argAt(Scheme, Args, 1);
        if (is_pair(when)) {
            extemp::TaskScheduler::I()->addTask(ivalue(pair_car(when)), ivalue(pair_cdr(when)),
                    cm, obj, 0, is_callback);
        } else {
            extemp::TaskScheduler::I()->addTask(ivalue(when),
                    ivalue(when) + Scheme->call_default_time, cm, obj, 0, is_callback);
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
  printf("ERROR:");
  ascii_normal();
  printf(" %s\n", extractCell(Scheme, Args, true, false).c_str());
  fflush(stdout);
  return Scheme->T;
}

static pointer printInfo(scheme* Scheme, pointer Args)
{
  ascii_info();
  printf("INFO:");
  ascii_normal();
  printf(" %s\n", extractCell(Scheme, Args, true, false).c_str());
  fflush(stdout);
  return Scheme->T;
}

static pointer printWarn(scheme* Scheme, pointer Args)
{
  ascii_warning();
  printf("WARN:");
  ascii_normal();
  printf(" %s\n", extractCell(Scheme, Args, true, false).c_str());
  fflush(stdout);
  return Scheme->T;
}

static pointer getClosureEnv(scheme* Scheme, pointer Args)
{
    // closure_env has no s7_scheme* to reach the funclet through, so there has
    // never been an environment to hand back here
    pointer env = closure_env(argAt(Scheme, Args, 1));
    return env ? env : Scheme->NIL;
}

static pointer scmAddForeignFunc(scheme* sc, pointer Args) {
    char* symbol_name = argString(sc, Args, 1);
    foreign_func func = foreign_func(argCptr(sc, Args, 2));
    pointer ffunc = mk_foreign_func_named(sc, func, symbol_name);
    // mk_symbol can allocate and trigger GC; without protection the fresh
    // ffunc (referenced only from this frame) is collected and the symbol
    // gets bound to a freed cell --- calls then die on whatever reuses it
    EnvInjector injector(sc, ffunc);
    scheme_define(sc, sc->global_env, mk_symbol(sc, symbol_name), ffunc);
    return ffunc;
}

typedef std::pair<std::string, std::string> entry_type;
// The codegen dictionary is written from whichever scheme process thread is
// compiling, so every access takes the lock and copies out --- handing back a
// pointer into the map would outlive the lock.
static std::mutex sImpCirDictMutex;
static entry_type sDictHistory[2];
static std::unordered_map<std::string, entry_type> sImpCirDict;

static entry_type getEntry(const char* Key)
{
    std::lock_guard<std::mutex> lock(sImpCirDictMutex);
    if (!strcmp(Key, "current")) {
        return sDictHistory[0];
    }
    if (!strcmp(Key, "previous")) {
        return sDictHistory[1];
    }
    return sImpCirDict[Key];
}

static pointer impcirGetName(scheme* Scheme, pointer Args)
{
    return mk_string(Scheme, getEntry(argString(Scheme, Args, 1)).first.c_str());
}

static pointer impcirGetType(scheme* Scheme, pointer Args)
{
    return mk_string(Scheme, getEntry(argString(Scheme, Args, 1)).second.c_str());
}

static pointer impcirAdd(scheme* Scheme, pointer Args)
{
    std::string key(argString(Scheme, Args, 1));
    entry_type entry(argString(Scheme, Args, 2), argString(Scheme, Args, 3));

    std::lock_guard<std::mutex> lock(sImpCirDictMutex);
    entry_type& slot = sImpCirDict[key];
    slot = std::move(entry);
    sDictHistory[1] = std::move(sDictHistory[0]);
    sDictHistory[0] = slot;
    return Scheme->T;
}

#define MISC_DEFS \
        FFI_DEF("cptr:get-i64", dataGETi64, 2, 0, false), \
        FFI_DEF("cptr:get-double", dataGETdouble, 2, 0, false), \
        FFI_DEF("cptr:get-float", dataGETfloat, 2, 0, false), \
        FFI_DEF("cptr:set-i64", dataSETi64, 3, 0, false), \
        FFI_DEF("cptr:set-double", dataSETdouble, 3, 0, false), \
        FFI_DEF("cptr:set-float", dataSETfloat, 3, 0, false), \
        FFI_DEF("cptr->string", cptrToString, 1, 0, false), \
        FFI_DEF("cptr:get-string", cptrToString, 1, 0, false), \
        FFI_DEF("string->cptr", stringToCptr, 1, 0, false), \
        FFI_DEF("string-strip", stringStrip, 1, 0, false), \
        FFI_DEF("string-hash", stringHash, 1, 0, false), \
        FFI_DEF("base64-encode", Base64Encode, 2, 0, false), \
        FFI_DEF("base64-decode", Base64Decode, 1, 0, false), \
        FFI_DEF("cname-encode", CNameEncode, 1, 0, false), \
        FFI_DEF("cname-decode", CNameDecode, 1, 0, false), \
        FFI_DEF("string-join", stringJoin, 2, 0, false), \
        FFI_DEF("call-cpp-at-time", callCPPAtTime, 5, 0, false), \
        FFI_DEF("now", getTime, 0, 0, false), \
        FFI_DEF("sexpr->string", sexprToString, 1, 0, true), \
        FFI_DEF("println", print, 0, 0, true), \
        FFI_DEF("print", print_no_new_line, 0, 0, true), \
        FFI_DEF("print-full", printFull, 0, 0, true), \
        FFI_DEF("print-full-nq", printFullNoQuotes, 0, 0, true), \
        FFI_DEF("print-error", printError, 0, 0, true), \
        FFI_DEF("print-info", printInfo, 0, 0, true), \
        FFI_DEF("print-warn", printWarn, 0, 0, true), \
        FFI_DEF("get-closure-env", getClosureEnv, 1, 0, false), \
        FFI_DEF("mk-ff", scmAddForeignFunc, 2, 0, false), \
        FFI_DEF("xtc:codegen:getname", impcirGetName, 1, 0, false), \
        FFI_DEF("xtc:codegen:gettype", impcirGetType, 1, 0, false), \
        FFI_DEF("xtc:codegen:addtodict", impcirAdd, 3, 0, false)
