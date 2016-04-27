static pointer pointerSize(scheme* Scheme, pointer Args)
{
    return mk_integer(Scheme, 8 * sizeof(uintptr_t));
}

static pointer mcjitEnabled(scheme* Scheme, pointer Args)
{
    return Scheme->T;
}

static pointer platform(scheme* Scheme, pointer Args)
{
#ifdef __APPLE__
    return mk_string(Scheme, "OSX");
#elif __linux__
    return mk_string(Scheme, "Linux");
#elif _WIN32
    return mk_string(Scheme, "Windows");
#endif
}

static pointer getShareDir(scheme* Scheme, pointer Args)
{
    return mk_string(Scheme, UNIV::SHARE_DIR.c_str());
}

static pointer cmdarg(scheme* Scheme, pointer Args)
{
    auto iter(UNIV::CMDPARAMS.find(string_value(pair_car(Args))));
    if (iter == UNIV::CMDPARAMS.end()) {
        return Scheme->F;
    }
    return mk_string(Scheme, iter->second.c_str());
}

static pointer openDynamicLib(scheme* Scheme, pointer Args)
{
#ifdef _WIN32
    // set up the DLL load path
    SetDllDirectory(""); // Plug "binary planting" security hole.
    if (!SetDllDirectory((extemp::UNIV::SHARE_DIR + "/libs/aot-cache").c_str())) {
        std::cout << "Warning: couldn't add libs/aot-cache/ to DLL search path" << std::endl;
    }
    void* lib_handle = LoadLibraryA(string_value(pair_car(Args)));
#else
    void* lib_handle = dlopen(string_value(pair_car(Args)), RTLD_LAZY|RTLD_GLOBAL);  // TODO: RTLD_NOW
#endif
    if (!lib_handle) {
        // if an optional second argument is non-nil, print the error
        if (pair_cdr(Args) != Scheme->NIL && pair_cadr(Args) != Scheme->F)
        {
#ifdef _WIN32
            std::cout << "LoadLibrary error:" << GetLastError() << std::endl;
#else
            printf("Bad: %s\n", dlerror());
#endif
        }
        return Scheme->F;
    }
    return mk_cptr(Scheme, lib_handle);
}

static pointer closeDynamicLib(scheme* Scheme, pointer Args)
{
#ifdef _WIN32
    FreeLibrary((HMODULE)cptr_value(pair_car(Args))) ;
#else
    dlclose(cptr_value(pair_car(Args)));
#endif
    return Scheme->T;
}

static pointer symbol_pointer(scheme* Scheme, pointer Args)
{
    void* library = cptr_value(pair_car(Args));
    char* symname = string_value(pair_cadr(Args));
#ifdef _WIN32
    void* ptr = (void*) GetProcAddress(HMODULE(library), symname);
#else
    void* ptr = dlsym(library, symname);
#endif
    if (!ptr) {
        return Scheme->F;
    }
    return mk_cptr(Scheme, ptr);
}

static pointer makeCptr(scheme* Scheme, pointer Args)
{
     auto num_bytes(ivalue(pair_car(Args)));
     void* ptr = calloc(1, num_bytes);
     return mk_cptr(Scheme, ptr);
}

static pointer slurpFile(scheme* Scheme, pointer Args)
{
    std::string filename(string_value(pair_car(Args)));
    std::string sharedir_filename(UNIV::SHARE_DIR + "/" + filename);

    // check raw path first, then prepend SHARE_DIR
    std::FILE *fp = std::fopen(filename.c_str(), "rb");
    if (!fp) {
      fp = std::fopen(sharedir_filename.c_str(), "rb");
    }
    if (fp) {
        std::string contents;
        std::fseek(fp, 0, SEEK_END);
        contents.resize(std::ftell(fp));
        std::rewind(fp);
        std::fread(&contents[0], 1, contents.size(), fp);
        std::fclose(fp);
        return mk_string(Scheme, contents.c_str());
    }
    return Scheme->F;
}

static pointer dirlist(scheme* Scheme, pointer Args)
{
#ifdef _WIN32
    char* path = string_value(pair_car(Args));
    std::tr2::sys::path bpath(path);
    if (!std::tr2::sys::exists(bpath)) {
        return Scheme->NIL;
    }
    if (!std::tr2::sys::is_directory(bpath)) {
        return Scheme->NIL;
    }
    std::tr2::sys::directory_iterator end_it;
    pointer list = Scheme->NIL;
    for (std::tr2::sys::directory_iterator it(bpath); it != end_it; ++it) {
        EnvInjector injector(Scheme, list);
        pointer tlist = cons(Scheme, mk_string(Scheme, it->path().string().c_str()), list);
        list = tlist;
    }
#else
    DIR* dp;
    dp = opendir(string_value(pair_car(Args)));
    pointer list = Scheme->NIL;
    if (dp) {
        struct dirent* ep;
        while ((ep = readdir(dp))) {
            EnvInjector injector(Scheme, list);
            pointer tlist = cons(Scheme, mk_string(Scheme, ep->d_name), list);
            list = tlist;
        }
        closedir(dp);
    } else {
        perror ("Couldn't open the directory");
    }
#endif
    return reverse(Scheme,list);
}

static pointer pathExpansion(scheme* Scheme, pointer Args)
{
#ifdef _WIN32
    char* exp_path = string_value(pair_car(Args));
#else
    char exp_path[8192];
    char* path = string_value(pair_car(Args));
    if (path[0] == '~') {
        char* h = getenv("HOME");
        strcpy(exp_path, h);
        strcat(exp_path, &path[1]);
    } else{
        realpath(path, exp_path);
    }
#endif
    return mk_string(Scheme, exp_path);
}

static pointer command(scheme* Scheme, pointer Args)
{
    // NOTE: doesn't work for Windows yet
    return mk_integer(Scheme, system(string_value(pair_car(Args))));
}

static pointer commandOutput(scheme* Scheme, pointer Args)
{
    char outbuf[8192];
#ifdef _WIN32
    FILE* stream = _popen(string_value(pair_car(Args)), "r");
#else
    FILE* stream = popen(string_value(pair_car(Args)), "r");
#endif
    if (stream && fgets(outbuf, 8192, stream))
    {
#ifdef _WIN32
        _pclose(stream);
#else
        pclose(stream);
#endif
        // get rid of the final newline
        size_t len = strnlen(outbuf, 8192);
        if (len < 8192) {
           outbuf[len - 1] = '\0';
        }
        return mk_string(Scheme, outbuf); // todo: THIS GO BOOM IF 8192
    }
    return Scheme->F;
}

static pointer setEnv(scheme* Scheme, pointer Args)
{
    char* var = string_value(pair_car(Args));
    char* val = string_value(pair_cadr(Args));

    int res;
#ifdef _WIN32
    res = _putenv_s(var, val);
#else
    res = setenv(var, val, 1); // overwrite = TRUE
#endif
    return mk_integer(Scheme, res);
}

static pointer setDefaultTimeout(scheme* Scheme, pointer Args)
{
    Scheme->m_process->setMaxDuration(ivalue(pair_car(Args)));
    return Scheme->T;
}

static pointer getDefaultTimeout(scheme* Scheme, pointer Args)
{
    return mk_integer(Scheme, Scheme->m_process->getMaxDuration());
}

#define SYS_DEFS \
        { "sys:pointer-size", &pointerSize }, \
        { "sys:mcjit-enabled", &mcjitEnabled }, \
        { "sys:platform", &platform }, \
        { "sys:share-dir", &getShareDir }, \
        { "sys:cmdarg", &cmdarg }, \
        { "sys:open-dylib", &openDynamicLib }, \
        { "sys:close-dylib", &closeDynamicLib }, \
        { "sys:symbol-cptr", &symbol_pointer }, \
        { "sys:make-cptr", &makeCptr }, \
        { "sys:slurp-file", &slurpFile }, \
        { "sys:directory-list", &dirlist }, \
        { "sys:expand-path", &pathExpansion }, \
        { "sys:command", &command }, \
        { "sys:command-output", &commandOutput }, \
        { "sys:set-env", &setEnv }, \
        { "sys:set-default-timeout", &setDefaultTimeout }, \
        { "sys:get-default-timeout", &getDefaultTimeout }
