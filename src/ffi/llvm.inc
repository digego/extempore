static pointer optimizeCompiles(scheme* Scheme, pointer Args)
{
    EXTLLVM::OPTIMIZE_COMPILES = (pair_car(Args) == Scheme->T);
    return Scheme->T;
}

static pointer optimizationLevel(scheme* Scheme, pointer Args)
{
    if (Args == Scheme->NIL) {
        return mk_integer(Scheme, EXTLLVM::OPTIMIZATION_LEVEL);
    }
    // Set optimization level (0-3)
    int level = ivalue(pair_car(Args));
    if (level < 0) level = 0;
    if (level > 3) level = 3;
    EXTLLVM::OPTIMIZATION_LEVEL = level;
    return mk_integer(Scheme, level);
}

static pointer jitCompileIRString(scheme* Scheme, pointer Args)
{
    auto modulePtr(jitCompile(string_value(pair_car(Args))));
    if (!modulePtr) {
        return Scheme->F;
    }
    return mk_cptr(Scheme, modulePtr);
}

static pointer ff_set_name(scheme* Scheme, pointer Args)
{
   pointer x = pair_car(Args);
   foreign_func ff = x->_object._ff;
   char* name = string_value(pair_cadr(Args));
   llvm_scheme_ff_set_name(ff, name);
   return Scheme->T;
}

static pointer ff_get_name(scheme* Scheme, pointer Args)
{
   pointer x = pair_car(Args);
   foreign_func ff = x->_object._ff;
   const char* name = llvm_scheme_ff_get_name(ff);
   return mk_string(Scheme,name);
}

static pointer get_function(scheme* Scheme, pointer Args)
{
    auto func(extemp::EXTLLVM::getFunction(string_value(pair_car(Args))));
    if (!func) {
        return Scheme->F;
    }
    return mk_cptr(Scheme, const_cast<llvm::Function*>(func));
}

static pointer get_globalvar(scheme* Scheme, pointer Args)
{
    auto var(extemp::EXTLLVM::getGlobalVariable(string_value(pair_car(Args))));
    if (!var) {
        return Scheme->F;
    }
    return mk_cptr(Scheme, const_cast<llvm::GlobalVariable*>(var));
}

static pointer get_struct_size(scheme* Scheme, pointer Args)
{
    char* struct_type_str = string_value(pair_car(Args));
    unsigned long long hash = string_hash(struct_type_str);
    char name[128];
    snprintf(name, sizeof(name), "_xtmT%llu", hash);
    char assm[1024];
    snprintf(assm, sizeof(assm), "%%%s = type %s", name, struct_type_str);

    long size = -1;
    EXTLLVM::getThreadSafeContext().withContextDo([&](llvm::LLVMContext* ctx) {
    llvm::SMDiagnostic pa;
        auto newM(llvm::parseAssemblyString(assm, pa, *ctx));
    if (!newM) {
            return;
    }
        auto type(llvm::StructType::getTypeByName(*ctx, name));
    if (!type) {
            return;
        }
        const auto& layout = newM->getDataLayout();
        size = layout.getStructLayout(type)->getSizeInBytes();
    });

    if (size < 0) {
        return Scheme->F;
    }
    return mk_integer(Scheme, size);
}

static llvm::StructType* getNamedType(const char* name) {
    llvm::StructType* result = nullptr;
    EXTLLVM::getThreadSafeContext().withContextDo([&](llvm::LLVMContext* ctx) {
        result = llvm::StructType::getTypeByName(*ctx, name);
    });
    return result;
}

static pointer get_named_struct_size(scheme* Scheme, pointer Args)
{
    auto type(getNamedType(string_value(pair_car(Args))));
    if (!type) {
        return Scheme->F;
    }
    auto& DL = EXTLLVM::JIT->getDataLayout();
    long size = DL.getStructLayout(type)->getSizeInBytes();
    return mk_integer(Scheme, size);
}

static pointer get_function_args(scheme* Scheme, pointer Args)
{
    auto func(extemp::EXTLLVM::getFunction(string_value(pair_car(Args))));
    if (!func) {
        return Scheme->F;
    }
    pointer str = mk_string(Scheme, formatLLVMType(func->getReturnType()).c_str());
    pointer p = cons(Scheme, str, Scheme->NIL);
    for (const auto& arg : func->args()) {
        {
            EnvInjector injector(Scheme, p);
            str = mk_string(Scheme, formatLLVMType(arg.getType()).c_str());
        }
        p = cons(Scheme, str, p);
    }
    return reverse_in_place(Scheme, Scheme->NIL, p);
}

static pointer get_function_varargs(scheme* Scheme, pointer Args)
{
    auto func(extemp::EXTLLVM::getFunction(string_value(pair_car(Args))));
    return (func && func->isVarArg()) ? Scheme->T : Scheme->F;
}

static pointer get_function_type(scheme* Scheme, pointer Args)
{
    auto func(extemp::EXTLLVM::getFunction(string_value(pair_car(Args))));
    if (!func) {
        return Scheme->F;
    }
    std::string typestr;
    llvm::raw_string_ostream ss(typestr);
    func->getFunctionType()->print(ss);
    return mk_string(Scheme, ss.str().c_str());
}

static pointer get_function_calling_conv(scheme* Scheme, pointer Args)
{
    auto func(extemp::EXTLLVM::getFunction(string_value(pair_car(Args))));
    if (!func) {
        return Scheme->F;
    }
    return mk_integer(Scheme, func->getCallingConv());
}

static pointer get_global_variable_type(scheme* Scheme, pointer Args)
{
    using namespace llvm;
    auto var(extemp::EXTLLVM::getGlobalVariable(string_value(pair_car(Args))));
    if (!var) {
        return Scheme->F;
    }
    std::string typestr;
    llvm::raw_string_ostream ss(typestr);
    var->getType()->print(ss);
    return mk_string(Scheme, ss.str().c_str());
}

static pointer get_function_pointer(scheme* Scheme, pointer Args)
{
    auto name(string_value(pair_car(Args)));
    auto addr = EXTLLVM::getFunctionAddress(name);
    if (!addr) {
        return Scheme->F;
    }
    return mk_cptr(Scheme, reinterpret_cast<void*>(addr));
}

static pointer remove_function(scheme* Scheme, pointer Args)
{
    const char* name = string_value(pair_car(Args));
    if (EXTLLVM::removeSymbol(name)) {
        EXTLLVM::removeFromGlobalMap(name);
        return Scheme->T;
    }
    return Scheme->F;
}

static pointer remove_global_var(scheme* Scheme, pointer Args)
{
    const char* name = string_value(pair_car(Args));
    if (EXTLLVM::removeSymbol(name)) {
        EXTLLVM::removeFromGlobalMap(name);
    return Scheme->T;
    }
    return Scheme->F;
}

static pointer erase_function(scheme* Scheme, pointer Args)
{
    const char* name = string_value(pair_car(Args));
    if (EXTLLVM::removeSymbol(name)) {
        EXTLLVM::removeFromGlobalMap(name);
    return Scheme->T;
    }
    return Scheme->F;
}

static pointer llvm_call_void_native(scheme* Scheme, pointer Args)
{
    char name[1024];
    strcpy(name, string_value(pair_car(Args)));
    strcat(name, "_native");

    auto addr = EXTLLVM::getFunctionAddress(name);
    if (!addr) {
        // Try without _native suffix.
        addr = EXTLLVM::getFunctionAddress(string_value(pair_car(Args)));
        if (!addr) {
        return Scheme->F;
    }
    }
    auto p = reinterpret_cast<void(*)(void)>(addr);
    p();
    return Scheme->T;
}

static std::atomic<uint64_t> CALL_STUB_COUNTER{0};

static pointer call_compiled(scheme* Scheme, pointer Args)
{
    // Get the llvm::Function* from the cptr argument
    auto func = reinterpret_cast<llvm::Function*>(cptr_value(pair_car(Args)));
    if (unlikely(!func)) {
        printf("No such function\n");
        return Scheme->F;
    }

    auto funcType = func->getFunctionType();
    Args = pair_cdr(Args);

    unsigned lgth = list_length(Scheme, Args);
    if (unlikely(lgth != funcType->getNumParams())) {
        printf("Wrong number of arguments for function!\n");
        return Scheme->F;
    }

    if (!extemp::EXTLLVM::JIT) {
        printf("LLVM JIT not initialized\n");
        return Scheme->F;
    }

    // Resolve the compiled function address (try with and without _native suffix)
    std::string name = func->getName().str();
    auto addr = EXTLLVM::getFunctionAddress(name);
    if (!addr) {
        addr = EXTLLVM::getFunctionAddress(name + "_native");
        if (!addr) {
            return Scheme->F;
        }
    }

    // Marshal arguments into raw values first (outside the JIT context).
    struct ArgValue {
        enum Kind { Int, Float, Double, Ptr } kind;
        llvm::Type* ty;
        uint64_t intVal;
        double doubleVal;
        float floatVal;
        void* ptrVal;
    };

    std::vector<ArgValue> argValues;
    argValues.reserve(lgth);

    pointer argList = Args;
    for (unsigned i = 0; i < lgth; ++i) {
        auto argTy = funcType->getParamType(i);
        pointer p = car(argList);
        argList = cdr(argList);

        switch (argTy->getTypeID()) {
        case llvm::Type::IntegerTyID: {
            if (unlikely(!is_integer(p))) {
                printf("Bad argument type %u\n", i);
                return Scheme->F;
            }
            argValues.push_back({ArgValue::Int, argTy, static_cast<uint64_t>(ivalue(p)), 0.0, 0.0f, nullptr});
            break;
        }
        case llvm::Type::FloatTyID: {
            if (unlikely(!is_real(p))) {
                printf("Bad argument type %u\n", i);
                return Scheme->F;
            }
            float f = static_cast<float>(rvalue(p));
            argValues.push_back({ArgValue::Float, argTy, 0u, 0.0, f, nullptr});
            break;
        }
        case llvm::Type::DoubleTyID: {
            if (unlikely(!is_real(p))) {
                printf("Bad argument type %u\n", i);
                return Scheme->F;
            }
            argValues.push_back({ArgValue::Double, argTy, 0u, rvalue(p), 0.0f, nullptr});
            break;
        }
        case llvm::Type::PointerTyID: {
            void* rawPtr = nullptr;
            if (is_string(p)) {
                rawPtr = static_cast<void*>(string_value(p));
            } else if (is_cptr(p)) {
                rawPtr = cptr_value(p);
            } else {
                printf("Bad argument type %u\n", i);
                return Scheme->F;
            }
            argValues.push_back({ArgValue::Ptr, argTy, 0u, 0.0, 0.0f, rawPtr});
            break;
        }
        default:
            printf("Unsupported argument type at index %u\n", i);
            return Scheme->F;
        }
    }

    const auto& DL = extemp::EXTLLVM::JIT->getDataLayout();
    std::string stubName = "__extemp_call_stub_" + std::to_string(CALL_STUB_COUNTER.fetch_add(1));
    std::unique_ptr<llvm::Module> module;

    // Build a tiny stub inside the thread-safe context.
    extemp::EXTLLVM::getThreadSafeContext().withContextDo([&](llvm::LLVMContext* ctx) {
        auto ptrIntTy = llvm::Type::getIntNTy(*ctx, DL.getPointerSizeInBits());

        std::vector<llvm::Value*> argConsts;
        argConsts.reserve(lgth);

        for (const auto& av : argValues) {
            switch (av.kind) {
            case ArgValue::Int:
                argConsts.push_back(llvm::ConstantInt::get(av.ty, av.intVal));
                break;
            case ArgValue::Float:
                argConsts.push_back(llvm::ConstantFP::get(av.ty, av.floatVal));
                break;
            case ArgValue::Double:
                argConsts.push_back(llvm::ConstantFP::get(av.ty, av.doubleVal));
                break;
            case ArgValue::Ptr: {
                auto ptrAsInt = llvm::ConstantInt::get(ptrIntTy, reinterpret_cast<uintptr_t>(av.ptrVal));
                argConsts.push_back(llvm::ConstantExpr::getIntToPtr(ptrAsInt, av.ty));
                break;
            }
            }
        }

        module = std::make_unique<llvm::Module>(stubName + "_module", *ctx);
        module->setDataLayout(DL);

        auto stubType = llvm::FunctionType::get(funcType->getReturnType(), false);
        auto stubFunc = llvm::Function::Create(stubType, llvm::Function::ExternalLinkage, stubName, module.get());
        stubFunc->setCallingConv(func->getCallingConv());

        auto entryBB = llvm::BasicBlock::Create(*ctx, "entry", stubFunc);
        llvm::IRBuilder<> builder(entryBB);

        auto targetAddrConst = llvm::ConstantInt::get(ptrIntTy, addr);
        auto targetPtr = llvm::ConstantExpr::getIntToPtr(targetAddrConst, llvm::PointerType::get(*ctx, 0));
        llvm::FunctionCallee callee(funcType, targetPtr);

        auto callInst = builder.CreateCall(callee, argConsts);
        callInst->setCallingConv(func->getCallingConv());

        if (funcType->getReturnType()->isVoidTy()) {
            builder.CreateRetVoid();
        } else {
            builder.CreateRet(callInst);
        }
    });

    if (!module) {
        return Scheme->F;
    }

    // JIT the stub
    auto TSM = llvm::orc::ThreadSafeModule(std::move(module), extemp::EXTLLVM::getThreadSafeContext());
    if (auto err = extemp::EXTLLVM::addTrackedModule(std::move(TSM), {stubName})) {
        std::cerr << "Failed to JIT call stub: " << llvm::toString(std::move(err)) << std::endl;
        return Scheme->F;
    }

    auto stubAddr = EXTLLVM::getFunctionAddress(stubName);
    if (!stubAddr) {
        printf("Failed to resolve call stub\n");
        return Scheme->F;
    }

    // Call the stub and marshal the result back to Scheme.
    auto retTy = funcType->getReturnType();
    switch (retTy->getTypeID()) {
    case llvm::Type::FloatTyID: {
        using StubFn = float(*)();
        float res = reinterpret_cast<StubFn>(stubAddr)();
        return mk_real(Scheme, res);
    }
    case llvm::Type::DoubleTyID: {
        using StubFn = double(*)();
        double res = reinterpret_cast<StubFn>(stubAddr)();
        return mk_real(Scheme, res);
    }
    case llvm::Type::IntegerTyID: {
        auto intTy = llvm::cast<llvm::IntegerType>(retTy);
        unsigned width = intTy->getBitWidth();
        uint64_t mask = (width >= 64) ? ~uint64_t(0) : ((uint64_t(1) << width) - 1);

        using StubFn = uint64_t(*)();
        uint64_t res = reinterpret_cast<StubFn>(stubAddr)() & mask;
        return mk_integer(Scheme, res);
    }
    case llvm::Type::PointerTyID: {
        using StubFn = void*(*)();
        void* res = reinterpret_cast<StubFn>(stubAddr)();
        return mk_cptr(Scheme, res);
    }
    case llvm::Type::VoidTyID: {
        using StubFn = void(*)();
        reinterpret_cast<StubFn>(stubAddr)();
        return Scheme->T;
    }
    default:
        printf("Unsupported return type for compiled call\n");
        return Scheme->F;
    }
}

static pointer llvm_convert_float_constant(scheme* Scheme, pointer Args)
{
    char* floatin = string_value(pair_car(Args));
    if (floatin[1] == 'x') {
        return pair_car(Args);
    }
    llvm::APFloat apf(llvm::APFloat::IEEEsingle(), llvm::StringRef(floatin));
    // TODO: if necessary, checks for inf/nan can be done here
    auto ival(llvm::APInt::doubleToBits(apf.convertToFloat()));
    return mk_string(Scheme, (std::string("0x") + llvm::utohexstr(ival.getLimitedValue(), true)).c_str());
}

static pointer llvm_convert_double_constant(scheme* Scheme, pointer Args)
{
    static_assert(sizeof(double) == sizeof(uint64_t), "sizeof(double) must be 8 bytes");
    char* floatin = string_value(pair_car(Args));
    if (floatin[1] == 'x') {
        return pair_car(Args);
    }
    llvm::APFloat apf(llvm::APFloat::IEEEdouble(), llvm::StringRef(floatin));
    // TODO: if necessary, checks for inf/nan can be done here
    auto ival(llvm::APInt::doubleToBits(apf.convertToFloat()));
    return mk_string(Scheme, (std::string("0x") + llvm::utohexstr(ival.getLimitedValue(), true)).c_str());
}

static pointer llvm_count(scheme* Scheme, pointer Args)
{
    return mk_integer(Scheme, EXTLLVM::LLVM_COUNT);
}

static pointer llvm_count_set(scheme* Scheme, pointer Args)
{
    EXTLLVM::LLVM_COUNT = ivalue(pair_car(Args));
    return llvm_count(Scheme, Args);
}

static pointer llvm_count_inc(scheme* Scheme, pointer Args)
{
    ++EXTLLVM::LLVM_COUNT;
    return llvm_count(Scheme, Args);
}

static pointer callClosure(scheme* Scheme, pointer Args)
{
    uint32_t** closure = reinterpret_cast<uint32_t**>(cptr_value(pair_car(Args)));
    auto fptr(reinterpret_cast<int64_t (*)(void*, int64_t)>(closure[0]));
    return mk_integer(Scheme, (*fptr)(closure[0], ivalue(pair_cadr(Args))));
}

static pointer printLLVMModule(scheme* Scheme, pointer Args) // TODO: This isn't used?
{
    std::string str;
    llvm::raw_string_ostream ss(str);
    if (list_length(Scheme, Args) > 0) {
        const llvm::GlobalValue* val = extemp::EXTLLVM::getGlobalValue(string_value(pair_car(Args)));
        if (!val) {
            std::cerr << "No such value found in LLVM Module" << std::endl;
            return Scheme->F;
        }
        ss << *val;
        printf("At address: %p\n%s\n",val, ss.str().c_str());
    } else {
        // Print all modules
        for (auto module : EXTLLVM::getModules()) {
            ss << *module;
        }
    }
    printf("%s", ss.str().c_str());
    return Scheme->T;
}

static pointer printLLVMFunction(scheme* Scheme, pointer Args)
{
    auto func(extemp::EXTLLVM::getFunction(string_value(pair_car(Args))));
    std::string str;
    llvm::raw_string_ostream ss(str);
    ss << *func;
    puts(ss.str().c_str());
    return Scheme->T;
}

static pointer llvm_print_all_closures(scheme* Scheme, pointer Args) // TODO
{
    char* x = string_value(pair_car(Args));
    char rgx[1024];
    strcpy(rgx, x);
    strcat(rgx, "_.*");
    for (auto module : EXTLLVM::getModules()) {
        for (const auto& func : module->getFunctionList()) {
            if (func.hasName() && rmatch(rgx, func.getName().data())) {
                std::string str;
                llvm::raw_string_ostream ss(str);
                ss << func;
                printf("\n---------------------------------------------------\n%s", ss.str().c_str());
            }
        }
    }
    return Scheme->T;
}

static pointer llvm_print_all_modules(scheme* Scheme, pointer Args) // TODO
{
    for (auto module : EXTLLVM::getModules()) {
        std::string str;
        llvm::raw_string_ostream ss(str);
        ss << *module;
        printf("\n---------------------------------------------------\n%s", ss.str().c_str());
    }
    return Scheme->T;
}

static pointer llvm_print_closure(scheme* Scheme, pointer Args) // TODO
{
    auto fname(string_value(pair_car(Args)));
    for (auto module : EXTLLVM::getModules()) {
        for (const auto& func : module->getFunctionList()) {
            if (func.hasName() && !strcmp(func.getName().data(), fname)) {
                std::string str;
                llvm::raw_string_ostream ss(str);
                ss << func;
                if (ss.str().find_first_of("{") != std::string::npos) {
                    std::cout << str << std::endl;
                }
            }
        }
    }
    return Scheme->T;
}

static pointer llvm_closure_last_name(scheme* Scheme, pointer Args)
{
    auto x(string_value(pair_car(Args)));
    char rgx[1024];
    strcpy(rgx, x);
    strcat(rgx, "__[0-9]*");
    const char* last_name(nullptr);
    for (auto module : EXTLLVM::getModules()) {
        for (const auto& func : module->getFunctionList()) {
            if (func.hasName() && rmatch(rgx, func.getName().data())) {
                last_name = func.getName().data();
            }
        }
    }
    if (last_name) {
        return mk_string(Scheme, last_name);
    }
    return Scheme->F;
}

static pointer llvm_disasm(scheme* Scheme, pointer Args)
{
    int lgth = list_length(Scheme, Args);
    int syntax = (lgth > 1) ? ivalue(pair_cadr(Args)) : 1;
    if (syntax > 1) {
      std::cout << "Syntax argument must be either 0: at&t or 1: intel" << std::endl;
      std::cout << "The default is 1: intel" << std::endl;
      syntax = 1;
    }
    auto name(llvm_closure_last_name(Scheme, Args));
    auto fptr(reinterpret_cast<unsigned char*>(cptr_value(get_function_pointer(Scheme,
            cons(Scheme, name, pair_cdr(Args))))));
    return mk_string(Scheme, extemp::EXTLLVM::llvm_disassemble(fptr, syntax));
}

static void registerJITSymbol(const char* name, void* addr) {
    if (!EXTLLVM::JIT) return;
    auto& ES = EXTLLVM::JIT->getExecutionSession();
    auto& JD = EXTLLVM::JIT->getMainJITDylib();

    llvm::orc::SymbolMap Symbols;
    Symbols[ES.intern(name)] = {
        llvm::orc::ExecutorAddr::fromPtr(addr),
        llvm::JITSymbolFlags::Exported
    };

    auto err = JD.define(llvm::orc::absoluteSymbols(std::move(Symbols)));
    if (err) {
        llvm::consumeError(std::move(err));
    }
}

static pointer bind_symbol(scheme* Scheme, pointer Args)
{
    auto library(cptr_value(pair_car(Args)));
    auto sym(string_value(pair_cadr(Args)));

#ifdef _WIN32
    auto ptr(reinterpret_cast<void*>(GetProcAddress(reinterpret_cast<HMODULE>(library), sym)));
#else
    auto ptr(dlsym(library, sym));
#endif
    if (likely(ptr)) {
        registerJITSymbol(sym, ptr);
        return Scheme->T;
    }
    return Scheme->F;
}

static pointer update_mapping(scheme* Scheme, pointer Args)
{
    auto sym(string_value(pair_car(Args)));
    auto ptr(cptr_value(pair_cadr(Args)));

    // Extempore doesn't track arbitrary symbol addresses under ORC.
    // Return 0 for "old".
    uint64_t oldaddr = 0;

    // Register new mapping
    registerJITSymbol(sym, ptr);

    return mk_cptr(Scheme, reinterpret_cast<void*>(oldaddr));
}

static std::unordered_map<std::string, std::string> LLVM_ALIAS_TABLE;

static pointer add_llvm_alias(scheme* Scheme, pointer Args)
{
    LLVM_ALIAS_TABLE[string_value(pair_car(Args))] = string_value(pair_cadr(Args));
    return Scheme->T;
}

static pointer get_llvm_alias(scheme* Scheme, pointer Args)
{
    auto iter(LLVM_ALIAS_TABLE.find(std::string(string_value(pair_car(Args)))));
    if (iter != LLVM_ALIAS_TABLE.end()) {
        return mk_string(Scheme, iter->second.c_str());
    }
    return Scheme->F;
}

static pointer get_named_type(scheme* Scheme, pointer Args)
{
    const char* name = string_value(pair_car(Args));
    if (name[0] == '%') {
        ++name;
    }
    int ptrDepth = 0;
    int len = strlen(name) - 1;
    while (len >= 0 && name[len] == '*') {
        ++ptrDepth;
        --len;
    }
    auto st = getNamedType(std::string(name, len + 1).c_str());
    if (st) {
        std::string body;
        llvm::raw_string_ostream ss(body);
        ss << "{ ";
        bool first = true;
        for (auto* elem : st->elements()) {
            if (!first) ss << ", ";
            first = false;
            elem->print(ss);
        }
        ss << " }";
        return mk_string(Scheme, (ss.str() + std::string(ptrDepth, '*')).c_str());
    }
    return Scheme->NIL;
}

static pointer list_modules(scheme* Scheme, pointer Args)
{
    // ORC keeps multiple modules. Expose the stored metadata clones.
    pointer p = Scheme->NIL;
    for (auto module : EXTLLVM::getModules()) {
        EnvInjector injector(Scheme, p);
        p = cons(Scheme, mk_cptr(Scheme, module), p);
    }
    return reverse_in_place(Scheme, Scheme->NIL, p);
}

static pointer export_llvmmodule_bitcode(scheme* Scheme, pointer Args)
{
    auto m(reinterpret_cast<llvm::Module*>(cptr_value(pair_car(Args))));
    if (!m) {
        return Scheme->F;
    }
    auto filename(string_value(pair_cadr(Args)));
#ifdef _WIN32
    std::string str;
    std::ofstream fout(filename);
    llvm::raw_string_ostream ss(str);
    ss << *m;
    std::string irStr = ss.str();
    // add dllimport (otherwise global variables won't work)
    std::string oldStr(" external global ");
    std::string newStr(" external dllimport global ");
    size_t pos = 0;
    while ((pos = irStr.find(oldStr, pos)) != std::string::npos) {
        irStr.replace(pos, oldStr.length(), newStr);
        pos += newStr.length();
    }
    // LLVM can't handle guaranteed tail call under win64 yet
    oldStr = std::string(" tail call ");
    newStr = std::string(" call ");
    pos = 0;
    while ((pos = irStr.find(oldStr, pos)) != std::string::npos) {
        irStr.replace(pos, oldStr.length(), newStr);
        pos += newStr.length();
    }
    fout << irStr; //ss.str();
    fout.close();
#else
    std::error_code errcode;
    llvm::raw_fd_ostream ss(filename, errcode, llvm::sys::fs::OF_None);
    if (errcode) {
      std::cout << errcode.message() << std::endl;
      return Scheme->F;
    }
    llvm::WriteBitcodeToFile(*m, ss);
#endif
    return Scheme->T;
}

static pointer emitModuleToObjectFile(scheme* Scheme, pointer Args)
{
    auto m = reinterpret_cast<llvm::Module*>(cptr_value(pair_car(Args)));
    if (!m) {
        return Scheme->F;
    }
    auto filename = string_value(pair_cadr(Args));

    // Get target triple from module or use host
    std::string triple = m->getTargetTriple().str();
    if (triple.empty()) {
        triple = llvm::sys::getProcessTriple();
    }

    // Look up the target
    std::string error;
    auto target = llvm::TargetRegistry::lookupTarget(triple, error);
    if (!target) {
        std::cerr << "llvm:emit-object-file: " << error << std::endl;
        return Scheme->F;
    }

    // Get CPU and features
    std::string cpu = extemp::UNIV::CPU.empty() ?
        std::string(llvm::sys::getHostCPUName()) : extemp::UNIV::CPU;

    std::string features;
    auto hostFeatures = llvm::sys::getHostCPUFeatures();
    for (auto& feature : hostFeatures) {
        if (!features.empty()) features += ",";
        features += (feature.getValue() ? "+" : "-");
        features += feature.getKey().str();
    }

    // Create target machine
    llvm::TargetOptions opt;
    opt.GuaranteedTailCallOpt = true;  // equivalent to llc -tailcallopt
    auto targetMachine = target->createTargetMachine(
        llvm::Triple(triple), cpu, features, opt,
        llvm::Reloc::PIC_,
        std::nullopt,
        llvm::CodeGenOptLevel::Aggressive);

    if (!targetMachine) {
        std::cerr << "llvm:emit-object-file: failed to create target machine" << std::endl;
        return Scheme->F;
    }

    // Set up data layout
    m->setDataLayout(targetMachine->createDataLayout());

    // Open output file
    std::error_code ec;
    llvm::raw_fd_ostream dest(filename, ec, llvm::sys::fs::OF_None);
    if (ec) {
        std::cerr << "llvm:emit-object-file: " << ec.message() << std::endl;
        delete targetMachine;
        return Scheme->F;
    }

    // Emit object file
    llvm::legacy::PassManager pass;
    if (targetMachine->addPassesToEmitFile(pass, dest, nullptr,
            llvm::CodeGenFileType::ObjectFile)) {
        std::cerr << "llvm:emit-object-file: target machine can't emit object files" << std::endl;
        delete targetMachine;
        return Scheme->F;
    }

    pass.run(*m);
    dest.flush();
    delete targetMachine;

    return Scheme->T;
}

#define LLVM_DEFS \
        { "llvm:optimize", &optimizeCompiles }, \
        { "llvm:optimization-level", &optimizationLevel }, \
        { "llvm:jit-compile-ir-string", &jitCompileIRString}, \
        { "llvm:ffi-set-name", &ff_set_name }, \
        { "llvm:ffi-get-name", &ff_get_name }, \
        { "llvm:get-function", &get_function }, \
        { "llvm:get-globalvar", &get_globalvar }, \
        { "llvm:get-struct-size", &get_struct_size }, \
        { "llvm:get-named-struct-size", &get_named_struct_size }, \
        { "llvm:get-function-args", &get_function_args }, \
        { "llvm:get-function-varargs", &get_function_varargs }, \
        { "llvm:get-function-type", &get_function_type }, \
        { "llvm:get-function-calling-conv", &get_function_calling_conv }, \
        { "llvm:get-global-variable-type", &get_global_variable_type }, \
        { "llvm:get-function-pointer", &get_function_pointer }, \
        { "llvm:remove-function", &remove_function }, \
        { "llvm:remove-globalvar", &remove_global_var }, \
        { "llvm:erase-function", &erase_function }, \
        { "llvm:call-void-func", &llvm_call_void_native }, \
        { "llvm:run", &call_compiled }, \
        { "llvm:convert-float", &llvm_convert_float_constant }, \
        { "llvm:convert-double", &llvm_convert_double_constant }, \
        { "llvm:list-modules", &list_modules }, \
        { "llvm:count", &llvm_count }, \
        { "llvm:count-set", &llvm_count_set }, \
        { "llvm:count++", &llvm_count_inc }, \
        { "llvm:call-closure", &callClosure }, \
        { "llvm:print", &llvm_print_all_modules }, \
        { "llvm:print-function", &printLLVMFunction }, \
        { "llvm:print-all-closures", &llvm_print_all_closures }, \
        { "llvm:print-closure", &llvm_print_closure }, \
        { "llvm:get-closure-work-name", &llvm_closure_last_name }, \
        { "llvm:disassemble", &llvm_disasm }, \
        { "llvm:bind-symbol", &bind_symbol }, \
        { "llvm:update-mapping", &update_mapping }, \
        { "llvm:add-llvm-alias", &add_llvm_alias }, \
        { "llvm:get-llvm-alias", &get_llvm_alias }, \
        { "llvm:get-named-type", &get_named_type }, \
        { "llvm:export-module", &export_llvmmodule_bitcode }, \
        { "llvm:emit-object-file", &emitModuleToObjectFile }
