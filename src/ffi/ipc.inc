// ipc stuff
static pointer newSchemeProcess(scheme* Scheme, pointer Args)
{
    std::string proc_name(argString(Scheme, Args, 1));
    int port = int(argInt(Scheme, Args, 2));
    SchemeProcess* sp = new SchemeProcess(UNIV::SHARE_DIR, proc_name, port, 0);
    sp->start();
    SchemeREPL* repl = new SchemeREPL(proc_name, sp);
    if (repl->connectToProcessAtHostname("localhost", port)) {
        return pair_car(Args); //return Scheme->T;
    }
    // The REPL deregisters itself from the name table; the process
    // destructor stops and joins its task and server threads.
    delete repl;
    delete sp;
    return Scheme->F;
}

static pointer connectToProcess(scheme* Scheme, pointer Args)
{
    std::string host_name(argString(Scheme, Args, 1));
    std::string proc_name(argString(Scheme, Args, 2));
    int port = int(argInt(Scheme, Args, 3));
    SchemeREPL* repl = new SchemeREPL(proc_name, nullptr);
    if (repl->connectToProcessAtHostname(host_name, port)) {
        return Scheme->T;
    }
    delete repl;
    return Scheme->F;
}

static SchemeREPL* findREPL(const std::string& Name)
{
    auto repl(SchemeREPL::I(Name));
    if (!repl) [[unlikely]] {
      std::cout << "Error: unknown scheme process '" << Name << "'" << std::endl;
      return nullptr;
   }
   return repl;
}

static bool appendArg(scheme* Scheme, std::stringstream& Stream, pointer Value)
{
    Stream << ' ';
    if (is_pair(Value) || is_vector(Value) || is_symbol(Value)) {
        Stream << "'";
        UNIV::printSchemeCell(Scheme, Stream, Value,true);
    } else if (Scheme->NIL == Value) {
        Stream << "'()";
    } else if (Value == Scheme->F) {
        Stream << "#f";
    } else if (Value == Scheme->T) {
        Stream << "#t";
    } else if (Value == Scheme->EOF_OBJ) {
        // ignore end of file
    } else if (is_closure(Value)) {
        std::stringstream tmp;
        pointer cargs = s7_closure_args(Scheme->sc, Value);
        pointer cbody = s7_closure_body(Scheme->sc, Value);
        pointer code = cons(Scheme, cargs, cbody);
        UNIV::printSchemeCell(Scheme, tmp, code, true);
        Stream << "(lambda " << tmp.str().substr(1);
    } else if (is_string(Value) || is_number(Value) || is_symbol(Value)) {
        UNIV::printSchemeCell(Scheme, Stream, Value, true);
    } else {
        PRINT_ERROR("Extempore's IPC mechanism cannot serialise this type - this maybe related to the return type as well as the arguments.\n");
        return false;
    }
    return true;
}

static pointer ipcCall(scheme* Scheme, pointer Args)
{
    auto repl(findREPL(argString(Scheme, Args, 1)));
    if (!repl) {
      return Scheme->F;
    }
    std::stringstream ss;
    pointer sym = pair_cadr(Args);
    Args = pair_cddr(Args);
    for (; is_pair(Args); Args = pair_cdr(Args)) {
        if (!appendArg(Scheme, ss, pair_car(Args))) {
            return Scheme->F;
        }
    }
    repl->writeString(std::string("(") + symname(sym) + ss.str() + ')');
    return Scheme->T;
}

static pointer ipcDefine(scheme* Scheme, pointer Args)
{
    auto repl(findREPL(argString(Scheme, Args, 1)));
    if (!repl) {
        return Scheme->F;
    }
    std::stringstream ss;
    pointer sym = pair_cadr(Args);
    if (!appendArg(Scheme, ss, pair_caddr(Args))) {
        return Scheme->F;
    }
    repl->writeString(std::string("(define ") + symname(sym) + ss.str() +')');
    return Scheme->T;
}

static pointer ipcEval(scheme* Scheme, pointer Args)
{
    auto repl(findREPL(argString(Scheme, Args, 1)));
    if (!repl) {
        return Scheme->F;
    }
    repl->writeString(argString(Scheme, Args, 2));
    return Scheme->T;
}

static pointer ipcSetPriority(scheme* Scheme, pointer Args)
{
    auto repl(findREPL(argString(Scheme, Args, 1)));
    if (!repl || !repl->getProcess()) {
        return Scheme->F;
    }
    repl->getProcess()->setPriority(argInt(Scheme, Args, 2));
    return Scheme->T;
}

static pointer ipcGetPriority(scheme* Scheme, pointer Args)
{
    auto repl(findREPL(argString(Scheme, Args, 1)));
    if (!repl || !repl->getProcess()) {
        return Scheme->F;
    }
    return mk_integer(Scheme, repl->getProcess()->getPriority());
}

static pointer getNameOfCurrentProcess(scheme* Scheme, pointer Args)
{
    return mk_string(Scheme, Scheme->m_process->getName().c_str());
}

#define IPC_DEFS \
        FFI_DEF("ipc:new", newSchemeProcess, 2, 0, false), \
        FFI_DEF("ipc:connect", connectToProcess, 3, 0, false), \
        FFI_DEF("ipc:call-async", ipcCall, 2, 0, true), \
        FFI_DEF("ipc:define", ipcDefine, 3, 0, false), \
        FFI_DEF("ipc:eval-string", ipcEval, 2, 0, false), \
        FFI_DEF("ipc:set-priority", ipcSetPriority, 2, 0, false), \
        FFI_DEF("ipc:get-priority", ipcGetPriority, 1, 0, false), \
        FFI_DEF("ipc:get-process-name", getNameOfCurrentProcess, 0, 0, false)
