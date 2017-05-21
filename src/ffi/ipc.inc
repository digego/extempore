// ipc stuff
static pointer newSchemeProcess(scheme* Scheme, pointer Args)
{
    std::string proc_name(string_value(pair_car(Args)));
    int port = ivalue(pair_cadr(Args));
    SchemeProcess* sp = new SchemeProcess(UNIV::SHARE_DIR, proc_name, port, 0);
    sp->start();
    SchemeREPL* repl = new SchemeREPL(proc_name, sp);
    if (repl->connectToProcessAtHostname("localhost", port)) {
        return pair_car(Args); //return Scheme->T;
    }
    sp->stop();
    delete sp;
    delete repl;
    return Scheme->F;
}

static pointer connectToProcess(scheme* Scheme, pointer Args)
{
    std::string host_name(string_value(pair_car(Args)));
    std::string proc_name(string_value(pair_cadr(Args)));
    int port = ivalue(pair_caddr(Args));
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
    if (unlikely(!repl)) {
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
        UNIV::printSchemeCell(Scheme, tmp, closure_code(Value), true);
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
    auto repl(findREPL(string_value(pair_car(Args))));
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
    auto repl(findREPL(string_value(pair_car(Args))));
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
    auto repl(findREPL(string_value(pair_car(Args))));
    if (!repl) {
        return Scheme->F;
    }
    repl->writeString(string_value(pair_cadr(Args)));
    return Scheme->T;
}

static pointer ipcSetPriority(scheme* Scheme, pointer Args)
{
    auto repl(findREPL(string_value(pair_car(Args))));
    if (!repl || !repl->getProcess()) {
        return Scheme->F;
    }
    repl->getProcess()->setPriority(ivalue(pair_cadr(Args)));
    return Scheme->T;
}

static pointer ipcGetPriority(scheme* Scheme, pointer Args)
{
    auto repl(findREPL(string_value(pair_car(Args))));
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
        { "ipc:new", &newSchemeProcess }, \
        { "ipc:connect", &connectToProcess }, \
        { "ipc:call-async", &ipcCall }, \
        { "ipc:define", &ipcDefine }, \
        { "ipc:eval-string", &ipcEval }, \
        { "ipc:set-priority", &ipcSetPriority }, \
        { "ipc:get-priority", &ipcGetPriority }, \
        { "ipc:get-process-name", &getNameOfCurrentProcess }
