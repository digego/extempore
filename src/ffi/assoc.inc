// assoc-strcmp is gone: the compiler's tables are keyed by symbols and use
// assq. The entry below only answers a stale caller with the reason, and
// goes with this file once SchemeFFI.cpp drops its include and the
// ASSOC_DEFS element of funcTable.
static pointer assocstrcmp_removed(scheme* Scheme, pointer Args)
{
    throw ScmRuntimeError(std::string(ffi_current_name())
                    + " has been removed: keys are symbols, use assq (or assoc for strings)",
            Args);
}

#define ASSOC_DEFS \
        FFI_DEF("assoc-strcmp", assocstrcmp_removed, 2, 0, false), \
        FFI_DEF("assoc-strcmp-all", assocstrcmp_removed, 2, 0, false)
