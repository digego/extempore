#ifndef _SCHEME_S7_PRIVATE_H
#define _SCHEME_S7_PRIVATE_H

#include "SchemeS7.h"
#include "EXTThread.h"
#include "s7.h"

#include <set>
#include <string>
#include <unordered_map>

namespace extemp {
class SchemeProcess;
}

struct scheme {
    s7_scheme* sc;

    int retcode;

    extemp::SchemeProcess* m_process;

    typedef std::set<pointer> env_type;
    env_type imp_env;

    pointer value;

    pointer NIL;
    pointer T;
    pointer F;
    pointer EOF_OBJ;
    pointer global_env;
    pointer outport;

    char name[256];

    uint64_t call_start_time;
    uint64_t call_end_time;
    uint64_t call_default_time;

    // Used by find_slot_in_env in EXTLLVM.cpp
    pointer envir;
    pointer args;

    void* ext_data;

    s7_pointer output_port;
};

scheme* scheme_wrapper_from_s7(s7_scheme* raw_sc);
void scheme_register_wrapper(s7_scheme* raw_sc, scheme* wrapper);

struct EnvInjector
{
    s7_scheme* sc;
    s7_int loc;

    EnvInjector(scheme* Scheme, pointer Pointer): sc(Scheme->sc) {
        loc = s7_gc_protect(sc, Pointer);
    }
    ~EnvInjector() {
        s7_gc_unprotect_at(sc, loc);
    }
};

inline int list_length(scheme* sc, pointer a)
{
    return static_cast<int>(s7_list_length(sc->sc, a));
}

inline pointer pair_caar(pointer p) { return pair_car(pair_car(p)); }
inline pointer pair_cadr(pointer p) { return pair_car(pair_cdr(p)); }
inline pointer pair_cdar(pointer p) { return pair_cdr(pair_car(p)); }
inline pointer pair_cddr(pointer p) { return pair_cdr(pair_cdr(p)); }
inline pointer pair_cadar(pointer p) { return pair_car(pair_cdar(p)); }
inline pointer pair_caadr(pointer p) { return pair_car(pair_cadr(p)); }
inline pointer pair_cdaar(pointer p) { return pair_cdr(pair_caar(p)); }
inline pointer pair_caddr(pointer p) { return pair_car(pair_cddr(p)); }
inline pointer pair_cddar(pointer p) { return pair_cdr(pair_cdar(p)); }
inline pointer pair_cdddr(pointer p) { return pair_cdr(pair_cddr(p)); }
inline pointer pair_cadddr(pointer p) { return pair_car(pair_cdddr(p)); }
inline pointer pair_cddddr(pointer p) { return pair_cdr(pair_cdddr(p)); }
inline pointer pair_caddddr(pointer p) { return pair_car(pair_cddddr(p)); }
inline pointer pair_cdddddr(pointer p) { return pair_cdr(pair_cddddr(p)); }
inline pointer pair_cadddddr(pointer p) { return pair_car(pair_cdddddr(p)); }
inline pointer pair_cddddddr(pointer p) { return pair_cdr(pair_cdddddr(p)); }
inline pointer pair_caddddddr(pointer p) { return pair_car(pair_cddddddr(p)); }

// One entry of a *_DEFS table in src/ffi/*.inc.  Declaring the arity next to
// the name and the function is what lets s7 reject a wrong-arity call with a
// message naming the primitive, instead of the body walking off the end of the
// argument list.  REST is true only for genuinely variadic primitives.
#define FFI_DEF(NAME, FN, REQUIRED, OPTIONAL, REST) \
        { NAME, ffi_def(NAME, &FN, REQUIRED, OPTIONAL, REST) }

// Argument accessors for FFI primitive bodies.  s7 has already checked the
// arity by the time a body runs, so these only check types; a bad type raises
// a Scheme error naming the primitive and the (1-based) argument position
// rather than reading through whatever the cell happens to hold.

[[noreturn]] inline void ffiWrongType(pointer Arg, int ArgN, const char* Expected)
{
    throw ScmRuntimeError(std::string(ffi_current_name()) + ": argument " +
                          std::to_string(ArgN) + " must be " + Expected, Arg);
}

inline pointer argAt(scheme* Scheme, pointer Args, int ArgN)
{
    pointer p = Args;
    for (int i = 1; i < ArgN && is_pair(p); ++i) {
        p = pair_cdr(p);
    }
    if (!is_pair(p)) {
        throw ScmRuntimeError(std::string(ffi_current_name()) + ": missing argument " +
                              std::to_string(ArgN), Scheme->NIL);
    }
    return pair_car(p);
}

inline int64_t argInt(scheme* Scheme, pointer Args, int ArgN)
{
    pointer p = argAt(Scheme, Args, ArgN);
    if (!is_number(p)) {
        ffiWrongType(p, ArgN, "a number");
    }
    return ivalue(p);
}

inline double argReal(scheme* Scheme, pointer Args, int ArgN)
{
    pointer p = argAt(Scheme, Args, ArgN);
    if (!is_number(p)) {
        ffiWrongType(p, ArgN, "a number");
    }
    return rvalue(p);
}

// char*, not const char*, to match string_value: s7 strings are mutable and a
// few callers (regex:replace) walk the buffer with strtol.
inline char* argString(scheme* Scheme, pointer Args, int ArgN)
{
    pointer p = argAt(Scheme, Args, ArgN);
    if (!is_string(p)) {
        ffiWrongType(p, ArgN, "a string");
    }
    return string_value(p);
}

// Strings are accepted wherever a cptr is: cptr_value hands back the character
// data, which xtlang relies on for passing strings to native code.
inline void* argCptr(scheme* Scheme, pointer Args, int ArgN)
{
    pointer p = argAt(Scheme, Args, ArgN);
    if (!is_cptr_or_str(p)) {
        ffiWrongType(p, ArgN, "a cptr or string");
    }
    return cptr_value(p);
}

#endif
