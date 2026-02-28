#ifndef _SCHEME_S7_PRIVATE_H
#define _SCHEME_S7_PRIVATE_H

#include "SchemeS7.h"
#include "EXTThread.h"
#include "EXTMonitor.h"
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

    // Legacy fields (used by dead code in EXTLLVM.cpp, called only from deleted Scheme.cpp)
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

#define pair_caar(p) pair_car(pair_car(p))
#define pair_cadr(p) pair_car(pair_cdr(p))
#define pair_cdar(p) pair_cdr(pair_car(p))
#define pair_cddr(p) pair_cdr(pair_cdr(p))
#define pair_cadar(p) pair_car(pair_cdr(pair_car(p)))
#define pair_caadr(p) pair_car(pair_car(pair_cdr(p)))
#define pair_cdaar(p) pair_cdr(pair_car(pair_car(p)))
#define pair_caddr(p) pair_car(pair_cdr(pair_cdr(p)))
#define pair_cddar(p) pair_cdr(pair_cdr(pair_car(p)))
#define pair_cdddr(p) pair_cdr(pair_cdr(pair_cdr(p)))
#define pair_cadddr(p) pair_car(pair_cdr(pair_cdr(pair_cdr(p))))
#define pair_cddddr(p) pair_cdr(pair_cdr(pair_cdr(pair_cdr(p))))
#define pair_caddddr(p) pair_car(pair_cdr(pair_cdr(pair_cdr(pair_cdr(p)))))
#define pair_cdddddr(p) pair_cdr(pair_cdr(pair_cdr(pair_cdr(pair_cdr(p)))))
#define pair_cadddddr(p) pair_car(pair_cdr(pair_cdr(pair_cdr(pair_cdr(pair_cdr(p))))))
#define pair_cddddddr(p) pair_cdr(pair_cdr(pair_cdr(pair_cdr(pair_cdr(pair_cdr(p))))))
#define pair_caddddddr(p) pair_car(pair_cdr(pair_cdr(pair_cdr(pair_cdr(pair_cdr(pair_cdr(p)))))))

#endif
