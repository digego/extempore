/*
 * Copyright (c) 2011, Andrew Sorensen
 * Original credits + licence for TinyScheme and Mini-Scheme below
 *
 * All rights reserved.
 *
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * Neither the name of the authors nor other contributors may be used to endorse
 * or promote products derived from this software without specific prior written
 * permission.
 *
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 */

////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//   Originally from TinyScheme v1.35 (2005) but subsequently reworked for use in impromptu
//   Modified again before initial inclusion in Extempore project January 2011
//
//   Original TinyScheme Credits Below:
//   Dimitrios Souflis (dsouflis@acm.org)
//   Based on MiniScheme (original credits follow)
//   (MINISCM)               coded by Atsushi Moriwaki (11/5/1989)
//   (MINISCM)           E-MAIL :  moriwaki@kurims.kurims.kyoto-u.ac.jp
//   (MINISCM) This version has been modified by R.C. Secrist.
//   (MINISCM)
//   (MINISCM) Mini-Scheme is now maintained by Akira KIDA.
//   (MINISCM)
//   (MINISCM) This is a revised and modified version by Akira KIDA.
//   (MINISCM)  current version is 0.85k4 (15 May 1994)
//
//   TinyScheme v.1.35 released under MIT licence.  This file also released under MIT licence.
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#ifndef _SCHEME_PRIVATE_H
#define _SCHEME_PRIVATE_H

#include "Scheme.h"
#include "EXTThread.h"
#include "EXTMonitor.h"

#include <set>
#include <stack>
#include <list>
#include <string>

namespace extemp {

class SchemeProcess;

}

/*------------------ Ugly internals -----------------------------------*/
/*------------------ Of interest only to FFI users --------------------*/

enum scheme_port_kind {
    port_free=0,
    port_file=1,
    port_string=2,
    port_srfi6=4,
    port_input=16,
    port_output=32
};

class CharPtrLess {
public:
    bool operator() ( const char* elem1, const char* elem2) const {
        return (strcmp(elem1,elem2) < 0) ? true : false;
    }
};

struct scheme {
    /* arrays for segments */
    func_alloc malloc;
    func_dealloc free;

    /* return code */
    int retcode;
    int tracing;

    /* added for concurrent treadmill GC */
    extemp::EXTMutex* mutex;
    extemp::EXTMonitor* Treadmill_Guard;
    extemp::SchemeProcess* m_process;
    bool treadmill_flip_active;
    bool treadmill_scanner_finished;


#define CELL_SEGSIZE   1500000  /* # of cells in one segment */
#define CELL_NSEGMENT   1    /* # of segments for cells */
    char *alloc_seg[CELL_NSEGMENT];
    pointer cell_seg[CELL_NSEGMENT];
    int     last_cell_seg;
    unsigned int dark;

    //baker treadmill
    pointer treadmill_bottom;
    pointer treadmill_top;
    pointer treadmill_free;
    pointer treadmill_scan;
    bool treadmill_stop;
    extemp::EXTThread* treadmill_scan_thread;

    pointer starting_cell;

    //pointer imp_env;
    typedef std::set<pointer> env_type;
    env_type imp_env; // can't be unordered_set because of potential iterator invalidation
//#define MARK_STACK_SIZE 200000 /* size of mark stack for knuth algorithm B */
//  pointer mark_stack[MARK_STACK_SIZE];

    /* We use 4 registers. */
    pointer args;            /* register for arguments of function */
    pointer envir;           /* stack register for current environment */
    pointer code;            /* register for current code */
    pointer dump;            /* stack register for next evaluation */
    pointer value;

    pointer func_called_by_extempore; //for use in stack tracing
    pointer last_symbol_apply;
    std::stack<pointer> applied_symbol_names;

    // USED BY DUMP_STACK_COPY
    pointer tmp_dump;
    pointer tmp_args;

    int interactive_repl;    /* are we in an interactive REPL? */

    struct cell _sink;
    pointer sink;            /* when mem. alloc. fails */
    struct cell _NIL;
    pointer NIL;             /* special cell representing empty cell */
    struct cell _HASHT;
    pointer T;               /* special cell representing #t */
    struct cell _HASHF;
    pointer F;               /* special cell representing #f */
    struct cell _EOF_OBJ;
    pointer EOF_OBJ;         /* special cell representing end-of-file object */
    pointer oblist;          /* pointer to symbol table */
    pointer global_env;      /* pointer to global environment */

    /* global pointers to special symbols */
    pointer LAMBDA;               /* pointer to syntax lambda */
    pointer QUOTE;           /* pointer to syntax quote */

    pointer QQUOTE;               /* pointer to symbol quasiquote */
    pointer UNQUOTE;         /* pointer to symbol unquote */
    pointer UNQUOTESP;       /* pointer to symbol unquote-splicing */
    pointer FEED_TO;         /* => */
    pointer COLON_HOOK;      /* *colon-hook* */
    pointer ERROR_HOOK;      /* *error-hook* */
    pointer LIVECODING_ERROR_HOOK;      /* *error-hook* */
    pointer SHARP_HOOK;  /* *sharp-hook* */

    pointer free_cell;       /* pointer to top of free cells */
    uint64_t    fcells;          /* # of free cells */
    uint64_t    total_memory_allocated;          /* total number of allocated cells */
    int64_t  allocation_request;

    pointer inport;
    pointer outport;
    pointer save_inport;
    pointer loadport;

#define MAXFIL 64
    port load_stack[MAXFIL];     /* Stack of open files for port -1 (LOADing) */
    int nesting_stack[MAXFIL];
    int file_i;
    int nesting;

//  char    gc_verbose;      /* if gc_verbose is not zero, print gc status */
//    int     gc_on;
    char    no_memory;       /* Whether mem. alloc. has failed */

#define LINESIZE 1024
    char    linebuff[LINESIZE];
    //char    strbuff[256];
    char    strbuff[256000];

    FILE *tmpfp;
    int tok;
    int print_flag;
    int op;

    char error_fname[256];
    int error_position;
    char name[256];

    uint64_t call_start_time;
    uint64_t call_end_time;
    uint64_t call_default_time;

    void *ext_data;     /* For the benefit of foreign functions */
    long gensym_cnt;

    struct scheme_interface *vptr;
    void *dump_base;     /* pointer to base of allocated dump stack */
    int dump_size;       /* number of frames allocated for dump stack */
};

/* operator code */
enum scheme_opcodes {
#define _OP_DEF(A,B,C,D,E,OP) OP,
#include "OPDefines.h"
    OP_MAXDEFINED
};

#define cons(sc,a,b) _cons(sc,a,b,0)
#define immutable_cons(sc,a,b) _cons(sc,a,b,1)

struct EnvInjector
{
    scheme::env_type&          m_env;
    scheme::env_type::iterator m_iter;

    EnvInjector(scheme* Scheme, pointer Pointer): m_env(Scheme->imp_env) {
        m_iter = m_env.insert(Pointer).first;
    }
    ~EnvInjector() {
        m_env.erase(m_iter);
    }
};

extern inline int list_length(scheme *sc, pointer a)
{
    int v(0);
    for (; likely(is_pair(a)); a = cdr(a)) {
       ++v;
    }
    return likely(a == sc->NIL) ? v : -1;
}

#endif
