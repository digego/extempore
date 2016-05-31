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


#include <inttypes.h>

#include <iostream>
#include <sstream>
#ifndef _WIN32
#include <execinfo.h>
#endif

#include "pcre.h"
#include "EXTLLVM.h"
#include "BranchPrediction.h"
//#include "EXTMonitor"
//#include "EXTThread"

//#include <iosfwd>
#include <iomanip>

#define _SCHEME_SOURCE
#include "SchemePrivate.h"
#ifndef _WIN32
#include <unistd.h>
#endif
#if USE_DL
# include "dynload.h"
#endif
#if USE_MATH
# include <math.h>
#endif
#include <limits.h>
#include <float.h>
#include <ctype.h>
#include <stddef.h>

#include "UNIV.h"
#include "SchemeProcess.h"

/* Used for documentation purposes, to signal functions in 'interface' */
#define INTERFACE

#define TOK_EOF     (-1)
#define TOK_LPAREN  0
#define TOK_RPAREN  1
#define TOK_DOT     2
#define TOK_ATOM    3
#define TOK_QUOTE   4
#define TOK_COMMENT 5
#define TOK_DQUOTE  6
#define TOK_BQUOTE  7
#define TOK_COMMA   8
#define TOK_ATMARK  9
#define TOK_SHARP   10
#define TOK_SHARP_CONST 11
#define TOK_VEC     12

# define BACKQUOTE '`'

/*
 *  Basic memory allocation units
 */

#define banner "Extempore"

#include <string.h>
#include <stdlib.h>
#ifndef macintosh
# include <malloc.h>
#else
/* static int strcmp(const char *s1, const char *s2) */
/* { */
/*   unsigned char c1, c2; */
/*   do { */
/*     c1 = tolower(*s1); */
/*     c2 = tolower(*s2); */
/*     if (c1 < c2) */
/*       return -1; */
/*     else if (c1 > c2) */
/*       return 1; */
/*     s1++, s2++; */
/*   } while (c1 != 0); */
/*   return 0; */
/* } */
#endif

#ifdef _WIN32
#define atoll _atoi64
#endif
/*
#if USE_STRLWR
static const char *strlwr(char *s) {
    const char *p=s;
//      while(*s) {
//              *s=tolower(*s);
//              s++;
//      }
    return p;
}
#endif
*/

#define strlwr(a) a

#ifndef prompt
# define prompt "> "
#endif

#ifndef InitFile
# define InitFile "init.scm"
#endif

#ifndef FIRST_CELLSEGS
# define FIRST_CELLSEGS 1
#endif


class ScmRuntimeError {
public:
  ScmRuntimeError(const char* _msg, pointer _p) {msg = _msg;p = _p;};
  const char* msg;
  pointer p;
};

static pointer _Error_1(scheme *sc, const char *s, pointer a, int location, int errnum=0);

//#define TREADMILL_DEBUG 1
//#define TREADMILL_CHECKS

inline void unlink(pointer p)
{
    pointer cw = p->_cw;
    pointer ccw = p->_ccw;
    cw->_ccw = ccw;
    ccw->_cw = cw;
    return;
}

//int missed_thread_insert = 0;
//int hit_thread_insert = 0;

static long long treadmill_inserts_per_cycle = 0;
static int last_call_to_insert_treadmill = 0;

inline void insert_treadmill(scheme* sc, pointer p)
{
    if(p->_colour == sc->dark)
    {
        return;
    }

#ifdef TREADMILL_CHECKS
    if(sc->treadmill_scanner_finished == true && sc->treadmill_flip_active == false)
    {
        std::cout << "ERROR: shouldn't be inserting a free cell now!!!" << p << std::endl;
    }
#endif


#ifdef TREADMILL_CHECKS
    uintptr_t lowptr =  (uintptr_t)((char*)sc->cell_seg[0]);
    uintptr_t highptr = (uintptr_t)((char*)sc->cell_seg[0]+(CELL_SEGSIZE * sizeof(struct cell)));
    uintptr_t actualptr = (uintptr_t)((char*)p);
    if ((actualptr > highptr) || (actualptr < lowptr)) {
      printf("Pointer not in Cell range!! %" PRIuPTR " != %" PRIuPTR ":%" PRIuPTR "\n",actualptr,lowptr,highptr);
    }
#endif

#ifdef TREADMILL_CHECKS
    if(p->_list_colour == 0)
      {
  std::cout << "ERROR[" << extemp::SchemeProcess::I(sc)->getName() <<"] should not be inserting a free cell on the grey list!!!" << p << std::endl;
  printf("last_call_to_insert_treadmill: %d\n",last_call_to_insert_treadmill);
    last_call_to_insert_treadmill = 0;
      printf("CELL: inserted is 0x%" PRIXPTR " base of memory is 0x%" PRIXPTR ":0x%" PRIXPTR "\n",(uintptr_t)p,(uintptr_t)sc->cell_seg[0],(uintptr_t)((char*)sc->cell_seg[0]+(CELL_SEGSIZE * sizeof(struct cell))));
      printf("CELL: inserted is %" PRIuPTR " base of memory is %" PRIuPTR ":%" PRIuPTR "\n",(uintptr_t)p,(uintptr_t)&(sc->cell_seg[0]),(uintptr_t)((char*)sc->cell_seg[0]+(CELL_SEGSIZE * sizeof(struct cell))));
      printf("CELL: %" PRIuPTR " of %" PRIuPTR "\n",(actualptr-lowptr),(highptr-lowptr));
      printf("left: %d  right: %d\n",p->_ccw->_list_colour,p->_cw->_list_colour);
#ifndef _WIN32
        void* callstack[128];
        int i, frames = backtrace(callstack, 128);
        char** strs = backtrace_symbols(callstack, frames);
        for (i = 0; i < frames; ++i) {
          printf("%s\n", strs[i]);
        }
        free(strs);
#endif
    }

    // if black don't insert again
    if(p->_list_colour == 3)
    {
        return;
    }

    // if grey don't insert again
    if(p->_list_colour == 2)
    {
        return;
    }
#endif

    extemp::EXTMutex::ScopedLock lock(*sc->mutex);
    // we might be locked while the treadmill flip happens
    // so when we wake up we should re-check our colour to make
    // sure that the flip hasn't already added us to the scanner list
    if(p->_colour == sc->dark)
    {
#ifdef TREADMILL_CHECKS
      //std::cout << "WARNING: inserting during flip ... this should be OK?" << p << std::endl;
#endif
        return;
    }

    pointer at = sc->treadmill_top;

#ifdef TREADMILL_CHECKS
    if(p == sc->treadmill_scan)
    {
        // the only time this should be valid is if sc->treadmill_scan == sc->treadmill_top
        if(sc->treadmill_scan == sc->treadmill_top)
        {
            std::cout << "WARNING MOVING SCAN POINTER: this should be OK because SCAN and TOP are currently equal?" << std::endl;
        }else{
            std::cout << "WARNING scan should never equal p in insert accept if scan == top  scan list colour is: " << sc->treadmill_scan->_list_colour << std::endl;
        }
    }
#endif
    if(p == sc->treadmill_top) {
        //std::cout << "WARNING MIGHT BE MOVING TOP POINTER BY MISTAKE" << std::endl;
#ifdef TREADMILL_CHECKS
        if(sc->treadmill_top->_ccw->_list_colour == 1)
        {
            sc->treadmill_top = sc->treadmill_top->_ccw;
            at = sc->treadmill_top;
        }else{
            printf("CELL COUNTER CLOCKWISE OF TOP IS NOT ECRU\n");
        }
        if(sc->treadmill_bottom == sc->treadmill_top)
            std::cout << "VERY VERY BAD - Bottom and Top met in insert_treadmill !!!!!!!  inserting(" << p << ")" << std::endl;
#else
        sc->treadmill_top = sc->treadmill_top->_ccw;
        at = sc->treadmill_top;
#endif
    }else if(p == sc->treadmill_bottom) {
        //std::cout << "WARNING MIGHT BE MOVING BOTTOM POINTER BY MISTAKE" << std::endl;
#ifdef TREADMILL_CHECKS
        if(sc->treadmill_bottom->_cw->_list_colour == 1)
        {
            if(sc->treadmill_bottom == sc->treadmill_free)
            {
                sc->treadmill_free = sc->treadmill_bottom->_cw;
            }
            sc->treadmill_bottom = sc->treadmill_bottom->_cw;
        }else{
            printf("CELL CLOCKWISE OF BOTTOM IS NOT ECRU\n");
        }
#else
        if(sc->treadmill_bottom == sc->treadmill_free)
        {
            sc->treadmill_free = sc->treadmill_bottom->_cw;
        }
        sc->treadmill_bottom = sc->treadmill_bottom->_cw;
        if(sc->treadmill_bottom == sc->treadmill_top) std::cout << "VERY VERY BAD - Bottom and Top met in insert_treadmill !!!!!!!" << std::endl;
#endif
        //if(p == at) at = sc->treadmill_bottom;
        //sc->treadmill_bottom = sc->treadmill_bottom->_cw;
    }else if(p == sc->treadmill_free) {
        std::cout << "WARNING MOVING FREE POINTER BY MISTAKE: THIS IS AN ERROR!!!!!!!" << std::endl;
        _Error_1(sc, "OUT OF MEMORY ERROR!", sc->NIL, 0, 0);
        return;
    }

    // make this sure this happens after check for p & top as at may have changed.
    if(sc->treadmill_scan == at) // this assumes that at is always sc->treadmill_top
    {
#ifdef TREADMILL_DEBUG
        std::cout << "SETTING TREADMILL_SCAN IN INSERT: " << at << " :: " << sc->treadmill_top << std::endl;
#endif
        sc->treadmill_scan = p;

#ifdef TREADMILL_CHECKS
        //printf("SCAN == TOP.  set SCAN to %p for insert\n",p);
#endif
    }

#ifdef TREADMILL_CHECKS
    p->_list_colour = 2; //set to grey
#endif

    treadmill_inserts_per_cycle++;

    unlink(p);
    p->_colour = sc->dark;
    p->_ccw = at;
    p->_cw = at->_cw;
    p->_cw->_ccw = p;
    at->_cw = p;
    //final sanity check
#ifdef TREADMILL_CHECKS
    if(p->_ccw == p || p->_cw == p)
    {
        printf("ERROR P refers to itself after insert\n");
    }
    if(at->_ccw == at || at->_cw == at)
    {
        printf("ERROR AT refers to itself after insert\n");
    }
#endif

    return;
}

/* ADJ is enough slack to align cells in a TYPE_BITS-bit boundary */
#define ADJ 32 // this is the minimum cell size in bytes!
static const unsigned TYPE_BITS = 5;
static const uint32_t T_MASKTYPE = (1 << TYPE_BITS) - 1;
static const uint32_t T_SYNTAX = 1 << 12;
static const uint32_t T_IMMUTABLE = 1 << 13;
static const uint32_t T_ATOM = 1 << 14;
static const uint32_t CLRATOM = ~T_ATOM;
static const uint32_t MARK = 1 << 15;
static const uint32_t UNMARK = ~MARK;

#if USE_MATH
static double round_per_R5RS(double x);
#endif
static int is_zero_double(double x);

static num num_zero;
static num num_one;

inline auto typeflag(pointer Ptr) -> decltype(cell::_flag)& { return Ptr->_flag; }
inline auto type(pointer Ptr) -> decltype(cell::_flag) { return typeflag(Ptr) & T_MASKTYPE; }
int pointer_type(pointer Ptr) { return type(Ptr); }
int is_string(pointer Ptr) { return type(Ptr) == T_STRING; }
int is_character(pointer Ptr) { return type(Ptr) == T_CHARACTER; }
int is_vector(pointer Ptr) { return type(Ptr) == T_VECTOR; }
int is_number(pointer Ptr) { return type(Ptr) == T_NUMBER; }
int is_symbol(pointer Ptr) { return type(Ptr) == T_SYMBOL; }
int is_port(pointer Ptr) { return type(Ptr) == T_PORT; }
int is_pair(pointer Ptr) { return type(Ptr) == T_PAIR; }
int is_environment(pointer Ptr) { return type(Ptr) == T_ENVIRONMENT; }
int is_proc(pointer Ptr) { return type(Ptr) == T_PROC; }
int is_foreign(pointer Ptr) { return type(Ptr) == T_FOREIGN; }
int is_cptr(pointer Ptr) { return type(Ptr) == T_CPTR; }
int is_cptr_or_str(pointer Ptr) { return is_cptr(Ptr) || is_string(Ptr); }
int is_syntax(pointer Ptr) { return typeflag(Ptr) & T_SYNTAX; }

int is_integer(pointer Ptr) { return Ptr->_object._number.num_type == T_INTEGER; }
int is_real(pointer Ptr) { return is_number(Ptr); }
int is_rational(pointer Ptr) { return Ptr->_object._number.num_type == T_RATIONAL; }
inline char*& strvalue(pointer Ptr) { return Ptr->_object._string._svalue; }
auto strlength(pointer Ptr) -> decltype(cell::_object._string._length)& { return Ptr->_object._string._length; }

char* string_value(pointer Ptr)
{
    if (unlikely(!is_string(Ptr))) {
      throw ScmRuntimeError("Attempting to return a string from a non-string obj", Ptr);
    }
    return strvalue(Ptr);
}

static inline const num& nvalue(pointer Ptr)
{
    return Ptr->_object._number;
}

int64_t ivalue(pointer Ptr)
{
    auto type(Ptr->_object._number.num_type);
    if (likely(type == T_INTEGER)) {
        return Ptr->_object._number.value.ivalue;
    }
    if (likely(type == T_REAL)) {
        return Ptr->_object._number.value.rvalue;
    }
    if (likely(type == T_RATIONAL)) {
        return Ptr->_object._number.value.ratvalue.n / Ptr->_object._number.value.ratvalue.d;
    }
    return 0;
}

int64_t i64value(pointer p)
{
    return ivalue(p);
}

int32_t i32value(pointer p)
{
    return ivalue(p);
}

int16_t i16value(pointer p)
{
    return ivalue(p);
}

int8_t i8value(pointer p)
{
    return ivalue(p);
}

bool i1value(scheme* _sc, pointer p)
{
    return p == _sc->T;
}

double r64value(pointer p)
{
    return rvalue(p);
}

float r32value(pointer p)
{
    return rvalue(p);
}

#define ivalue_unchecked(p)   (p)->_object._number.value.ivalue
#define rvalue_unchecked(p)   (p)->_object._number.value.rvalue
#define ratvalue_unchecked(p) (p)->_object._number.value.ratvalue
#define set_integer(p)        (p)->_object._number.num_type = T_INTEGER;
#define set_real(p)           (p)->_object._number.num_type = T_REAL;
#define set_rational(p)       (p)->_object._number.num_type = T_RATIONAL;

long long charvalue(pointer p)
{
    //if(!is_character(p)) _Error_1(sc, "Attempting to return a character from a non-character obj", p, sc->code->_debugger->_size);//[NSException raise:@"IncorrectSchemeOBJ" format:@"Attempting to return a character from a non-character obj"];
    return ivalue_unchecked(p);
}

long long charvalue_sc(scheme* sc, pointer p)
{
    if(!is_character(p)) _Error_1(sc, "Attempting to return a character from a non-character obj", p, sc->code->_debugger->_size);
    return ivalue_unchecked(p);
}

#define is_inport(p) (type(p)==T_PORT && p->_object._port->kind&port_input)
#define is_outport(p) (type(p)==T_PORT && p->_object._port->kind&port_output)

#define car(p)           ((p)->_object._cons._car)
#define cdr(p)           ((p)->_object._cons._cdr)

pointer pair_car(pointer p)
{
  if(!is_pair(p)) throw ScmRuntimeError("Attempting to access the car of a primitive",p);
    return car(p);
}

pointer pair_car_sc(scheme* sc, pointer p)
{
    if(!is_pair(p)) _Error_1(sc,"Attempting to access the car of a primitive",0,sc->code->_debugger->_size);
    return car(p);
}

pointer pair_cdr(pointer p)
{
    if(!is_pair(p)&&!is_continuation(p))
    {
      throw ScmRuntimeError("Attempting to access the cdr of a primitive",p);
    }
    return cdr(p);
}

// AS CHANGE TO CHECK VALIDITY OF P
char* symname(pointer p)
{
    if(!is_symbol(p)) {
      throw ScmRuntimeError("Attempting to return a string from non-symbol obj",p);
        //[NSException raise:@"IncorrectSchemeOBJ" format:@"Attempting to return a string from non-symbol obj"];
    }
    return strvalue(car(p));
}

inline char* symname_sc(scheme* sc,pointer p)
{
    if (unlikely(!is_symbol(p))) {
        _Error_1(sc, "Attempting to return a string from non-symbol obj", p, sc->code->_debugger->_size);
    }
    return strvalue(car(p));
}

#if USE_PLIST
inline int hasprop(pointer p)     { return (typeflag(p)&T_SYMBOL); }
#define symprop(p)       cdr(p)
#endif

//int is_objc(pointer p) { return (type(p) == T_OBJC); }

void* cptr_value(pointer p)
{
  if(!is_cptr(p)) {
     if(is_string(p)) return (void*) strvalue(p);
     else throw ScmRuntimeError("Attempting to return a cptr from a non-cptr obj",p);
        //[NSException raise:@"IncorrectSchemeOBJ" format:@"Attempting to return a cptr from a non-cptr obj"];
    }
    return p->_object._cptr;
}

void* cptr_value_sc(scheme* sc, pointer p)
{
  if(!is_cptr(p)) {
    if(!is_string(p)) {
      _Error_1(sc, "Attempting to return a cptr from a non-cptr obj", p, sc->code->_debugger->_size);
    }else{
      return strvalue(p);
    }
  }
   return p->_object._cptr;
}

inline char *syntaxname(pointer p) { return strvalue(car(p)); }
#define procnum(p)       ivalue(p)
//const char *procname(pointer x);
static const char *opcodename(int opcode);

int is_closure(pointer p)  { return (type(p)==T_CLOSURE); }
int is_macro(pointer p)    { return (type(p)==T_MACRO); }
pointer closure_code(pointer p)   { return car(p); }
pointer closure_env(pointer p)    { return cdr(p); }

int is_continuation(pointer p)    { return (type(p)==T_CONTINUATION); }
#define cont_dump(p)     cdr(p)

/* To do: promise should be forced ONCE only */
int is_promise(pointer p)  { return (type(p)==T_PROMISE); }

#define setenvironment(p)    typeflag(p) = T_ENVIRONMENT

#define is_atom(p)       (typeflag(p)&T_ATOM)
#define setatom(p)       typeflag(p) |= T_ATOM
#define clratom(p)       typeflag(p) &= CLRATOM

#define is_mark(p)       (typeflag(p)&MARK)
#define setmark(p)       typeflag(p) |= MARK
#define clrmark(p)       typeflag(p) &= UNMARK

inline int is_immutable(pointer p) { return (typeflag(p)&T_IMMUTABLE); }
inline void setimmutable(pointer p) { typeflag(p) |= T_IMMUTABLE; }

#define caar(p)          car(car(p))
#define cadr(p)          car(cdr(p))
#define cdar(p)          cdr(car(p))
#define cddr(p)          cdr(cdr(p))
#define cadar(p)         car(cdr(car(p)))
#define caddr(p)         car(cdr(cdr(p)))
#define cadaar(p)        car(cdr(car(car(p))))
#define cadddr(p)        car(cdr(cdr(cdr(p))))
#define cddddr(p)        cdr(cdr(cdr(cdr(p))))

#if USE_CHAR_CLASSIFIERS
static inline int Cisalpha(int c) { return isascii(c) && isalpha(c); }
static inline int Cisdigit(int c) { return isascii(c) && isdigit(c); }
static inline int Cisspace(int c) { return isascii(c) && isspace(c); }
static inline int Cisupper(int c) { return isascii(c) && isupper(c); }
static inline int Cislower(int c) { return isascii(c) && islower(c); }
#endif

#if USE_ASCII_NAMES
static const char *charnames[32]={
    "nul",
    "soh",
    "stx",
    "etx",
    "eot",
    "enq",
    "ack",
    "bel",
    "bs",
    "ht",
    "lf",
    "vt",
    "ff",
    "cr",
    "so",
    "si",
    "dle",
    "dc1",
    "dc2",
    "dc3",
    "dc4",
    "nak",
    "syn",
    "etb",
    "can",
    "em",
    "sub",
    "esc",
    "fs",
    "gs",
    "rs",
    "us"
};

static int is_ascii_name(const char *name, int *pc) {
    int i;
    for(i=0; i<32; i++) {
        if(strcmp(name,charnames[i])==0) {
            *pc=i;
            return 1;
        }
    }
    if(strcmp(name,"del")==0) {
        *pc=127;
        return 1;
    }
    return 0;
}

#endif

static int file_push(scheme *sc, const char *fname);
static void file_pop(scheme *sc);
static int file_interactive(scheme *sc);
static inline int is_one_of(const char *s, int c);
static int alloc_cellseg(scheme *sc, int n);
static long binary_decode(const char *s);
static inline pointer get_cell(scheme *sc, pointer a, pointer b);
static pointer _get_cell(scheme *sc, pointer a, pointer b);
static void finalize_cell(scheme *sc, pointer a);
static pointer mk_number(scheme *sc, const num& n);
static pointer mk_empty_string(scheme *sc, int len, char fill);
static char *store_string(scheme *sc, int len, const char *str, char fill);
static pointer mk_atom(scheme *sc, char *q);
static pointer mk_sharp_const(scheme *sc, char *name);
static pointer mk_port(scheme *sc, port *p);
static pointer port_from_filename(scheme *sc, const char *fn, int prop);
static pointer port_from_file(scheme *sc, FILE *, int prop);
static pointer port_from_string(scheme *sc, char *start, char *past_the_end, int prop);
static port *port_rep_from_filename(scheme *sc, const char *fn, int prop);
static port *port_rep_from_file(scheme *sc, FILE *, int prop);
static port *port_rep_from_string(scheme *sc, char *start, char *past_the_end, int prop);
static void port_close(scheme *sc, pointer p, int flag);
//static void mark(pointer a);
static void treadmill_mark_roots(scheme* sc, pointer a, pointer b);
static void* treadmill_scanner(void* obj);

static int basic_inchar(port *pt);
static int inchar(scheme *sc);
static void backchar(scheme *sc, int c);
static char   *readstr_upto(scheme *sc, char *delim);
static pointer readstrexp(scheme *sc);
static inline void skipspace(scheme *sc);
static int token(scheme *sc);
static void printslashstring(scheme *sc, char *s, int len);
static void atom2str(scheme *sc, pointer l, int f, char **pp, int *plen);
static void printatom(scheme *sc, pointer l, int f);
static pointer mk_proc(scheme *sc, enum scheme_opcodes op);
//static pointer mk_closure(scheme *sc, pointer c, pointer e);
//pointer mk_continuation(scheme *sc);

/*
static void dump_stack_mark(scheme *);
*/
static pointer dump_stack_copy(scheme *sc);
static pointer opexe_0(scheme *sc, enum scheme_opcodes op);
static pointer opexe_1(scheme *sc, enum scheme_opcodes op);
static pointer opexe_2(scheme *sc, enum scheme_opcodes op);
static pointer opexe_3(scheme *sc, enum scheme_opcodes op);
static pointer opexe_4(scheme *sc, enum scheme_opcodes op);
static pointer opexe_5(scheme *sc, enum scheme_opcodes op);
static pointer opexe_6(scheme *sc, enum scheme_opcodes op);
static void Eval_Cycle(scheme *sc, enum scheme_opcodes op);
static void assign_syntax(scheme *sc, char *name);
static int syntaxnum(pointer p);
static void assign_proc(scheme *sc, enum scheme_opcodes, char *name);
static void treadmill_flip(scheme* sc, pointer a, pointer b);

pointer reverse(scheme *sc, pointer a);
pointer reverse_in_place(scheme *sc, pointer term, pointer list);
pointer append(scheme *sc, pointer a, pointer b);
int list_length(scheme *sc, pointer a);
pointer list_ref(scheme *sc, int pos, pointer a);
int eqv(pointer a, pointer b);
int eqv_sc(scheme* sc, pointer a, pointer b);

static inline int64_t num_ivalue(const num& Val) {
    if (likely(Val.num_type == T_INTEGER)) {
        return Val.value.ivalue;
    }
    if (likely(Val.num_type == T_REAL)) {
        return Val.value.rvalue;
    }
    return Val.value.ratvalue.n / Val.value.ratvalue.d;
}

static inline double num_rvalue(const num& Val) {
    if (likely(Val.num_type == T_INTEGER)) {
        return Val.value.ivalue;
    }
    if (likely(Val.num_type == T_REAL)) {
        return Val.value.rvalue;
    }
    return double(Val.value.ratvalue.n) / Val.value.ratvalue.d;
}

static const num_type MY_ARR[] = { T_INTEGER, T_REAL, T_REAL, T_RATIONAL, T_REAL, T_INTEGER, T_RATIONAL };

static num num_add(const num& a, const num& b)
{
    num ret;
    ret.num_type = MY_ARR[a.num_type + b.num_type];
    if (likely(ret.num_type == T_INTEGER)) {
        ret.value.ivalue = a.value.ivalue + b.value.ivalue;
        return ret;
    }
    if (likely(ret.num_type == T_REAL)) {
        ret.value.rvalue = num_rvalue(a) + num_rvalue(b);
        return ret;
    }
    if (a.num_type == T_INTEGER) {
        ret.value.ratvalue.n = a.value.ivalue * b.value.ratvalue.d + b.value.ratvalue.n;
        ret.value.ratvalue.d = b.value.ratvalue.d;
    } else if (b.num_type == T_INTEGER) {
        ret.value.ratvalue.n = b.value.ivalue * a.value.ratvalue.d + a.value.ratvalue.n;
        ret.value.ratvalue.d = a.value.ratvalue.d;
    } else {
        ret.value.ratvalue.n = a.value.ratvalue.n * b.value.ratvalue.d + b.value.ratvalue.n * a.value.ratvalue.d;
        ret.value.ratvalue.d = a.value.ratvalue.d * b.value.ratvalue.d;
    }
    return ret;
}

static num num_mul(const num& a, const num& b)
{
    num ret;
    ret.num_type = MY_ARR[a.num_type + b.num_type];
    if (likely(ret.num_type == T_INTEGER)) {
        ret.value.ivalue = a.value.ivalue * b.value.ivalue;
        return ret;
    }
    if (likely(ret.num_type == T_REAL)) {
        ret.value.rvalue = num_rvalue(a) * num_rvalue(b);
        return ret;
    }
    if (a.num_type == T_INTEGER) {
        ret.value.ratvalue.n = a.value.ivalue * b.value.ratvalue.n;
        ret.value.ratvalue.d = b.value.ratvalue.d;
    } else if (b.num_type == T_INTEGER) {
        ret.value.ratvalue.n = b.value.ivalue * a.value.ratvalue.n;
        ret.value.ratvalue.d = a.value.ratvalue.d;
    } else {
        ret.value.ratvalue.n = a.value.ratvalue.n * b.value.ratvalue.n;
        ret.value.ratvalue.d = a.value.ratvalue.d * b.value.ratvalue.d;
    }
    return ret;
}

static num num_div(const num& a, const num& b)
{
    num ret;
    ret.num_type = MY_ARR[a.num_type + b.num_type];
    if (likely(ret.num_type == T_INTEGER)) {
        if (unlikely(!(a.value.ivalue % b.value.ivalue))) {
            ret.value.ivalue = a.value.ivalue / b.value.ivalue;
            return ret;
        }
        ret.num_type = T_RATIONAL;
        ret.value.ratvalue.n = a.value.ivalue;
        ret.value.ratvalue.d = b.value.ivalue;
        return ret;
    }
    if (likely(ret.num_type == T_REAL)) {
        ret.value.rvalue = num_rvalue(a) / num_rvalue(b);
        return ret;
    }
    if (a.num_type == T_INTEGER) {
        ret.value.ratvalue.n = a.value.ivalue * b.value.ratvalue.d;
        ret.value.ratvalue.d = b.value.ratvalue.n;
    } else if (b.num_type == T_INTEGER) {
        ret.value.ratvalue.n = a.value.ratvalue.n;
        ret.value.ratvalue.d = b.value.ivalue * a.value.ratvalue.d;
    } else {
        ret.value.ratvalue.n = a.value.ratvalue.n * b.value.ratvalue.d;
        ret.value.ratvalue.d = a.value.ratvalue.d * b.value.ratvalue.n;
    }
    return ret;
}

static num num_bitnot(const num& a)
{
    num ret;
    ret.num_type = a.num_type;
    if (likely(ret.num_type == T_INTEGER)) {
        ret.value.ivalue = ~a.value.ivalue;
        return ret;
    }
    ret.num_type = T_INTEGER;
    ret.value.ivalue= ~num_ivalue(a);
    return ret;
}

#define BBOP(NAME, OP) \
    static num num_bit ## NAME(const num& a, const num& b) { \
        num ret; \
        ret.num_type = MY_ARR[a.num_type + b.num_type]; \
        if (likely(ret.num_type == T_INTEGER)) { \
            ret.value.ivalue = a.value.ivalue OP b.value.ivalue; \
            return ret; \
        } \
        ret.num_type = T_INTEGER; \
        ret.value.ivalue = num_ivalue(a) OP num_ivalue(b); \
        return ret; \
    }

BBOP(and, &)
BBOP(or, |)
BBOP(eor, ^)
BBOP(lsl, <<)
BBOP(lsr, >>)

static num num_intdiv(const num& a, const num& b)
{
    num ret;
    ret.num_type = MY_ARR[a.num_type + b.num_type];
    if (likely(ret.num_type == T_INTEGER)) {
        ret.value.ivalue = a.value.ivalue / b.value.ivalue;
        return ret;
    }
    ret.num_type = T_INTEGER;
    ret.value.ivalue = int64_t(num_rvalue(a) / num_rvalue(b));
    return ret;
}

static num num_sub(const num& a, const num& b)
{
    num ret;
    ret.num_type = MY_ARR[a.num_type + b.num_type];
    if (likely(ret.num_type == T_INTEGER)) {
        ret.value.ivalue = a.value.ivalue - b.value.ivalue;
        return ret;
    }
    if (likely(ret.num_type == T_REAL)) {
        ret.value.rvalue = num_rvalue(a) - num_rvalue(b);
        return ret;
    }
    if (a.num_type == T_INTEGER) {
        ret.value.ratvalue.n = a.value.ivalue * b.value.ratvalue.d - b.value.ratvalue.n;
        ret.value.ratvalue.d = b.value.ratvalue.d;
    } else if(b.num_type == T_INTEGER) {
        ret.value.ratvalue.n = a.value.ratvalue.n - b.value.ivalue * a.value.ratvalue.d;
        ret.value.ratvalue.d = a.value.ratvalue.d;
    } else {
        ret.value.ratvalue.n = a.value.ratvalue.n * b.value.ratvalue.d - b.value.ratvalue.n * a.value.ratvalue.d;
        ret.value.ratvalue.d = a.value.ratvalue.d * b.value.ratvalue.d;
    }
    return ret;
}

static num num_rem(const num& a, const num& b)
{
    num ret;
    ret.num_type = T_INTEGER;
    auto e1 = num_ivalue(a);
    auto e2 = num_ivalue(b);
    auto res = e1 % e2;
    /* modulo should have same sign as second operand */
    if (res > 0) {
        if (e1 < 0) {
            res -= std::llabs(e2);
        }
    } else if (res < 0) {
        if (e1 > 0) {
            res += std::llabs(e2);
        }
    }
    ret.value.ivalue = res;
    return ret;
}

static num num_mod(const num& a, const num& b)
{
    num ret;
    ret.num_type = MY_ARR[a.num_type + b.num_type];
    if (likely(ret.num_type == T_INTEGER)) {
        auto e1 = num_ivalue(a);
        auto e2 = num_ivalue(b);
        auto res = e1 % e2;
        if (res * e2 < 0) {    /* modulo should have same sign as second operand */  // TODO: NOT FOR MODUL
            if (res > 0) {
                res -= std::llabs(e2);
            } else {
                res += std::llabs(e2);
            }
        }
        ret.value.ivalue = res;
        return ret;
    }
    if (likely(ret.num_type == T_REAL)) {
        ret.value.rvalue = fmod(num_rvalue(a), num_rvalue(b));
        return ret;
    }
    ret = num_div(a, b);
    ret.value.ratvalue.n = ret.value.ratvalue.n % ret.value.ratvalue.d;
    return num_mul(b, ret);
    return ret;
}

static int num_eq(const num& a, const num& b)
{
    auto type(MY_ARR[a.num_type + b.num_type]);
    if (likely(type == T_INTEGER)) {
        return a.value.ivalue == b.value.ivalue;
    }
    if (likely(type == T_REAL)) {
        return num_rvalue(a) == num_rvalue(b);
    }
    if (b.num_type == T_INTEGER) {
        return a.value.ratvalue.d * b.value.ivalue == a.value.ratvalue.n;
    }
    if (a.num_type == T_INTEGER) {
        return b.value.ratvalue.d * a.value.ivalue == b.value.ratvalue.n;
    }
    return a.value.ratvalue.n == b.value.ratvalue.n && a.value.ratvalue.d == b.value.ratvalue.d;
}

static int num_gt(const num& a, const num& b)
{
    if (a.num_type == T_INTEGER && b.num_type == T_INTEGER) {
        return a.value.ivalue > b.value.ivalue;
    }
    return num_rvalue(a) > num_rvalue(b);
}

static int num_le(const num& a, const num& b)
{
    return !num_gt(a, b);
}

static int num_lt(const num& a, const num& b)
{
    if (a.num_type == T_INTEGER && b.num_type == T_INTEGER) {
        return a.value.ivalue < b.value.ivalue;
    }
    return num_rvalue(a) < num_rvalue(b);
}

static int num_ge(const num& a, const num& b)
{
    return !num_lt(a, b);
}

#if USE_MATH
/* Round to nearest. Round to even if midway */
static double round_per_R5RS(double x) {
    double fl=floor(x);
    double ce=ceil(x);
    double dfl=x-fl;
    double dce=ce-x;
    if(dfl>dce) {
        return ce;
    } else if(dfl<dce) {
        return fl;
    } else {
        if(fmod(fl,2.0)==0.0) {       /* I imagine this holds */
            return fl;
        } else {
            return ce;
        }
    }
}
#endif

static int is_zero_double(double x) {
    return x<DBL_MIN && x>-DBL_MIN;
}

static long binary_decode(const char *s) {
    long x=0;

    while(*s!=0 && (*s=='1' || *s=='0')) {
        x<<=1;
        x+=*s-'0';
        s++;
    }

    return x;
}


/* allocate new cell segment */
static int alloc_cellseg(scheme *sc, int n) {
    pointer newp;
    pointer last;
    pointer p;
    char *cp;
    long i;
    int k;
    int adj=ADJ;

    if(adj<sizeof(struct cell)) {
        adj=sizeof(struct cell);
    }

    //std::cout << "ALLOCATE MEMORY: " << sc->last_cell_seg << std::endl;

    //std::cout << "FCELLS: " << sc->fcells << std::endl;

    for (k = 0; k < n; k++) {
        if (sc->last_cell_seg >= CELL_NSEGMENT - 1)
            return k;
        cp = (char*) sc->malloc(CELL_SEGSIZE * sizeof(struct cell)+adj);
        if (cp == 0)
            return k;
        i = ++sc->last_cell_seg ;
        sc->alloc_seg[i] = cp;
        /* adjust in TYPE_BITS-bit boundary */
        //if((  *((unsigned*)cp) )%adj!=0) {
        //    cp=(char*)(adj*((long)cp/adj+1));
        //}
        /* insert new segment in address order */
        newp=(pointer)cp;
  //  std::cout << "ALLOCATING MEM IN PROCESS: " << extemp::SchemeProcess::I(sc)->getName() << std::endl;
  //printf("Alloced Memory[%d]:  0x%" PRIXPTR ":0x%" PRIXPTR " 0x%" PRIXPTR ":0x%" PRIXPTR "\n",i,(uintptr_t)&newp[0],(uintptr_t)&newp[CELL_SEGSIZE],(uintptr_t)cp,(uintptr_t)(cp+(CELL_SEGSIZE * sizeof(struct cell))));
  //printf("Alloced Memory[%d]: %" PRIuPTR ":%" PRIuPTR " %" PRIuPTR ":%" PRIuPTR "\n",i,(uintptr_t)&newp[0],(uintptr_t)&newp[CELL_SEGSIZE],(uintptr_t)cp,(uintptr_t)(cp+(CELL_SEGSIZE * sizeof(struct cell))));

        sc->cell_seg[i] = newp;
        while (i > 0 && sc->cell_seg[i - 1] > sc->cell_seg[i]) {
                  p = sc->cell_seg[i];
            sc->cell_seg[i] = sc->cell_seg[i - 1];
            sc->cell_seg[--i] = p;
        }
        //      sc->fcells += CELL_SEGSIZE;
        last = newp + CELL_SEGSIZE - 1;

        //sc->NIL = newp;
        int k=0;
        for (p = newp; p <= last; p++, k++) {
            typeflag(p) = 0;
            p->_colour = !sc->dark; // initially set all cells to light
#ifdef TREADMILL_CHECKS
            p->_list_colour = 1; //all ecru
#endif
            cdr(p) = 0; //sc->NIL; //(p==last) ? p + 1 : newp;
            car(p) = 0; //sc->NIL;
                        //added to alloc
                        //p->_colour = (k<100000) ? 0 : 1;
            (p)->_cw = (p==last) ? newp : p+1;
            (p)->_ccw = (p==newp) ? last : p-1;
        }

    }
    sc->total_memory_allocated = CELL_SEGSIZE;

    //added to alloc
    sc->starting_cell = newp;
    sc->treadmill_free = newp;
    sc->treadmill_top = sc->treadmill_free->_ccw;
    sc->treadmill_scan = sc->treadmill_top;
    //sc->treadmill_scan->_colour = sc->dark;
    pointer ttt = sc->treadmill_free;
    //    sc->fcells = 0;
    for(int i=0;i<(CELL_SEGSIZE/2);i++)
    {
        ttt->_colour = !sc->dark;
#ifdef TREADMILL_CHECKS
        ttt->_list_colour = 0;
#endif
  //    sc->fcells++;
        ttt = ttt->_cw;
    }
    sc->treadmill_bottom = ttt; //sc->treadmill_free + (CELL_SEGSIZE/2);//100000;
    //std::cout << sc->fcells << " number of free cells" << std::endl;

#ifdef TREADMILL_CHECKS
    //printf("////////////// SANITY CHECK TREADMILL AFTER ALLOCATION /////////////\n");
    if(sc->treadmill_bottom->_ccw->_list_colour != 0)
    {
        printf("_CCW OF BOTTOM SHOULD BE FREE CELL!\n");
    }
    if(sc->treadmill_top != sc->treadmill_scan)
    {
        printf("SCAN & TOP SHOULD BE AT THE SAME LOCATION!!\n");
    }
    if(sc->treadmill_scan->_cw != sc->treadmill_free)
    {
        printf("FREE SHOULD BE _CW of SCAN\n");
    }
    if(sc->treadmill_top->_list_colour != sc->treadmill_scan->_list_colour != sc->treadmill_bottom->_list_colour != 1)
    {
        printf("SCAN & TOP & BOTTOM SHOULD BE ECRU!\n");
    }
    if(sc->treadmill_free->_list_colour != 0)
    {
        printf("FREE SHOULD BE WHITE!!\n");
    }
    //check no gaps between ecru cells
    pointer ecru_check = sc->treadmill_bottom;
    int ecrus = 0;
    while(ecru_check != sc->treadmill_free){
        if(ecru_check->_list_colour != 1)
        {
            printf("Should have complete list of ecrus between BOTTOM and FREE moving clockwise!  Catastrophic error in GC\n");
        }
        ecrus++;
        ecru_check = ecru_check->_cw;
    }
    //check no gaps between free cells
    pointer free_check = sc->treadmill_free;
    int frees = 0;
    while(free_check != sc->treadmill_bottom){
        if(free_check->_list_colour != 0)
        {
            printf("Should have complete list of frees between free and bottom moving clockwise!  Catastrophic error in GC\n");
        }
        frees++;
        free_check = free_check->_cw;
    }
    //if(frees+ecrus != CELL_SEGSIZE)
    //{
    //printf("FREES(%d)  ECRUS(%d)  TOTAL(%d)  CELLSEG(%lld)\n",frees,ecrus,frees+ecrus,sc->total_memory_allocated);
    //}
    //printf("------------- FINISHED SANITY CHECK TREADMILL AFTER ALLOCATION ---------------\n");
#endif

    char str[256];
    //    sprintf(str,"Allocated: %d cell segements for a total of %d.  Free cells = %lld",n,sc->last_cell_seg,sc->fcells);
    //sprintf(str,"Allocated: %d cell segments for a total of %d.",n,sc->last_cell_seg);
    //CPPBridge::notification(str);
    //std::cout << "Allocated: " << n << " Cell Segments For A Total Of " << sc->last_cell_seg << ",  Free Cells = " << sc->fcells << std::endl;
    return n;
}



static inline pointer get_cell(scheme* Scheme, pointer A, pointer B)
{
    if (unlikely(Scheme->treadmill_free == Scheme->treadmill_bottom)) {
        //std::cout << "START FLIP FROM GET_CELL " << Scheme << std::endl;
        treadmill_flip(Scheme, A, B);
        //std::cout << "FINISHED FLIP FROM GET_CELL " << Scheme << std::endl;
        // if(Scheme->fcells<=0) { // if even after flip we have no free cells
        //   std::cout <<  "Out of memory!!.  Catastrophic error!!  free cells: " << Scheme->fcells << std::endl;
        //   exit(0);
        // }
    }
    auto x(Scheme->treadmill_free);
#ifdef TREADMILL_CHECKS
    if (x->_list_colour) {
        _Error_1(Scheme, "Cell is not empty.  Catastrophic error in GC", Scheme->NIL,0);
        //printf("Error: cell is not empty");
    }
#endif
    finalize_cell(Scheme, x);
    typeflag(x) = 0;
    car(x) = Scheme->NIL;
    cdr(x) = Scheme->NIL;
    ////////////////  these for debugger //////////////////
    x->_debugger = Scheme->NIL; // might not need this?
    x->_size = 0; // or this?
    ///////////////////////////////////////////////////////
    Scheme->treadmill_free = Scheme->treadmill_free->_cw;
    x->_colour = Scheme->dark;
#ifdef TREADMILL_CHECKS
    x->_list_colour = 3; // black
#endif
    //    --Scheme->fcells;
    return x;
}


/* get new cons cell */
pointer _cons(scheme *sc, pointer a, pointer b, int immutable) {
    pointer x = get_cell(sc,a, b);

    typeflag(x) = T_PAIR;
    if(immutable) {
        setimmutable(x);
    }

    ////////////// write barrier for treadmill
    if(a->_colour != sc->dark)
    {
      //std::cout << "INSERT FROM CONS: " << a << " " << (int)a->_colour << " != " << sc->dark << std::endl << std::flush;
#ifdef TREADMILL_CHECKS
      last_call_to_insert_treadmill = 1;
#endif
            insert_treadmill(sc,a);
    }
    if(b->_colour != sc->dark)
      {
#ifdef TREADMILL_CHECKS
      last_call_to_insert_treadmill = 2;
#endif
        insert_treadmill(sc,b);
    }
    //////////////////////////////////////////

    car(x) = a;
    cdr(x) = b;

    return (x);
}

/* ========== oblist implementation  ========== */

static int hash_fn(const char *key, int table_size);

static pointer oblist_initial_value(scheme *sc)
{
    return mk_vector(sc, 65521);
}

/* returns the new symbol */
static pointer oblist_add_by_name(scheme *sc, const char *name)
{
    pointer x;
    int location;

    x = immutable_cons(sc, mk_string(sc, name), sc->NIL);
    typeflag(x) = T_SYMBOL;
    setimmutable(car(x));

    location = hash_fn(name, sc->oblist->_size);
    set_vector_elem(sc, sc->oblist, location,
                    immutable_cons(sc, x, vector_elem(sc->oblist, location)));
    return x;
}

static inline pointer oblist_find_by_name(scheme *sc, const char *name)
{
    int location;
    pointer x;
    char *s;
    location = hash_fn(name, sc->oblist->_size); //ivalue_unchecked(sc->oblist));
    for (x = vector_elem(sc->oblist, location); x != sc->NIL; x = cdr(x)) {
        s = symname_sc(sc,car(x));
        /* case-insensitive, per R5RS section 2. */
        if(strcmp(name, s) == 0) {
            return car(x);
        }
    }
    return sc->NIL;
}

static pointer oblist_all_symbols(scheme *sc)
{
    int i;
    pointer x;
    pointer ob_list = sc->NIL;

    for (i = 0; i < sc->oblist->_size; i++) {
        for (x  = vector_elem(sc->oblist, i); x != sc->NIL; x = cdr(x)) {
            ob_list = cons(sc, x, ob_list);
        }
    }
    return ob_list;
}

static pointer mk_port(scheme *sc, port *p) {
    pointer x = get_cell(sc, sc->NIL, sc->NIL);

    typeflag(x) = T_PORT|T_ATOM;
    x->_object._port=p;
    return (x);
}

pointer mk_foreign_func(scheme *sc, foreign_func f) {
    pointer x = get_cell(sc, sc->NIL, sc->NIL);

    typeflag(x) = (T_FOREIGN | T_ATOM);
    x->_object._ff=f;
    return (x);
}

pointer mk_cptr(scheme *sc, void* p) {
    pointer x = get_cell(sc, sc->NIL, sc->NIL);

    typeflag(x) = (T_CPTR | T_ATOM);
    x->_object._cptr = p;
    return (x);
}

int retained = 0;

pointer mk_character(scheme *sc, int c) {
    pointer x = get_cell(sc,sc->NIL, sc->NIL);

    typeflag(x) = (T_CHARACTER | T_ATOM);
    ivalue_unchecked(x)= c;
    set_integer(x);
    return (x);
}

/* get number atom (integer) */
pointer mk_integer(scheme* Scheme, long long Num) {
    pointer x = get_cell(Scheme, Scheme->NIL, Scheme->NIL);
    //std::cout << "NUM: " << Num << " :: " << sizeof(long) << " :: " << sizeof(int) << std::endl;
    typeflag(x) = T_NUMBER | T_ATOM;
    ivalue_unchecked(x) = Num;
    set_integer(x);
    //std::cout << "NUM2: " << ivalue(x) << std::endl;
    return x;
}

pointer mk_i64(scheme *sc, long long num) {
    return mk_integer(sc, num);
}

pointer mk_i32(scheme *sc, int num) {
    return mk_integer(sc, num);
}

pointer mk_i16(scheme *sc, short num) {
    return mk_integer(sc, (int)num);
}

pointer mk_i8(scheme *sc, char num) {
    return mk_integer(sc, num);
}

pointer mk_i1(scheme *sc, bool num) {
    return mk_integer(sc, num);
}

pointer mk_real(scheme *sc, double n) {
    pointer x = get_cell(sc,sc->NIL, sc->NIL);

    typeflag(x) = (T_NUMBER | T_ATOM);
    rvalue_unchecked(x)= n;
    set_real(x);
    return (x);
}

pointer mk_double(scheme* sc, double n) {
    return mk_real(sc, n);
}

pointer mk_float(scheme* sc, float n) {
    return mk_real(sc, (double) n);
}

int64_t gcd(int64_t a, int64_t b)
{
    while (b) {
        auto r(a % b);
        a = b;
        b = r;
    }
    return a;
}

pointer mk_rational(scheme *sc, long long n, long long d) {
    if(d==0) {
        _Error_1(sc,"Cannot make rational with 0 denominator",sc->NIL,sc->code->_size);
        return sc->NIL;
    }
    pointer x = get_cell(sc,sc->NIL, sc->NIL);
    if(n==0) { // return integer 0
        typeflag(x) = (T_NUMBER | T_ATOM);
        ivalue_unchecked(x)=0;
        set_integer(x);
    }else{
        auto _gcd = gcd(n, d);

        typeflag(x) = (T_NUMBER | T_ATOM);
        //NSLog(@"MKRAT: %d %d",n,d);

        ratvalue_unchecked(x).n=n/_gcd;
        ratvalue_unchecked(x).d=d/_gcd;
        //std::cout << "MK_RATIONAL: " << ratvalue_unchecked(x).n << "/" << ratvalue_unchecked(x).d << std::endl << std::flush;
        set_rational(x);
    }
    return (x);
}

static pointer mk_number(scheme *sc, const num& n) {
    if (likely(n.num_type == T_INTEGER)) {
        return mk_integer(sc, n.value.ivalue);
    }
    if (likely(n.num_type == T_REAL)) {
        return mk_real(sc,n.value.rvalue);
    }
    if (likely(n.num_type == T_RATIONAL)) {
        return mk_rational(sc, n.value.ratvalue.n, n.value.ratvalue.d);
    }
    return sc->NIL;
}

/* allocate name to string area */
static char *store_string(scheme *sc, int len_str, const char *str, char fill) {
    char *q;

    q=(char*)sc->malloc(len_str+1);
    if(q==0) {
        sc->no_memory=1;
        return sc->strbuff;
    }
    if(str!=0) {
        strcpy(q, str);
    } else {
        memset(q, fill, len_str);
        q[len_str]=0;
    }
    return (q);
}

/* get new string */
pointer mk_string(scheme *sc, const char *str) {
    if(str == 0) str = "";
    return mk_counted_string(sc,str,strlen(str));
}

pointer mk_counted_string(scheme *sc, const char *str, int len) {
    pointer x = get_cell(sc, sc->NIL, sc->NIL);

    strvalue(x) = store_string(sc,len,str,0);
    typeflag(x) = (T_STRING | T_ATOM);
    strlength(x) = len;
    return (x);
}

static pointer mk_empty_string(scheme *sc, int len, char fill) {
    pointer x = get_cell(sc, sc->NIL, sc->NIL);

    strvalue(x) = store_string(sc,len,0,fill);
    typeflag(x) = (T_STRING | T_ATOM);
    strlength(x) = len;
    return (x);
}

pointer mk_vector(scheme *sc, int len) {
    pointer x = get_cell(sc, sc->NIL, sc->NIL);

    typeflag(x) = (T_VECTOR | T_ATOM);
    x->_size = len;
    x->_object._cptr = (char*) sc->malloc(len * sizeof(pointer));
    sc->allocation_request += len;

    //std::cout << "POINTER: " << x << " CPTR: " << x->_object._cptr << std::endl;
    //ivalue_unchecked(x)=len;
    //set_integer(x);
    fill_vector(sc,x,sc->NIL);

    return x;
}

/* static*/ void fill_vector(scheme* sc, pointer vec, pointer obj) {
    int i;
    int num = vec->_size;
    pointer* cptr = (pointer*) vec->_object._cptr;

    ////////////// write barrier for treadmill
#ifdef TREADMILL_CHECKS
      last_call_to_insert_treadmill = 3;
#endif
    insert_treadmill(sc,obj);
    //////////////////////////////////////////

    for(i=0; i<num; i++) {
        cptr[i] = obj;
    }
}

/* get new symbol */
pointer mk_symbol(scheme *sc, const char *name) {
//
//       if(sc->inport != sc->NIL && sc->inport->_object._port->kind&port_string) {
//               int position = 0;
//               char* ptr = sc->inport->_object._port->rep.string.start;
//               for( ; &ptr[position] < &sc->inport->_object._port->rep.string.curr[0]; position++);
//               int lgth = strlen(name);
//               std::cout << "sym:" << name << "  pos:" << (position-lgth) << "  lgth:" << lgth << std::endl; //"  start:" << &sc->inport->_object._port->rep.string.start[0] << "  curr:" << &sc->inport->_object._port->rep.string.curr[0] << std::endl;
//       }
//
    pointer x;

    /* first check oblist */
    x = oblist_find_by_name(sc, name);
    if (x != sc->NIL) {
        return (x);
    } else {
        x = oblist_add_by_name(sc, name);
        return (x);
    }
}


//impromtpu's evil gensym for making uninterned symbols
pointer gensym(scheme *sc) {
    pointer x;
    char name[40];
    sc->gensym_cnt++;
    if(sc->gensym_cnt>10000000) sc->gensym_cnt = 0;
    sprintf(name,"gensym-%ld",sc->gensym_cnt);
    //printf("gensym %s\n",name);
    x = immutable_cons(sc, mk_string(sc, name), sc->NIL);
    typeflag(x) = T_SYMBOL;
    setimmutable(car(x));
    return (x);
}


/* make symbol or number atom from string */
static pointer mk_atom(scheme *sc, char *q) {
    char    c, *p;
    char *ratn, *ratd; //added by as for rational numbers support
    int has_dec_point=0;
    int has_rational=0;
    int has_fp_exp = 0;


#if USE_COLON_HOOK
    if((p=strstr(q,"::"))!=0) {
        *p=0;
        return cons(sc, sc->COLON_HOOK,
                    cons(sc,
                         cons(sc,
                              sc->QUOTE,
                              cons(sc, mk_atom(sc,p+2), sc->NIL)),
                         cons(sc, mk_symbol(sc,strlwr(q)), sc->NIL)));
    }
#endif

    ratn = q;
    p = q;
    c = *p++;
    if ((c == '+') || (c == '-')) {
        c = *p++;
        if (c == '.') {
            has_dec_point=1;
            c = *p++;
        }
        if (!isdigit(c)) {
            return (mk_symbol(sc, strlwr(q)));
        }
    } else if (c == '.') {
        has_dec_point=1;
        c = *p++;
        if (!isdigit(c)) {
            return (mk_symbol(sc, strlwr(q)));
        }
    }else if (c == 0) {
        return sc->NIL;
    } else if (!isdigit(c)) {
        return (mk_symbol(sc, strlwr(q)));
    }

    for ( ; (c = *p) != 0; ++p) {
        if (!isdigit(c)) {
            if(c=='.') {
                if(!has_dec_point) {
                    has_dec_point=1;
                    continue;
                }
            }
            if(c=='/') {
                if(!has_rational) {
                    has_rational=1;
                    ratd = p;
                    ratd++;
                    continue;
                }
            }
            else if ((c == 'e') || (c == 'E')) {
                if(!has_fp_exp) {
                    has_dec_point = 1; /* decimal point illegal
                                          from now on */
                    p++;
                    if ((*p == '-') || (*p == '+') || isdigit(*p)) {
                        continue;
                    }
                }
            }
            return (mk_symbol(sc, strlwr(q)));
        }
    }
    if(has_dec_point) {
        return mk_real(sc,atof(q));
    }
    if(has_rational) {
        //std::cout << "N: " << atoll(ratn) << " D: " << atoll(ratd) << std::endl;
        return mk_rational(sc,atoll(ratn),atoll(ratd));
    }
    return (mk_integer(sc, atoll(q)));
}

/* make constant */
static pointer mk_sharp_const(scheme *sc, char *name) {
    long    x;
    char    tmp[256];

    if (!strcmp(name, "t"))
        return (sc->T);
    else if (!strcmp(name, "f"))
        return (sc->F);
    else if (*name == 'o') {/* #o (octal) */
        sprintf(tmp, "0%s", name+1);
        sscanf(tmp, "%lo", &x);
        return (mk_integer(sc, x));
    } else if (*name == 'd') {    /* #d (decimal) */
        sscanf(name+1, "%ld", &x);
        return (mk_integer(sc, x));
    } else if (*name == 'x') {    /* #x (hex) */
        sprintf(tmp, "0x%s", name+1);
        sscanf(tmp, "%lx", &x);
        return (mk_integer(sc, x));
    } else if (*name == 'b') {    /* #b (binary) */
        x = binary_decode(name+1);
        return (mk_integer(sc, x));
    } else if (*name == '\\') { /* #\w (character) */
        int c=0;
        if(strcmp(name+1,"space")==0) {
            c=' ';
        } else if(strcmp(name+1,"newline")==0) {
            c='\n';
        } else if(strcmp(name+1,"return")==0) {
            c='\r';
        } else if(strcmp(name+1,"tab")==0) {
            c='\t';
        } else if(name[1]=='x' && name[2]!=0) {
            int c1=0;
            if(sscanf(name+2,"%x",&c1)==1 && c1<256) {
                c=c1;
            } else {
                return sc->NIL;
            }
#if USE_ASCII_NAMES
        } else if(is_ascii_name(name+1,&c)) {
            /* nothing */
#endif
        } else if(name[2]==0) {
            c=name[1];
        } else {
            return sc->NIL;
        }
        return mk_character(sc,c);
    } else
        return (sc->NIL);
}


struct dump_stack_frame {
    enum scheme_opcodes op;
    pointer args;
    pointer envir;
    pointer code;
};

static void treadmill_mark_roots(scheme* sc, pointer a, pointer b) {
#ifdef TREADMILL_DEBUG
    std::cout << "TREADMILL: MARK ROOTS" << std::endl;//  top:" << sc->treadmill_top << "  scan: " << sc->treadmill_scan << "  bottom:" << sc->treadmill_bottom << "  scan: " << sc->treadmill_scan << std::endl;
#endif

    sc->mutex->lock();

#ifdef TREADMILL_CHECKS
      last_call_to_insert_treadmill = 5;
#endif

    insert_treadmill(sc, sc->oblist);
    insert_treadmill(sc, sc->global_env);
    insert_treadmill(sc, sc->args);
    insert_treadmill(sc, sc->envir);
    insert_treadmill(sc, sc->code);

    intptr_t nframes = (intptr_t)sc->dump;
    int i;
    for(i=0; i<nframes; i++) {
        struct dump_stack_frame *frame;
        frame = (struct dump_stack_frame *)sc->dump_base + i;
        insert_treadmill(sc,frame->args);
        insert_treadmill(sc,frame->envir);
        insert_treadmill(sc,frame->code);
    }

    insert_treadmill(sc, sc->tmp_dump);

    insert_treadmill(sc, sc->value);
    insert_treadmill(sc, sc->inport);
    insert_treadmill(sc, sc->save_inport);
    insert_treadmill(sc, sc->outport);
    insert_treadmill(sc, sc->loadport);

    insert_treadmill(sc, sc->T);
    insert_treadmill(sc, sc->F);
    insert_treadmill(sc, sc->NIL);
    insert_treadmill(sc, sc->EOF_OBJ);
    insert_treadmill(sc, sc->sink);

    insert_treadmill(sc, a);
    insert_treadmill(sc, b);

    /* mark pointers */
    for (auto pointer : sc->imp_env) {
        insert_treadmill(sc, pointer);
    }

    sc->mutex->unlock();

#ifdef TREADMILL_DEBUG
    std::cout << "TREADMILL FINISHED MARKING ROOTS" << std::endl;
#endif
    return;
}


static void treadmill_flip(scheme* sc,pointer a,pointer b)
{
#ifdef TREADMILL_DEBUG
    std::cout << "TREADMILL: FLIP " << pthread_self() << std::endl << std::flush;
#endif

    sc->treadmill_flip_active = true;

    while(!sc->treadmill_scanner_finished)// sc->treadmill_scan != sc->treadmill_top)
    {
#ifdef TREADMILL_DEBUG
        std::cout << "TREADMILL: FLIP SPINNING" << std::endl << std::flush;
#endif

#ifdef EXT_BOOST
        std::this_thread::sleep_for(std::chrono::microseconds(50));
#else
        usleep(50);
#endif
    }
#ifdef TREADMILL_DEBUG
    std::cout << "TREADMILL: FINISHSED SPINNING - ON WITH THE WORK" << std::endl << std::flush;
#endif

#ifdef TREADMILL_CHECKS
    //std::cout << "START FLIP*******************************************************************************************    Scheme Instance:" << sc << std::endl << std::flush;
    ///////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    // Some very basic sanity checking
    //printf("==============FLIP SANITY CHECKS====================\n");
    if(sc->treadmill_top != sc->treadmill_scan) {
        printf("Top & Scan must match!  Catastrophic error in GC\n");
    }
    if(sc->treadmill_bottom != sc->treadmill_free) {
        printf("Bottom & Free must match!  Catastrophic error in GC\n");
    }
    if(sc->treadmill_top->_list_colour != 1 || sc->treadmill_bottom->_list_colour != 1 || sc->treadmill_scan->_list_colour != 1 || sc->treadmill_free->_list_colour != 1)
    {
        printf("COLOUR MISMATCH  TOP(%d) FREE(%d) SCAN(%d) BOTTOM(%d)\n",sc->treadmill_top->_list_colour,sc->treadmill_free->_list_colour,sc->treadmill_scan->_list_colour,sc->treadmill_bottom->_list_colour);
    }
    if(sc->treadmill_top->_cw->_list_colour != 3 || sc->treadmill_scan->_cw->_list_colour != 3)
    {
        printf("Top & Scan should have black to clockwise!  Catastrophic error in GC\n");
    }
    if(sc->treadmill_bottom->_ccw->_list_colour != 3 || sc->treadmill_free->_ccw->_list_colour != 3)
    {
        printf("Bottom & Free should have black to counter clockwise!  Catastrophic error in GC\n");
    }
    //check no gaps between ecru cells
    pointer ecru_check = sc->treadmill_top;
    long long ecruscells = 0;
    while(ecru_check != sc->treadmill_bottom->_ccw){
        if(ecru_check->_list_colour != 1)
        {
            printf("Should have complete list of ecrus between TOP and BOTTOM moving counter clockwise!  Catastrophic error in GC\n");
        }
        ecruscells++;
        ecru_check = ecru_check->_ccw;
    }
    //check no gaps between black cells
    long long blacks = 0;
    pointer black_check = sc->treadmill_bottom->_ccw;
    while(black_check != sc->treadmill_top){
        if(black_check->_list_colour != 3)
        {
            printf("Should have complete list of blacks between BOTTOM and TOP moving counter clockwise!  Catastrophic error in GC\n");
        }
        blacks++;
        black_check = black_check->_ccw;
    }
    //printf("BLACKS(%lld)  ECRUS(%lld)  TOTAL(%lld)  SEGSIZE(%lld)\n",blacks,ecruscells,blacks+ecruscells,sc->total_memory_allocated);
    //printf("-----------------DONE FLIP SANITY CHECKS-----------------\n");
    /////////////////////////////////////////////////////////////////////
    ///////////////////////////////////////////////////////////////////////
#endif

    // flip dark
    sc->dark = !sc->dark;
#ifdef TREADMILL_DEBUG
    std::cout << "TREADMILL: FLIPPING DARK BIT TO: " << sc->dark << std::endl << std::flush;
#endif

#ifdef TREADMILL_CHECKS
    // Sanity Check for _cw links
    pointer ktt = sc->treadmill_top;
    long long check = 0;
    long long ecrus = 0;
    while(true)
    {
        if(ktt->_list_colour == 0 || ktt->_list_colour == 2)
        {
            _Error_1(sc,"Shouldn't be any grey or white cells left at this stage!  Catastrophic error in GC",sc->NIL,0);
        }
        if(ktt->_list_colour == 1) ecrus++;
        ktt->_colour = !sc->dark;
        ktt->_list_colour = 1; // set all cells to ecru
        ktt = ktt->_cw;
        check++;
        if(ktt == sc->treadmill_top) break;
        if(check>sc->total_memory_allocated)
        {
            _Error_1(sc,"Memory Map Has Changed in FLIP _cw.  Catastrophic error in GC",sc->NIL,0);
        }
    }
    if(check<sc->total_memory_allocated)
    {
        printf("CHECK _CW: %lld\n",check);
        _Error_1(sc,"LOST Memory _cw.  Catastrophic error in GC",sc->NIL,0);
    }

    // also check that all _ccw links are working
    ktt = sc->treadmill_top;
    check = 0;
    while(true)
    {
        ktt = ktt->_ccw;
        check++;
        if(ktt == sc->treadmill_top) break;
        if(check>sc->total_memory_allocated)
        {
            _Error_1(sc,"Memory Map Has Changed in FLIP _ccw.  Catastrophic error in GC",sc->NIL,0);
        }
    }
    if(check<sc->total_memory_allocated)
    {
        printf("CHECK _CCW: %lld\n",check);
        _Error_1(sc,"LOST Memory _ccw.  Catastrophic error in GC",sc->NIL,0);
    }
#endif


    sc->treadmill_bottom = sc->treadmill_top->_cw; //->_ccw;
    sc->treadmill_top = sc->treadmill_free->_ccw; //t->_ccw; //sc->treadmill_free->_ccw; //tmp->_ccw;// sc->treadmill_scan->_ccw;
    sc->treadmill_scan = sc->treadmill_top; //sc->treadmill_free->_ccw;
    //sc->treadmill_scan->_colour = sc->dark;

//#ifdef TREADMILL_CHECKS
    /////////////////////////////////////////////////////////////
    //Sanity checks marking free cell colours
    long long free_cells = 0;
    pointer t = sc->treadmill_free;
#ifdef TREADMILL_CHECKS
    for( ; t != sc->treadmill_bottom ; ++free_cells)
    {
             t->_list_colour = 0; //set ecrus to frees
             t = t->_cw;
    }
#endif

#ifdef TREADMILL_CHECKS
    if(free_cells != ecrus)
    {
        printf("FREE CELLS: %lld   OLD ECRUS: %lld\n",free_cells,ecrus);
        _Error_1(sc, "Old Ecrus should match exactly to new free_cells!", sc->NIL,0);
    }
#endif

#ifdef TREADMILL_DEBUG
    std::cout << "TREADMILL: # FREE CELLS : " << free_cells << std::endl << std::flush;
#endif

    //std::cout << "CELLS IN FREE LIST: " << free_cells << std::endl;
    //    sc->fcells = free_cells;
    //if(sc->fcells < 20000 || (sc->fcells < (sc->allocation_request+20000)))
    if(treadmill_inserts_per_cycle > ((sc->total_memory_allocated/2)-20000))
    {
//              sc->mutex->Lock(); // lock and don't unlock because we're totally broken :(
//              std::cout << "TREADMILL: RUNNING OUT OF MEMORY!" << std::endl << std::flush;
//              _Error_1(sc, "OUT OF MEMORY ERROR!!", sc->NIL, 0, 0);
//              return;

        //////////////////////////////////////////
        // ADD NEW MEMORY
        int adj=ADJ;

        if(adj<sizeof(struct cell)) {
            adj=sizeof(struct cell);
        }

        long long alloc_size = (sc->allocation_request>=0) ? sc->allocation_request : 100000;

        char* newmem = (char*) sc->malloc(alloc_size * sizeof(struct cell)+adj);
        if (newmem == 0) {
            std::cout << "NO AVAILABLE MEMORY!" << std::endl;
            exit(1);
        }

        sc->total_memory_allocated += alloc_size;
        std::cout << "ALLOCATED NEW MEMORY: new_total(" << sc->total_memory_allocated << ")" << std::endl;

        /* adjust in TYPE_BITS-bit boundary */
        if((  *((unsigned*)newmem) )%adj!=0) {
            newmem=(char*)(adj*((long)newmem/adj+1));
        }

        pointer first = (pointer) newmem;
        pointer last = ((pointer) newmem)+(alloc_size-1);
        long long k=0;
        for (pointer p = first; k<alloc_size; p++, k++) {
            typeflag(p) = 0;
            p->_colour = !sc->dark; // initially set all cells to light
#ifdef TREADMILL_CHECKS
            p->_list_colour = 0; //all white!
#endif
            cdr(p) = 0; //sc->NIL; //(p==last) ? p + 1 : newp;
            car(p) = 0; //sc->NIL;
            if(p==last) {
                sc->treadmill_bottom->_ccw = p;
            }
            if(p==first) {
                sc->treadmill_bottom->_ccw->_cw = p;
            }
            (p)->_cw = (p==last) ? sc->treadmill_bottom : p+1;
            (p)->_ccw = (p==first) ? sc->treadmill_bottom->_ccw : p-1;
        }
  //    sc->fcells+=alloc_size;
    }
    sc->allocation_request = -1;

//#endif
    /////////////////////////////////////////////////////////////////

    treadmill_mark_roots(sc,a,b);

#ifdef TREADMILL_CHECKS
    ///////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    // Some very basic sanity checking
    //printf("==============POST MARK SANITY CHECKS====================\n");
    if(sc->treadmill_top->_list_colour != 1 || sc->treadmill_bottom->_list_colour != 1) {
        printf("Top & Bottom must both be ecru!   TOP(%d) BOTTOM(%d) Catastrophic error in GC\n",sc->treadmill_top->_list_colour,sc->treadmill_bottom->_list_colour);
    }
    if(sc->treadmill_top->_cw->_list_colour != 2)
    {
        printf("TOP _CW must be grey!  TOP_CW(%d)\n",sc->treadmill_top->_cw->_list_colour);
    }
    if(sc->treadmill_bottom->_ccw->_list_colour != 0)
    {
        printf("BOTTOM _CCW must be pure white (free)\n");
    }
    if(sc->treadmill_free->_list_colour != 0)
    {
        printf("Free must be on a white cell\n");
    }
    if(sc->treadmill_scan->_list_colour != 2)
    {
        printf("Scan must be on a grey cell\n");
    }
    if(sc->treadmill_free->_ccw != sc->treadmill_scan && sc->treadmill_scan->_cw != sc->treadmill_free)
    {
        printf("Free and Scan must be next to each other\n");
    }
    //check no gaps between ecru cells
    ecru_check = sc->treadmill_top;
    ecrus = 0;
    while(ecru_check != sc->treadmill_bottom->_ccw){
        if(ecru_check->_list_colour != 1)
        {
            printf("Should have complete list of ecrus between TOP and BOTTOM moving counter clockwise!  Catastrophic error in GC\n");
        }
        ecrus++;
        ecru_check = ecru_check->_ccw;
    }
    //check no gaps between white cells
    long long whites = 0;
    pointer white_check = sc->treadmill_free; //sc->treadmill_bottom->_ccw;
    pointer white_check_ccw = sc->treadmill_bottom->_ccw;
    while(white_check != sc->treadmill_bottom){
        if(white_check_ccw == sc->treadmill_free->_ccw)
        {
            printf("Should have complete list of whites between BOTTOM and FREE moving counter clockwise! (%lld) Catastrophic error in GC\n",whites);
        }
        if(white_check->_list_colour != 0)
        {
            printf("Should have complete list of whites between FREE and BOTTOM moving clockwise! (%lld) Catastrophic error in GC\n",whites);
        }
        whites++;
        white_check = white_check->_cw;
        white_check_ccw = white_check->_ccw;
    }
    //check no gaps between grey cells
    long long greys = 0;
    pointer grey_check = sc->treadmill_scan;
    while(grey_check != sc->treadmill_top){
        if(grey_check->_list_colour != 2)
        {
            printf("Should have complete list of whites between BOTTOM and FREE moving counter clockwise!  Catastrophic error in GC\n");
        }
        greys++;
        grey_check = grey_check->_ccw;
    }
    //printf("GREYS(%lld)  WHITES(%lld)  ECRUS(%lld)  TOTAL(%lld)  SEGSIZE(%lld)\n",greys,whites,ecrus,greys+whites+ecrus,sc->total_memory_allocated);
    //printf("-----------------DONE FLIP SANITY CHECKS-----------------\n");
    /////////////////////////////////////////////////////////////////////
    ///////////////////////////////////////////////////////////////////////
    //std::cout << "FINISHED FLIP**********************************************************************************************************************************" << std::endl << std::flush;
#endif

    treadmill_inserts_per_cycle=0;
    sc->treadmill_flip_active = false;
    //Treadmill_Guard.Unlock();
    while(sc->treadmill_scanner_finished == true)
    {
        try{
            sc->Treadmill_Guard->signal();
        }catch( ... ) {
            std::cout << "ERROR: SENDING NOTIFICATION TO SCANNER THREAD" << std::endl << std::flush;
        }
#ifdef EXT_BOOST
        std::this_thread::sleep_for(std::chrono::microseconds(50));
#else
        usleep(50);
#endif
#ifdef TREADMILL_DEBUG
        std::cout << "SPINNING FLIP WAITING FOR NOTIFICATION" << std::endl << std::flush;
#endif
    }
#ifdef TREADMILL_DEBUG
    std::cout << "FINISHED WITH FLIP" << std::endl << std::flush;
#endif
    return;
}



static void* treadmill_scanner(void* obj)
{
   scheme* sc = (scheme*) obj;
   int total_previous_scan = 0;

   sc->mutex->lock();
   while(true)
     {
       sc->treadmill_scanner_finished = false;
#ifdef TREADMILL_DEBUG
       std::cout << "TREADMILL: START SCAN " << std::endl << std::flush;
#endif

       //treadmill_mark_roots(sc);

       total_previous_scan = 0;
       //mutex.Lock();

       while(!sc->treadmill_flip_active || sc->treadmill_scan != sc->treadmill_top) { // untill the flip is activated we need to keep checking for new objects that may be added to the grey list
         int count = 0;
         unsigned int dark = sc->dark;
         while(sc->treadmill_scan != sc->treadmill_top) //scanned_cell != sc->treadmill_top)
           {
#ifdef TREADMILL_CHECKS
             if(sc->treadmill_scan->_colour != dark) std::cout << "TREADMILL: SCAN OBJ NOT DARKENED!!!" << std::endl << std::flush;
#endif
             pointer scan = sc->treadmill_scan;

             if(is_vector(scan)) {
               int length = scan->_size;

               pointer* cptr = (pointer*) scan->_object._cptr;
               for(int i=0; i<length; i++) {
                 pointer p = cptr[i];
                 if(p->_colour != dark)
                   {
#ifdef TREADMILL_CHECKS
      last_call_to_insert_treadmill = 6;
#endif
                     insert_treadmill(sc,p);
                   }
               }
             }

             if(is_continuation(scan))
               {
                 //#ifdef TREADMILL_CHECKS
                 //std::cout << "Insert Continuation into Treadmill" << std::endl;
                 //#endif
                 pointer d = cont_dump(scan);
                 unsigned int* dump = (unsigned int*) cptr_value_sc(sc,d);
                 int nframes = dump[0];
                 dump_stack_frame* frames = (dump_stack_frame*)&dump[1];

                 // for each frame check the args and code lists
                 for(int j=0;j<nframes;j++)
                   {
                     // envir
                     pointer env = frames[j].envir;
                     if(env->_colour != dark)
                       {
#ifdef TREADMILL_CHECKS
      last_call_to_insert_treadmill = 7;
#endif
                         insert_treadmill(sc, env);
                       }

                     // args
                     pointer args = frames[j].args;
                     if(args->_colour != dark)
                       {
#ifdef TREADMILL_CHECKS
      last_call_to_insert_treadmill = 8;
#endif
                         insert_treadmill(sc, args);
                       }

                     // copy code
                     pointer code = frames[j].code;
                     if(code->_colour != dark)
                       {
#ifdef TREADMILL_CHECKS
      last_call_to_insert_treadmill = 9;
#endif
                         insert_treadmill(sc, code);
                       }
                   }
               }

             if(!is_atom(scan))
               {
                 pointer a = scan->_object._cons._car;
                 pointer b = scan->_object._cons._cdr;
                 if(a->_colour != dark)
                   {
#ifdef TREADMILL_CHECKS
      last_call_to_insert_treadmill = 10;
#endif
                     insert_treadmill(sc, a);
                   }
                 if(b->_colour != dark)
                   {
#ifdef TREADMILL_CHECKS
      last_call_to_insert_treadmill = 11;
#endif
                     insert_treadmill(sc, b);
                   }
               }

#ifdef TREADMILL_CHECKS
             sc->treadmill_scan->_list_colour = 3;
#endif
             sc->treadmill_scan = sc->treadmill_scan->_ccw;
             total_previous_scan++;

             if(!(count%100)) {         // force a yield every now and then?
               sc->mutex->unlock();
               sc->mutex->lock();
             }
             count++;

           }
         sc->mutex->unlock(); // yeild here to let interpreter add greys to the treadmill!!
#ifdef EXT_BOOST
         std::this_thread::sleep_for(std::chrono::microseconds(500));
#else
         usleep(500);
#endif
         sc->mutex->lock(); // But lock again after sleep!
       }
#ifdef EXT_BOOST
#else
       sc->Treadmill_Guard->lock();
#endif
#ifdef TREADMILL_DEBUG
       std::cout << "TREADMILL: FINISHED SCAN: " << sc->treadmill_flip_active << std::endl << std::flush;
#endif

       if(sc->treadmill_scan != sc->treadmill_top)
         {
           std::cout << "HUGE ERROR:  SHOULD NOT FINISH SCANNER UNTIL SCAN == TOP" << std::endl << std::flush;
         }

       sc->mutex->unlock();
       sc->treadmill_scanner_finished = true;
       sc->Treadmill_Guard->wait();
#ifdef TREADMILL_DEBUG
       std::cout << "WAKING UP SCANNER" << std::endl << std::flush;
#endif
       sc->mutex->lock();
#ifdef EXT_BOOST
#else
       sc->Treadmill_Guard->unlock();
#endif
       if(sc->treadmill_stop) break; // exit treadmill thread
     }

   return sc;
}

int released = 0;
static void finalize_cell(scheme *sc, pointer a) {
    if(is_vector(a)) {
        //std::cout << "RELEASE VECTOR: pointer(" << a << ")  cptr(" << a->_object._cptr << ")" << std::endl;
        sc->free(a->_object._cptr);
        a->_object._cptr = 0; //nil;
    } else if(is_string(a)) {
        sc->free(strvalue(a));
    } else if(is_port(a)) {
        if(a->_object._port->kind&port_file
           && a->_object._port->rep.stdio.closeit) {
            port_close(sc,a,port_input|port_output);
        }
        sc->free(a->_object._port);
    }
}

/* ========== Routines for Reading ========== */

static int file_push(scheme *sc, const char *fname) {
    FILE *fin=fopen(fname,"r");
    if(fin!=0) {
        sc->file_i++;
        sc->load_stack[sc->file_i].kind=port_file|port_input;
        sc->load_stack[sc->file_i].rep.stdio.file=fin;
        sc->load_stack[sc->file_i].rep.stdio.closeit=1;
        sc->nesting_stack[sc->file_i]=0;
        sc->loadport->_object._port=sc->load_stack+sc->file_i;
    }
    return fin!=0;
}

static void file_pop(scheme *sc) {
    sc->nesting=sc->nesting_stack[sc->file_i];
    if(sc->file_i!=0) {
        port_close(sc,sc->loadport,port_input);
        sc->file_i--;
        sc->loadport->_object._port=sc->load_stack+sc->file_i;
        if(file_interactive(sc)) {
            putstr(sc,prompt);
        }
    }
}

static int file_interactive(scheme *sc) {
    return sc->file_i==0 && sc->load_stack[0].rep.stdio.file==stdin
        && sc->inport->_object._port->kind&port_file;
}

static port *port_rep_from_filename(scheme *sc, const char *fn, int prop) {
    FILE *f;
    char *rw;
    port *pt;
    if(prop==(port_input|port_output)) {
        rw=(char*)"a+";
    } else if(prop==port_output) {
        rw=(char*)"w";
    } else {
        rw=(char*)"r";
    }
    f=fopen(fn,rw);
    if(f==0) {
        return 0;
    }
    pt=port_rep_from_file(sc,f,prop);
    pt->rep.stdio.closeit=1;
    return pt;
}

static pointer port_from_filename(scheme *sc, const char *fn, int prop) {
    port *pt;
    pt=port_rep_from_filename(sc,fn,prop);
    if(pt==0) {
        return sc->NIL;
    }
    return mk_port(sc,pt);
}

static port *port_rep_from_file(scheme *sc, FILE *f, int prop) {
    char *rw;
    port *pt;
    pt=(port*)sc->malloc(sizeof(port));
    if(pt==0) {
        return 0;
    }
    if(prop==(port_input|port_output)) {
        rw=(char*)"a+";
    } else if(prop==port_output) {
        rw=(char*)"w";
    } else {
        rw=(char*)"r";
    }
    pt->kind=port_file|prop;
    pt->rep.stdio.file=f;
    pt->rep.stdio.closeit=0;
    return pt;
}

static pointer port_from_file(scheme *sc, FILE *f, int prop) {
    port *pt;
    pt=port_rep_from_file(sc,f,prop);
    if(pt==0) {
        return sc->NIL;
    }
    return mk_port(sc,pt);
}

static port *port_rep_from_string(scheme *sc, char *start, char *past_the_end, int prop) {
    port *pt;
    pt=(port*)sc->malloc(sizeof(port));
    if(pt==0) {
        return 0;
    }
    pt->kind=port_string|prop;
    pt->rep.string.start=start;
    pt->rep.string.curr=start;
    pt->rep.string.past_the_end=past_the_end;
    return pt;
}

static pointer port_from_string(scheme *sc, char *start, char *past_the_end, int prop) {
    port *pt;
    pt=port_rep_from_string(sc,start,past_the_end,prop);
    if(pt==0) {
        return sc->NIL;
    }
    return mk_port(sc,pt);
}

#define BLOCK_SIZE 256

static port *port_rep_from_scratch(scheme *sc) {
  port *pt;
  char *start;
  pt=(port*)sc->malloc(sizeof(port));
  if(pt==0) {
    return 0;
  }
  start=(char*)sc->malloc(BLOCK_SIZE);
  if(start==0) {
    return 0;
  }
  memset(start,' ',BLOCK_SIZE-1);
  start[BLOCK_SIZE-1]='\0';
  pt->kind=port_string|port_output|port_srfi6;
  pt->rep.string.start=start;
  pt->rep.string.curr=start;
  pt->rep.string.past_the_end=start+BLOCK_SIZE-1;
  return pt;
}

static pointer port_from_scratch(scheme *sc) {
  port *pt;
  pt=port_rep_from_scratch(sc);
  if(pt==0) {
    return sc->NIL;
  }
  return mk_port(sc,pt);
}

static void port_close(scheme *sc, pointer p, int flag) {
    port *pt=p->_object._port;
    pt->kind&=~flag;
    if((pt->kind & (port_input|port_output))==0) {
        if(pt->kind&port_file) {
            fclose(pt->rep.stdio.file);
        }
        pt->kind=port_free;
    }
}

/* get new character from input file */
static int inchar(scheme *sc) {
    int c;
    port *pt;
again:
    pt=sc->inport->_object._port;
    c=basic_inchar(pt);
    if(c==EOF && sc->inport==sc->loadport && sc->file_i!=0) {
        file_pop(sc);
        if(sc->nesting!=0) {
            return EOF;
        }
        goto again;
    }
    return c;
}

static int basic_inchar(port *pt) {
    if(pt->kind&port_file) {
        return fgetc(pt->rep.stdio.file);
    } else {
        if(*pt->rep.string.curr==0
           || pt->rep.string.curr==pt->rep.string.past_the_end) {
            return EOF;
        } else {
            return *pt->rep.string.curr++;
        }
    }
}

/* back character to input buffer */
static inline void backchar(scheme *sc, int c) {
    port *pt;
    if(c==EOF) return;
    pt=sc->inport->_object._port;
    if(pt->kind&port_file) {
        ungetc(c,pt->rep.stdio.file);
    } else {
        if(pt->rep.string.curr!=pt->rep.string.start) {
            --pt->rep.string.curr;
        }
    }
}

static int realloc_port_string(scheme *sc, port *p)
{
  char *start=p->rep.string.start;
  size_t new_size=p->rep.string.past_the_end-start+1+BLOCK_SIZE;
  char *str=(char*)sc->malloc(new_size);
  if(str) {
    memset(str,' ',new_size-1);
    str[new_size-1]='\0';
    strcpy(str,start);
    p->rep.string.start=str;
    p->rep.string.past_the_end=str+new_size-1;
    p->rep.string.curr-=start-str;
    sc->free(start);
    return 1;
  } else {
    return 0;
  }
}

void putstr(scheme *sc, const char *s) {
  port *pt=sc->outport->_object._port;
  if(pt->kind&port_file) {
    fputs(s,pt->rep.stdio.file);
  } else {
    for(;*s;s++) {
      if(pt->rep.string.curr!=pt->rep.string.past_the_end) {
        *pt->rep.string.curr++=*s;
      } else if(pt->kind&port_srfi6&&realloc_port_string(sc,pt)) {
        *pt->rep.string.curr++=*s;
      }
    }
  }
}

static void putchars(scheme *sc, const char *s, int len) {
  port *pt=sc->outport->_object._port;
  if(pt->kind&port_file) {
    fwrite(s,1,len,pt->rep.stdio.file);
  } else {
    for(;len;len--) {
      if(pt->rep.string.curr!=pt->rep.string.past_the_end) {
        *pt->rep.string.curr++=*s++;
      } else if(pt->kind&port_srfi6&&realloc_port_string(sc,pt)) {
        *pt->rep.string.curr++=*s++;
      }
    }
  }
}

void putcharacter(scheme *sc, int c) {
  port *pt=sc->outport->_object._port;
  if(pt->kind&port_file) {
    fputc(c,pt->rep.stdio.file);
  } else {
    if(pt->rep.string.curr!=pt->rep.string.past_the_end) {
      *pt->rep.string.curr++=c;
    } else if(pt->kind&port_srfi6&&realloc_port_string(sc,pt)) {
      *pt->rep.string.curr++=c;
    }
  }
}

/* read characters up to delimiter, but cater to character constants */
static char   *readstr_upto(scheme *sc, char *delim) {
    char   *p = sc->strbuff;

    while (!is_one_of(delim, (*p++ = inchar(sc))));
    if(p==sc->strbuff+2 && p[-2]=='\\') {
        *p=0;
    } else {
        backchar(sc,p[-1]);
        *--p = '\0';
    }
    return sc->strbuff;
}

/* read string expression "xxx...xxx" */
static pointer readstrexp(scheme *sc) {
    char *p = sc->strbuff;
    int c;
    int c1=0;
    enum { st_ok, st_bsl, st_x1, st_x2} state=st_ok;

    for (;;) {
        c=inchar(sc);
        if(c==EOF || p-sc->strbuff>sizeof(sc->strbuff)-1) {
            return sc->F;
        }
        switch(state) {
        case st_ok:
            switch(c) {
            case '\\':
                state=st_bsl;
                break;
            case '"':
                *p=0;
                return mk_counted_string(sc,sc->strbuff,p-sc->strbuff);
            default:
                *p++=c;
                break;
            }
            break;
        case st_bsl:
            switch(c) {
            case 'x':
            case 'X':
                state=st_x1;
            c1=0;
            break;
            case 'n':
                *p++='\n';
                state=st_ok;
                break;
            case 't':
                *p++='\t';
                state=st_ok;
                break;
            case 'r':
                *p++='\r';
                state=st_ok;
                break;
            case '"':
                *p++='"';
                state=st_ok;
                break;
            default:
                *p++=c;
                state=st_ok;
                break;
            }
            break;
        case st_x1:
        case st_x2:
            c=toupper(c);
            if(c>='0' && c<='F') {
                if(c<='9') {
                    c1=(c1<<4)+c-'0';
                } else {
                    c1=(c1<<4)+c-'A'+10;
                }
                if(state==st_x1) {
                    state=st_x2;
                } else {
                    *p++=c1;
                    state=st_ok;
                }
            } else {
                return sc->F;
            }
            break;
        }
    }
}

/* check c is in chars */
static inline int is_one_of(const char *s, int c) {
    if(c==EOF) return 1;
    while (*s)
        if (*s++ == c)
            return (1);
    return (0);
}

/* skip white characters && cntrl characters */
static inline void skipspace(scheme *sc) {
    int c = inchar(sc);
    while (isspace(c) || iscntrl(c))
    {
        c = inchar(sc);
    }
    if(c!=EOF) {
        backchar(sc,c);
    }
}

/* get token */
static int token(scheme *sc) {
    int c;
    skipspace(sc);
    switch (c=inchar(sc)) {
    case EOF:
        return (TOK_EOF);
    case '(':
        return (TOK_LPAREN);
    case ')':
        return (TOK_RPAREN);
    case '.':
        c=inchar(sc);
        if (is_one_of(" \n\t",c)) {
            return (TOK_DOT);
        } else {
            backchar(sc,c);
            backchar(sc,'.');
            return TOK_ATOM;
        }
    case '\'':
        return (TOK_QUOTE);
    case ';':
        return (TOK_COMMENT);
    case '"':
        return (TOK_DQUOTE);
    case BACKQUOTE:
        return (TOK_BQUOTE);
    case ',':
        if ((c=inchar(sc)) == '@')
            return (TOK_ATMARK);
        else {
            backchar(sc,c);
            return (TOK_COMMA);
        }
    case '#':
        c=inchar(sc);
        if (c == '(') {
            return (TOK_VEC);
        } else if(c == '!') {
            return TOK_COMMENT;
        } else {
            backchar(sc,c);
            if(is_one_of((char*)" tfodxb\\",c)) {
                return TOK_SHARP_CONST;
            } else {
                return (TOK_SHARP);
            }
        }
    default:
        backchar(sc,c);
        return (TOK_ATOM);
    }
}

/* ========== Routines for Printing ========== */
#define   ok_abbrev(x)   (is_pair(x) && cdr(x) == sc->NIL)

static void printslashstring(scheme *sc, char *p, int len) {
    int i;
    unsigned char *s=(unsigned char*)p;
    putcharacter(sc,'"');
    for ( i=0; i<len; i++) {
        if(*s==0xff || *s=='"' || *s<' ' || *s=='\\') {
            putcharacter(sc,'\\');
            switch(*s) {
            case '"':
                putcharacter(sc,'"');
                break;
            case '\n':
                putcharacter(sc,'n');
                break;
            case '\t':
                putcharacter(sc,'t');
                break;
            case '\r':
                putcharacter(sc,'r');
                break;
            case '\\':
                putcharacter(sc,'\\');
                break;
            default: {
                int d=*s/16;
                putcharacter(sc,'x');
                if(d<10) {
                    putcharacter(sc,d+'0');
                } else {
                    putcharacter(sc,d-10+'A');
                }
                d=*s%16;
                if(d<10) {
                    putcharacter(sc,d+'0');
                } else {
                    putcharacter(sc,d-10+'A');
                }
            }
            }
        } else {
            putcharacter(sc,*s);
        }
        s++;
    }
    putcharacter(sc,'"');
}


/* print atoms */
static void printatom(scheme *sc, pointer l, int f) {
    char *p;
    int len;
    atom2str(sc,l,f,&p,&len);
    putchars(sc,p,len);
}


/* Uses internal buffer unless string pointer is already available */
static void atom2str(scheme *sc, pointer l, int f, char **pp, int *plen) {
    char *p = NULL;
    if (l == sc->NIL) {
        p = (char*)"()";
    } else if (l == sc->T) {
        p = (char*)"#t";
    } else if (l == sc->F) {
        p = (char*)"#f";
    } else if (l == sc->EOF_OBJ) {
        p = (char*)"#<EOF>";
    } else if (is_port(l)) {
        p = sc->strbuff;
        strcpy(p, "#<PORT>");
    } else if (is_number(l)) {
        p = sc->strbuff;
        if (is_integer(l)) {
            sprintf(p, "%" PRId64, ivalue_unchecked(l));
        } else if(is_rational(l)) {
            sprintf(p, "%" PRId64 "/%" PRId64, ratvalue_unchecked(l).n,ratvalue_unchecked(l).d);
            //sprintf(p, "%ld/%ld", l->_object._number.value.ratvalue.n, l->_object._number.value.ratvalue.d);
        } else {
            //std::stringstream ss;
            //ss << std::fixed << std::showpoint << rvalue_unchecked(l);
            //p = (char*) ss.str().c_str();
            sprintf(p, "%#.20g", rvalue_unchecked(l));
            //sprintf(p, "%#.4e", rvalue_unchecked(l));
        }
    } else if (is_string(l)) {
        if (!f) {
            p = strvalue(l);
        } else { /* Hack, uses the fact that printing is needed */
            *pp=sc->strbuff;
            *plen=0;
            printslashstring(sc, strvalue(l), strlength(l));
            return;
        }
    } else if (is_character(l)) {
        int c=charvalue_sc(sc,l);
        p = sc->strbuff;
        if (!f) {
            p[0]=c;
            p[1]=0;
        } else {
            switch(c) {
            case ' ':
                sprintf(p,"#\\space"); break;
            case '\n':
                sprintf(p,"#\\newline"); break;
            case '\r':
                sprintf(p,"#\\return"); break;
            case '\t':
                sprintf(p,"#\\tab"); break;
            default:
#if USE_ASCII_NAMES
                if(c==127) {
                    strcpy(p,"#\\del"); break;
                } else if(c<32) {
                    strcpy(p,"#\\"); strcat(p,charnames[c]); break;
                }
#else
                if(c<32) {
                    sprintf(p,"#\\x%x",c); break;
                }
#endif
                sprintf(p,"#\\%c",c); break;
            }
        }
    } else if (is_symbol(l)) {
        p = symname_sc(sc,l);
    } else if (is_proc(l)) {
        p = sc->strbuff;
        sprintf(p, "#<%s PROCEDURE %" PRId64 ">", procname(l),procnum(l));
    } else if (is_macro(l)) {
        p = (char*)"#<MACRO>";
    } else if (is_closure(l)) {
        p = (char*)"#<CLOSURE>";
    } else if (is_promise(l)) {
        p = (char*)"#<PROMISE>";
    } else if (is_foreign(l)) {
        p = sc->strbuff;
        sprintf(p, "#<FOREIGN PROCEDURE %" PRId64 ">", procnum(l));
    } else if (is_continuation(l)) {
        p = (char*)"#<CONTINUATION>";
    } else if (is_cptr(l)) {
        p = (char*)"#<CPTR>";
        // } else if (is_objc(l)) {
        //      p = (char*)"#<OBJC>";
    } else {
        p = (char*)"#<ERROR>";
    }
    *pp=p;
    *plen=strlen(p);
}
/* ========== Routines for Evaluation Cycle ========== */

/* make closure. c is code. e is environment */
pointer mk_closure(scheme *sc, pointer c, pointer e) {
    pointer x = get_cell(sc, c, e);

    typeflag(x) = T_CLOSURE;
    ////////////// write barrier for treadmill
#ifdef TREADMILL_CHECKS
      last_call_to_insert_treadmill = 12;
#endif
    insert_treadmill(sc,c);
#ifdef TREADMILL_CHECKS
      last_call_to_insert_treadmill = 13;
#endif
    insert_treadmill(sc,e);
    //////////////////////////////////////////

    car(x) = c;
    cdr(x) = e;
    return (x);
}

/* make continuation. */
pointer mk_continuation(scheme *sc) {
    pointer y = dump_stack_copy(sc);
    pointer x = get_cell(sc, y, sc->NIL);
    typeflag(x) = T_CONTINUATION;
    cont_dump(x) = y;
    return (x);
}

static pointer list_star(scheme *sc, pointer d) {
    pointer p, q;
    if(cdr(d)==sc->NIL) {
        return car(d);
    }
    p=cons(sc,car(d),cdr(d));
    q=p;
    while(cdr(cdr(p))!=sc->NIL) {
        d=cons(sc,car(p),cdr(p));
        if(cdr(cdr(p))!=sc->NIL) {
            p=cdr(d);
        }
    }
    // NOT SURE ABOUT THIS ONE? DOING Q for good measure
    ////////////// write barrier for treadmill
#ifdef TREADMILL_CHECKS
      last_call_to_insert_treadmill = 14;
#endif
    insert_treadmill(sc,car(cdr(p)));
#ifdef TREADMILL_CHECKS
      last_call_to_insert_treadmill = 15;
#endif
    insert_treadmill(sc,q);
    //////////////////////////////////////////

    cdr(p)=car(cdr(p));
    return q;
}

/* reverse list -- produce new list */
/*static*/ pointer reverse(scheme *sc, pointer a) {
    /* a must be checked by gc */
    if(a == sc->NIL) return sc->NIL;

    pointer p = sc->NIL;

    for ( ; is_pair(a); a = cdr(a)) {
        p = cons(sc, car(a), p);
    }
    return (p);
}

/* reverse list --- in-place */
/*static*/ pointer reverse_in_place(scheme *sc, pointer term, pointer list) {
    pointer p = list, result = term, q;

    while (p != sc->NIL) {
        q = cdr(p);
        cdr(p) = result;
        result = p;
        p = q;
    }
    ////////////// write barrier for treadmill
#ifdef TREADMILL_CHECKS
      last_call_to_insert_treadmill = 16;
#endif
    insert_treadmill(sc,result);
    ////////////////////////////////////////////
    return (result);
}

/* append list -- produce new list */
/*static*/ pointer append(scheme *sc, pointer a, pointer b) {
    pointer p = b, q;

    if (a != sc->NIL) {
        a = reverse(sc, a);
        while (a != sc->NIL) {
            q = cdr(a);
            cdr(a) = p;
            p = a;
            a = q;
        }
    }
    ////////////// write barrier for treadmill
#ifdef TREADMILL_CHECKS
      last_call_to_insert_treadmill = 17;
#endif
    insert_treadmill(sc,p);
    //////////////////////////////////////////

    return (p);
}

/* equivalence of atoms */
/*static*/ int eqv(pointer a, pointer b) {
    if (is_string(a)) {
        if (is_string(b))
            return (strvalue(a) == strvalue(b));
        else
            return (0);
    } else if (is_number(a)) {
        if (is_number(b))
            return num_eq(nvalue(a),nvalue(b));
        else
            return (0);
    } else if (is_character(a)) {
        if (is_character(b))
            return charvalue(a)==charvalue(b);
        else
            return (0);
    } else if (is_port(a)) {
        if (is_port(b))
            return a==b;
        else
            return (0);
    } else if (is_proc(a)) {
        if (is_proc(b))
            return procnum(a)==procnum(b);
        else
            return (0);
    } else {
        return (a == b);
    }
}

/*static*/ int eqv_sc(scheme* sc, pointer a, pointer b) {
    if (is_string(a)) {
        if (is_string(b))
            return (strvalue(a) == strvalue(b));
        else
            return (0);
    } else if (is_number(a)) {
        if (is_number(b))
            return num_eq(nvalue(a),nvalue(b));
        else
            return (0);
    } else if (is_character(a)) {
        if (is_character(b))
            return charvalue_sc(sc,a)==charvalue_sc(sc,b);
        else
            return (0);
    } else if (is_port(a)) {
        if (is_port(b))
            return a==b;
        else
            return (0);
    } else if (is_proc(a)) {
        if (is_proc(b))
            return procnum(a)==procnum(b);
        else
            return (0);
    } else if (is_cptr(a)) {
        if (is_cptr(b))
            return a->_object._cptr==b->_object._cptr;
        else
            return (0);
        // } else if (is_objc(a)) {
        //      if (is_objc(b))
        //              return a->_object._cptr==b->_object._cptr;
        //      else
        //              return (0);
    } else {
        return (a == b);
    }
}


/* true or false value macro */
/* () is #t in R5RS */
#define is_true(p)       ((p) != sc->F)
#define is_false(p)      ((p) == sc->F)

/* ========== Environment implementation  ========== */

static inline uint64_t str_hash(const char* str)
{
    uint64_t result(0);
    unsigned char c;
    while((c = *(str++))) {
        result = result * 33 + c;
    }
    return result;
}

static inline int hash_fn(const char *key, int table_size)
{
    return str_hash(key) % table_size;
}

/*
 * In this implementation, each frame of the environment may be
 * a hash table: a vector of alists hashed by variable name.
 * In practice, we use a vector only for the initial frame;
 * subsequent frames are too small and transient for the lookup
 * speed to out-weigh the cost of making a new vector.
 */
static void new_frame_in_env(scheme *sc, pointer old_env)
{
    pointer new_frame;

    /* The interaction-environment has about 300 variables in it. */
    if (old_env == sc->NIL) {
        new_frame = mk_vector(sc, 65521);
    } else {
        new_frame = sc->NIL;
    }

    sc->envir = immutable_cons(sc, new_frame, old_env);
    setenvironment(sc->envir);
}

static inline void new_slot_spec_in_env(scheme *sc, pointer env,
                                        pointer variable, pointer value)
{
  pointer slot = immutable_cons(sc, variable, value);

  if (is_vector(car(env))) {
    int location = hash_fn(symname_sc(sc,variable), (car(env))->_size);
    set_vector_elem(sc, car(env), location,
                    immutable_cons(sc, slot, vector_elem(car(env), location)));
  } else {
    pointer tmp = immutable_cons(sc, slot, car(env));
    ////////////// write barrier for treadmill
#ifdef TREADMILL_CHECKS
    last_call_to_insert_treadmill = 18;
#endif
    insert_treadmill(sc,tmp);
    //////////////////////////////////////////
    car(env) = tmp;
  }
}

pointer find_slot_in_env(scheme *sc, pointer env, pointer hdl, int all)
{
    pointer x = NULL;
    pointer y = NULL;;
    int location;

    for (x = env; x != sc->NIL; x = cdr(x)) {
        if (is_vector(car(x))) {
            location = hash_fn(symname_sc(sc,hdl), (car(x))->_size);
            y = vector_elem(car(x), location);
        } else {
            y = car(x);
        }
        for ( ; y != sc->NIL; y = cdr(y)) {
            if (caar(y) == hdl) {
                break;
            }
        }
        if (y != sc->NIL) {
            break;
        }
        if(!all) {
            return sc->NIL;
        }
    }
    if (x != sc->NIL) {
        return car(y);
    }
    return sc->NIL;
}


inline void new_slot_in_env(scheme *sc, pointer variable, pointer value)
{
    new_slot_spec_in_env(sc, sc->envir, variable, value);
}

inline void set_slot_in_env(scheme *sc, pointer slot, pointer value)
{
    ////////////// write barrier for treadmill
#ifdef TREADMILL_CHECKS
      last_call_to_insert_treadmill = 19;
#endif
    insert_treadmill(sc,value);
    //////////////////////////////////////////

    cdr(slot) = value;
}

inline pointer slot_value_in_env(pointer slot)
{
    return cdr(slot);
}


/* ========== Evaluation Cycle ========== */
static pointer _Error_1(scheme *sc, const char *s, pointer a, int location, int errnum) {
    // closure stack trace
    int cnt = 0;
    char* fname = (char*) "toplevel";
    char* current = 0;
    std::stringstream sss;
    if(is_symbol(sc->last_symbol_apply)) {
        fname = symname_sc(sc,sc->last_symbol_apply);
        current = symname_sc(sc,sc->last_symbol_apply);
        sss << symname_sc(sc,sc->last_symbol_apply);
    }

    if(current==0) {
      while(!sc->applied_symbol_names.empty() && cnt<10)
        {
          pointer item = sc->applied_symbol_names.top();
          std::stringstream ss;
          extemp::UNIV::printSchemeCell(sc, ss, item, true);
          std::cout << "stack-catch: " << ss.str() << std::endl;
          cnt++;
          sc->applied_symbol_names.pop();
        }
    }

    while(!sc->applied_symbol_names.empty() && cnt<10 && current != 0)
    {
        pointer item = sc->applied_symbol_names.top();
        if(is_symbol(item)) {
            if(strcmp(current,symname_sc(sc,item)) == 0) {
                sc->applied_symbol_names.pop();
                continue;
            }
            sss << " <- " << symname_sc(sc,item);
            current = symname_sc(sc,item);
        }
        cnt++;
        sc->applied_symbol_names.pop();
    }
    //if(cnt>9) sss << " ... " << std::endl;
    //else sss << std::endl;

    // log message
    char msg[256*10];
    int position = 0;
    if(a!=0) {
        position = location; //a->_debugger->_size;
        std::stringstream ss;
        extemp::UNIV::printSchemeCell(sc, ss, a, true);
        //sprintf(msg, "position:(%d) in function \"%s\"\n%s\nwith: %s\nTrace: %s",position,fname,s,ss.str().c_str(),sss.str().c_str());
        sprintf(msg, "%s %s\nTrace: %s",s,ss.str().c_str(),sss.str().c_str());
        sc->error_position = position;
    }else{
        position = location; //sc->code->_size;
        //sprintf(msg, "position:(%d) in function \"%s\"\n%s\nTrace: %s",position,fname,s,sss.str().c_str());
        sprintf(msg, "%s\nTrace: %s",s,sss.str().c_str());
        sc->error_position = position;
    }
    std::cout << msg << std::endl;
    //printf(msg);
    // CPPBridge::log("");
    // CPPBridge::error(msg);
    // CPPBridge::log("");

    if(is_symbol(sc->last_symbol_apply)) {
        memcpy(sc->error_fname,strvalue(car(sc->last_symbol_apply)),strlength(car(sc->last_symbol_apply)));
    }
    //memset(fname, 0, 256);
    // this as error return string - we parse this in the editor so it's format is important!
    sprintf(msg,"%s::%d::%s",fname,position,s);
    putstr(sc, msg); // this line sends fname to scheme stderr (which is read as a return result by schemeinterface)



#if USE_ERROR_HOOK
    pointer x;
    pointer hdl=sc->ERROR_HOOK;

    x=find_slot_in_env(sc,sc->envir,hdl,1);
    if (x != sc->NIL && slot_value_in_env(x) != sc->NIL) {
        if(a!=0) {
            sc->code = cons(sc, cons(sc, sc->QUOTE, cons(sc,(a), sc->NIL)), sc->NIL);
        } else {
            sc->code = sc->NIL;
        }
        sc->code = cons(sc, mk_string(sc, (s)), sc->code);
        setimmutable(car(sc->code));
        sc->code = cons(sc, slot_value_in_env(x), sc->code);
        sc->op = (int)OP_EVAL;

        sc->last_symbol_apply = sc->NIL;
        sc->call_end_time = ULLONG_MAX;
        // empty applied_symbol_names stack is empty
        while(!sc->applied_symbol_names.empty())
        {
            sc->applied_symbol_names.pop();
        }
        return sc->T;
    }

    hdl = sc->LIVECODING_ERROR_HOOK;
    x=find_slot_in_env(sc,sc->envir,hdl,1);
    if (x != sc->NIL && slot_value_in_env(x) != sc->NIL ) {
        if(a!=0) {
            pointer p1 = mk_integer(sc, errnum);
            pointer p2 = cons(sc,p1,sc->NIL);
            pointer p3;
            {
                EnvInjector injector(sc, p2);
                p3 = cons(sc, sc->QUOTE, cons(sc, a, sc->NIL)); // need to quote 'a
            }
            sc->code = cons(sc, p3, p2);
            //sc->code = cons(sc, cons(sc, sc->QUOTE, cons(sc,(a), sc->NIL)), sc->NIL);
    } else {
            //sc->code = sc->NIL;
            pointer p1 = mk_integer(sc, errnum);
            pointer p2 = cons(sc,p1,sc->NIL);
            pointer p3;
            {
                EnvInjector injector(sc, p2);
                p3 = cons(sc, sc->QUOTE, cons(sc, sc->F, sc->NIL));
            }
            sc->code = cons(sc, p3, p2);
    }
    sc->code = cons(sc, mk_string(sc, (s)), sc->code);
    setimmutable(car(sc->code));
    sc->code = cons(sc, slot_value_in_env(x), sc->code);
    sc->op = (int)OP_EVAL;

    sc->last_symbol_apply = sc->NIL;
    sc->call_end_time = ULLONG_MAX;
    // empty applied_symbol_names stack is empty
    while(!sc->applied_symbol_names.empty())
      {
        sc->applied_symbol_names.pop();
      }
    return sc->T;
  }



#endif

  if(a!=0) {
    sc->args = cons(sc, (a), sc->NIL);
  } else {
    sc->args = sc->NIL;
  }
  //sc->args = cons(sc, mk_string(sc, (s)), sc->args);
  sc->args = cons(sc, mk_string(sc, fname), sc->args);
  setimmutable(car(sc->args));
  sc->op = (int)OP_ERR0;

  // empty applied_symbol_names stack is empty
  while(!sc->applied_symbol_names.empty())
    {
      sc->applied_symbol_names.pop();
    }
  sc->last_symbol_apply = sc->NIL;
  sc->call_end_time = ULLONG_MAX;

  return sc->T;
}
#define Error_1(sc,s,a,l) return _Error_1(sc,s,a,l)
#define Error_0(sc,s,l)    return _Error_1(sc,s,0,l)

/* Too small to turn into function */
# define  BEGIN     do {
# define  END  } while (0)
#define s_goto(sc,a) BEGIN                      \
    sc->op = (int)(a);                          \
    return sc->T; END

#define s_return(sc,a) return _s_return(sc,a)

#ifndef USE_SCHEME_STACK

#define STACK_GROWTH 3

static void s_save(scheme *sc, enum scheme_opcodes op, pointer args, pointer code)
{
    sc->applied_symbol_names.push(sc->last_symbol_apply);
//      if(is_symbol(sc->last_symbol_apply)) {
//              std::cout << "PUSH SYM " << sc->applied_symbol_names.size() << " " << symname(sc->last_symbol_apply) << std::endl;
//      }else{
//              std::cout << "PUSH SYM " << sc->applied_symbol_names.size() << std::endl;
//      }

    intptr_t nframes = (intptr_t)sc->dump;
    struct dump_stack_frame *next_frame;

    /* enough room for the next frame? */
    if (nframes >= sc->dump_size) {
        sc->dump_size += STACK_GROWTH;
        /* alas there is no sc->realloc */
        sc->dump_base = realloc(sc->dump_base, sizeof(struct dump_stack_frame) * sc->dump_size);
    }
    next_frame = (struct dump_stack_frame *)sc->dump_base + nframes;
    next_frame->op = op;
    next_frame->args = args;
    next_frame->envir = sc->envir;
    next_frame->code = code;
    sc->dump = (pointer)(nframes+1);
}

static pointer _s_return(scheme *sc, pointer a)
{
    if(sc->applied_symbol_names.empty()) {
        sc->last_symbol_apply = sc->NIL;
    }else{
        sc->last_symbol_apply = sc->applied_symbol_names.top();
        sc->applied_symbol_names.pop();
    }

    intptr_t nframes = (intptr_t)sc->dump;
    struct dump_stack_frame *frame;

    sc->value = (a);
    if (nframes <= 0) {
        return sc->NIL;
    }
    nframes--;
    frame = (struct dump_stack_frame *)sc->dump_base + nframes;
    sc->op = frame->op;
    sc->args = frame->args;
    sc->envir = frame->envir;
    sc->code = frame->code;
    sc->dump = (pointer)nframes;
    return sc->T;
}

static inline void dump_stack_reset(scheme *sc)
{
    /* in this implementation, sc->dump is the number of frames on the stack */
    sc->dump = (pointer)0;
}

static inline void dump_stack_initialize(scheme *sc)
{
    sc->dump_size = 0;
    sc->dump_base = NULL;
    dump_stack_reset(sc);
}

static void dump_stack_free(scheme *sc)
{
    sc->free(sc->dump_base);
    sc->dump_base = NULL;
    sc->dump = (pointer)0;
    sc->dump_size = 0;
}
/*
static inline void dump_stack_mark(scheme *sc)
{
    intptr_t nframes = (intptr_t)sc->dump;
    int i;
    for(i=0; i<nframes; i++) {
        struct dump_stack_frame *frame;
        frame = (struct dump_stack_frame *)sc->dump_base + i;
        mark(frame->args);
        mark(frame->envir);
        mark(frame->code);
    }
}
*/

static pointer list_copy(scheme* Scheme, pointer Args)
{
    pointer ret = Scheme->NIL;
    for (; Args != Scheme->NIL; Args = cdr(Args)) {
        ret = cons(Scheme, car(Args), ret);
    }
    return reverse_in_place(Scheme, Scheme->NIL, ret);
}

static void dump_stack_continuation_set(scheme* sc, pointer p)
{
    unsigned int* olddump = (unsigned int*) cptr_value_sc(sc,p);
    ptrdiff_t nframes = olddump[0];
    memcpy(sc->dump_base, &olddump[1], nframes * sizeof(struct dump_stack_frame));
    sc->dump = pointer(nframes);
    dump_stack_frame* frames = (dump_stack_frame*) sc->dump_base;
    for (int j=0;j<nframes;j++) {
        frames[j].args = list_copy(sc, frames[j].args);
        if (is_symbol(frames[j].code)) {
            frames[j].code = mk_symbol(sc, symname_sc(sc,frames[j].code));
        } else {
            frames[j].code = list_copy(sc, frames[j].code);
        }
    }
}

pointer dump_stack_copy(scheme* sc)
{
    intptr_t nframes = (intptr_t)sc->dump;

    unsigned int* newdump = (unsigned int*) malloc(4+(nframes*sizeof(struct dump_stack_frame)));
    memcpy(&newdump[1],sc->dump_base, nframes*sizeof(struct dump_stack_frame));
    newdump[0] = nframes;

    pointer new_stack_ptr = mk_cptr(sc, newdump);
    EnvInjector injector(sc, new_stack_ptr);

    dump_stack_frame* frames = (dump_stack_frame*)&newdump[1];
    for (int j=0;j < nframes; j++)
    {
        frames[j].args = list_copy(sc, frames[j].args);
        if (is_symbol(frames[j].code)) {
            frames[j].code = mk_symbol(sc, symname_sc(sc,frames[j].code));
        } else {
            frames[j].code = list_copy(sc, frames[j].code);
        }
    }
    return new_stack_ptr;
}

#else

static inline void dump_stack_reset(scheme *sc)
{
    sc->dump = sc->NIL;
}

static inline void dump_stack_initialize(scheme *sc)
{
    dump_stack_reset(sc);
}

static void dump_stack_free(scheme *sc)
{
    sc->dump = sc->NIL;
}

static pointer _s_return(scheme *sc, pointer a) {
    sc->value = (a);
    if(sc->dump==sc->NIL) return sc->NIL;
    sc->op = ivalue(car(sc->dump));
    sc->args = cadr(sc->dump);
    sc->envir = caddr(sc->dump);
    sc->code = cadddr(sc->dump);
    sc->dump = cddddr(sc->dump);
    return sc->T;
}

static void s_save(scheme *sc, enum scheme_opcodes op, pointer args, pointer code) {
    sc->dump = cons(sc, sc->envir, cons(sc, (code), sc->dump));
    sc->dump = cons(sc, (args), sc->dump);
    sc->dump = cons(sc, mk_integer(sc, (long)(op)), sc->dump);
}

// NOTE:  This is probably really needlessly inefficient
// I'm probably doing to much gc_masking etc. and should
// probably avoid using the list reverse.
//
// Also note that this function uses tmp_dump and tmp_args
// two new pointers added to the scheme data structure and
// marked before each GC.  Probably could do without these
// as well :)
//
// All that said, at least continuations now work :)
static pointer dump_stack_copy(scheme* sc, pointer dump) {
    sc->tmp_dump = dump;

    sc->tmp_dump = reverse(sc,sc->tmp_dump);
    sc->tmp_args = sc->NIL;
    for( ;sc->tmp_dump != sc->NIL; sc->tmp_dump=cdr(sc->tmp_dump))
    {
        pointer val = car(sc->tmp_dump);
        if(is_pair(val) && !is_environment(val)) {
            int lgth = list_length(sc, val);
            if(lgth<0) { // pair
                sc->tmp_args = cons(sc, cons(sc,car(val),cdr(val)), sc->tmp_args);
            }else{ // list
                pointer t = sc->NIL;
                for(int i=0;i<lgth;i++)
                {
                    t = cons(sc, list_ref(sc,i,val), t);
                }
                sc->tmp_args = cons(sc, reverse(sc,t), sc->tmp_args);
            }
        }else{
            sc->tmp_args = cons(sc, val, sc->tmp_args);
        }
    }

    return sc->tmp_args;
}
/*
static inline void dump_stack_mark(scheme *sc)
{
    mark(sc->dump);
}
*/
#endif

#define s_retbool(tf)    s_return(sc,(tf) ? sc->T : sc->F)

static pointer opexe_0(scheme *sc, enum scheme_opcodes op) {
    pointer x, y;
    switch (op) {
    case OP_LOAD:       /* load */
        if(file_interactive(sc)) {
            fprintf(sc->outport->_object._port->rep.stdio.file,
                    "Loading %s\n", strvalue(car(sc->args)));
        }
        if (!file_push(sc,strvalue(car(sc->args)))) {
            Error_1(sc,"unable to open", car(sc->args), sc->code->_debugger->_size);
        }
        if(file_interactive(sc)) {
            putstr(sc,"\n");
        }
        sc->nesting=0;
        //dump_stack_reset(sc);
        sc->envir = sc->global_env;
        sc->save_inport=sc->inport;
        sc->inport = sc->loadport;
        //s_save(sc,OP_T0LVL, sc->NIL, sc->NIL);
        //s_save(sc,OP_VALUEPRINT, sc->NIL, sc->NIL);
        s_save(sc,OP_T1LVL, sc->NIL, sc->NIL);
        if (file_interactive(sc)) {
            putstr(sc,prompt);
        }
        s_goto(sc,OP_READ_INTERNAL);

    case OP_T0LVL: /* top level */
        if(file_interactive(sc)) {
            putstr(sc,"\n");
        }
        sc->nesting=0;
        dump_stack_reset(sc);
        sc->envir = sc->global_env;
        sc->save_inport=sc->inport;
        sc->inport = sc->loadport;
        s_save(sc,OP_T0LVL, sc->NIL, sc->NIL);
        s_save(sc,OP_VALUEPRINT, sc->NIL, sc->NIL);
        s_save(sc,OP_T1LVL, sc->NIL, sc->NIL);
        if (file_interactive(sc)) {
            putstr(sc,prompt);
        }
        s_goto(sc,OP_READ_INTERNAL);

    case OP_T1LVL: /* top level */
        sc->code = sc->value;
        sc->inport=sc->save_inport;
        s_goto(sc,OP_EVAL);

    case OP_READ_INTERNAL:       /* internal read */
        sc->tok = token(sc);
        if(sc->tok==TOK_EOF) {
            if(sc->inport==sc->loadport) {
                sc->args=sc->NIL;
                s_goto(sc,OP_QUIT);
            } else {
                s_return(sc,sc->EOF_OBJ);
            }
        }
        s_goto(sc,OP_RDSEXPR);

    case OP_GENSYM:
        s_return(sc, gensym(sc));

    case OP_VALUEPRINT: /* print evaluation result */
                        /* OP_VALUEPRINT is always pushed, because when changing from
                           non-interactive to interactive mode, it needs to be
                           already on the stack */
        if(sc->tracing) {
            putstr(sc,"\nGives: ");
        }
        if(file_interactive(sc)) {
            sc->print_flag = 1;
            sc->args = sc->value;
            s_goto(sc,OP_P0LIST);
        } else {
            s_return(sc,sc->value);
        }

    case OP_EVAL:       /* main part of evaluation */
#if USE_TRACING
        if(sc->tracing) {
            /*s_save(sc,OP_VALUEPRINT,sc->NIL,sc->NIL);*/
            s_save(sc,OP_REAL_EVAL,sc->args,sc->code);
            sc->args=sc->code;
            putstr(sc,"\nEval: ");
            s_goto(sc,OP_P0LIST);
        }
        /* fall through */
    case OP_REAL_EVAL:
#endif
        if (is_symbol(sc->code)) {    /* symbol */
            //printf("evaluating symbol: %s\n",symname_sc(sc,sc->code));
            x=find_slot_in_env(sc,sc->envir,sc->code,1);
            if (x != sc->NIL) {
                pointer xx = slot_value_in_env(x);
                xx->_debugger = sc->code->_debugger;
                s_return(sc,xx);
                //s_return(sc,slot_value_in_env(x));
            } else {
              if(llvm_check_valid_dot_symbol(sc,symname(sc->code))) {
                s_return(sc,sc->code);
              }else{
                Error_1(sc,"eval: unbound variable:", sc->code, sc->code->_debugger->_size);
              }
            }
        } else if (is_pair(sc->code)) {
            if (is_syntax(x = car(sc->code))) {     /* SYNTAX */
                sc->code = cdr(sc->code);
                s_goto(sc,syntaxnum(x));
            } else {/* first, eval top element and eval arguments */
                s_save(sc,OP_E0ARGS, sc->NIL, sc->code);
                /* If no macros => s_save(sc,OP_E1ARGS, sc->NIL, cdr(sc->code));*/
                car(sc->code)->_debugger = sc->code;
                sc->code = car(sc->code);
                s_goto(sc,OP_EVAL);
            }
        } else {
            s_return(sc,sc->code);
        }

    case OP_E0ARGS:     /* eval arguments */
        if (is_macro(sc->value)) {    /* macro expansion */
            s_save(sc,OP_DOMACRO, sc->code, sc->NIL);
            //pointer swap_debug = sc->code;
            sc->args = cons(sc,sc->code, sc->NIL);
            sc->code = sc->value;
            //sc->code->_debugger = swap_debug;
            s_goto(sc,OP_APPLY);
        } else {
            sc->code = cdr(sc->code);
            s_goto(sc,OP_E1ARGS);
        }

    case OP_E1ARGS:     /* eval arguments */
        sc->args = cons(sc, sc->value, sc->args);
        if (is_pair(sc->code)) { /* continue */
            s_save(sc,OP_E1ARGS, sc->args, cdr(sc->code));

            car(sc->code)->_debugger = sc->code;
            sc->code = car(sc->code);
            sc->args = sc->NIL;
            s_goto(sc,OP_EVAL);
        } else {  /* end */
            sc->args = reverse_in_place(sc, sc->NIL, sc->args);
            sc->code = car(sc->args);
            sc->args = cdr(sc->args);
            s_goto(sc,OP_APPLY);
        }

#if USE_TRACING
    case OP_TRACING: {
        int tr=sc->tracing;
        sc->tracing=ivalue(car(sc->args));
        s_return(sc,mk_integer(sc,tr));
    }
#endif

    case OP_APPLY:      /* apply 'code' to 'args' */
#if USE_TRACING
        if(sc->tracing) {
            s_save(sc,OP_REAL_APPLY,sc->args,sc->code);
            sc->print_flag = 1;
            /*   sc->args=cons(sc,sc->code,sc->args);*/
            putstr(sc,"\nApply to: ");
            s_goto(sc,OP_P0LIST);
        }

        /* fall through */
    case OP_REAL_APPLY:
#endif
        if (is_proc(sc->code)) {
            s_goto(sc,procnum(sc->code));   /* PROCEDURE */
        } else if (is_foreign(sc->code)) {
            x=sc->code->_object._ff(sc,sc->args);
            s_return(sc,x);
        } else if (is_closure(sc->code) || is_macro(sc->code) || is_promise(sc->code)) { /* CLOSURE */

            sc->last_symbol_apply = closure_code(sc->code)->_debugger;
//                              if(is_symbol(sc->last_symbol_apply)) {
//                                      std::cout << "SYM: " << symname(sc->last_symbol_apply) << std::endl;
//                              }

            /* Should not accept promise */
            /* make environment */
            new_frame_in_env(sc, closure_env(sc->code));
            for (x = car(closure_code(sc->code)), y = sc->args; is_pair(x); x = cdr(x), y = cdr(y))
            {
                if (y == sc->NIL) {
                    Error_1(sc,"not enough arguments calling: ",sc->args,sc->code->_debugger->_size);
                    std::cout << "NOT ENOUGH ARGS" << std::endl;
                } else {
                    new_slot_in_env(sc, car(x), car(y));
                }
            }
            if (x == sc->NIL) {
                /*--
                 * if (y != sc->NIL) {
                 *   Error_0(sc,"too many arguments");
                 * }
                 */
            } else if (is_symbol(x)) {
                new_slot_in_env(sc, x, y);
            } else {
                Error_1(sc,"syntax error in closure: not a symbol:", x,sc->code->_debugger->_size);
                std::cout << "SYNTAX ERROR" << std::endl;
            }

            sc->func_called_by_extempore = car(sc->code);
            sc->code = cdr(closure_code(sc->code));
            sc->args = sc->NIL;
            s_goto(sc,OP_BEGIN);
        } else if (is_continuation(sc->code)) { /* CONTINUATION */
            dump_stack_continuation_set(sc, cont_dump(sc->code));
            s_return(sc,sc->args != sc->NIL ? car(sc->args) : sc->NIL);
        } else {
          if(llvm_check_valid_dot_symbol(sc, symname(sc->code))) {
            // all good so far so now we check llvm
            pointer ppp = llvm_scheme_env_set(sc, symname(sc->code));
            s_return(sc,ppp);
          }else{
            Error_0(sc,"illegal function",sc->code->_debugger->_size);
          }
        }

    case OP_DOMACRO:    /* do macro */
        sc->code = sc->value;
        //this size stuff here for debugging
        sc->code->_size = sc->args->_size;
        s_goto(sc,OP_EVAL);

    case OP_LAMBDA:     /* lambda */
        s_return(sc,mk_closure(sc, sc->code, sc->envir));

    case OP_MKCLOSURE: /* make-closure */
        x=car(sc->args);
        if(car(x)==sc->LAMBDA) {
            x=cdr(x);
        }
        if(cdr(sc->args)==sc->NIL) {
            y=sc->envir;
        } else {
            y=cadr(sc->args);
        }
        s_return(sc,mk_closure(sc, x, y));

    case OP_QUOTE:      /* quote */
        x=car(sc->code);
        s_return(sc,car(sc->code));

    case OP_DEF0:  /* define */
        if (is_pair(car(sc->code))) {
            //std::stringstream ss;
            // imp::SchemeInterface::printSchemeCell(sc, ss, car(sc->code), true);
            //std::cout << "new func: " << ss.str() << std::endl;
            char* pp;
            int ll;
            atom2str(sc,caar(sc->code),0,&pp,&ll);
            std::string str(pp);
            //tinyscheme code
            x = caar(sc->code);
            sc->code = cons(sc, sc->LAMBDA, cons(sc, cdar(sc->code), cdr(sc->code)));
        } else {
            x = car(sc->code);
            sc->code = cadr(sc->code);

            if(is_pair(sc->code)) {
                char* pp;
                int ll;
                atom2str(sc,cadr(sc->code),0,&pp,&ll);
                std::string str2(pp);
                //std::stringstream ss;
                // imp::SchemeInterface::printSchemeCell(sc, ss, cadr(sc->code), true);
                std::string str(symname_sc(sc,x));
                str = str + " ";
                //str = ss.str().insert(1,str);
                str = str2.insert(1,str);
                // if(strcmp("test",symname_sc(sc,x)) == 0) printf("test = %p,%p",cdr(sc->code),x);
            }
        }

        if(is_pair(sc->code)) {
            cdr(sc->code)->_debugger = x;
        }
//                      std::stringstream ss;
//                      imp::SchemeInterface::printSchemeCell(sc, ss, sc->code);
//                      //std::cout << "new func: " << ss.str() << std::endl;
//                      std::string str(symname_sc(sc,x));
//                      str = str + " ";
//                      str = ss.str().insert(1,str);
//                      sc->reverse_symbol_lookup->insert(std::make_pair(cdr(sc->code),x));
//                      //                              if(strcmp("test",symname_sc(sc,x)) == 0) printf("test = %p,%p",cdr(sc->code),x);
//              }
        /////////////////////////////////////////////////////////////////////////////////////////////////////////////


        if (!is_symbol(x)) {
            Error_1(sc,"variable is not a symbol",x,sc->code->_debugger->_size);
        }
        s_save(sc,OP_DEF1, sc->NIL, x);
        s_goto(sc,OP_EVAL);

    case OP_DEF1:  /* define */
        x=find_slot_in_env(sc,sc->envir,sc->code,0);
        if (x != sc->NIL) {
            set_slot_in_env(sc, x, sc->value);
        } else {
            new_slot_in_env(sc, sc->code, sc->value);
        }
        s_return(sc,sc->code);


    case OP_DEFP:  /* defined? */
        x=sc->envir;
        if(cdr(sc->args)!=sc->NIL) {
            x=cadr(sc->args);
        }
        s_retbool(find_slot_in_env(sc,x,car(sc->args),1)!=sc->NIL);

    case OP_SET0:       /* set! */
        s_save(sc,OP_SET1, sc->NIL, car(sc->code));
        sc->code = cadr(sc->code);
        s_goto(sc,OP_EVAL);

    case OP_SET1:       /* set! */
        y=find_slot_in_env(sc,sc->envir,sc->code,1);
        if (y != sc->NIL) {
            set_slot_in_env(sc, y, sc->value);
            s_return(sc,sc->value);
        } else {
            Error_1(sc,"set!: unbound variable:", 0, sc->code->_debugger->_size);
        }

    case OP_BEGIN:      /* begin */
        if (!is_pair(sc->code)) {
            s_return(sc,sc->code);
        }
        if (cdr(sc->code) != sc->NIL) {
            s_save(sc,OP_BEGIN, sc->NIL, cdr(sc->code));
        }
        sc->code = car(sc->code);
        s_goto(sc,OP_EVAL);

    case OP_IF0:        /* if */
        s_save(sc,OP_IF1, sc->NIL, cdr(sc->code));
        sc->code = car(sc->code);
        s_goto(sc,OP_EVAL);

    case OP_IF1:        /* if */
        if (is_true(sc->value))
            sc->code = car(sc->code);
        else
            sc->code = cadr(sc->code);  /* (if #f 1) ==> () because
                                         * car(sc->NIL) = sc->NIL */
        s_goto(sc,OP_EVAL);

    case OP_LET0:       /* let */
        sc->args = sc->NIL;
        sc->value = sc->code;
        sc->code = is_symbol(car(sc->code)) ? cadr(sc->code) : car(sc->code);
        s_goto(sc,OP_LET1);

    case OP_LET1:       /* let (calculate parameters) */
        sc->args = cons(sc, sc->value, sc->args);
        if (is_pair(sc->code)) { /* continue */
            s_save(sc,OP_LET1, sc->args, cdr(sc->code));
            //sorensen replaced this:
            //sc->code = cadar(sc->code);

            // with this:
            sc->code = car(sc->code);
            if(!is_pair(sc->code))
              Error_0(sc,"Scm Error: Poorly formed let!", sc->code->_size);
            sc->code = cadr(sc->code);
            ////////////////////

            sc->args = sc->NIL;
            s_goto(sc,OP_EVAL);
        } else {  /* end */
            sc->args = reverse_in_place(sc, sc->NIL, sc->args);
            sc->code = car(sc->args);
            sc->args = cdr(sc->args);
            s_goto(sc,OP_LET2);
        }

    case OP_LET2:       /* let */
        new_frame_in_env(sc, sc->envir);
        for (x = is_symbol(car(sc->code)) ? cadr(sc->code) : car(sc->code), y = sc->args;
             y != sc->NIL; x = cdr(x), y = cdr(y)) {
            new_slot_in_env(sc, caar(x), car(y));
        }
        if (is_symbol(car(sc->code))) {    /* named let */
          if(!is_pair(cadr(sc->code))) {
            Error_0(sc,"Scm Error: Poorly formed named let!", sc->code->_size);
          }
          for (x = cadr(sc->code), sc->args = sc->NIL; x != sc->NIL; x = cdr(x)) {
            sc->args = cons(sc, caar(x), sc->args);
          }
          x = mk_closure(sc, cons(sc, reverse_in_place(sc, sc->NIL, sc->args), cddr(sc->code)), sc->envir);
          new_slot_in_env(sc, car(sc->code), x);
          sc->code = cddr(sc->code);
          sc->args = sc->NIL;
        } else {
            sc->code = cdr(sc->code);
            sc->args = sc->NIL;
        }
        s_goto(sc,OP_BEGIN);

    case OP_LET0AST:    /* let* */{
        if (car(sc->code) == sc->NIL) {
            new_frame_in_env(sc, sc->envir);
            sc->code = cdr(sc->code);
            s_goto(sc,OP_BEGIN);
        }
        s_save(sc,OP_LET1AST, cdr(sc->code), car(sc->code));

        ////////// AS IMP CODE
        if(!is_pair(car(sc->code)))
        {
            Error_0(sc,"Let* cannot be used for a named let!", sc->code->_size);
        }
        //////////////////////
//                      std::stringstream sstt;
//                      imp::SchemeInterface::printSchemeCell(sc, sstt, sc->code);
//                      std::cout << ":::  " << sstt.str() << std::endl;
        sc->code = cadaar(sc->code);
        s_goto(sc,OP_EVAL);
    }
    case OP_LET1AST:    /* let* (make new frame) */
        new_frame_in_env(sc, sc->envir);
        s_goto(sc,OP_LET2AST);

    case OP_LET2AST:    /* let* (calculate parameters) */
        new_slot_in_env(sc, caar(sc->code), sc->value);
        sc->code = cdr(sc->code);
        if (is_pair(sc->code)) { /* continue */
            s_save(sc,OP_LET2AST, sc->args, sc->code);
            sc->code = cadar(sc->code);
            sc->args = sc->NIL;
            s_goto(sc,OP_EVAL);
        } else {  /* end */
            sc->code = sc->args;
            sc->args = sc->NIL;
            s_goto(sc,OP_BEGIN);
        }

    default:
        sprintf(sc->strbuff, "%d: illegal operator", sc->op);
        Error_0(sc,sc->strbuff,0);
        // ASIMP
        std::cout << "ILLEGAL OPERATION " << sc->op << std::endl;
        ///////////////
    }
    return sc->T;
}

static pointer opexe_1(scheme *sc, enum scheme_opcodes op) {
    pointer x, y;

    switch (op) {
    case OP_LET0REC:    /* letrec */
        new_frame_in_env(sc, sc->envir);
        sc->args = sc->NIL;
        sc->value = sc->code;
        sc->code = car(sc->code);
        s_goto(sc,OP_LET1REC);

    case OP_LET1REC:    /* letrec (calculate parameters) */
        sc->args = cons(sc, sc->value, sc->args);
        if (is_pair(sc->code)) { /* continue */
            s_save(sc,OP_LET1REC, sc->args, cdr(sc->code));
            sc->code = cadar(sc->code);
            sc->args = sc->NIL;
            s_goto(sc,OP_EVAL);
        } else {  /* end */
            sc->args = reverse_in_place(sc, sc->NIL, sc->args);
            sc->code = car(sc->args);
            sc->args = cdr(sc->args);
            s_goto(sc,OP_LET2REC);
        }

    case OP_LET2REC:    /* letrec */
        for (x = car(sc->code), y = sc->args; y != sc->NIL; x = cdr(x), y = cdr(y)) {
            new_slot_in_env(sc, caar(x), car(y));
        }
        sc->code = cdr(sc->code);
        sc->args = sc->NIL;
        s_goto(sc,OP_BEGIN);

    case OP_COND0:      /* cond */
        if (!is_pair(sc->code)) {
            Error_0(sc,"syntax error in cond",0);
        }
        s_save(sc,OP_COND1, sc->NIL, sc->code);
        sc->code = caar(sc->code);
        s_goto(sc,OP_EVAL);

    case OP_COND1:      /* cond */
        if (is_true(sc->value)) {
            if ((sc->code = cdar(sc->code)) == sc->NIL) {
                s_return(sc,sc->value);
            }
            if(car(sc->code)==sc->FEED_TO) {
                if(!is_pair(cdr(sc->code))) {
                    Error_0(sc,"syntax error in cond",0);
                }
                x=cons(sc, sc->QUOTE, cons(sc, sc->value, sc->NIL));
                sc->code=cons(sc,cadr(sc->code),cons(sc,x,sc->NIL));
                s_goto(sc,OP_EVAL);
            }
            s_goto(sc,OP_BEGIN);
        } else {
            if ((sc->code = cdr(sc->code)) == sc->NIL) {
                s_return(sc,sc->NIL);
            } else {
                s_save(sc,OP_COND1, sc->NIL, sc->code);
                sc->code = caar(sc->code);
                s_goto(sc,OP_EVAL);
            }
        }

    case OP_DELAY:      /* delay */
        x = mk_closure(sc, cons(sc, sc->NIL, sc->code), sc->envir);
        typeflag(x)=T_PROMISE;
        s_return(sc,x);

    case OP_AND0:       /* and */
        if (sc->code == sc->NIL) {
            s_return(sc,sc->T);
        }
        s_save(sc,OP_AND1, sc->NIL, cdr(sc->code));
        sc->code = car(sc->code);
        s_goto(sc,OP_EVAL);

    case OP_AND1:       /* and */
        if (is_false(sc->value)) {
            s_return(sc,sc->value);
        } else if (sc->code == sc->NIL) {
            s_return(sc,sc->value);
        } else {
            s_save(sc,OP_AND1, sc->NIL, cdr(sc->code));
            sc->code = car(sc->code);
            s_goto(sc,OP_EVAL);
        }

    case OP_OR0:        /* or */
        if (sc->code == sc->NIL) {
            s_return(sc,sc->F);
        }
        s_save(sc,OP_OR1, sc->NIL, cdr(sc->code));
        sc->code = car(sc->code);
        s_goto(sc,OP_EVAL);

    case OP_OR1:        /* or */
        if (is_true(sc->value)) {
            s_return(sc,sc->value);
        } else if (sc->code == sc->NIL) {
            s_return(sc,sc->value);
        } else {
            s_save(sc,OP_OR1, sc->NIL, cdr(sc->code));
            sc->code = car(sc->code);
            s_goto(sc,OP_EVAL);
        }

    case OP_C0STREAM:   /* cons-stream */
        s_save(sc,OP_C1STREAM, sc->NIL, cdr(sc->code));
        sc->code = car(sc->code);
        s_goto(sc,OP_EVAL);

    case OP_C1STREAM:   /* cons-stream */
        sc->args = sc->value;  /* save sc->value to register sc->args for gc */
        x = mk_closure(sc, cons(sc, sc->NIL, sc->code), sc->envir);
        typeflag(x)=T_PROMISE;
        s_return(sc,cons(sc, sc->args, x));

    case OP_MACRO0:     /* macro */
        if (is_pair(car(sc->code))) {
            x = caar(sc->code);
            sc->code = cons(sc, sc->LAMBDA, cons(sc, cdar(sc->code), cdr(sc->code)));
            cdr(sc->code)->_debugger = x;
        } else {
            x = car(sc->code);
            sc->code = cadr(sc->code);
            sc->code->_debugger = x;
        }
        if (!is_symbol(x)) {
            Error_1(sc,"variable is not a symbol",x,sc->code->_debugger->_size);
        }

        s_save(sc,OP_MACRO1, sc->NIL, x);
        s_goto(sc,OP_EVAL);

    case OP_MACRO1:     /* macro */
        typeflag(sc->value) = T_MACRO;
        x = find_slot_in_env(sc, sc->envir, sc->code, 0);
        if (x != sc->NIL) {
            set_slot_in_env(sc, x, sc->value);
        } else {
            new_slot_in_env(sc, sc->code, sc->value);
        }
        s_return(sc,sc->code);

    case OP_CASE0:      /* case */
      s_save(sc,OP_CASE1, sc->NIL, cdr(sc->code));
      sc->code = car(sc->code);
      s_goto(sc,OP_EVAL);

    case OP_CASE1:      /* case */
      for (x = sc->code; x != sc->NIL; x = cdr(x)) {
        if (!is_pair(y = caar(x))) {
          if(is_symbol(caar(x)) && (strcmp(symname(caar(x)),"else")==0)) {
            // all good we are in else case
          }else{
            Error_1(sc,"Syntax Error: Case clause must have the form ((<datum1> ...) <exp1> <exp2> ...) not",car(x),sc->code->_debugger->_size);
          }
          break;
        }
        for ( ; y != sc->NIL; y = cdr(y)) {
          if (eqv_sc(sc ,car(y), sc->value)) {
            break;
          }
        }
        if (y != sc->NIL) {
          break;
        }
      }
      if (x != sc->NIL) {
        if (is_pair(caar(x))) {
          sc->code = cdar(x);
          s_goto(sc,OP_BEGIN);
        } else {/* else */
          s_save(sc,OP_CASE2, sc->NIL, cdar(x));
          sc->code = caar(x);
          s_goto(sc,OP_EVAL);
        }
      } else {
        s_return(sc,sc->NIL);
      }

    case OP_CASE2:      /* case */
      if (is_true(sc->value)) {
        s_goto(sc,OP_BEGIN);
      } else {
        s_return(sc,sc->NIL);
      }

    case OP_PAPPLY:     /* apply */
        sc->code = car(sc->args);
        sc->args = list_star(sc,cdr(sc->args));
  if(!is_pair(sc->args) && sc->args != sc->NIL) {
    Error_1(sc,"Error: Apply must finish with a string argument! not:",sc->args,sc->code->_debugger->_size);
  }
        /*sc->args = cadr(sc->args);*/
        s_goto(sc,OP_APPLY);

    case OP_PEVAL: /* eval */
        if(cdr(sc->args)!=sc->NIL) {
            sc->envir=cadr(sc->args);
        }
        sc->code = car(sc->args);
        s_goto(sc,OP_EVAL);

    case OP_CONTINUATION:    /* call-with-current-continuation */
        sc->code = car(sc->args);
        sc->args = cons(sc, mk_continuation(sc), sc->NIL);
        //sc->args = cons(sc, mk_continuation(sc, sc->dump), sc->NIL);
        s_goto(sc,OP_APPLY);

    default:
        sprintf(sc->strbuff, "%d: illegal operator", sc->op);
        Error_0(sc,sc->strbuff,0);
        // ASIMP
        std::cout << "ILLEGAL OPERATION " << sc->op << std::endl;
        ///////////////
    }
    return sc->T;
}

static pointer opexe_2(scheme *sc, enum scheme_opcodes op) {
    pointer x;
    num v;
#if USE_MATH
    double dd;
#endif

    switch (op) {
#if USE_MATH
    case OP_INEX2EX:    /* inexact->exact */
        x=car(sc->args);
        if(is_integer(x)) {
            s_return(sc,x);
        } else if(modf(rvalue_unchecked(x),&dd)==0.0) {
            s_return(sc,mk_integer(sc,ivalue(x)));
        } else {
            Error_1(sc,"inexact->exact: not integral:",x,sc->code->_debugger->_size);
        }

    case OP_EXP:
        x=car(sc->args);
        s_return(sc, mk_real(sc, exp(rvalue(x))));

    case OP_LOG:
        x=car(sc->args);
        s_return(sc, mk_real(sc, log(rvalue(x))));

    case OP_SIN:
        x=car(sc->args);
        s_return(sc, mk_real(sc, sin(rvalue(x))));

    case OP_COS:
        x=car(sc->args);
        s_return(sc, mk_real(sc, cos(rvalue(x))));

    case OP_TAN:
        x=car(sc->args);
        s_return(sc, mk_real(sc, tan(rvalue(x))));

    case OP_ASIN:
        x=car(sc->args);
        s_return(sc, mk_real(sc, asin(rvalue(x))));

    case OP_ACOS:
        x=car(sc->args);
        s_return(sc, mk_real(sc, acos(rvalue(x))));

    case OP_ATAN:
        x=car(sc->args);
        if(cdr(sc->args)==sc->NIL) {
            s_return(sc, mk_real(sc, atan(rvalue(x))));
        } else {
            pointer y=cadr(sc->args);
            s_return(sc, mk_real(sc, atan2(rvalue(x),rvalue(y))));
        }

    case OP_SQRT:
        x=car(sc->args);
        s_return(sc, mk_real(sc, sqrt(rvalue(x))));

    case OP_EXPT:
        x=car(sc->args);
        if(cdr(sc->args)==sc->NIL) {
            Error_0(sc,"expt: needs two arguments",sc->code->_debugger->_size);
        } else {
            pointer y=cadr(sc->args);
            s_return(sc, mk_real(sc, pow(rvalue(x),rvalue(y))));
        }

    case OP_FLOOR:
        x=car(sc->args);
        s_return(sc, mk_real(sc, floor(rvalue(x))));

    case OP_CEILING:
        x=car(sc->args);
        s_return(sc, mk_real(sc, ceil(rvalue(x))));

    case OP_TRUNCATE : {
        double rvalue_of_x ;
        x=car(sc->args);
        rvalue_of_x = rvalue(x) ;
        if (rvalue_of_x > 0) {
            s_return(sc, mk_real(sc, floor(rvalue_of_x)));
        } else {
            s_return(sc, mk_real(sc, ceil(rvalue_of_x)));
        }
    }

    case OP_ROUND:
        x=car(sc->args);
        s_return(sc, mk_real(sc, round_per_R5RS(rvalue(x))));
#endif

    case OP_ADD:        /* + */
        v = num_zero;
        for (x = sc->args; x != sc->NIL; x = cdr(x)) {
            v = num_add(v, nvalue(car(x)));
        }
        s_return(sc, mk_number(sc, v));

    case OP_MUL:        /* * */
        v=num_one;
        for (x = sc->args; x != sc->NIL; x = cdr(x)) {
            v=num_mul(v,nvalue(car(x)));
        }
        s_return(sc,mk_number(sc, v));

    case OP_SUB:        /* - */
        if(cdr(sc->args)==sc->NIL) {
            x=sc->args;
            v=num_zero;
        } else {
            x = cdr(sc->args);
            v = nvalue(car(sc->args));
        }
        for (; x != sc->NIL; x = cdr(x)) {
            v=num_sub(v,nvalue(car(x)));
        }
        s_return(sc,mk_number(sc, v));

    case OP_DIV:        /* / */
        if(cdr(sc->args)==sc->NIL) {
            x=sc->args;
            v=num_one;
        } else {
            x = cdr(sc->args);
            v = nvalue(car(sc->args));
        }
        for (; x != sc->NIL; x = cdr(x)) {
            if (!is_zero_double(rvalue(car(x))))
                v=num_div(v,nvalue(car(x)));
            else {
                std::cout << "DIV BY ZERO ERROR" << std::endl;
                Error_0(sc,"/: division by zero",sc->code->_debugger->_size);
            }
        }
        s_return(sc,mk_number(sc, v));
  case OP_BITNOT:        /* ~ */
    v=num_bitnot(nvalue(car(sc->args)));
    s_return(sc,mk_number(sc, v));

  case OP_BITAND:        /* & */
    v=num_zero;
    x = sc->args;
    if (x != sc->NIL) {
      v = nvalue(car(x));
      for (x = cdr(x); x != sc->NIL; x = cdr(x)) {
        v=num_bitand(v,nvalue(car(x)));
      }
    }
    s_return(sc,mk_number(sc, v));

  case OP_BITOR:        /* | */
    v=num_zero;
    for (x = sc->args; x != sc->NIL; x = cdr(x)) {
      v=num_bitor(v,nvalue(car(x)));
    }
    s_return(sc,mk_number(sc, v));

  case OP_BITEOR:        /* ^ */
    v=num_zero;
    for (x = sc->args; x != sc->NIL; x = cdr(x)) {
      v=num_biteor(v,nvalue(car(x)));
    }
    s_return(sc,mk_number(sc, v));

  case OP_BITLSL:        /* << */
    v=num_zero;
    x = sc->args;
    if (x != sc->NIL) {
      v = nvalue(car(x));
      for (x = cdr(x); x != sc->NIL; x = cdr(x)) {
        v=num_bitlsl(v,nvalue(car(x)));
      }
    }
    s_return(sc,mk_number(sc, v));

  case OP_BITLSR:        /* >> */
    v=num_zero;
    x = sc->args;
    if (x != sc->NIL) {
      v = nvalue(car(x));
      for (x = cdr(x); x != sc->NIL; x = cdr(x)) {
        v=num_bitlsr(v,nvalue(car(x)));
      }
    }
    s_return(sc,mk_number(sc, v));

    case OP_INTDIV:        /* quotient */
        if(cdr(sc->args)==sc->NIL) {
            x=sc->args;
            v=num_one;
        } else {
            x = cdr(sc->args);
            v = nvalue(car(sc->args));
        }
        for (; x != sc->NIL; x = cdr(x)) {
            if (ivalue(car(x)) != 0)
                v=num_intdiv(v,nvalue(car(x)));
            else {
                std::cout << "DIV BY ZERO ERROR" << std::endl;
                Error_0(sc,"quotient: division by zero",sc->code->_debugger->_size);
            }
        }
        s_return(sc,mk_number(sc, v));

    case OP_REM:        /* remainder */
        v = nvalue(car(sc->args));
        if (ivalue(cadr(sc->args)) != 0)
            v=num_rem(v,nvalue(cadr(sc->args)));
        else {
            std::cout << "DIV BY ZERO ERROR" << std::endl;
            Error_0(sc,"remainder: division by zero",sc->code->_debugger->_size);
        }
        s_return(sc,mk_number(sc, v));

    case OP_MOD:        /* modulo */
        v = nvalue(car(sc->args));
        //if (ivalue(cadr(sc->args)) != 0)
        if (!is_zero_double(rvalue(cadr(sc->args))))
            v=num_mod(v,nvalue(cadr(sc->args)));
        else {
            std::cout << "DIV BY ZERO ERROR" << std::endl;
            Error_0(sc,"modulo: division by zero",sc->code->_debugger->_size);
        }
        s_return(sc,mk_number(sc, v));

    case OP_CAR:        /* car */
        s_return(sc,caar(sc->args));

    case OP_CDR:        /* cdr */
        s_return(sc,cdar(sc->args));

    case OP_CONS:       /* cons */
        cdr(sc->args) = cadr(sc->args);
        ////////////// write barrier for treadmill
#ifdef TREADMILL_CHECKS
      last_call_to_insert_treadmill = 20;
#endif
        insert_treadmill(sc,cdr(sc->args));
        //////////////////////////////////////////
        s_return(sc,sc->args);

    case OP_SETCAR:     /* set-car! */
        if(!is_immutable(car(sc->args))) {
    ////////////// write barrier for treadmill
#ifdef TREADMILL_CHECKS
      last_call_to_insert_treadmill = 21;
#endif
            insert_treadmill(sc,cadr(sc->args));
            //////////////////////////////////////////

            caar(sc->args) = cadr(sc->args);
            s_return(sc,car(sc->args));
        } else {
            Error_0(sc,"set-car!: unable to alter immutable pair",sc->code->_debugger->_size);
        }

    case OP_SETCDR:     /* set-cdr! */
        if(!is_immutable(car(sc->args))) {
    ////////////// write barrier for treadmill
#ifdef TREADMILL_CHECKS
      last_call_to_insert_treadmill = 22;
#endif
            insert_treadmill(sc,cadr(sc->args));
            //////////////////////////////////////////

            cdar(sc->args) = cadr(sc->args);
            s_return(sc,car(sc->args));
        } else {
            Error_0(sc,"set-cdr!: unable to alter immutable pair",sc->code->_debugger->_size);
        }

    case OP_CHAR2INT: { /* char->integer */
        char c;
        c=(char)ivalue(car(sc->args));
        s_return(sc,mk_integer(sc,(unsigned char)c));
    }

    case OP_INT2CHAR: { /* integer->char */
        unsigned char c;
        c=(unsigned char)ivalue(car(sc->args));
        s_return(sc,mk_character(sc,(char)c));
    }

    case OP_CHARUPCASE: {
        unsigned char c;
        c=(unsigned char)ivalue(car(sc->args));
        c=toupper(c);
        s_return(sc,mk_character(sc,(char)c));
    }

    case OP_CHARDNCASE: {
        unsigned char c;
        c=(unsigned char)ivalue(car(sc->args));
        c=tolower(c);
        s_return(sc,mk_character(sc,(char)c));
    }

    case OP_STR2SYM:  /* string->symbol */
        s_return(sc,mk_symbol(sc,strvalue(car(sc->args))));

    case OP_STR2ATOM: /* string->atom */ {
        char *s=strvalue(car(sc->args));
        if(*s=='#') {
            s_return(sc, mk_sharp_const(sc, s+1));
        } else {
            s_return(sc, mk_atom(sc, s));
        }
    }

    case OP_SYM2STR: /* symbol->string */
        x=mk_string(sc,symname_sc(sc,car(sc->args)));
        setimmutable(x);
        s_return(sc,x);
    case OP_ATOM2STR: /* atom->string */
        x=car(sc->args);
        if(is_number(x) || is_character(x) || is_string(x) || is_symbol(x)) {
            char *p;
            int len;
            atom2str(sc,x,0,&p,&len);
            s_return(sc,mk_counted_string(sc,p,len));
        } else {
            Error_1(sc, "atom->string: not an atom:", x, sc->code->_debugger->_size);
        }

    case OP_MKSTRING: { /* make-string */
        int fill=' ';
        int len;

        len=ivalue(car(sc->args));

        if(cdr(sc->args)!=sc->NIL) {
            fill=charvalue_sc(sc,cadr(sc->args));
        }
        s_return(sc,mk_empty_string(sc,len,(char)fill));
    }

    case OP_STRLEN:  /* string-length */
        s_return(sc,mk_integer(sc,strlength(car(sc->args))));

    case OP_STRREF: { /* string-ref */
        char *str;
        int index;

        str=strvalue(car(sc->args));

        index=ivalue(cadr(sc->args));

        if(index>=strlength(car(sc->args))) {
            Error_1(sc,"string-ref: out of bounds:",cadr(sc->args), sc->code->_debugger->_size);
        }

        s_return(sc,mk_character(sc,((unsigned char*)str)[index]));
    }

    case OP_STRSET: { /* string-set! */
        char *str;
        int index;
        int c;

        if(is_immutable(car(sc->args))) {
            Error_1(sc,"string-set!: unable to alter immutable string:",car(sc->args), sc->code->_debugger->_size);
        }
        str=strvalue(car(sc->args));

        index=ivalue(cadr(sc->args));
        if(index>=strlength(car(sc->args))) {
            Error_1(sc,"string-set!: out of bounds:",cadr(sc->args), sc->code->_debugger->_size);
        }

        c=charvalue_sc(sc,caddr(sc->args));

        str[index]=(char)c;
        s_return(sc,car(sc->args));
    }

    case OP_STRAPPEND: { /* string-append */
        /* in 1.29 string-append was in Scheme in init.scm but was too slow */
        int len = 0;
        pointer newstr;
        char *pos;

        /* compute needed length for new string */
        for (x = sc->args; x != sc->NIL; x = cdr(x)) {
            len += strlength(car(x));
        }
        newstr = mk_empty_string(sc, len, ' ');
        /* store the contents of the argument strings into the new string */
        for (pos = strvalue(newstr), x = sc->args; x != sc->NIL;
             pos += strlength(car(x)), x = cdr(x)) {
            memcpy(pos, strvalue(car(x)), strlength(car(x)));
        }
        s_return(sc, newstr);
    }

    case OP_SUBSTR: { /* substring */
        char *str;
        int index0;
        int index1;
        int len;

        str=strvalue(car(sc->args));

        index0=ivalue(cadr(sc->args));

        if(index0>strlength(car(sc->args))) {
            Error_1(sc,"substring: start out of bounds:",cadr(sc->args), sc->code->_debugger->_size);
        }

        if(cddr(sc->args)!=sc->NIL) {
            index1=ivalue(caddr(sc->args));
            if(index1>strlength(car(sc->args)) || index1<index0) {
                Error_1(sc,"substring: end out of bounds:",caddr(sc->args),sc->code->_debugger->_size);
            }
        } else {
            index1=strlength(car(sc->args));
        }

        len=index1-index0;
        x=mk_empty_string(sc,len,' ');
        memcpy(strvalue(x),str+index0,len);
        strvalue(x)[len]=0;

        s_return(sc,x);
    }

    case OP_VECTOR: {   /* vector */
        int i;
        pointer vec;
        int len=list_length(sc,sc->args);
        if(len<0) {
            Error_1(sc,"vector: not a proper list:",sc->args,sc->code->_debugger->_size);
        }
        vec=mk_vector(sc,len);
        for (x = sc->args, i = 0; is_pair(x); x = cdr(x), i++) {
            set_vector_elem(sc, vec,i,car(x));
        }
        s_return(sc,vec);
    }

    case OP_MKVECTOR: { /* make-vector */
        pointer fill=sc->NIL;
        int len;
        pointer vec;

        len=ivalue(car(sc->args));

        if(cdr(sc->args)!=sc->NIL) {
            fill=cadr(sc->args);
        }
        vec=mk_vector(sc,len);
        if(fill!=sc->NIL) {
            fill_vector(sc,vec,fill);
        }
        s_return(sc,vec);
    }

    case OP_VECLEN:  /* vector-length */
        s_return(sc,mk_integer(sc,(car(sc->args))->_size));
        //s_return(sc,mk_integer(sc,ivalue(car(sc->args))));

    case OP_VECREF: { /* vector-ref */
        int index;

        index=ivalue(cadr(sc->args));

        if(index>=(car(sc->args))->_size) {
            Error_1(sc,"vector-ref: out of bounds:",cadr(sc->args),sc->code->_debugger->_size);
        }

        s_return(sc,vector_elem(car(sc->args),index));
    }

    case OP_VECSET: {   /* vector-set! */
        int index;

        if(is_immutable(car(sc->args))) {
            Error_1(sc,"vector-set!: unable to alter immutable vector:",car(sc->args),sc->code->_debugger->_size);
        }

        index=ivalue(cadr(sc->args));
        if(index>=(car(sc->args))->_size) {
            Error_1(sc,"vector-set!: out of bounds:",cadr(sc->args),sc->code->_debugger->_size);
        }

        set_vector_elem(sc, car(sc->args),index,caddr(sc->args));
        s_return(sc,car(sc->args));
    }

    default:
        sprintf(sc->strbuff, "%d: illegal operator", sc->op);
        Error_0(sc,sc->strbuff,sc->code->_debugger->_size);
        // ASIMP
        std::cout << "ILLEGAL OPERATION " << sc->op << std::endl;
        ///////////////
    }
    return sc->T;
}

/*static*/ int list_length(scheme *sc, pointer a) {
    int v=0;
    pointer x;
    for (x = a, v = 0; is_pair(x); x = cdr(x)) {
           ++v;
    }

    if(x==sc->NIL) {
           return v;
    }

    return -1;
}


// keys of assoc lst MUST be strings OR symbols
pointer assoc_strcmp(scheme *sc, pointer key, pointer lst, bool all)
{
    pointer errVal = (!all) ? sc->F : sc->NIL;
    pointer retlist = errVal;
    if (likely(is_symbol(key))) {
        auto skey = strvalue(car(key));
        for (auto x = lst; is_pair(x); x = cdr(x)) {
            auto pair = car(x);
            if (unlikely(!is_pair(pair))) {
                return errVal;
            }
            auto r1 = car(pair);
            if (unlikely(!is_symbol(r1))) {
                return errVal;
            }
            auto lkey = strvalue(car(r1));
            if (!strcmp(lkey, skey)) {
                if (likely(!all)) {
                    return pair;
                }
                retlist = cons(sc, pair, retlist);
            }
        }
        return retlist;
    }
    if (is_string(key)) {
        auto skey = strvalue(key);
        for (auto x = lst; is_pair(x); x = cdr(x)) {
            auto pair = car(x);
            if (unlikely(!is_pair(pair))) {
                return errVal;
            }
            auto lkey = strvalue(car(pair));
            if (!strcmp(lkey,skey)) {
                if (likely(!all)) {
                    return pair;
                }
                retlist = cons(sc, pair, retlist);
            }
        }
        return retlist;
    }
    // it not neccessarily a problem for the key to be a non-symbol/string
    // although it should return false of course
    // which it does after falling through to the final return
    return retlist;
}

pointer list_ref(scheme *sc, const int pos, pointer a) {
    pointer x;
    for (int i=0; i <= pos; ++i, a = cdr(a))
    {
        if (unlikely(!is_pair(a))) {
            return sc->NIL;
        }
        x = car(a);
    }
    return x;
}

static pointer opexe_3(scheme *sc, enum scheme_opcodes op) {
    pointer x;
    num v;
    int (*comp_func)(const num&, const num&) = 0;

    switch (op) {
    case OP_NOT:        /* not */
        s_retbool(is_false(car(sc->args)));
    case OP_BOOLP:       /* boolean? */
        s_retbool(car(sc->args) == sc->F || car(sc->args) == sc->T);
    case OP_EOFOBJP:       /* boolean? */
        s_retbool(car(sc->args) == sc->EOF_OBJ);
    case OP_NULLP:       /* null? */
        s_retbool(car(sc->args) == sc->NIL);
    case OP_NUMEQ:      /* = */
    case OP_LESS:       /* < */
    case OP_GRE:        /* > */
    case OP_LEQ:        /* <= */
    case OP_GEQ:        /* >= */
        switch(op) {
        case OP_NUMEQ: comp_func=num_eq; break;
        case OP_LESS:  comp_func=num_lt; break;
        case OP_GRE:   comp_func=num_gt; break;
        case OP_LEQ:   comp_func=num_le; break;
        case OP_GEQ:   comp_func=num_ge; break;
        }
        x=sc->args;
        v=nvalue(car(x));
        x=cdr(x);

        for (; x != sc->NIL; x = cdr(x)) {
            if(!comp_func(v,nvalue(car(x)))) {
                s_retbool(0);
            }
            v=nvalue(car(x));
        }
        s_retbool(1);
    case OP_SYMBOLP:     /* symbol? */
        s_retbool(is_symbol(car(sc->args)));
    case OP_NUMBERP:     /* number? */
        s_retbool(is_number(car(sc->args)));
    case OP_STRINGP:     /* string? */
        s_retbool(is_string(car(sc->args)));
    case OP_INTEGERP:     /* integer? */
        s_retbool(is_integer(car(sc->args)));
    case OP_REALP:     /* real? */
        s_retbool(is_number(car(sc->args))); /* All numbers are real */
    case OP_RATIONALP:     /* rational? */
        s_retbool(is_rational(car(sc->args)));
    case OP_CHARP:     /* char? */
        s_retbool(is_character(car(sc->args)));
#if USE_CHAR_CLASSIFIERS
    case OP_CHARAP:     /* char-alphabetic? */
        s_retbool(Cisalpha(ivalue(car(sc->args))));
    case OP_CHARNP:     /* char-numeric? */
        s_retbool(Cisdigit(ivalue(car(sc->args))));
    case OP_CHARWP:     /* char-whitespace? */
        s_retbool(Cisspace(ivalue(car(sc->args))));
    case OP_CHARUP:     /* char-upper-case? */
        s_retbool(Cisupper(ivalue(car(sc->args))));
    case OP_CHARLP:     /* char-lower-case? */
        s_retbool(Cislower(ivalue(car(sc->args))));
#endif
    case OP_PORTP:     /* port? */
        s_retbool(is_port(car(sc->args)));
    case OP_INPORTP:     /* input-port? */
        s_retbool(is_inport(car(sc->args)));
    case OP_OUTPORTP:     /* output-port? */
        s_retbool(is_outport(car(sc->args)));
    case OP_PROCP:       /* procedure? */
        /*--
         * continuation should be procedure by the example
         * (call-with-current-continuation procedure?) ==> #t
         * in R^3 report sec. 6.9
         */
        s_retbool(is_proc(car(sc->args)) || is_closure(car(sc->args))
                  || is_continuation(car(sc->args)) || is_foreign(car(sc->args)));
    case OP_PAIRP:       /* pair? */
        s_retbool(is_pair(car(sc->args)));
    case OP_LISTP: {     /* list? */
        pointer slow, fast;
        slow = fast = car(sc->args);
        while (1) {
            if (!is_pair(fast)) s_retbool(fast == sc->NIL);
            fast = cdr(fast);
            if (!is_pair(fast)) s_retbool(fast == sc->NIL);
            fast = cdr(fast);
            slow = cdr(slow);
            if (fast == slow) {
                /* the fast pointer has looped back around and caught up
                   with the slow pointer, hence the structure is circular,
                   not of finite length, and therefore not a list */
                s_retbool(0);
            }
        }
    }
    case OP_ENVP:        /* environment? */
        s_retbool(is_environment(car(sc->args)));
    case OP_VECTORP:     /* vector? */
        s_retbool(is_vector(car(sc->args)));
    case OP_CPTRP: /* cstr? */
        s_retbool(is_cptr(car(sc->args)));
        // case OP_OBJCP: /* cstr? */
        //      s_retbool(is_objc(car(sc->args)));
    case OP_EQ:         /* eq? */
        s_retbool(car(sc->args) == cadr(sc->args));
    case OP_EQV:        /* eqv? */
        s_retbool(eqv_sc(sc, car(sc->args), cadr(sc->args)));
    default:
        sprintf(sc->strbuff, "%d: illegal operator", sc->op);
        Error_0(sc,sc->strbuff,sc->code->_debugger->_size);
        // ASIMP
        std::cout << "ILLEGAL OPERATION " << sc->op << std::endl;
        ///////////////
    }
    return sc->T;
}

static pointer opexe_4(scheme *sc, enum scheme_opcodes op) {
    pointer x, y;

    switch (op) {
    case OP_FORCE:      /* force */
        sc->code = car(sc->args);
        if (is_promise(sc->code)) {
            /* Should change type to closure here */
            s_save(sc, OP_SAVE_FORCED, sc->NIL, sc->code);
            sc->args = sc->NIL;
            s_goto(sc,OP_APPLY);
        } else {
            s_return(sc,sc->code);
        }

    case OP_SAVE_FORCED:     /* Save forced value replacing promise */
        memcpy(sc->code,sc->value,sizeof(struct cell));
        s_return(sc,sc->value);

    case OP_WRITE:      /* write */
    case OP_DISPLAY:    /* display */
    case OP_WRITE_CHAR: /* write-char */
        if(is_pair(cdr(sc->args))) {
            if(cadr(sc->args)!=sc->outport) {
                x=cons(sc,sc->outport,sc->NIL);
                s_save(sc,OP_SET_OUTPORT, x, sc->NIL);
                sc->outport=cadr(sc->args);
            }
        }
        sc->args = car(sc->args);
        if(op==OP_WRITE) {
            sc->print_flag = 1;
        } else {
            sc->print_flag = 0;
        }
        s_goto(sc,OP_P0LIST);

    case OP_NEWLINE:    /* newline */
        if(is_pair(sc->args)) {
            if(car(sc->args)!=sc->outport) {
                x=cons(sc,sc->outport,sc->NIL);
                s_save(sc,OP_SET_OUTPORT, x, sc->NIL);
                sc->outport=car(sc->args);
            }
        }
        putstr(sc, "\n");
        s_return(sc,sc->T);

    case OP_ERR0:  /* error */
        sc->retcode=-1;
        if (!is_string(car(sc->args))) {
            sc->args=cons(sc,mk_string(sc," -- "),sc->args);
            setimmutable(car(sc->args));
        }
        //putstr(sc, "Error: ");
        //putstr(sc, strvalue(car(sc->args)));
        sc->args = cdr(sc->args);
        s_goto(sc,OP_ERR1);

    case OP_ERR1:  /* error */
        putstr(sc, " ");
        if (sc->args != sc->NIL) {
            s_save(sc,OP_ERR1, cdr(sc->args), sc->NIL);
            sc->args = car(sc->args);
            sc->print_flag = 1;
            s_goto(sc,OP_P0LIST);
        } else {
            putstr(sc, "\n");
            if(sc->interactive_repl) {
                s_goto(sc,OP_T0LVL);
            } else {
                return sc->NIL;
            }
        }

    case OP_REVERSE:    /* reverse */
        s_return(sc,reverse(sc, car(sc->args)));

    case OP_LIST_STAR: /* list* */
        s_return(sc,list_star(sc,sc->args));

    case OP_APPEND:     /* append */
        if(sc->args==sc->NIL) {
            s_return(sc,sc->NIL);
        }
        x=car(sc->args);
        if(cdr(sc->args)==sc->NIL) {
            s_return(sc,sc->args);
        }
        for (y = cdr(sc->args); y != sc->NIL; y = cdr(y)) {
            x=append(sc,x,car(y));
        }
        s_return(sc,x);

#if USE_PLIST
    case OP_PUT:        /* put */
        if (!hasprop(car(sc->args)) || !hasprop(cadr(sc->args))) {
            Error_0(sc,"illegal use of put",sc->code->_debugger->_size);
        }
        for (x = symprop(car(sc->args)), y = cadr(sc->args); x != sc->NIL; x = cdr(x)) {
            if (caar(x) == y) {
                break;
            }
        }
        if (x != sc->NIL)
            cdar(x) = caddr(sc->args);
        else
            symprop(car(sc->args)) = cons(sc, cons(sc, y, caddr(sc->args)),
                                          symprop(car(sc->args)));
        s_return(sc,sc->T);

    case OP_GET:        /* get */
        if (!hasprop(car(sc->args)) || !hasprop(cadr(sc->args))) {
            Error_0(sc,"illegal use of get",sc->code->_debugger->_size);
        }
        for (x = symprop(car(sc->args)), y = cadr(sc->args); x != sc->NIL; x = cdr(x)) {
            if (caar(x) == y) {
                break;
            }
        }
        if (x != sc->NIL) {
            s_return(sc,cdar(x));
        } else {
            s_return(sc,sc->NIL);
        }
#endif /* USE_PLIST */
    case OP_QUIT:       /* quit */
        if(is_pair(sc->args)) {
            sc->retcode=ivalue(car(sc->args));
        }
        return (sc->NIL);

    case OP_NEWSEGMENT: /* new-segment */
        if (!is_pair(sc->args) || !is_number(car(sc->args))) {
            Error_0(sc,"new-segment: argument must be a number",0);
        }
        alloc_cellseg(sc, (int) ivalue(car(sc->args)));
        s_return(sc,sc->T);

    case OP_OBLIST: /* oblist */
        s_return(sc, oblist_all_symbols(sc));

    case OP_CURR_INPORT: /* current-input-port */
        s_return(sc,sc->inport);

    case OP_CURR_OUTPORT: /* current-output-port */
        s_return(sc,sc->outport);

    case OP_OPEN_INFILE: /* open-input-file */
    case OP_OPEN_OUTFILE: /* open-output-file */
    case OP_OPEN_INOUTFILE: /* open-input-output-file */ {
        int prop=0;
        pointer p;
        switch(op) {
        case OP_OPEN_INFILE:     prop=port_input; break;
        case OP_OPEN_OUTFILE:    prop=port_output; break;
        case OP_OPEN_INOUTFILE: prop=port_input|port_output; break;
        }
        p=port_from_filename(sc,strvalue(car(sc->args)),prop);
        if(p==sc->NIL) {
            s_return(sc,sc->F);
        }
        s_return(sc,p);
    }

#if USE_STRING_PORTS
    case OP_OPEN_INSTRING: /* open-input-string */
    case OP_OPEN_INOUTSTRING: /* open-input-output-string */ {
      int prop=0;
      pointer p;
      switch(op) {
      case OP_OPEN_INSTRING:     prop=port_input; break;
      case OP_OPEN_INOUTSTRING:  prop=port_input|port_output; break;
      }
      p=port_from_string(sc, strvalue(car(sc->args)),
                         strvalue(car(sc->args))+strlength(car(sc->args)), prop);
      if(p==sc->NIL) {
        s_return(sc,sc->F);
      }
      s_return(sc,p);
    }
    case OP_OPEN_OUTSTRING: /* open-output-string */ {
      pointer p;
      if(car(sc->args)==sc->NIL) {
        p=port_from_scratch(sc);
        if(p==sc->NIL) {
          s_return(sc,sc->F);
        }
      } else {
        p=port_from_string(sc, strvalue(car(sc->args)),
                           strvalue(car(sc->args))+strlength(car(sc->args)),
                           port_output);
        if(p==sc->NIL) {
          s_return(sc,sc->F);
        }
      }
      s_return(sc,p);
    }
    case OP_GET_OUTSTRING: /* get-output-string */ {
      port *p;

      if ((p=car(sc->args)->_object._port)->kind&port_string) {
        off_t size;
        char *str;

        size=p->rep.string.curr-p->rep.string.start+1;
        if((str=(char*)sc->malloc(size))) {
          pointer s;

          memcpy(str,p->rep.string.start,size-1);
          str[size-1]='\0';
          s=mk_string(sc,str);
          sc->free(str);
          s_return(sc,s);
        }
      }
      s_return(sc,sc->F);
    }
#endif

    case OP_CLOSE_INPORT: /* close-input-port */
        port_close(sc,car(sc->args),port_input);
        s_return(sc,sc->T);

    case OP_CLOSE_OUTPORT: /* close-output-port */
        port_close(sc,car(sc->args),port_output);
        s_return(sc,sc->T);

    case OP_INT_ENV: /* interaction-environment */
        s_return(sc,sc->global_env);

    case OP_CURR_ENV: /* current-environment */
        s_return(sc,sc->envir);

    }
    return sc->T;
}

static pointer opexe_5(scheme *sc, enum scheme_opcodes op) {
    pointer x;

    if(sc->nesting!=0) {
        int n=sc->nesting;
        sc->nesting=0;
        sc->retcode=-1;
        Error_1(sc,"unmatched parentheses:",mk_integer(sc,n),0);
    }

    switch (op) {
        /* ========== reading part ========== */
    case OP_READ:
        if(!is_pair(sc->args)) {
            s_goto(sc,OP_READ_INTERNAL);
        }
        if(!is_inport(car(sc->args))) {
            Error_1(sc,"read: not an input port:",car(sc->args),sc->code->_debugger->_size);
        }
        if(car(sc->args)==sc->inport) {
            s_goto(sc,OP_READ_INTERNAL);
        }
        x=sc->inport;
        sc->inport=car(sc->args);
        //////////////////////////////////////////////
        //IMPROMPTU BUG WHEN CALLING FROM scheme_call
        // if sc->inport is not actually an inport
        // we don't want to call OP_SET_INPORT after
        // doing our read to reset the inport
        if(is_inport(x))
        {
            x=cons(sc,x,sc->NIL);
            s_save(sc,OP_SET_INPORT, x, sc->NIL);
        }
        //////////////////////////////////////////////
        s_goto(sc,OP_READ_INTERNAL);

    case OP_READ_CHAR: /* read-char */
    case OP_PEEK_CHAR: /* peek-char */ {
        int c;
        if(is_pair(sc->args)) {
            if(car(sc->args)!=sc->inport) {
                x=sc->inport;
                x=cons(sc,x,sc->NIL);
                s_save(sc,OP_SET_INPORT, x, sc->NIL);
                sc->inport=car(sc->args);
            }
        }
        c=inchar(sc);
        if(c==EOF) {
            s_return(sc,sc->EOF_OBJ);
        }
        if(sc->op==OP_PEEK_CHAR) {
            backchar(sc,c);
        }
        s_return(sc,mk_character(sc,c));
    }

    case OP_CHAR_READY: /* char-ready? */ {
        pointer p=sc->inport;
        int res;
        if(is_pair(sc->args)) {
            p=car(sc->args);
        }
        res=p->_object._port->kind&port_string;
        s_retbool(res);
    }

    case OP_SET_INPORT: /* set-input-port */
        sc->inport=car(sc->args);
        s_return(sc,sc->value);

    case OP_SET_OUTPORT: /* set-output-port */
        sc->outport=car(sc->args);
        s_return(sc,sc->value);

    case OP_RDSEXPR:
        switch (sc->tok) {
        case TOK_EOF:
            if(sc->inport==sc->loadport) {
                sc->args=sc->NIL;
                s_goto(sc,OP_QUIT);
            } else {
                s_return(sc,sc->EOF_OBJ);
            }
        case TOK_COMMENT: {
            int c;
            while ((c=inchar(sc)) != '\n' && c!=EOF)
                ;
            sc->tok = token(sc);
            s_goto(sc,OP_RDSEXPR);
        }
        case TOK_VEC:
            s_save(sc,OP_RDVEC,sc->NIL,sc->NIL);
            /* fall through */
        case TOK_LPAREN:
            sc->tok = token(sc);
            if (sc->tok == TOK_RPAREN) {
                s_return(sc,sc->NIL);
            } else if (sc->tok == TOK_DOT) {
                Error_0(sc,"syntax error: illegal dot expression",sc->code->_debugger->_size);
            } else {
                sc->nesting_stack[sc->file_i]++;
                s_save(sc,OP_RDLIST, sc->NIL, sc->NIL);
                s_goto(sc,OP_RDSEXPR);
            }
        case TOK_QUOTE:
            s_save(sc,OP_RDQUOTE, sc->NIL, sc->NIL);
            sc->tok = token(sc);
            s_goto(sc,OP_RDSEXPR);
        case TOK_BQUOTE:
            sc->tok = token(sc);
            if(sc->tok==TOK_VEC) {
                s_save(sc,OP_RDQQUOTEVEC, sc->NIL, sc->NIL);
                sc->tok=TOK_LPAREN;
                s_goto(sc,OP_RDSEXPR);
            } else {
                s_save(sc,OP_RDQQUOTE, sc->NIL, sc->NIL);
            }
            s_goto(sc,OP_RDSEXPR);
        case TOK_COMMA:
            s_save(sc,OP_RDUNQUOTE, sc->NIL, sc->NIL);
            sc->tok = token(sc);
            s_goto(sc,OP_RDSEXPR);
        case TOK_ATMARK:
            s_save(sc,OP_RDUQTSP, sc->NIL, sc->NIL);
            sc->tok = token(sc);
            s_goto(sc,OP_RDSEXPR);
        case TOK_ATOM:
            s_return(sc,mk_atom(sc, readstr_upto(sc, (char*)"();\t\n\r ")));
        case TOK_DQUOTE:
            x=readstrexp(sc);
            if(x==sc->F) {
                Error_0(sc,"Error reading string",sc->args->_size);
            }
            setimmutable(x);
            s_return(sc,x);
        case TOK_SHARP: {
            pointer f=find_slot_in_env(sc,sc->envir,sc->SHARP_HOOK,1);
            if(f==sc->NIL) {
                Error_0(sc,"undefined sharp expression",sc->args->_size);
            } else {
                sc->code=cons(sc,slot_value_in_env(f),sc->NIL);
                s_goto(sc,OP_EVAL);
            }
        }
        case TOK_SHARP_CONST:
            if ((x = mk_sharp_const(sc, readstr_upto(sc, (char*)"();\t\n\r "))) == sc->NIL) {
        Error_0(sc,"undefined sharp expression",sc->args->_size);
            } else {
                s_return(sc,x);
            }
        default:
    if(sc->tok == 1) {
      std::cout << "ILLEGAL RIGHT BRACKET!!!" << std::endl;
      std::cout << "Check for unbalanced expression" << std::endl;
      Error_0(sc,"Illegal right bracket: unbalanced expr?",sc->args->_size);
    }else{
      std::cout << "ILLEGAL TOKEN: " << sc->tok << std::endl;
      Error_0(sc,"syntax error: illegal token",sc->args->_size);
    }
        }
        break;

    case OP_RDLIST: {
        sc->args = cons(sc, sc->value, sc->args);

        //////////////////////////////////////////
        //added for debugger
        if(sc->inport != sc->NIL && sc->inport->_object._port->kind&port_string) {
            int position = 0;
            char* ptr = sc->inport->_object._port->rep.string.start;
            for( ; &ptr[position] < &sc->inport->_object._port->rep.string.curr[0]; position++);
            //int lgth = strlen(name);
            if(is_symbol(sc->value))
            {
                sc->args->_size = position - car(sc->value)->_object._string._length;
                //std::cout << car(sc->value)->_object._string._svalue << " pos: " << sc->args->_size << std::endl;
            }
        }
//                      std::stringstream ss;
//                      imp::SchemeInterface::printSchemeCell(sc, ss, sc->value);
//                      std::cout << "stuff: " << ss.str() << std::endl;

        sc->tok = token(sc);
        if (sc->tok == TOK_COMMENT) {
            int c;
            while ((c=inchar(sc)) != '\n' && c!=EOF)
                ;
            sc->tok = token(sc);
        }
        if (sc->tok == TOK_RPAREN) {
            int c = inchar(sc);
            if (c != '\n') backchar(sc,c);
            sc->nesting_stack[sc->file_i]--;
            s_return(sc,reverse_in_place(sc, sc->NIL, sc->args));
        } else if (sc->tok == TOK_DOT) {
            s_save(sc,OP_RDDOT, sc->args, sc->NIL);
            sc->tok = token(sc);
            s_goto(sc,OP_RDSEXPR);
        } else {
            s_save(sc,OP_RDLIST, sc->args, sc->NIL);;
            s_goto(sc,OP_RDSEXPR);
        }
    }

    case OP_RDDOT:
        if (token(sc) != TOK_RPAREN) {
            Error_0(sc,"syntax error: illegal dot expression",sc->code->_debugger->_size);
        } else {
            sc->nesting_stack[sc->file_i]--;
            s_return(sc,reverse_in_place(sc, sc->value, sc->args));
        }

    case OP_RDQUOTE:
        s_return(sc,cons(sc, sc->QUOTE, cons(sc, sc->value, sc->NIL)));

    case OP_RDQQUOTE:
        s_return(sc,cons(sc, sc->QQUOTE, cons(sc, sc->value, sc->NIL)));

    case OP_RDQQUOTEVEC:
        s_return(sc,cons(sc, mk_symbol(sc,"apply"),
                         cons(sc, mk_symbol(sc,"vector"),
                              cons(sc,cons(sc, sc->QQUOTE,
                                           cons(sc,sc->value,sc->NIL)),
                                   sc->NIL))));

    case OP_RDUNQUOTE:
        s_return(sc,cons(sc, sc->UNQUOTE, cons(sc, sc->value, sc->NIL)));

    case OP_RDUQTSP:
        s_return(sc,cons(sc, sc->UNQUOTESP, cons(sc, sc->value, sc->NIL)));

    case OP_RDVEC:
        sc->args=sc->value;
        s_goto(sc,OP_VECTOR);

        /* ========== printing part ========== */
    case OP_P0LIST:
        if(is_vector(sc->args)) {
            putstr(sc,"#(");
            sc->args=cons(sc,sc->args,mk_integer(sc,0));
            s_goto(sc,OP_PVECFROM);
        } else if(is_environment(sc->args)) {
            putstr(sc,"#<ENVIRONMENT>");
            s_return(sc,sc->T);
        } else if (!is_pair(sc->args)) {
            printatom(sc, sc->args, sc->print_flag);
            s_return(sc,sc->T);
        } else if (car(sc->args) == sc->QUOTE && ok_abbrev(cdr(sc->args))) {
            putstr(sc, "'");
            sc->args = cadr(sc->args);
            s_goto(sc,OP_P0LIST);
        } else if (car(sc->args) == sc->QQUOTE && ok_abbrev(cdr(sc->args))) {
            putstr(sc, "`");
            sc->args = cadr(sc->args);
            s_goto(sc,OP_P0LIST);
        } else if (car(sc->args) == sc->UNQUOTE && ok_abbrev(cdr(sc->args))) {
            putstr(sc, ",");
            sc->args = cadr(sc->args);
            s_goto(sc,OP_P0LIST);
        } else if (car(sc->args) == sc->UNQUOTESP && ok_abbrev(cdr(sc->args))) {
            putstr(sc, ",@");
            sc->args = cadr(sc->args);
            s_goto(sc,OP_P0LIST);
        } else {
            putstr(sc, "(");
            s_save(sc,OP_P1LIST, cdr(sc->args), sc->NIL);
            sc->args = car(sc->args);
            s_goto(sc,OP_P0LIST);
        }

    case OP_P1LIST:
        if (is_pair(sc->args)) {
            s_save(sc,OP_P1LIST, cdr(sc->args), sc->NIL);
            putstr(sc, " ");
            sc->args = car(sc->args);
            s_goto(sc,OP_P0LIST);
        } else if(is_vector(sc->args)) {
            s_save(sc,OP_P1LIST,sc->NIL,sc->NIL);
            putstr(sc, " . ");
            s_goto(sc,OP_P0LIST);
        } else {
            if (sc->args != sc->NIL) {
                putstr(sc, " . ");
                printatom(sc, sc->args, sc->print_flag);
            }
            putstr(sc, ")");
            s_return(sc,sc->T);
        }
    case OP_PVECFROM: {
        int i=ivalue_unchecked(cdr(sc->args));
        pointer vec=car(sc->args);
        int len=ivalue_unchecked(vec);
        if(i==len) {
            putstr(sc,")");
            s_return(sc,sc->T);
        } else {
            pointer elem=vector_elem(vec,i);
            ivalue_unchecked(cdr(sc->args))=i+1;
            s_save(sc,OP_PVECFROM, sc->args, sc->NIL);
            sc->args=elem;
            putstr(sc," ");
            s_goto(sc,OP_P0LIST);
        }
    }

    default:
        sprintf(sc->strbuff, "%d: illegal operator", sc->op);
        Error_0(sc,sc->strbuff,0);
        // ASIMP
        std::cout << "ILLEGAL OPERATION " << sc->op << std::endl;
        ///////////////
    }
    return sc->T;
}

static pointer opexe_6(scheme *sc, enum scheme_opcodes op) {
    pointer x, y;
    long v;

    switch (op) {
    case OP_LIST_LENGTH:     /* length */   /* a.k */
        v=list_length(sc,car(sc->args));
        if(v<0) {
            Error_1(sc,"length: not a list:",car(sc->args),sc->args->_size);
        }
        s_return(sc,mk_integer(sc, v));

    case OP_ASSQ:       /* assq */     /* a.k */
        x = car(sc->args);
        for (y = cadr(sc->args); is_pair(y); y = cdr(y)) {
            if (!is_pair(car(y))) {
                Error_0(sc,"unable to handle non pair element",sc->args->_size);
            }
            if (x == caar(y))
                break;
        }
        if (is_pair(y)) {
            s_return(sc,car(y));
        } else {
            s_return(sc,sc->F);
        }


    case OP_GET_CLOSURE:     /* get-closure-code */   /* a.k */
        sc->args = car(sc->args);
        if (sc->args == sc->NIL) {
            s_return(sc,sc->F);
        } else if (is_closure(sc->args)) {
            s_return(sc,cons(sc, sc->LAMBDA, closure_code(sc->value)));
        } else if (is_macro(sc->args)) {
            s_return(sc,cons(sc, sc->LAMBDA, closure_code(sc->value)));
        } else {
            s_return(sc,sc->F);
        }
    case OP_CLOSUREP:        /* closure? */
        /*
         * macro object is also a closure.
         * Therefore, (closure? <#MACRO>) ==> #t
         */
        s_retbool(is_closure(car(sc->args)));
    case OP_MACROP:          /* macro? */
        s_retbool(is_macro(car(sc->args)));
    default:
        sprintf(sc->strbuff, "%d: illegal operator", sc->op);
        Error_0(sc,sc->strbuff,0);
        // ASIMP
        std::cout << "ILLEGAL OPERATION " << sc->op << std::endl;
        ///////////////
    }
    return sc->T; /* NOTREACHED */
}

typedef pointer (*dispatch_func)(scheme *, enum scheme_opcodes);

typedef int (*test_predicate)(pointer);
static int is_any(pointer p) { return 1;}
static int is_num_integer(pointer p) {
    return is_number(p) && ((p)->_object._number.num_type==0);
    //return is_number(p) && ((p)->_object._number.is_fixnum);
}
static int is_nonneg(pointer p) {
    return is_num_integer(p) && ivalue(p)>=0;
}

/* Correspond carefully with following defines! */
static struct {
    test_predicate fct;
    const char *kind;
} tests[]={
    {0,0}, /* unused */
    {is_any, 0},
    {is_string, "string"},
    {is_symbol, "symbol"},
    {is_port, "port"},
    {0,"input port"},
    {0,"output_port"},
    {is_environment, "environment"},
    {is_pair, "pair"},
    {0, "pair or '()"},
    {is_character, "character"},
    {is_vector, "vector"},
    {is_number, "number"},
    {is_num_integer, "integer"},
    {is_nonneg, "non-negative integer"},
    {is_cptr, "cptr"} //,
    // {is_objc, "objc"}
};

#define TST_NONE 0
#define TST_ANY "\001"
#define TST_STRING "\002"
#define TST_SYMBOL "\003"
#define TST_PORT "\004"
#define TST_INPORT "\005"
#define TST_OUTPORT "\006"
#define TST_ENVIRONMENT "\007"
#define TST_PAIR "\010"
#define TST_LIST "\011"
#define TST_CHAR "\012"
#define TST_VECTOR "\013"
#define TST_NUMBER "\014"
#define TST_INTEGER "\015"
#define TST_NATURAL "\016"
#define TST_CPTR "\017"
//#define TST_OBJC "\018"

typedef struct {
    dispatch_func func;
    char *name;
    int min_arity;
    int max_arity;
    char *arg_tests_encoding;
} op_code_info;

#define INF_ARG 0xffff

static op_code_info dispatch_table[]= {
#define _OP_DEF(A,B,C,D,E,OP) {A,(char*)B,C,D,(char*)E},
#include "OPDefines.h"
    { 0, NULL, 0, 0, NULL }
};

//
//  Dodgy Function to return string name for common opcodes
//
//
static const char* opcodename(int opcode)
{
    switch(opcode)
    {
    case 0:
        return "load";
    case 1:
        return "t0lvl";
    case 2:
        return "t1lvl";
    case 3:
        return "read_internal";
    case 4:
        return "gensym";
    case 5:
        return "valueprint";
    case 6:
        return "eval";
    case 7:
        return "e0args";
    case 8:
        return "e1args";
    case 9:
        return "apply";
    case 10:
        return "domacro";
    case 11:
        return "lambda";
    case 12:
        return "mkclosure";
    case 13:
        return "quote";
    case 14:
        return "def0";
    case 15:
        return "def1";
    case 16:
        return "defp";
    case 17:
        return "begin";
    case 18:
        return "if0";
    case 19:
        return "if1";
    case 20:
        return "set0";
    case 21:
        return "set1";
    case 22:
        return "let0";
    case 23:
        return "let1";
    case 24:
        return "let2";
    case 25:
        return "let0ast";
    case 26:
        return "let1ast";
    case 27:
        return "let2ast";
    case 28:
        return "let0rec";
    case 29:
        return "let1rec";
    case 30:
        return "let2rec";
    case 31:
        return "cond0";
    case 32:
        return "cond1";
    case 33:
        return "delay";
    case 34:
        return "and0";
    case 35:
        return "and1";
    case 36:
        return "or0";
    case 37:
        return "or1";
    case 38:
        return "c0stream";
    case 39:
        return "c1stream";
    case 40:
        return "macro0";
    case 41:
        return "macro1";
    case 42:
        return "case0";
    case 43:
        return "case1";
    case 44:
        return "case2";

    case 165:
        return "rdsexpr";
    case 166:
        return "rdlist";
    case 167:
        return "rddot";
    case 168:
        return "rdquote";
    case 169:
        return "rdqquote";
    case 170:
        return "rdqquotevec";
    case 171:
        return "rdunquote";
    case 172:
        return "rduqtsp";
    case 173:
        return "rdvec";
    case 174:
        return "p0list";
    case 175:
        return "p1list";
    case 176:
        return "pvecfrom";

    default:
        return dispatch_table[opcode].name;
    }
    //return dispatch_table[opcode].name;
}

const char *procname(pointer x) {
    int n=procnum(x);
    const char *name=dispatch_table[n].name;
    if(name==0) {
        name="ILLEGAL!";
    }
    return name;
}

/* kernel of this interpreter */
static void Eval_Cycle(scheme *sc, enum scheme_opcodes op) {
    int count=0;
    int old_op;

    sc->op = op;
    for (;;) {
        if(extemp::UNIV::TIME > sc->call_end_time)
        {
            std::cout << "TIME:" << extemp::UNIV::TIME << " END:" << sc->call_end_time << std::endl;
            char msg[512];
            if(is_symbol(sc->last_symbol_apply)) {
                sprintf(msg,"\"%s\" Exceeded maximum rumtime. If you need a higher default process execution time use sys:set-default-timeout\n",symname_sc(sc,sc->last_symbol_apply));
            }else{
                sprintf(msg,"Exceeded maximum rumtime. If you need a higher default process execution time use sys:set-default-timeout\n");
            }
            sc->call_end_time = ULLONG_MAX;
            _Error_1(sc, msg, sc->NIL, sc->code->_debugger->_size);
            return;
        }
        op_code_info *pcd=dispatch_table+sc->op;
        if (pcd->name!=0) { /* if built-in function, check arguments */
            char msg[512];
            int ok=1;
            //pointer error = 0;
            int n=list_length(sc,sc->args);

            /* Check number of arguments */
            if(n<pcd->min_arity) {
                ok=0;
                sprintf(msg,"function(%s): needs%s %d argument(s)",
                        pcd->name,
                        pcd->min_arity==pcd->max_arity?"":" at least",
                        pcd->min_arity);
                // ASIMP
                std::cout << "PROBLEM HERE A? " << std::endl;
                /////////
            }
            if(ok && n>pcd->max_arity) {
                ok=0;
                sprintf(msg,"function(%s): needs%s %d argument(s)",
                        pcd->name,
                        pcd->min_arity==pcd->max_arity?"":" at most",
                        pcd->max_arity);
                std::cout << "PROBLEM HERE B? " << std::endl;
            }
            if(ok) {
                if(pcd->arg_tests_encoding!=0) {
                    int i=0;
                    int j;
                    const char *t=pcd->arg_tests_encoding;
                    pointer arglist=sc->args;
                    do {
                        pointer arg=car(arglist);
                        j=(int)t[0];
                        if(j==TST_INPORT[0]) {
                            if(!is_inport(arg)) break;
                        } else if(j==TST_OUTPORT[0]) {
                            if(!is_outport(arg)) break;
                        } else if(j==TST_LIST[0]) {
                            if(arg!=sc->NIL && !is_pair(arg)) break;
                        } else {
                            if(!tests[j].fct(arg)) break;
                        }

                        if(t[1]!=0) {/* last test is replicated as necessary */
                            t++;
                        }
                        arglist=cdr(arglist);
                        i++;
                    } while(i<n);
                    if(i<n) {
                        ok=0;
                        std::stringstream ss;
                        extemp::UNIV::printSchemeCell(sc, ss, sc->args, true);
                        sprintf(msg,"function(%s): argument %d must be: %s\nargument values: %s",
                                pcd->name,
                                i+1,
                                tests[j].kind,
                                ss.str().c_str());
                        /////////
                    }
                }
            }
            if(!ok) {
                //std::cout << "GENERAL EVAL ERROR !!!" << std::endl;
                /////////
                //if(_Error_1(sc,msg,0)==sc->NIL) {
                if(_Error_1(sc,msg,sc->code,sc->code->_debugger->_size)==sc->NIL) {
                    return;
                }
                pcd=dispatch_table+sc->op;
            }
        }
        old_op=sc->op;
        if (pcd->func(sc, (enum scheme_opcodes)sc->op) == sc->NIL) {
            return;
        }
        if(sc->no_memory) {
            std::cout << " NO MEMORY ERROR " << std::endl;
            fprintf(stderr,"No memory!\n");
            return;
        }
        count++;
    }
}

/* ========== Initialization of internal keywords ========== */

static void assign_syntax(scheme *sc, char *name) {
    pointer x;

    x = oblist_add_by_name(sc, name);
    typeflag(x) |= T_SYNTAX;
}

static void assign_proc(scheme *sc, enum scheme_opcodes op, char *name) {
    pointer x, y;

    x = mk_symbol(sc, name);
    y = mk_proc(sc,op);
    new_slot_in_env(sc, x, y);
}

static pointer mk_proc(scheme *sc, enum scheme_opcodes op) {
    pointer y;

    y = get_cell(sc, sc->NIL, sc->NIL);
    typeflag(y) = (T_PROC | T_ATOM);
    ivalue_unchecked(y) = (long) op;
    set_integer(y);
    return y;
}

/* Hard-coded for the given keywords. Remember to rewrite if more are added! */
static int syntaxnum(pointer p) {
    const char *s=strvalue(car(p));
    switch(strlength(car(p))) {
    case 2:
        if(s[0]=='i') return OP_IF0;        /* if */
        else return OP_OR0;                 /* or */
    case 3:
        if(s[0]=='a') return OP_AND0;      /* and */
        else return OP_LET0;               /* let */
    case 4:
        switch(s[3]) {
        case 'e': return OP_CASE0;         /* case */
        case 'd': return OP_COND0;         /* cond */
        case '*': return OP_LET0AST;       /* let* */
        default: return OP_SET0;           /* set! */
        }
    case 5:
        switch(s[2]) {
        case 'g': return OP_BEGIN;         /* begin */
        case 'l': return OP_DELAY;         /* delay */
        case 'c': return OP_MACRO0;        /* macro */
        default: return OP_QUOTE;          /* quote */
        }
    case 6:
        switch(s[2]) {
        case 'm': return OP_LAMBDA;        /* lambda */
        case 'f': return OP_DEF0;          /* define */
        default: return OP_LET0REC;        /* letrec */
        }
    default:
        return OP_C0STREAM;                /* cons-stream */
    }
}

scheme* extempore_scheme_init_new() {
  scheme *sc=(scheme*)malloc(sizeof(scheme));
  printf("Scheme (xtm) init new %p \n",sc);
    return sc;
}

scheme *scheme_init_new() {
  scheme *sc=(scheme*)malloc(sizeof(scheme));
  //printf("Scheme init new %p \n",sc);
    if(!scheme_init(sc)) {
        free(sc);
        return 0;
    } else {
        return sc;
    }
}

scheme *scheme_init_new_custom_alloc(func_alloc malloc, func_dealloc free) {

  scheme *sc=(scheme*)malloc(sizeof(scheme));
  //printf("Scheme init new custom %p \n",sc);
    if(!scheme_init_custom_alloc(sc,malloc,free)) {
        free(sc);
        return 0;
    } else {
        return sc;
    }
}

int scheme_init(scheme *sc) {
    return scheme_init_custom_alloc(sc,malloc,free);
}

int scheme_init_custom_alloc(scheme *sc, func_alloc malloc, func_dealloc free) {
    new (sc) scheme; // initialize properly
    int i, n=sizeof(dispatch_table)/sizeof(dispatch_table[0]);
    pointer x;
    //printf("Scheme init custom alloc %p\n",sc);

    num_zero.num_type=T_INTEGER; //is_fixnum=1;
    num_zero.value.ivalue=0;
    num_one.num_type=T_INTEGER; //is_fixnum=1;
    num_one.value.ivalue=1;

#if USE_
    sc->vptr=&vtbl;
#endif
    sc->gensym_cnt=0;
    sc->malloc=malloc;
    sc->free=free;
    sc->last_cell_seg = -1;

    sc->fcells = 0;
    sc->allocation_request = -1;
    sc->no_memory=0;
    sc->dark = 1;

    if (alloc_cellseg(sc,FIRST_CELLSEGS) != FIRST_CELLSEGS) {
        sc->no_memory=1;
        return 0;
    }

    sc->NIL = get_cell(sc,sc->NIL,sc->NIL);
    sc->sink = get_cell(sc,sc->NIL,sc->NIL);
    sc->T = get_cell(sc,sc->NIL,sc->NIL);
    sc->F = get_cell(sc,sc->NIL,sc->NIL);
    sc->EOF_OBJ = get_cell(sc,sc->NIL,sc->NIL);

    sc->free_cell = sc->NIL;
    sc->inport=sc->NIL;
    sc->outport=sc->NIL;
    sc->save_inport=sc->NIL;
    sc->loadport=sc->NIL;
    sc->nesting=0;
    sc->interactive_repl=0;

    dump_stack_initialize(sc);
    sc->code = sc->NIL;
    sc->tracing=0;
    sc->args = sc->NIL;
    sc->value = sc->NIL;

    /* init sc->NIL */
    typeflag(sc->NIL) = (T_ATOM | MARK);
    car(sc->NIL) = cdr(sc->NIL) = sc->NIL;
    /* init T */
    typeflag(sc->T) = (T_ATOM | MARK);
    car(sc->T) = cdr(sc->T) = sc->T;
    /* init F */
    typeflag(sc->F) = (T_ATOM | MARK);
    car(sc->F) = cdr(sc->F) = sc->F;
    sc->oblist = oblist_initial_value(sc);

    /* init global_env */
    new_frame_in_env(sc, sc->NIL);
    sc->global_env = sc->envir;
    /* init else */
    x = mk_symbol(sc,"else");
    new_slot_in_env(sc, x, sc->T);

    assign_syntax(sc, (char*)"lambda");
    assign_syntax(sc, (char*)"quote");
    assign_syntax(sc, (char*)"define");
    assign_syntax(sc, (char*)"if");
    assign_syntax(sc, (char*)"begin");
    assign_syntax(sc, (char*)"set!");
    assign_syntax(sc, (char*)"let");
    assign_syntax(sc, (char*)"let*");
    assign_syntax(sc, (char*)"letrec");
    assign_syntax(sc, (char*)"cond");
    assign_syntax(sc, (char*)"delay");
    assign_syntax(sc, (char*)"and");
    assign_syntax(sc, (char*)"or");
    assign_syntax(sc, (char*)"cons-stream");
    assign_syntax(sc, (char*)"macro");
    assign_syntax(sc, (char*)"case");

    for(i=0; i<n; i++) {
        if(dispatch_table[i].name!=0) {
            assign_proc(sc, (enum scheme_opcodes)i, (char*)dispatch_table[i].name);
        }
    }

    /* initialization of global pointers to special symbols */
    sc->LAMBDA = mk_symbol(sc, "lambda");
    sc->QUOTE = mk_symbol(sc, "quote");
    sc->QQUOTE = mk_symbol(sc, "quasiquote");
    sc->UNQUOTE = mk_symbol(sc, "unquote");
    sc->UNQUOTESP = mk_symbol(sc, "unquote-splicing");
    sc->FEED_TO = mk_symbol(sc, "=>");
    sc->COLON_HOOK = mk_symbol(sc,"*colon-hook*");
    sc->LIVECODING_ERROR_HOOK = mk_symbol(sc, "*livecoding-error-hook*");
    sc->ERROR_HOOK = mk_symbol(sc, "*error-hook*");
    sc->SHARP_HOOK = mk_symbol(sc, "*sharp-hook*");

    //sc->imp_env = sc->NIL;
    //sc->imp_env.clear();
    sc->tmp_dump = sc->NIL;
    sc->tmp_args = sc->NIL;
    sc->func_called_by_extempore = sc->NIL;

    // setup treadmill stuff
    sc->mutex = new extemp::EXTMutex("treadmill_mutex");
    sc->mutex->init();
    sc->Treadmill_Guard = new extemp::EXTMonitor("treadmill_guard");
    sc->Treadmill_Guard->init();

    sc->treadmill_flip_active = false;
    sc->treadmill_scanner_finished = false;


    //define keywords
    int dispatch_table_length = sizeof(dispatch_table) / sizeof(dispatch_table[0]);

    treadmill_mark_roots(sc, sc->NIL, sc->NIL);

    ///////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    // Some very basic sanity checking
#ifdef TREADMILL_CHECKS
    bool empty_grey_list = false;
    printf("==============ALLOC POST MARK SANITY CHECKS====================\n");
    if(sc->treadmill_top->_list_colour != 1 || sc->treadmill_bottom->_list_colour != 1) {
        printf("Top & Bottom must both be ecru!  TOP(%d)  BOTTOM(%d) Catastrophic error in GC\n",sc->treadmill_top->_list_colour,sc->treadmill_bottom->_list_colour);
    }
    if(sc->treadmill_scan->_cw->_list_colour != 3)
    {
        printf("Should be black cells clockwise of SCAN\n");
    }
    if(sc->treadmill_top != sc->treadmill_scan)
    {
        empty_grey_list = true;
        if(sc->treadmill_top->_cw->_list_colour != 2)
        {
            printf("TOP _CW must be grey!\n");
        }
        if(sc->treadmill_scan->_list_colour != 2)
        {
            printf("Scan must be on a grey cell\n");
        }
    }
    if(sc->treadmill_bottom->_ccw->_list_colour != 0)
    {
        printf("BOTTOM _CCW must be pure white (free)\n");
    }
    if(sc->treadmill_free->_list_colour != 0)
    {
        printf("Free must be on a white cell\n");
    }
    //check no gaps between ecru cells
    pointer ecru_check = sc->treadmill_top;
    long long ecrus = 0;
    while(ecru_check != sc->treadmill_bottom->_ccw){
        if(ecru_check->_list_colour != 1)
        {
            printf("Should have complete list of ecrus between TOP and BOTTOM moving counter clockwise!  Catastrophic error in GC\n");
        }
        ecrus++;
        ecru_check = ecru_check->_ccw;
    }
    //check no gaps between white cells
    long long whites = 0;
    pointer white_check = sc->treadmill_free;
    while(white_check != sc->treadmill_bottom){
        if(white_check->_list_colour != 0)
        {
            printf("Should have complete list of whites between BOTTOM and FREE moving counter clockwise!  Catastrophic error in GC\n");
        }
        whites++;
        white_check = white_check->_cw;
    }
    //check no gaps between black cells
    long long blacks = 0;
    pointer black_check = sc->treadmill_scan->_cw;
    while(black_check != sc->treadmill_free){
        if(black_check->_list_colour != 3)
        {
            printf("Should have complete list of blacks between SCAN and FREE moving counter clockwise  colour(%d)!  Catastrophic error in GC\n",black_check->_list_colour);
        }
        blacks++;
        black_check = black_check->_cw;
    }
    //check no gaps between grey cells
    long long greys = 0;
    if(!empty_grey_list) {
        pointer grey_check = sc->treadmill_scan;
        while(grey_check != sc->treadmill_top){
            if(grey_check->_list_colour != 0)
            {
                printf("Should have complete list of whites between BOTTOM and FREE moving counter clockwise!  Catastrophic error in GC\n");
            }
            greys++;
            grey_check = grey_check->_ccw;
        }
    }
    printf("BLACKS(%lld)  GREYS(%lld)  WHITES(%lld)  ECRUS(%lld)  TOTAL(%lld)  SEGSIZE(%lld)\n",blacks,greys,whites,ecrus,blacks+greys+whites+ecrus,sc->total_memory_allocated);
    printf("-----------------DONE ALLOC POST MARK CHECK-----------------\n");
#endif
    /////////////////////////////////////////////////////////////////////
    ///////////////////////////////////////////////////////////////////////


    sc->treadmill_stop = false;
    sc->treadmill_scan_thread = new extemp::EXTThread(&treadmill_scanner, sc, "treadmill");
    sc->treadmill_scan_thread->start();

    sc->call_end_time = ULLONG_MAX;
    //sc->call_default_time = 158760000ll;  // 1 hour
    sc->call_default_time = extemp::UNIV::MINUTE() * 5;

    sc->last_symbol_apply = sc->NIL;

    return !sc->no_memory;
}

void scheme_set_input_port_file(scheme *sc, FILE *fin) {
    sc->inport=port_from_file(sc,fin,port_input);
}

void scheme_set_input_port_string(scheme *sc, char *start, char *past_the_end) {
    sc->inport=port_from_string(sc,start,past_the_end,port_input);
}

void scheme_set_output_port_file(scheme *sc, FILE *fout) {
    sc->outport=port_from_file(sc,fout,port_output);
}

void scheme_set_output_port_string(scheme *sc, char *start, char *past_the_end) {
    sc->outport=port_from_string(sc,start,past_the_end,port_output);
}

void scheme_set_external_data(scheme *sc, void *p) {
    sc->ext_data=p;
}

void scheme_deinit(scheme *sc) {
    int i;

    sc->treadmill_stop = true;
    sc->Treadmill_Guard->signal();

    sc->oblist=sc->NIL;
    sc->global_env=sc->NIL;
    dump_stack_free(sc);
    sc->envir=sc->NIL;
    sc->code=sc->NIL;
    sc->args=sc->NIL;
    sc->value=sc->NIL;
    if(is_port(sc->inport)) {
        typeflag(sc->inport) = T_ATOM;
    }
    sc->inport=sc->NIL;
    sc->outport=sc->NIL;
    if(is_port(sc->save_inport)) {
        typeflag(sc->save_inport) = T_ATOM;
    }
    sc->save_inport=sc->NIL;
    if(is_port(sc->loadport)) {
        typeflag(sc->loadport) = T_ATOM;
    }
    sc->loadport=sc->NIL;
//      sc->gc_verbose=0;
//      sc->gc_on = 0;

    for(i=0; i<=sc->last_cell_seg; i++) {
        sc->free(sc->alloc_seg[i]);
    }
}

void scheme_load_file(scheme *sc, FILE *fin) {
    dump_stack_reset(sc);
    sc->envir = sc->global_env;
    sc->file_i=0;
    sc->load_stack[0].kind=port_input|port_file;
    sc->load_stack[0].rep.stdio.file=fin;
    sc->loadport=mk_port(sc,sc->load_stack);
    sc->retcode=0;
    if(fin==stdin) {
        sc->interactive_repl=1;
    }
    sc->inport=sc->loadport;

    sc->func_called_by_extempore = sc->NIL;
    sc->call_end_time = extemp::UNIV::TIME+(uint64_t)(extemp::UNIV::HOUR());

    try{
        Eval_Cycle(sc, OP_T0LVL);
    }catch(ScmRuntimeError err){
        _Error_1(sc,err.msg,err.p,0,0);
    }

    typeflag(sc->loadport)=T_ATOM;
    if(sc->retcode==0) {
        sc->retcode=sc->nesting!=0;
    }

    while(!sc->applied_symbol_names.empty())
    {
        sc->applied_symbol_names.pop();
    }
    sc->last_symbol_apply = sc->NIL;
    sc->error_position = -1;

    sc->call_end_time = ULLONG_MAX;
}

void scheme_load_string(scheme *sc, const char *cmd, uint64_t start_time, uint64_t end_time) {

    dump_stack_reset(sc);
    sc->envir = sc->global_env;
    sc->file_i=0;
    sc->load_stack[0].kind=port_input|port_string;
    sc->load_stack[0].rep.string.start=(char*)cmd; /* This func respects const */
    sc->load_stack[0].rep.string.past_the_end=(char*)cmd+strlen(cmd);
    sc->load_stack[0].rep.string.curr=(char*)cmd;
    sc->loadport=mk_port(sc,sc->load_stack);
    sc->retcode=0;
    sc->interactive_repl=0;
    sc->inport=sc->loadport;

    sc->func_called_by_extempore = sc->NIL;

    //std::cout << "START: " << start_time << "  END: " << end_time << std::endl;

    sc->call_start_time = start_time;
    sc->call_end_time = end_time;

    try{
                Eval_Cycle(sc, OP_T0LVL);
    }catch(ScmRuntimeError err){
      std::cout << "Error: evaluating expr: " << cmd << std::endl;
                _Error_1(sc,err.msg,err.p,0,0);
    }catch(std::exception& e) {
                _Error_1(sc,e.what(),sc->NIL,0,0);
        }catch(std::string& e) {
                _Error_1(sc,e.c_str(),sc->NIL,0,0);
        }

    typeflag(sc->loadport)=T_ATOM;
    if(sc->retcode==0) {
        sc->retcode=sc->nesting!=0;
    }

    while(!sc->applied_symbol_names.empty())
    {
        sc->applied_symbol_names.pop();
    }
    sc->last_symbol_apply = sc->NIL;
    sc->error_position = -1;

    sc->call_end_time = ULLONG_MAX;
}

void scheme_define(scheme *sc, pointer envir, pointer symbol, pointer value) {
    pointer x;

    x=find_slot_in_env(sc,envir,symbol,0);
    if (x != sc->NIL) {
        set_slot_in_env(sc, x, value);
    } else {
        new_slot_spec_in_env(sc, envir, symbol, value);
    }
}

//#if !STANDALONE
void scheme_apply0(scheme *sc, const char *procname) {
    pointer carx=mk_symbol(sc,procname);
    pointer cdrx=sc->NIL;
    dump_stack_reset(sc);
    sc->envir = sc->global_env;
    sc->code = cons(sc,carx,cdrx);
    sc->interactive_repl=0;
    sc->retcode=0;

    try{
        Eval_Cycle(sc, OP_EVAL);
    }catch(ScmRuntimeError err){
        _Error_1(sc,err.msg,err.p,0,0);
    }
}

void scheme_call_without_stack_reset(scheme *sc, pointer func, pointer args)
{
    sc->args = args;
    sc->code = func;

    sc->func_called_by_extempore = func;

    sc->interactive_repl = 0;
    sc->retcode = 0;

    try{
        Eval_Cycle(sc, OP_APPLY);
    }catch(ScmRuntimeError err){
        _Error_1(sc,err.msg,err.p,0,0);
    }
}

void scheme_call(scheme *sc, pointer func, pointer args, uint64_t start_time, uint64_t end_time) {
    dump_stack_reset(sc);
    sc->envir = sc->global_env;
    sc->args = args;
    sc->code = func;

    sc->call_start_time = start_time;
    sc->call_end_time = end_time;

    sc->func_called_by_extempore = func;

    sc->interactive_repl = 0;
    sc->retcode = 0;

    try{
        Eval_Cycle(sc, OP_APPLY);
    }catch(ScmRuntimeError err){
        _Error_1(sc,err.msg,err.p,0,0);
    }

    while(!sc->applied_symbol_names.empty())
    {
        sc->applied_symbol_names.pop();
    }
    sc->last_symbol_apply = sc->NIL;
    sc->error_position = -1;

    sc->call_end_time = ULLONG_MAX;
}
//#endif
