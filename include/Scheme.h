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


#ifndef _SCHEME_H
#define _SCHEME_H

#include <stdio.h>
#include <string.h>
#include <stdint.h>

#include "BranchPrediction.h"

#define macintosh 1
#define USE_INTERFACE 1

/*
 * Default values for #define'd symbols
 */
#ifndef STANDALONE       /* If used as standalone interpreter */
# define STANDALONE 1
#endif

#ifndef _MSC_VER
# define USE_STRCASECMP 1
# define USE_STRLWR 1
# define SCHEME_EXPORT
#else
# define USE_STRCASECMP 0
# define USE_STRLWR 0
# ifdef _SCHEME_SOURCE
#  define SCHEME_EXPORT __declspec(dllexport)
# else
#  define SCHEME_EXPORT __declspec(dllimport)
# endif
#endif

#if USE_NO_FEATURES
# define USE_MATH 0
# define USE_CHAR_CLASSIFIERS 0
# define USE_ASCII_NAMES 0
# define USE_STRING_PORTS 0
# define USE_ERROR_HOOK 0
# define USE_TRACING 0
# define USE_COLON_HOOK 0
# define USE_DL 0
# define USE_PLIST 0
#endif

/*
 * Leave it defined if you want continuations, and also for the Sharp Zaurus.
 * Undefine it if you only care about faster speed and not strict Scheme compatibility.
 */
//#define USE_SCHEME_STACK

#if USE_DL
# define USE_INTERFACE 1
#endif


#ifndef USE_MATH         /* If math support is needed */
# define USE_MATH 1
#endif

#ifndef USE_CHAR_CLASSIFIERS  /* If char classifiers are needed */
# define USE_CHAR_CLASSIFIERS 1
#endif

#ifndef USE_ASCII_NAMES  /* If extended escaped characters are needed */
# define USE_ASCII_NAMES 1
#endif

#ifndef USE_STRING_PORTS      /* Enable string ports */
# define USE_STRING_PORTS 1
#endif

#ifndef USE_TRACING
# define USE_TRACING 1
#endif

#ifndef USE_PLIST
# define USE_PLIST 1
#endif

/* To force system errors through user-defined error handling (see *error-hook*) */
#ifndef USE_ERROR_HOOK
# define USE_ERROR_HOOK 1
#endif

#ifndef USE_COLON_HOOK   /* Enable qualified qualifier */
# define USE_COLON_HOOK 1
#endif

#ifndef USE_STRCASECMP   /* stricmp for Unix */
# define USE_STRCASECMP 0
#endif

#ifndef USE_STRLWR
# define USE_STRLWR 1
#endif

#ifndef STDIO_ADDS_CR    /* Define if DOS/Windows */
# define STDIO_ADDS_CR 0
#endif

//#ifndef INLINE
//# define INLINE
//#endif

#ifndef USE_INTERFACE
# define USE_INTERFACE 0
#endif

typedef struct scheme scheme;
typedef struct cell* pointer;

typedef void* (*func_alloc)(size_t);
typedef void (*func_dealloc)(void*);

extern "C"
{
scheme *scheme_init_new();
scheme *scheme_init_new_custom_alloc(func_alloc malloc, func_dealloc free);
int scheme_init(scheme *sc);
int scheme_init_custom_alloc(scheme *sc, func_alloc, func_dealloc);
void scheme_deinit(scheme *sc);
void scheme_set_input_port_file(scheme *sc, FILE *fin);
void scheme_set_input_port_string(scheme *sc, char *start, char *past_the_end);
void scheme_set_output_port_file(scheme *sc, FILE *fin);
void scheme_set_output_port_string(scheme *sc, char *start, char *past_the_end);
void scheme_load_file(scheme *sc, FILE *fin);
void scheme_load_string(scheme *sc, const char *cmd, uint64_t start_time, uint64_t end_time);
void scheme_apply0(scheme *sc, const char *procname);
pointer scheme_apply1(scheme *sc, const char *procname, pointer);
const char *procname(pointer x);
pointer mk_continuation(scheme* sc);
pointer mk_closure(scheme* sc, pointer c, pointer e);
void scheme_call(scheme *sc, pointer func, pointer args, uint64_t start_time, uint64_t call_duration);
void scheme_call_without_stack_reset(scheme *sc, pointer func, pointer args);
void scheme_set_external_data(scheme *sc, void *p);
void scheme_define(scheme *sc, pointer env, pointer symbol, pointer value);

pointer find_slot_in_env(scheme *sc, pointer env, pointer sym, int all);
void set_slot_in_env(scheme *sc, pointer slot, pointer value);
pointer slot_value_in_env(pointer slot);
void new_slot_in_env(scheme *sc, pointer variable, pointer value);
pointer reverse(scheme *sc, pointer a);
pointer reverse_in_place(scheme *sc, pointer term, pointer list);
pointer append(scheme *sc, pointer a, pointer b);
int list_length(scheme *sc, pointer a);
pointer assoc_strcmp(scheme *sc, pointer key, pointer alist, bool all = false);
int is_real(pointer a);
char *string_value(pointer p);
pointer list_ref(scheme* sc, int pos, pointer a);
int eqv(pointer a, pointer b);
pointer mk_vector(scheme *sc, int len);
void fill_vector(scheme* sc, pointer vec, pointer obj);
pointer set_vector_elem(scheme* sc, pointer vec, int ielem, pointer a);
int scheme_init(scheme* sc);
scheme* extempore_scheme_init_new();

typedef pointer (*foreign_func)(scheme *, pointer);

pointer _cons(scheme *sc, pointer a, pointer b, int immutable);
pointer mk_integer(scheme *sc, long long num);
pointer mk_i64(scheme *sc, long long num);
pointer mk_i32(scheme *sc, int num);
pointer mk_i16(scheme *sc, short num);
pointer mk_i8(scheme *sc, char num);
pointer mk_i1(scheme *sc, bool num);
pointer mk_real(scheme *sc, double num);
pointer mk_double(scheme *sc, double num);
pointer mk_float(scheme *sc, float num);

pointer mk_rational(scheme *sc, long long n, long long d);
pointer mk_symbol(scheme *sc, const char *name);
pointer gensym(scheme *sc);
//pointer rungc(scheme* sc, pointer a, pointer b);
pointer mk_string(scheme *sc, const char *str);
pointer mk_counted_string(scheme *sc, const char *str, int len);
pointer mk_character(scheme *sc, int c);
pointer mk_foreign_func(scheme *sc, foreign_func f);
pointer mk_cptr(scheme* sc, void* p);
void putstr(scheme *sc, const char *s);
int pointer_type(pointer);
void treadmill_print(scheme* sc, char* title);

void scheme_define(scheme *sc, pointer env, pointer symbol, pointer value);
pointer cons(scheme *sc, pointer a, pointer b);
pointer immutable_cons(scheme *sc, pointer a, pointer b);
pointer mk_integer(scheme *sc, long long num);
pointer mk_real(scheme *sc, double num);
pointer mk_rational(scheme *sc, long long n, long long d);
pointer mk_symbol(scheme *sc, const char *name);
pointer gensym(scheme *sc);
//    pointer (*rungc)(scheme* sc, pointer a, pointer b);
pointer mk_string(scheme *sc, const char *str);
pointer mk_counted_string(scheme *sc, const char *str, int len);
pointer mk_character(scheme *sc, int c);
pointer mk_vector(scheme *sc, int len);
pointer mk_foreign_func(scheme *sc, foreign_func f);
pointer mk_cptr(scheme* sc, void* p);
void putstr(scheme *sc, const char *s);
void putcharacter(scheme *sc, int c);

int is_string(pointer p);
char* string_value(pointer p);
int is_number(pointer p);
int64_t ivalue(pointer p);
int64_t i64value(pointer p);
int32_t i32value(pointer p);
int16_t i16value(pointer p);
int8_t i8value(pointer p);
bool i1value(scheme* _sc, pointer p);
double rvalue(pointer p);
double r64value(pointer p);
float r32value(pointer p);
int is_integer(pointer p);
int is_rational(pointer p);
int is_character(pointer p);
long long charvalue(pointer p);
int is_vector(pointer p);
long long vector_length(pointer vec);
void fill_vector(scheme* sc, pointer vec, pointer elem);
pointer vector_elem(pointer vec, int ielem);
pointer set_vector_elem(scheme* sc, pointer vec, int ielem, pointer newel);
int is_port(pointer p);

int is_pair(pointer p);
pointer pair_car(pointer p);
pointer pair_cdr(pointer p);
pointer set_car(scheme* sc, pointer p, pointer q);
pointer set_cdr(scheme* sc, pointer p, pointer q);

int is_symbol(pointer Ptr);
char* symname(pointer p);

int is_syntax(pointer p);
int is_proc(pointer p);
int is_foreign(pointer p);
int is_cptr(pointer p);
int is_cptr_or_str(pointer p);
void* cptr_value(pointer p);
char* syntaxname(pointer p);
int is_closure(pointer p);
int is_macro(pointer p);
pointer closure_code(pointer p);
pointer closure_env(pointer p);

int is_continuation(pointer p);
int is_promise(pointer p);
int is_environment(pointer p);
int is_immutable(pointer p);
void setimmutable(pointer p);

void load_file(scheme *sc, FILE *fin);
void load_string(scheme *sc, const char *input);

enum scheme_types {
    T_STRING = 1,
    T_NUMBER = 2,
    T_SYMBOL = 3,
    T_PROC = 4,
    T_PAIR = 5,
    T_CLOSURE = 6,
    T_CONTINUATION = 7,
    T_FOREIGN = 8,
    T_CHARACTER = 9,
    T_PORT = 10,
    T_VECTOR = 11,
    T_MACRO = 12,
    T_PROMISE = 13,
    T_ENVIRONMENT = 14,
    T_CPTR = 15,
    T_LAST_SYSTEM_TYPE = 15
};

struct port {
    unsigned char kind;
    union {
    struct {
        FILE *file;
        int closeit;
    } stdio;
    struct {
        char *start;
        char *past_the_end;
        char *curr;
    } string;
    } rep;
};

enum num_type {
    T_INTEGER = 0,
    T_REAL = 1,
    T_RATIONAL = 3
};

/* num, for generic arithmetic */
typedef struct num {
    num_type num_type;
    union {
        int64_t ivalue;
        double rvalue;
        struct {
            int64_t n;
            int64_t d;
        } ratvalue;
    } value;
} num;

/* cell structure */
struct cell {
    unsigned int _flag;
    unsigned char _colour;
    unsigned char _list_colour;
    unsigned int _size;
    cell* _debugger;
    cell* _cw;
    cell* _ccw;
    union {
        struct {
            char* _svalue;
            int   _length;
        } _string;
        num _number;
        port* _port;
        foreign_func _ff;
        struct {
            cell* _car;
            cell* _cdr;
        } _cons;
        void* _cptr;
    } _object;
};

inline double rvalue(pointer Ptr)
{
    auto type(Ptr->_object._number.num_type);
    if (likely(type == T_INTEGER)) {
        return Ptr->_object._number.value.ivalue;
    }
    if (likely(type == T_REAL)) {
        return Ptr->_object._number.value.rvalue;
    }
    if (likely(type == T_RATIONAL)) {
        return double(Ptr->_object._number.value.ratvalue.n) / Ptr->_object._number.value.ratvalue.d;
    }
    return 0.0;
}

inline pointer vector_elem(pointer Vector, int Index)
{
    return reinterpret_cast<pointer*>(Vector->_object._cptr)[Index];
}

extern void insert_treadmill(scheme* sc, pointer p);

inline pointer set_vector_elem(scheme* Scheme, pointer Vector, int Index, pointer Val)
{
#ifdef TREADMILL_CHECKS
    last_call_to_insert_treadmill = 4;
#endif
    insert_treadmill(Scheme, Val);
    reinterpret_cast<pointer*>(Vector->_object._cptr)[Index] = Val;
    return Val;
}

}

#endif

