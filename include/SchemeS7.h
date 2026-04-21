#ifndef _SCHEME_S7_H
#define _SCHEME_S7_H

#include <cstdio>
#include <cstring>
#include <cstdint>

#include "BranchPrediction.h"
#include "UNIV.h"
#include "s7.h"

#ifndef _MSC_VER
#define SCHEME_EXPORT
#else
#ifdef _SCHEME_SOURCE
#define SCHEME_EXPORT __declspec(dllexport)
#else
#define SCHEME_EXPORT __declspec(dllimport)
#endif
#endif

extern "C" {

// pointer is already typedef'd in UNIV.h as s7_cell* which matches s7_pointer
typedef pointer (*foreign_func)(struct scheme*, pointer);

struct scheme;

scheme* scheme_init_new();
void scheme_deinit(scheme* sc);
void scheme_set_input_port_file(scheme* sc, FILE* fin);
void scheme_set_input_port_string(scheme* sc, char* start, char* past_the_end);
void scheme_set_output_port_file(scheme* sc, FILE* fout);
void scheme_set_output_port_string(scheme* sc, char* start, char* past_the_end);
void scheme_load_file(scheme* sc, FILE* fin);
void scheme_load_string(scheme* sc, const char* cmd, uint64_t start_time, uint64_t end_time);
void scheme_apply0(scheme* sc, const char* procname);
pointer scheme_apply1(scheme* sc, const char* procname, pointer);
void scheme_call(scheme* sc, pointer func, pointer args, uint64_t start_time,
                 uint64_t call_duration);
void scheme_call_without_stack_reset(scheme* sc, pointer func, pointer args);
void scheme_set_external_data(scheme* sc, void* p);
void scheme_define(scheme* sc, pointer env, pointer symbol, pointer value);

pointer find_slot_in_env(scheme* sc, pointer env, pointer sym, int all);
void set_slot_in_env(scheme* sc, pointer slot, pointer value);
pointer slot_value_in_env(pointer slot);
void new_slot_in_env(scheme* sc, pointer variable, pointer value);
pointer reverse(scheme* sc, pointer a);
pointer reverse_in_place(scheme* sc, pointer term, pointer list);
pointer append(scheme* sc, pointer a, pointer b);
pointer assoc_strcmp(scheme* sc, pointer key, pointer alist, bool all = false);

EXPORT char* string_value(pointer p);
EXPORT pointer list_ref(scheme* sc, int pos, pointer a);
int eqv(pointer a, pointer b);
EXPORT pointer mk_vector(scheme* sc, int len);
EXPORT void fill_vector(scheme* sc, pointer vec, pointer obj);
pointer set_vector_elem(scheme* sc, pointer vec, int ielem, pointer a);

pointer _cons(scheme* sc, pointer a, pointer b, int immutable);
EXPORT pointer mk_integer(scheme* sc, long long num);
EXPORT pointer mk_i64(scheme* sc, long long num);
EXPORT pointer mk_i32(scheme* sc, int num);
EXPORT pointer mk_i16(scheme* sc, short num);
EXPORT pointer mk_i8(scheme* sc, char num);
EXPORT pointer mk_i1(scheme* sc, bool num);
EXPORT pointer mk_real(scheme* sc, double num);
EXPORT pointer mk_double(scheme* sc, double num);
EXPORT pointer mk_float(scheme* sc, float num);

EXPORT pointer mk_rational(scheme* sc, long long n, long long d);
EXPORT pointer mk_symbol(scheme* sc, const char* name);
EXPORT pointer gensym(scheme* sc);
EXPORT pointer mk_string(scheme* sc, const char* str);
EXPORT pointer mk_counted_string(scheme* sc, const char* str, int len);
EXPORT pointer mk_character(scheme* sc, int c);
EXPORT pointer mk_foreign_func(scheme* sc, foreign_func f);
EXPORT pointer mk_cptr(scheme* sc, void* p);
void putstr(scheme* sc, const char* s);
EXPORT int pointer_type(pointer p);

pointer mk_continuation(scheme* sc);
pointer mk_closure(scheme* sc, pointer c, pointer e);
const char* procname(pointer x);

pointer cons(scheme* sc, pointer a, pointer b);
pointer immutable_cons(scheme* sc, pointer a, pointer b);
void putcharacter(scheme* sc, int c);

EXPORT int is_number(pointer p);
EXPORT double r64value(pointer p);
EXPORT float r32value(pointer p);
EXPORT int is_rational(pointer p);
EXPORT int is_character(pointer p);
EXPORT long long charvalue(pointer p);
EXPORT int is_vector(pointer p);
long long vector_length(pointer vec);
pointer vector_elem(pointer vec, int ielem);

EXPORT int is_port(pointer p);

pointer pair_car(pointer p);
pointer pair_cdr(pointer p);
pointer set_car(scheme* sc, pointer p, pointer q);
pointer set_cdr(scheme* sc, pointer p, pointer q);

EXPORT int is_symbol(pointer p);
EXPORT char* symname(pointer p);

EXPORT int is_string(pointer p);
EXPORT int is_syntax(pointer p);
EXPORT int is_proc(pointer p);
EXPORT int is_foreign(pointer p);
EXPORT void* cptr_value(pointer p);
EXPORT char* syntaxname(pointer p);
EXPORT int is_closure(pointer p);
EXPORT int is_macro(pointer p);
pointer closure_code(pointer p);
pointer closure_env(pointer p);

EXPORT int is_continuation(pointer p);
EXPORT int is_promise(pointer p);
EXPORT int is_environment(pointer p);
int is_immutable(pointer p);
void setimmutable(pointer p);

EXPORT int is_pair(pointer p);
EXPORT int is_integer(pointer p);
EXPORT int is_real(pointer p);
EXPORT int is_cptr(pointer p);
EXPORT int is_cptr_or_str(pointer p);

int64_t ivalue(pointer p);
double rvalue(pointer p);

EXPORT int64_t i64value(pointer p);
EXPORT int32_t i32value(pointer p);
EXPORT int16_t i16value(pointer p);
EXPORT int8_t i8value(pointer p);
EXPORT bool i1value(scheme* sc, pointer p);
EXPORT double r64value(pointer p);
EXPORT float r32value(pointer p);
EXPORT long long charvalue(pointer p);
EXPORT int is_integer_extern(pointer p);

void load_file(scheme* sc, FILE* fin);
EXPORT void load_string(scheme* sc, const char* input);

int list_length(scheme* sc, pointer a);

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

}  // extern "C"

class ScmRuntimeError {
  public:
    ScmRuntimeError(const char* _msg, pointer _p) {
        msg = _msg;
        p = _p;
    }
    const char* msg;
    pointer p;
};

#endif
