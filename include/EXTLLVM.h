/*
 * Copyright (c) 2011, Andrew Sorensen
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

#ifndef _EXTLLVM_H
#define _EXTLLVM_H

#include "Scheme.h"
#include <vector>
#include <string>
#include <memory>

#include "EXTMutex.h"
#include "BranchPrediction.h"
#include "UNIV.h"

struct zone_hooks_t {
  uint64_t space; // here just so we don't get <i8*,i8*>
  void* hook; // xtlang closure of type [void]*
  zone_hooks_t* hooks;
};

// WARNING WARNING WARNING - HERE BE DRAGONS
// THIS STRUCTURE IS REFERENCED FROM GENERATED CODE
// DO NOT ALTER IT!!!

struct llvm_zone_t {
  void* memory;
  uint64_t offset;
  uint64_t mark;
  uint64_t size;
  zone_hooks_t* cleanup_hooks;
  llvm_zone_t* memories;
};

struct _llvm_callback_struct_ {
    void(*fptr)(void*);
    void* dat;
};

struct llvm_zone_stack
{
    llvm_zone_t* head;
    llvm_zone_stack* tail;
};

struct closure_address_table;

extern THREAD_LOCAL llvm_zone_stack* tls_llvm_zone_stack;
extern THREAD_LOCAL uint64_t tls_llvm_zone_stacksize;

extern "C"
{

const char* llvm_scheme_ff_get_name(foreign_func ff);
void llvm_scheme_ff_set_name(foreign_func ff,const char* name);
void llvm_runtime_error(int error, void* arg);

bool llvm_zone_copy_ptr(void* ptr1, void* ptr2);
void llvm_zone_ptr_set_size(void* ptr, uint64_t size);
uint64_t llvm_zone_ptr_size(void* ptr);
void llvm_zone_print(llvm_zone_t* zone);
void llvm_destroy_zone_after_delay(llvm_zone_t* zone, uint64_t delay);

bool llvm_ptr_in_zone(llvm_zone_t*, void*);
bool llvm_ptr_in_current_zone(void*);

void llvm_schedule_callback(long long, void*);
void* llvm_get_function_ptr(char* n);
pointer llvm_scheme_env_set(scheme* _sc, char* sym);
bool llvm_check_valid_dot_symbol(scheme* sc, char* symbol);
bool regex_split(char* str, char** a, char** b);

static inline uint64_t string_hash(const char* str)
{
    uint64_t result(0);
    unsigned char c;
    while((c = *(str++))) {
        result = result * 33 + uint8_t(c);
    }
    return result;
}

  void llvm_send_udp(char* host, int port, void* message, int message_length);
  int32_t llvm_frames();
  int32_t llvm_channels();
  int32_t llvm_in_channels();

  double imp_randd();
  float imp_randf();
  int64_t imp_rand1_i64(int64_t a);
  int64_t imp_rand2_i64(int64_t a, int64_t b);
  int32_t imp_rand1_i32(int32_t a);
  int32_t imp_rand2_i32(int32_t a, int32_t b);
  double imp_rand1_d(double a);
  double imp_rand2_d(double a, double b);
  float imp_rand1_f(float a);
  float imp_rand2_f(float a, float b);

  closure_address_table* new_address_table();
  closure_address_table* add_address_table(llvm_zone_t* zone, char* name, uint32_t offset, char* type, int alloctype, struct closure_address_table* table);
  closure_address_table* get_address_table(const char* name, closure_address_table* table);
  uint32_t get_address_offset(uint64_t id, closure_address_table* table);
  char* get_address_type(uint64_t id, closure_address_table* table);
  bool check_address_exists(uint64_t id, closure_address_table* table);
  bool check_address_type(uint64_t id, closure_address_table* table, const char* type);

  double llvm_tan(double x);
  double llvm_cosh(double x);
  double llvm_tanh(double x);
  double llvm_sinh(double x);
  double llvm_asin(double x);
  double llvm_atan(double x);
  double llvm_atan2(double x,double y);
  /* double llvm_ceil(double x); */
  /* double llvm_floor(double x); */
  /* double llvm_exp(double x); */
  /* double llvm_fmod(double x,double y); */
  /* double llvm_pow(double x,double y); */
  /* double llvm_log(double x); */
  /* double llvm_log2(double x); */
  /* double llvm_log10(double x); */
  /* double llvm_sqrt(double x); */
  /* double llvm_fabs(double x);   */

}

///////////////////////////////////////////////////
// this added for dogdy continuations support
/* ucontext_t* llvm_make_ucontext(); */
/* ucontext_t* llvm_scheme_process_ucontext(); */
///////////////////////////////////////////////////

namespace llvm {

class Module;
class GlobalVariable;
class GlobalValue;
class Function;
class StructType;
class ModuleProvider;
class SectionMemoryManager;
class ExecutionEngine;

namespace legacy
{

class PassManager;

}

} // end llvm namespace

namespace extemp
{

namespace EXTLLVM
{

const unsigned LLVM_ZONE_ALIGN = 32; // MUST BE POWER OF 2!
const unsigned LLVM_ZONE_ALIGNPAD = LLVM_ZONE_ALIGN - 1;

inline llvm_zone_t* llvm_zone_create(uint64_t size)
{
    auto zone(reinterpret_cast<llvm_zone_t*>(malloc(sizeof(llvm_zone_t))));
    if (unlikely(!zone)) {
        abort(); // in case a leak can be analyzed post-mortem
    }
#ifdef _WIN32
    zone->memory = malloc(size_t(size)); // TODO: what about alignment for Windows???
#else
    posix_memalign(&zone->memory, LLVM_ZONE_ALIGN, size_t(size));
#endif
    zone->mark = 0;
    zone->offset = 0;
    if (unlikely(!zone->memory)) {
        size = 0;
    }
    zone->size = size;
    zone->cleanup_hooks = nullptr;
    zone->memories = nullptr;
    return zone;
}

extern "C" void llvm_zone_destroy(llvm_zone_t* Zone);

inline llvm_zone_t* llvm_zone_reset(llvm_zone_t* Zone)
{
    Zone->offset = 0;
    return Zone;
}

extern "C" void* llvm_zone_malloc(llvm_zone_t* zone, uint64_t size);
extern "C" void* llvm_zone_malloc_from_current_zone(uint64_t size);

inline llvm_zone_stack* llvm_threads_get_zone_stack()
{
    return tls_llvm_zone_stack;
}

inline void llvm_threads_set_zone_stack(llvm_zone_stack* Stack)
{
    tls_llvm_zone_stack = Stack;
}

inline void llvm_push_zone_stack(llvm_zone_t* Zone)
{
    auto stack(reinterpret_cast<llvm_zone_stack*>(malloc(sizeof(llvm_zone_stack))));
    stack->head = Zone;
    stack->tail = llvm_threads_get_zone_stack();
    llvm_threads_set_zone_stack(stack);
    return;
}

inline llvm_zone_t* llvm_peek_zone_stack()
{
    llvm_zone_t* z = 0;
    llvm_zone_stack* stack = llvm_threads_get_zone_stack();
    if (unlikely(!stack)) {  // for the moment create a "DEFAULT" zone if stack is NULL
#if DEBUG_ZONE_STACK
        printf("TRYING TO PEEK AT A NULL ZONE STACK\n");
#endif
        llvm_zone_t* z = llvm_zone_create(1024 * 1024 * 1); // default root zone is 1M
        llvm_push_zone_stack(z);
        stack = llvm_threads_get_zone_stack();
#if DEBUG_ZONE_STACK
        printf("Creating new 1M default zone %p:%lld on ZStack:%p\n",z,z->size,stack);
#endif
        return z;
    }
    z = stack->head;
#if DEBUG_ZONE_STACK
    printf("%p: peeking at zone %p:%lld\n",stack,z,z->size);
#endif
    return z;
}

extern "C" llvm_zone_t* llvm_pop_zone_stack();

inline void llvm_threads_inc_zone_stacksize() {
    ++tls_llvm_zone_stacksize;
}

inline void llvm_threads_dec_zone_stacksize() {
    --tls_llvm_zone_stacksize;
}

inline uint64_t llvm_threads_get_zone_stacksize() {
    return tls_llvm_zone_stacksize;
}

uint64_t getSymbolAddress(const std::string&);
void addModule(llvm::Module* m);

extern llvm::ExecutionEngine* EE; // TODO: nobody should need this (?)
extern llvm::Module* M;
extern int64_t LLVM_COUNT;
extern bool OPTIMIZE_COMPILES;
extern bool VERIFY_COMPILES;
extern llvm::legacy::PassManager* PM;
extern llvm::legacy::PassManager* PM_NO;
extern std::vector<llvm::Module*> Ms;

void initLLVM();
const llvm::Function* getFunction(const char* name);
const llvm::GlobalVariable* getGlobalVariable(const char* name);
const llvm::GlobalValue* getGlobalValue(const char* name);
inline std::vector<llvm::Module*>& getModules() { return Ms; } // not going to protect these!!!
extern "C" const char* llvm_disassemble(const unsigned char*  Code, int Syntax);

}

}

#endif
