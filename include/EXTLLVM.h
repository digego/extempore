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
#include <map>
#include <string>
#include <memory>
 //#include <ucontext.h>

typedef struct _zone_hooks_t {
  uint64_t space; // here just so we don't get <i8*,i8*>
  void* hook; // xtlang closure of type [void]*
  struct _zone_hooks_t* hooks;
} zone_hooks_t;

typedef struct _llvm_zone_t {
  void* memory;
  uint64_t offset;
  uint64_t mark;
  uint64_t size;
  zone_hooks_t* cleanup_hooks;
  struct _llvm_zone_t* memories;
} llvm_zone_t;

typedef struct _llvm_callback_struct_ {
    void(*fptr)(void*);
    void* dat;
  } _llvm_callback_struct_;


/* extern double (&cosd)(double); */
/* extern double (&tand)(double); */
/* extern double (&sind)(double); */
/* extern double (&coshd)(double); */
/* extern double (&tanhd)(double); */
/* extern double (&sinhd)(double); */
/* extern double (&acosd)(double); */
/* extern double (&asind)(double); */
/* extern double (&atand)(double); */
/* extern double (&atan2d)(double,double); */
/* extern double (&ceild)(double); */
/* extern double (&floord)(double); */
/* extern double (&expd)(double); */
/* extern double (&fmodd)(double,double); */
/* extern double (&powd)(double,double); */
/* extern double (&logd)(double); */
/* extern double (&log2d)(double); */
/* extern double (&log10d)(double); */
/* extern double (&sqrtd)(double); */
/* extern double (&fabsd)(double); */


extern "C"
{
  char* llvm_disassemble(const unsigned char*,int syntax);  
  void* malloc16 (size_t s);
  void free16(void* p);
llvm_zone_t* llvm_threads_get_callback_zone();
const char*  llvm_scheme_ff_get_name(foreign_func ff);
void llvm_scheme_ff_set_name(foreign_func ff,const char* name);
void llvm_runtime_error(int error, void* arg);
llvm_zone_t* llvm_zone_create(uint64_t size);
llvm_zone_t* llvm_zone_reset(llvm_zone_t* zone);
bool llvm_zone_copy_ptr(void* ptr1, void* ptr2);
void llvm_zone_mark(llvm_zone_t* zone);
uint64_t llvm_zone_mark_size(llvm_zone_t* zone);
void llvm_zone_ptr_set_size(void* ptr, uint64_t size);
uint64_t llvm_zone_ptr_size(void* ptr);
void llvm_zone_destroy(llvm_zone_t* zone);
void llvm_zone_print(llvm_zone_t* zone);
void llvm_destroy_zone_after_delay(llvm_zone_t* zone, uint64_t delay);
void* llvm_zone_malloc(llvm_zone_t* zone, uint64_t size);
llvm_zone_t* llvm_pop_zone_stack();
llvm_zone_t* llvm_peek_zone_stack();
void llvm_push_zone_stack(llvm_zone_t*);
bool llvm_ptr_in_zone(llvm_zone_t*, void*);
bool llvm_ptr_in_current_zone(void*);

void llvm_schedule_callback(long long, void*);  
void* llvm_get_function_ptr(char* n);
pointer llvm_scheme_env_set(scheme* _sc, char* sym);
bool llvm_check_valid_dot_symbol(scheme* sc, char* symbol);
bool regex_split(char* str, char** a, char** b);

uint64_t string_hash(unsigned char* str);

  void* llvm_memset(void* ptr, int32_t c, int64_t n);
  int llvm_printf(char* format, ...);
  int llvm_fprintf(FILE* f, char* format, ...);
  int llvm_sprintf(char* str, char* format, ...);
  int llvm_sscanf(char* buffer, char* format, ...);
  int llvm_fscanf(FILE* f, char* format, ...);
  void llvm_send_udp(char* host, int port, void* message, int message_length);
  int32_t llvm_samplerate();
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
  int64_t llvm_now();

  void* thread_fork(void*(*start_routine)(void*),void* args);
  int thread_join(void* thread);
  int thread_kill(void* thread);
  int thread_equal(void* thread1, void* thread2);
  int thread_equal_self(void* thread1);
  void* thread_self();
  int64_t thread_sleep(int64_t secs, int64_t nanosecs);
  void* mutex_create();
  int mutex_destroy(void* mutex);
  int mutex_lock(void* mutex);
  int mutex_unlock(void* mutex);
  int mutex_trylock(void* mutex);

  struct closure_address_table* new_address_table();
  struct closure_address_table* add_address_table(llvm_zone_t* zone, char* name, uint32_t offset, char* type, int alloctype, struct closure_address_table* table);
  struct closure_address_table* get_address_table(const char* name, closure_address_table* table);
  uint32_t get_address_offset(uint64_t id, closure_address_table* table);
  char* get_address_type(uint64_t id, closure_address_table* table);
  bool check_address_exists(uint64_t id, closure_address_table* table);
  bool check_address_type(uint64_t id, closure_address_table* table, const char* type);

  //  double llvm_cos(double x);
  // double llvm_sin(double x);  
  double llvm_tan(double x);
  double llvm_cosh(double x);
  double llvm_tanh(double x);
  double llvm_sinh(double x);
  double llvm_acos(double x);
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
#ifdef EXT_MCJIT
  class SectionMemoryManager;
#endif
  class ExecutionEngine;  

  namespace legacy {
    class PassManager;
  }
} // end llvm namespace

namespace extemp {

    class EXTLLVM {
    public:
	EXTLLVM();
	~EXTLLVM();
	static EXTLLVM* I() { return &SINGLETON; }	
	
	void initLLVM();

  llvm::Module* activeModule();
  llvm::Function* getFunction(std::string);
  llvm::GlobalVariable* getGlobalVariable(std::string);
  llvm::StructType* getNamedType(std::string);
  llvm::GlobalValue* getGlobalValue(std::string);
#ifdef EXT_MCJIT  
  uint64_t getSymbolAddress(std::string);
#endif
  void addModule(llvm::Module* m) { Ms.push_back(m); }
  std::vector<llvm::Module*>& getModules() { return Ms; }
	
	static int64_t LLVM_COUNT;
	static bool OPTIMIZE_COMPILES;	
	static bool VERIFY_COMPILES;	


	llvm::Module* M;
	llvm::ModuleProvider* MP;
	llvm::ExecutionEngine* EE;
  llvm::legacy::PassManager* PM;
#ifdef EXT_MCJIT
  std::unique_ptr<llvm::SectionMemoryManager> MM;
#endif  

    private:
  std::vector<llvm::Module*> Ms;  
	static EXTLLVM SINGLETON;
    };

} // end extemp namespace

#endif
