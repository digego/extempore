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
 * ARE DISCLEXTD. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE 
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
 * POSSIBILITY OF SUCH DAMAGE.
 *
 */

#include "stdarg.h"
#include "EXTLLVM.h"
#include "UNIV.h"
#include "SchemeFFI.h"
#include "TaskScheduler.h"
#include "Scheme.h"
#include "pcre.h"
#include "OSC.h"
#include "math.h"

#ifdef TARGET_OS_WINDOWS
#include <malloc.h>
#else
#include <sys/types.h>
#endif

#ifdef TARGET_OS_LINUX
#include <sys/syscall.h>
#endif

#ifdef EXT_BOOST
#include <boost/asio.hpp>
#else
#include <sys/errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>         /* host to IP resolution       */
#include <sys/fcntl.h>
#include <arpa/inet.h>
#endif

#ifndef TARGET_OS_WINDOWS
#include <unistd.h>
#endif

//#include "llvm/Analysis/DebugInfo.h"
//#include "llvm/Analysis/Verifier.h"
#include "llvm/Assembly/Parser.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/CallingConv.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/DataLayout.h"

#include "llvm/Support/TargetSelect.h"


//#include "llvm/ModuleProvider.h"

// #include "llvm/ExecutionEngine/Interpreter.h"
// #include "llvm/ExecutionEngine/GenericValue.h"
#include "llvm/Target/TargetOptions.h"
// #include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/SourceMgr.h"
// #include "llvm/Analysis/Verifier.h"
// #include "llvm/Target/TargetData.h"
#include "llvm/LinkAllPasses.h"
#include "llvm/PassManager.h"

#include "llvm/ExecutionEngine/JIT.h"
#include "llvm/ExecutionEngine/MCJIT.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/ExecutionEngine/Interpreter.h"
#include "llvm/ExecutionEngine/GenericValue.h"

#include "SchemeProcess.h"

#define DEBUG_ZONE_STACK 0
#define DEBUG_ZONE_ALLOC 0
#define LEAKY_ZONES 1
#define EXTENSIBLE_ZONES 1

// llvm_scheme foreign function -> string name
// also is not thread safe!
std::map<foreign_func,std::string> LLVM_SCHEME_FF_MAP;

// this must be global. we should therefore
// make it thread safe but I'm not going to bother
// while still testing.
std::map<void*,uint64_t> LLVM_ZONE_ALLOC_MAP;
// same as above.
std::map<std::string,std::string> LLVM_STR_CONST_MAP;

extemp::EXTMutex alloc_mutex("alloc mutex");

//#ifdef TARGET_OS_WINDOWS
//double log2(double num) {
//	return log(num)/log(2.0);
//}
//#endif

// double (&cosd)(double) = cos;
// double (&tand)(double) = tan;
// double (&sind)(double) = sin;
// double (&coshd)(double) = cosh;
// double (&tanhd)(double) = tanh;
// double (&sinhd)(double) = sinh; 
// double (&acosd)(double) = acos;
// double (&asind)(double) = asin; 
// double (&atand)(double) = atan;
// double (&atan2d)(double,double) = atan2;
// double (&ceild)(double) = ceil;
// double (&floord)(double) = floor;
// double (&expd)(double) = exp;
// double (&fmodd)(double,double) = fmod;
// double (&powd)(double,double) = pow;
// double (&logd)(double) = log;
// double (&log2d)(double) = log2;
// double (&log10d)(double) = log10;
// double (&sqrtd)(double) = sqrt;
// double (&fabsd)(double) = fabs;

void* malloc16 (size_t s) {
  unsigned char *p;
  unsigned char *porig = (unsigned char*) malloc (s + 0x10);   // allocate extra
  if (porig == NULL) return NULL;                              // catch out of memory
  p = (unsigned char*) (((uintptr_t) porig + 16) & (~0x0f));   // insert padding
  *(p-1) = p - porig;                                          // store padding size
  return p;
}

void free16(void *p) {
  unsigned char *porig = (unsigned char*) p;  // work out original
  porig = porig - *(porig-1);                 // by subtracting padding
  free (porig);                               // then free that
}

const char* llvm_scheme_ff_get_name(foreign_func ff) 
{
   return (LLVM_SCHEME_FF_MAP[ff]).c_str();
}

void llvm_scheme_ff_set_name(foreign_func ff,const char* name) 
{
  LLVM_SCHEME_FF_MAP[ff] = std::string(name);
  return;
}

// LLVM RUNTIME ERROR
void llvm_runtime_error(int error,void* arg)
{
  ascii_text_color(0,2,10);
  switch(error){
  case 1:
    printf("LLVM zptr_copy - invalid zptr! %p\n",arg);
    break;    
  default:
    break;
  }
  ascii_text_color(0,7,10);
  return;
}

//////////////////////////////////////////////////////////////////
// this whole zone section should 
// all be thread safe of course
// but currently isn't!
// FIX ME!!
typedef struct llvm_zone_stack
{
    llvm_zone_t* head;
    llvm_zone_stack* tail;
} llvm_zone_stack;


#ifdef EXT_BOOST
std::map<boost::thread::id,llvm_zone_stack*> LLVM_ZONE_STACKS;
std::map<boost::thread::id,uint64_t> LLVM_ZONE_STACKSIZES;
std::map<boost::thread::id,llvm_zone_t*> LLVM_CALLBACK_ZONES;
#else
std::map<long,llvm_zone_stack*> LLVM_ZONE_STACKS;
std::map<long,uint64_t> LLVM_ZONE_STACKSIZES;
std::map<uint64_t,llvm_zone_t*> LLVM_CALLBACK_ZONES;
#endif

int LLVM_ZONE_ALIGN = 16;
int LLVM_ZONE_ALIGNPAD = LLVM_ZONE_ALIGN-1;

// this is going to cause concurrency problems at some stage.
// you really need to FIX IT!
llvm_zone_t* llvm_threads_get_callback_zone()
{
  llvm_zone_t* zone = 0;
#ifdef EXT_BOOST
  zone = LLVM_CALLBACK_ZONES[boost::this_thread::get_id()];
  if(!zone) {
    zone = llvm_zone_create(1024*1024*1); // default callback zone 1M
    LLVM_CALLBACK_ZONES[boost::this_thread::get_id()] = zone;
  }
#elif TARGET_OS_MAC
  mach_port_t tid = pthread_mach_thread_np(pthread_self());  
  zone = LLVM_CALLBACK_ZONES[(long)tid];
  if(!zone) {
    zone = llvm_zone_create(1024*1024*1); // default callback zone 1M
    LLVM_CALLBACK_ZONES[(long)tid] = zone;
  }  
#else
  pid_t tid = (pid_t) syscall (SYS_gettid);
  zone = LLVM_CALLBACK_ZONES[(long)tid];
  if(!zone) {
    zone = llvm_zone_create(1024*1024*1); // default callback zone 1M
    LLVM_CALLBACK_ZONES[(long)tid] = zone;
  }
#endif
  return zone;
}

// this is going to cause concurrency problems at some stage.
// you really need to FIX IT!
llvm_zone_stack* llvm_threads_get_zone_stack()
{
  llvm_zone_stack* stack = 0;
#ifdef EXT_BOOST
  stack = LLVM_ZONE_STACKS[boost::this_thread::get_id()];
#elif TARGET_OS_MAC
  mach_port_t tid = pthread_mach_thread_np(pthread_self());
  stack = LLVM_ZONE_STACKS[(long)tid];
#else
  pid_t tid = (pid_t) syscall (SYS_gettid);
  stack = LLVM_ZONE_STACKS[(long)tid];
#endif
  return stack;
}


// this is going to cause concurrency problems at some stage.
// you really need to FIX IT!
void llvm_threads_set_zone_stack(llvm_zone_stack* llvm_zone_stack)
{
#ifdef EXT_BOOST
  LLVM_ZONE_STACKS[boost::this_thread::get_id()] = llvm_zone_stack;
#elif TARGET_OS_MAC
  mach_port_t tid = pthread_mach_thread_np(pthread_self());
  LLVM_ZONE_STACKS[(long)tid] = llvm_zone_stack;
#else
  pid_t tid = (pid_t) syscall (SYS_gettid);
  LLVM_ZONE_STACKS[(long)tid] = llvm_zone_stack;
#endif
  return;
}

void llvm_threads_inc_zone_stacksize()
{
#ifdef EXT_BOOST
  LLVM_ZONE_STACKSIZES[boost::this_thread::get_id()] += 1;
#elif TARGET_OS_MAC
  mach_port_t tid = pthread_mach_thread_np(pthread_self());
  LLVM_ZONE_STACKSIZES[(long)tid] += 1;
#else
  pid_t tid = (pid_t) syscall (SYS_gettid);
  LLVM_ZONE_STACKS[(long)tid] += 1;
#endif
  return;
}

void llvm_threads_dec_zone_stacksize()
{
#ifdef EXT_BOOST
  LLVM_ZONE_STACKSIZES[boost::this_thread::get_id()] -= 1;
#elif TARGET_OS_MAC
  mach_port_t tid = pthread_mach_thread_np(pthread_self());
  LLVM_ZONE_STACKSIZES[(long)tid] -= 1;
#else
  pid_t tid = (pid_t) syscall (SYS_gettid);
  LLVM_ZONE_STACKS[(long)tid] -= 1;
#endif
  return;
}

uint64_t llvm_threads_get_zone_stacksize()
{
  uint64_t size = 0;
#ifdef EXT_BOOST
  size = LLVM_ZONE_STACKSIZES[boost::this_thread::get_id()];
#elif TARGET_OS_MAC
  mach_port_t tid = pthread_mach_thread_np(pthread_self());
  size = LLVM_ZONE_STACKSIZES[(long)tid];
#else
  pid_t tid = (pid_t) syscall (SYS_gettid);
  size = LLVM_ZONE_STACKSIZES[(long)tid];
#endif
  return size;
}

void llvm_push_zone_stack(llvm_zone_t* z)
{
    llvm_zone_stack* stack = (llvm_zone_stack*) malloc(sizeof(llvm_zone_stack));
    stack->head = z;
    stack->tail = llvm_threads_get_zone_stack();
    llvm_threads_set_zone_stack(stack);

#if DEBUG_ZONE_STACK          
    llvm_threads_inc_zone_stacksize();
    if(stack->tail) {
      printf("%p: push new zone %p:%lld onto old zone %p:%lld stacksize:%lld\n",stack,z,z->size,stack->tail->head,stack->tail->head->size,llvm_threads_get_zone_stacksize());
    } else {
      printf("%p: push new zone %p:%lld onto empty stack\n",stack,z,z->size);
    }
#endif
    //printf("zones: %lld\n",llvm_threads_get_zone_stacksize());
    return;
}

llvm_zone_t* llvm_peek_zone_stack()
{
    llvm_zone_t* z = 0;
    llvm_zone_stack* stack = llvm_threads_get_zone_stack();
    if(!stack) {  // for the moment create a "DEFAULT" zone if stack is NULL      
#if DEBUG_ZONE_STACK      
      printf("TRYING TO PEEK AT A NULL ZONE STACK\n"); 
#endif
      llvm_zone_t* z = llvm_zone_create(1024*1024*1); // default root zone is 1M
      llvm_push_zone_stack(z);
      stack = llvm_threads_get_zone_stack();
#if DEBUG_ZONE_STACK      
      printf("Creating new 1M default zone %p:%lld on ZStack:%p\n",z,z->size,stack);
#endif      
      return z;
    }else{
      z = stack->head;
#if DEBUG_ZONE_STACK      
      printf("%p: peeking at zone %p:%lld\n",stack,z,z->size);
#endif
      return z;
    }
}

llvm_zone_t* llvm_pop_zone_stack()
{
    llvm_zone_stack* stack = llvm_threads_get_zone_stack();
    if(!stack) {
#if DEBUG_ZONE_STACK      
      printf("TRYING TO POP A ZONE FROM AN EMPTY ZONE STACK\n");
#endif
      return 0;
    }
    llvm_zone_t* head = stack->head;
    llvm_zone_stack* tail = stack->tail;
#if DEBUG_ZONE_STACK    
    llvm_threads_dec_zone_stacksize();
    if(tail == NULL) {
      printf("%p: popping zone %p:%lld from stack with no tail\n",stack,head,head->size);
    }else{
      printf("%p: popping new zone %p:%lld back to old zone %p:%lld\n",stack,head,head->size,tail->head,tail->head->size);
    }
#endif
    free(stack);
    llvm_threads_set_zone_stack(tail);
    return head;
}

llvm_zone_t* llvm_zone_create(uint64_t size)
{
  llvm_zone_t* zone = (llvm_zone_t*) malloc(sizeof(llvm_zone_t));
  if (zone == NULL)
    {
      ascii_text_color(0,3,10);      
      printf("Catastrophic memory failure!\n");
      ascii_text_color(0,9,10);
      exit(1);
    }
#ifdef TARGET_OS_WINDOWS
    zone->memory = malloc((size_t) size);
#else
    // zone->memory = malloc((size_t) size);
    posix_memalign(&zone->memory,LLVM_ZONE_ALIGN,(size_t)size);
#endif
    zone->mark = 0;
    zone->offset = 0;
    if(zone->memory == NULL) {
      ascii_text_color(0,3,10);      
      printf("Failed to allocate memory for Zone!\n");
      ascii_text_color(0,9,10);    
      size = 0;
    }
    zone->size = size;
    zone->memories = NULL;
#if DEBUG_ZONE_ALLOC    
    printf("CreateZone: %x:%x:%lld:%lld\n",zone,zone->memory,zone->offset,zone->size);
#endif
    return zone;
}

llvm_zone_t* llvm_zone_reset(llvm_zone_t* zone)
{
    zone->offset = 0;
    return zone;
}

void llvm_zone_destroy(llvm_zone_t* zone)
{
#if DEBUG_ZONE_ALLOC  
    printf("DestroyZone: %p:%p:%lld:%lld\n",zone,zone->memory,zone->offset,zone->size);
#endif
    if(zone->memories != NULL) llvm_zone_destroy(zone->memories);
    free(zone->memory);    
    free(zone);
    return;
}

void llvm_zone_print(llvm_zone_t* zone)
{
  printf("Zone(%p) Mem(%p) Offset(%lld) Size(%lld)\n",zone,zone->memory,zone->offset,zone->size);
  return;
}

// void* llvm_zone_malloc(llvm_zone_t* zone, uint64_t size)
// {
//     alloc_mutex.lock();
// #if DEBUG_ZONE_ALLOC
//     printf("MallocZone: %p:%p:%lld:%lld:%lld\n",zone,zone->memory,zone->offset,zone->size,size);
// #endif
//     if(zone->offset+size >= zone->size)
//     {
// 	// if LEAKY ZONE is TRUE then just print a warning and just leak the memory
// #if LEAKY_ZONES
// 	printf("\nZone:%p size:%lld is full ... leaking %lld bytes\n",zone,zone->size,size);
//         printf("Leaving a leaky zone can be dangerous ... particularly for concurrency\n");
//         fflush(NULL);
// 	return malloc((size_t)size);
// #else
// 	printf("\nZone:%p size:%lld is full ... exiting!\n",zone,zone->size,size);
//         fflush(NULL);
// 	exit(1);
// #endif
//     }
//     void* newptr = (void*)(((char*)zone->memory)+zone->offset);
//     memset(newptr,0,size); // clear memory
//     zone->offset += size; 
//     // add ptr size to alloc map
//     LLVM_ZONE_ALLOC_MAP[newptr] = size;
//     alloc_mutex.unlock();
//     //extemp::SchemeProcess::I(pthread_self())->llvm_zone_ptr_set_size(newptr, size);
//     return newptr;
// }

void* llvm_zone_malloc(llvm_zone_t* zone, uint64_t size)
{
    alloc_mutex.lock();
#if DEBUG_ZONE_ALLOC
    printf("MallocZone: %p:%p:%lld:%lld:%lld\n",zone,zone->memory,zone->offset,zone->size,size);
#endif
    if(zone->offset+size >= zone->size)
    {

#if EXTENSIBLE_ZONES // if extensible_zones is true then extend zone size by zone->size
    if(size > zone->size) zone->size = size;
    zone->size = zone->size * 2; // keep doubling zone size for each new allocation  
    // ascii_text_color(0,3,10);      
    // printf("Warning ");
    // ascii_text_color(0,9,10);
    // printf("zone:%p size:%lld is full ... extending\n",zone,zone->size);
    llvm_zone_t* newzone = llvm_zone_create(zone->size);
    void* tmp = newzone->memory;
    newzone->memories = zone->memories;
    newzone->memory = zone->memory;
    zone->memory = tmp;
    zone->memories = newzone;
    llvm_zone_reset(zone);
#elif LEAKY_ZONES       // if LEAKY ZONE is TRUE then just print a warning and just leak the memory
        printf("\nZone:%p size:%lld is full ... leaking %lld bytes\n",zone,zone->size,size);
      printf("Leaving a leaky zone can be dangerous ... particularly for concurrency\n");
      fflush(NULL);
	return malloc((size_t)size);
#else
	printf("\nZone:%p size:%lld is full ... exiting!\n",zone,zone->size,size);
        fflush(NULL);
	exit(1);
#endif
    }
    uint64_t sa = size+LLVM_ZONE_ALIGNPAD;
    size = sa - sa%LLVM_ZONE_ALIGN; // adjust size to an alignment boundary
    void* newptr = (void*)(((char*)zone->memory)+zone->offset);
    memset(newptr,0,size); // clear memory
    zone->offset += size; 
    // add ptr size to alloc map
    LLVM_ZONE_ALLOC_MAP[newptr] = size;
    alloc_mutex.unlock();
    //extemp::SchemeProcess::I(pthread_self())->llvm_zone_ptr_set_size(newptr, size);
    return newptr;
}

void llvm_zone_mark(llvm_zone_t* zone)
{
    zone->mark = zone->offset;
}

uint64_t llvm_zone_mark_size(llvm_zone_t* zone)
{
    return zone->offset - zone->mark;
}

void llvm_zone_ptr_set_size(void* ptr, uint64_t size)
{
    // not sure if I definitely need this here
    // probably do though so better safe than sorry
    alloc_mutex.lock();
    LLVM_ZONE_ALLOC_MAP[ptr] = size;
    alloc_mutex.unlock();
    //printf("set ptr: %p  to size: %lld\n",ptr,size);
    return;
}

uint64_t llvm_zone_ptr_size(void* ptr)
{
    // return ptr size from alloc map
    return LLVM_ZONE_ALLOC_MAP[ptr];
    //return extemp::SchemeProcess::I(pthread_self())->llvm_zone_ptr_get_size(ptr);
}

bool llvm_zone_copy_ptr(void* ptr1, void* ptr2)
{
    uint64_t size1 = llvm_zone_ptr_size(ptr1);
    uint64_t size2 = llvm_zone_ptr_size(ptr2);

    if(size1 != size2) { 
  //printf("Bad LLVM ptr copy - size mismatch setting %p:%lld -> %p:%lld\n",ptr1,size1,ptr2,size2); 
      return 1;
    }
    if(size1 == 0) {
  //printf("Bad LLVM ptr copy - size mismatch setting %p:%lld -> %p:%lld\n",ptr1,size1,ptr2,size2); 
      return 1;
    }

    //printf("zone_copy_ptr: %p,%p,%lld,%lld\n",ptr2,ptr1,size1,size2);
    memcpy(ptr2, ptr1, size1);
    return 0;		
}

bool llvm_ptr_in_zone(llvm_zone_t* zone, void* ptr)
{
    if( (ptr >= zone->memory) && (ptr < ((char*)zone->memory)+zone->size) ) return true;
    while(zone->memories != NULL) {
      zone = zone->memories;
      if( (ptr >= zone->memory) && (ptr < ((char*)zone->memory)+zone->size) ) return true;
    }
    return false;
}

bool llvm_ptr_in_current_zone(void* ptr)
{
  return llvm_ptr_in_zone(llvm_peek_zone_stack(),ptr);
}


extemp::CM* FreeWithDelayCM = mk_cb(extemp::SchemeFFI::I(),extemp::SchemeFFI,freeWithDelay);
void free_after_delay(char* dat, double delay)
{
    //printf("freeWithDelay %p\n",zone);
    extemp::CM* cb = FreeWithDelayCM; 
    extemp::Task<char*>* task = new extemp::Task<char*>(extemp::UNIV::TIME+delay,44100,cb,dat);
    extemp::TaskScheduler::I()->add(task);
}

extemp::CM* DestroyMallocZoneWithDelayCM = mk_cb(extemp::SchemeFFI::I(),extemp::SchemeFFI,destroyMallocZoneWithDelay);
void llvm_destroy_zone_after_delay(llvm_zone_t* zone, uint64_t delay)
{
    //printf("destroyWithDelay %p\n",zone);
    extemp::CM* cb = DestroyMallocZoneWithDelayCM;
    extemp::Task<llvm_zone_t*>* task = new extemp::Task<llvm_zone_t*>(extemp::UNIV::TIME+delay,44100,cb,zone);
    extemp::TaskScheduler::I()->add(task);
}

void llvm_schedule_callback(long long time, void* dat)
{
  //printf("scheduled callback %lld\n",time);
  extemp::SchemeProcess* proc = extemp::SchemeProcess::I(); //extemp::SchemeProcess::I()->extemporeCallback(time,dat);

  uint64_t current_time = time; //task->getStartTime();
  uint64_t duration = 1000000000; //task->getDuration();
  extemp::TaskScheduler::I()->addTask(current_time, duration, proc->extempore_lang_cb, dat, 0, true);
  return;
}

void* llvm_get_function_ptr(char* fname)
{
  //using namespace llvm;
  // llvm::Module* M = extemp::EXTLLVM::I()->M;
  // llvm::Function* func = M->getFunction(std::string(fname));
  llvm::Function* func = extemp::EXTLLVM::I()->getFunction(std::string(fname));
  if(func == 0) {
    // throw std::runtime_error("Extempore runtime error: error retrieving function in llvm_get_function_ptr");
    return NULL;
  }

  void* p = extemp::EXTLLVM::I()->EE->getPointerToFunction(func);

  // if(p==NULL) {
  //     throw std::runtime_error("Extempore runtime error: null ptr retrieving function ptr in llvm_get_function_ptr");
  // }
  return p;
}

char* extitoa(int64_t val) {
	/*
  int base = 10;
  static char buf[32] = {0};        
  int i = 30;        
  for(; val && i ; --i, val /= base)        
    buf[i] = "0123456789abcdef"[val % base]; 
	*/
  static char buf[32] = {0};
  sprintf(buf,"%lld",val);
  return buf;//&buf[i+1];        
}

unsigned long string_hash(unsigned char* str) 
{
  unsigned long hash = 0;
  int c;
  
  while (c = *str++)
    hash = c + (hash << 6) + (hash << 16) - hash;
  
  return hash;
}

int llvm_printf(char* format, ...)
{
    va_list ap;
    va_start(ap,format);
#ifdef TARGET_OS_WINDOWS
    char* ret = (char*) _alloca(2048);
#else
    char* ret = (char*) alloca(2048);
#endif
    int returnval = vsnprintf(ret, 2048, format, ap);
    printf("%s",ret);
    fflush(stdout);	
    va_end(ap);
    return returnval;
}

int llvm_fprintf(FILE* stream, char* format, ...)
{
    va_list ap;
    va_start(ap,format);
    int returnval = vfprintf(stream, format, ap);
    va_end(ap);
    return returnval;
}

int llvm_sprintf(char* str, char* format, ...)
{
    va_list ap;
    va_start(ap,format);
    int returnval = vsprintf(str, format, ap);
    //printf("in the wing: %s\n",str);
    va_end(ap);
    return returnval;
}

int llvm_sscanf(char* buffer, char* format, ...)
{
    va_list ap;
    va_start(ap,format);
#ifdef TARGET_OS_WINDOWS
    char* ret = (char*) _alloca(2048);
#else
    char* ret = (char*) alloca(2048);
#endif
    int returnval = sscanf(buffer, format, ap);
    printf("%s",ret);
    fflush(stdout);	
    va_end(ap);
    return returnval;
}

int llvm_fscanf(FILE* stream, char* format, ...)
{
    va_list ap;
    va_start(ap,format);
    int returnval = vfscanf(stream, format, ap);
    va_end(ap);
    return returnval;
}

void llvm_send_udp(char* host, int port, void* message, int message_length)
{
  int length = message_length;
  int ret = 0;
  char* ptr;

#ifdef EXT_BOOST
  boost::asio::io_service io_service;
  boost::asio::ip::udp::resolver::iterator end;
  boost::asio::ip::udp::resolver resolver(io_service);
  std::stringstream ss;
  ss << port;
  boost::asio::ip::udp::resolver::query newQuery(boost::asio::ip::udp::v4(),host, ss.str());
  boost::asio::ip::udp::resolver::iterator iter = resolver.resolve(newQuery);

  boost::asio::ip::udp::endpoint sa = *iter;
#else
  struct sockaddr_in sa;
  struct hostent* hen; /* host-to-IP translation */

  /* Address resolution stage */
  hen = gethostbyname(host);
  if (!hen) {
    printf("OSC Error: Could no resolve host name\n");
    return;			
  }

  memset(&sa, 0, sizeof(sa));

  sa.sin_family = AF_INET;
  sa.sin_port = htons(port);
  memcpy(&sa.sin_addr.s_addr, hen->h_addr_list[0], hen->h_length);
#endif		


#ifdef EXT_BOOST
  boost::asio::ip::udp::socket* fd = 0;
#else
  int fd = 0;
#endif

#ifdef EXT_BOOST
  int err = 0;
  boost::asio::io_service service;
  boost::asio::ip::udp::socket socket(service);
  socket.open(boost::asio::ip::udp::v4());
  socket.send_to(boost::asio::buffer(message, length), sa);
#else
  fd = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);			
  int err = sendto(fd, message, length, 0, (struct sockaddr*)&sa, sizeof(sa));
  close(fd);
#endif
  if(err < 0)
    {
      if(err == EMSGSIZE) {
	printf("Error: OSC message too large: UDP 8k message MAX\n");
      }else{
	printf("Error: Problem sending OSC message %d\n",err);
      }			

    }

  return;
}


long long llvm_get_next_prime(long long start)
{
    long long  how_many = start+100000;
    long long  *array = (long long*) calloc(how_many, sizeof(long long));
    long long  i, prime, multiple;
    /*  mark each int as potentially prime */
    for (i=0; i<how_many; i++)
	array[i] = 1;
    /* special cases: 0, 1 not considered prime */
    array[0] = array[1] = 0;
    /* foreach starting prime, mark every multiple as non-prime */
    prime = 0;
    while (1) {
	/* skip non-primes to find first prime */
	for (; (prime < how_many) && (!array[prime]); ++prime)
	    continue;
	if (prime >= how_many)
	    break;
	for (multiple=2*prime; multiple<how_many; multiple+=prime) {
	    array[multiple] = 0;
	}
	++prime;
    }
    /* Now that we have marked all multiple of primes as non-prime, */
    /* print the remaining numbers that fell through the sieve, and */
    /* are thus prime */
    for (i=start+1; i<how_many; i++) {
	if(array[i]) return i;
    }
    return -1;
}

/////////////////////////////////////////////
//
// some native threading support xtlang
//
/////////////////////////////////////////////

void* thread_fork(void*(*start_routine)(void*),void* args) {
#ifdef TARGET_OS_WINDOWS
  return NULL;
#else
  pthread_t* t = (pthread_t*) malloc(sizeof(pthread_t));
  int res = pthread_create(t,NULL,start_routine,args);
  if(res == 0) return t;
  else return NULL;
#endif
}

int thread_join(void* thread) {
#ifdef TARGET_OS_WINDOWS
  return NULL;
#else
  pthread_t* t = (pthread_t*) thread;
  return pthread_join(*t,NULL);
#endif
}

int thread_kill(void* thread) {
#ifdef TARGET_OS_WINDOWS
  return NULL;
#else
  pthread_t* t = (pthread_t*) thread;
  return pthread_cancel(*t);
#endif
}

void* thread_self() {
#ifdef TARGET_OS_WINDOWS
  return NULL;
#else
  pthread_t t = pthread_self();
  return &t;
#endif  
}

// return value is number of nanosecs sleep missed by
int64_t thread_sleep(int64_t secs, int64_t nanosecs) {
#ifdef TARGET_OS_WINDOWS
  return NULL;
#else
  struct timespec a, b;
  a.tv_sec = secs;
  a.tv_nsec = nanosecs;
  nanosleep(&a,&b);
  return b.tv_nsec;
#endif  
}

void* mutex_create() {
#ifdef TARGET_OS_WINDOWS
  return NULL;
#else
  pthread_mutex_t* mutex = (pthread_mutex_t*) malloc(sizeof(pthread_mutex_t));
  int res = pthread_mutex_init(mutex,NULL);
  if(res == 0) return mutex;
  else return NULL;
#endif
}

int mutex_destroy(void* mutex) {
#ifdef TARGET_OS_WINDOWS
  return NULL;
#else
  pthread_mutex_t* m = (pthread_mutex_t*) mutex;
  return pthread_mutex_destroy(m);
#endif
}

int mutex_lock(void* mutex) {
#ifdef TARGET_OS_WINDOWS
  return -1;
#else
  pthread_mutex_t* m = (pthread_mutex_t*) mutex;
  return pthread_mutex_lock(m);
#endif
}

int mutex_unlock(void* mutex) {
#ifdef TARGET_OS_WINDOWS
  return -1;
#else
  pthread_mutex_t* m = (pthread_mutex_t*) mutex;
  return pthread_mutex_unlock(m);
#endif
}

int mutex_trylock(void* mutex) {
#ifdef TARGET_OS_WINDOWS
  return -1;
#else
  pthread_mutex_t* m = (pthread_mutex_t*) mutex;
  return pthread_mutex_trylock(m);
#endif
}



/////////////////////////////////////////////////
// This added for dodgy continuations support
// ucontext_t* llvm_make_ucontext()
// {
//   ucontext_t* ctx = (ucontext_t*) malloc(sizeof(ucontext_t));
//   ctx->uc_stack.ss_sp   = (void*) malloc(1024*1024); //iterator_stack;
//   ctx->uc_stack.ss_size = 1024*1024;
//   return ctx;
// }

// ucontext_t* llvm_scheme_process_ucontext()
// {
//   extemp::SchemeProcess* proc = extemp::SchemeProcess::I(); //extemp::SchemeProcess::I()->extemporeCallback(time,dat);
//   ucontext_t* ctx = proc->getContext();
//   return ctx;
// }
///////////////////////////////////////////////////

void* llvm_memset(void* ptr, int32_t c, int64_t n)
{
    return memset(ptr, c, (size_t)n);
}


// these are helpers for runtime debugging in llvm
void llvm_print_pointer(void* ptr)
{
    printf("llvm:ptr:>%p -- %lld\n",ptr,*((int64_t*)ptr));
    return;
}

void llvm_print_i32(int32_t num)
{
    printf("llvm:i32:>%d\n",num);
    return;
}

void llvm_print_i64(int64_t num)
{
    printf("llvm:i64:>%lld\n",num);
    return;
}

void llvm_print_f32(float num)
{
    printf("llvm:f32:>%f\n",num);
    return;
}

void llvm_print_f64(double num)
{
    printf("llvm:f64:>%f\n",num);
    return;
}

int64_t llvm_now()
{
    return extemp::UNIV::TIME;
}

// double llvm_cos(double x) { return cos(x); }
// double llvm_sin(double x) { return sin(x); }
double llvm_tan(double x) { return tan(x); }
double llvm_cosh(double x) { return cosh(x); }
double llvm_tanh(double x) { return tanh(x); }
double llvm_sinh(double x) { return sinh(x); }
double llvm_acos(double x) { return acos(x); }
double llvm_asin(double x) { return asin(x); }
double llvm_atan(double x) { return atan(x); }
double llvm_atan2(double x,double y) { return atan2(x,y); }
// double llvm_ceil(double x) { return ceil(x); }
// double llvm_floor(double x) { return floor(x); }
// double llvm_exp(double x) { return exp(x); }
// double llvm_fmod(double x,double y) { return fmod(x,y); }
// double llvm_pow(double x,double y) { return pow(x,y); }
// double llvm_log(double x) { return log(x); }
// double llvm_log2(double x) { return log2(x); }
// double llvm_log10(double x) { return log10(x); }
// double llvm_sqrt(double x) { return sqrt(x); }
// double llvm_fabs(double x) { return fabs(x); }


// double llvm_samplerate()
// {
//     return (double) extemp::UNIV::SAMPLERATE;
// }

// double llvm_frames()
// {
//   return (double) extemp::UNIV::FRAMES;
// }

// double llvm_channels_in()
// {
//     return (double) extemp::UNIV::CHANNELS;
// }

// double llvm_channels_out()
// {
//     return (double) extemp::UNIV::IN_CHANNELS;
// }

double imp_randd()
{
#ifdef EXT_BOOST
  return extemp::UNIV::random();
#else
    return (double)rand()/(double)RAND_MAX;
#endif
}

float imp_randf()
{
#ifdef EXT_BOOST
  return extemp::UNIV::random();
#else
    return (float)rand()/(float)RAND_MAX;
#endif
}

int64_t imp_rand1_i64(int64_t a)
{
#ifdef EXT_BOOST
  return (int64_t) extemp::UNIV::random()*a;
#else
  return (int64_t) (((double)rand()/(double)RAND_MAX)*(double)a);
#endif
}

int64_t imp_rand2_i64(int64_t a, int64_t b)
{
#ifdef EXT_BOOST
  return (int64_t) a+(extemp::UNIV::random()*((double)b-(double)a));
#else
  return (int64_t) a+(((double)rand()/(double)RAND_MAX)*((double)b-(double)a));
#endif
}

int32_t imp_rand1_i32(int32_t a)
{
#ifdef EXT_BOOST
  return (int32_t) extemp::UNIV::random()*(double)a;
#else
  return (int32_t) (((double)rand()/(double)RAND_MAX)*(double)a);
#endif
}

int32_t imp_rand2_i32(int32_t a, int32_t b)
{
#ifdef EXT_BOOST
  return (int32_t) a+(extemp::UNIV::random()*((double)b-(double)a));
#else
  return (int32_t) a+(((double)rand()/(double)RAND_MAX)*((double)b-(double)a));
#endif
}

double imp_rand1_d(double a)
{
#ifdef EXT_BOOST
  return extemp::UNIV::random()*a;
#else
  return ((double)rand()/(double)RAND_MAX)*a;
#endif
}

double imp_rand2_d(double a, double b)
{
#ifdef EXT_BOOST
  return a+(extemp::UNIV::random()*(b-a));
#else
  return a+(((double)rand()/(double)RAND_MAX)*(b-a));
#endif
}

float imp_rand1_f(float a)
{
#ifdef EXT_BOOST
  return extemp::UNIV::random()*a;
#else
  return ((double)rand()/(double)RAND_MAX)*a;
#endif
}

float imp_rand2_f(float a, float b)
{
#ifdef EXT_BOOST
  return a+((float)extemp::UNIV::random()*(b-a));
#else
  return a+(((float)rand()/(float)RAND_MAX)*(b-a));
#endif
}


///////////////////////////////////

///////////////////////////////////////////////////////////////////////
// This here for Extempore Compiler Runtime.
// This is temporary and needs to replaced with something sensible!
struct closure_address_table
{
    char* name;
    uint32_t offset;
    char* type;		
    struct closure_address_table* next;
};

struct closure_address_table* get_address_table(const char* name, closure_address_table* table)
{
    while(table)
    {
	if(strcmp(table->name,name)) return table;
	table = table->next;
    }
    printf("Unable to locate %s in closure environment a\n",name);
    return 0;
}

uint32_t get_address_offset(const char* name, closure_address_table* table)
{
    while(table)
    {
        //printf("tablename: %s\n",table->name);
	if(strcmp(table->name,name) == 0) {
	    //printf("in %s returning offset %d from %s\n",table->name,table->offset,name);
	    return table->offset;	
	}
	table = table->next;
    }
    printf("Unable to locate %s in closure environment b\n",name);
    return 0;
}

char* get_address_type(const char* name, closure_address_table* table)
{
    while(table)
    {
      if(strcmp(table->name,name) == 0) {
	return table->type;
      }
      table = table->next;
    }
    printf("Unable to locate %s in closure environment c\n",name);
    return 0;  
}

bool check_address_exists(const char* name, closure_address_table* table)
{
    while(table)
    {
      if(strcmp(table->name,name) == 0) {
	return true;
      }
      table = table->next;
    }
    return false;  
}

bool check_address_type(const char* name, closure_address_table* table, const char* type)
{
  while(table)
    {
      if(strcmp(table->name,name) == 0) {
        if(strcmp(table->type,type)!=0) {
          printf("Runtime Type Error: bad type %s for %s. Should be %s\n",type,name,table->type);
          return 0;
        }else{
          return 1;
        }	  
      }
      table = table->next;
    }
  printf("Unable to locate %s in closure environment type: %s d\n",name,type);
  return 0;
}

struct closure_address_table* new_address_table()
{
    return 0; // NULL for empty table
}
 
struct closure_address_table* add_address_table(llvm_zone_t* zone, char* name, uint32_t offset, char* type, int alloctype, struct closure_address_table* table)
{
  struct closure_address_table* t = NULL;
  if(alloctype == 1) {
    t = (struct closure_address_table*) malloc(sizeof(struct closure_address_table));
  /* }  else if(alloctype == 2) {
#ifdef TARGET_OS_WINDOWS
    t = (struct closure_address_table*) _alloca(sizeof(struct closure_address_table));
#else
    t = (struct closure_address_table*) alloca(sizeof(struct closure_address_table));
#endif */
  } else {
    t = (struct closure_address_table*) llvm_zone_malloc(zone,sizeof(struct closure_address_table));
  }

  t->name = name;
  t->offset = offset;
  t->type = type;
  t->next = table;
  return t;
}

// bool rsplit(char* regex, char* str, char* a, char* b)
// {
//   char* data = str;
//   int length = strlen(data);
//   char* pattern = regex;		
//   pcre *re; 
//   const char *error; 
//   int erroffset; 
//   //printf("dat: data\n");
//   // should probably move this regex compile to global
//   re = pcre_compile(	pattern, /* the pattern */ 
// 			0, /* default options */ 
// 			&error, /* for error message */ 
// 			&erroffset, /* for error offset */ 
// 			NULL); /* use default character tables */		
//   int rc; 
//   int ovector[60];					
//   rc = pcre_exec(	re, /* result of pcre_compile() */ 
// 			NULL, /* we didn’t study the pattern */ 
// 			data, /* the subject string */ 
// 			strlen(data), /* the length of the subject string */ 
// 			0, /* start at offset 0 in the subject */ 
// 			0, /* default options */ 
// 			ovector, /* vector of integers for substring information */ 
// 			60); /* number of elements (NOT size in bytes) */
  
//   if(rc<1 || rc>1) return false; // then we failed
//   int range = ovector[0];
//   memset(a,0,range+1);
//   memcpy(a,data,range);
//   memset(b,0,(length-(range+1))+1);
//   memcpy(b,data+range+1,(length-(range+1)));
//   return true;
// }

bool llvm_check_valid_dot_symbol(scheme* sc, char* symbol) {
  char a[256];
  char b[256];
  if(!rsplit((char*)"\\.", symbol, (char*) a, (char*) b)) {
    //printf("Eval error: not valid dot syntax\n");
    return false;
  }
  pointer x=find_slot_in_env(sc,sc->envir,mk_symbol(sc,a),1);
  if(x==sc->NIL) { // || !is_closure(x)) { // then we failed
    //printf("Eval error: not valid dot syntax: bad value\n");
    return false;
  }else{
    //llvm::Module* M = extemp::EXTLLVM::I()->M;
    std::string funcname(a);
    std::string getter("_getter");
    //llvm::Function* func = M->getFunction(funcname+getter);
    llvm::Function* func = extemp::EXTLLVM::I()->getFunction(funcname+getter);
    if(func) {
      return true;
    }else{
      //printf("Eval error: No compiler match for %s\n",symbol);
      return false; 
    }
  }
}

pointer llvm_scheme_env_set(scheme* _sc, char* sym)
{
  using namespace llvm; 
  char fname[256];
  char tmp[256];
  char vname[256];
  char tname[256];
  if(!(rsplit((char*)"\\.",sym, (char*) fname, (char*) tmp))) {
    printf("Error attempting to set environment variable in closure bad split %s\n",sym);
    return _sc->F;  
  }
  if(!rsplit((char*)":",tmp, (char*) vname,(char*) tname)) {
    memset(tname, 0, 256);
    memset(vname, 0, 256);
    memcpy(vname, tmp, 256);
  }   
  //printf("in llvm scheme env set %s.%s:%s\n",fname,vname,tname);
  
  // Module* M = extemp::EXTLLVM::I()->M;
  std::string funcname(fname);
  std::string getter("_getter");
  //llvm::Function* func = M->getFunction(funcname+getter); //std::string(string_value(pair_car(args))));
  llvm::Function* func = extemp::EXTLLVM::I()->getFunction(funcname+getter);
  if(func == 0) {
    printf("Error: no matching function for %s.%s\n",fname,vname);
    return _sc->F; 
  }
  
  void*(*p)() = (void*(*)()) extemp::EXTLLVM::I()->EE->getPointerToFunction(func);
  
  if(p==NULL) {
    printf("Error attempting to set environment variable in closure %s.%s\n",fname,vname);
    return _sc->F;
  }
  
  size_t*** closur = (size_t***) p();
  size_t** closure = *closur;
  //uint32_t** closure = (uint32_t**) cptr_value(pair_car(args));
  closure_address_table* addy_table = (closure_address_table*) *(closure+0);
  // check address exists
  if(!check_address_exists(vname, addy_table)) {
    ascii_text_color(0,1,10);
    printf("RunTime Error:");
    ascii_text_color(0,7,10);
    printf(" slot");
    ascii_text_color(1,7,10);
    printf(" %s.%s ",fname,vname);
    ascii_text_color(0,7,10);
    printf("does not exist!\n");
    ascii_text_color(0,9,10);    
    return _sc->F;
  }
  char* eptr = (char*) *(closure+1);
  char* type = get_address_type(vname,addy_table);
  uint32_t offset = get_address_offset(vname,addy_table);

  //printf("type: %s  offset: %d\n",type, offset);

  pointer value = 0;
  if(_sc->args == _sc->NIL) {
    //value = 0;
    value = _sc->NIL;
  } else {   
    value = pair_car(_sc->args);
  }

  if(strcmp(type,"i32")==0) {
    int32_t** ptr = (int32_t**) (eptr+offset);
    if(value == _sc->NIL) {
      return mk_integer(_sc, **ptr);
    } else {
      **ptr = (int32_t) ivalue(value);
      return _sc->T;
    }
  }else if(strcmp(type,"i64")==0){
    uint64_t** ptr = (uint64_t**) (eptr+offset);
    if(value == _sc->NIL) {
      return mk_integer(_sc, **ptr);
    } else {
      **ptr = ivalue(value);
      return _sc->T;
    }        
  }else if(strcmp(type,"float") == 0){
    float** ptr = (float**) (eptr+offset);
    if(value == _sc->NIL) {
      return mk_real(_sc, **ptr);
    } else {
      **ptr = rvalue(value);
      return _sc->T;
    }            
  }else if(strcmp(type,"double")==0){
    double** ptr = (double**) (eptr+offset);
    if(value == _sc->NIL) {
      return mk_real(_sc, **ptr);
    } else {
      **ptr = rvalue(value);
      return _sc->T;
    }            
  }else{ // else pointer type
    char*** ptr = (char***) (eptr+offset);
    if(value == _sc->NIL) {      
      return mk_cptr(_sc, (void*) **ptr);
    } else {
      **ptr = (char*) cptr_value(value);
      //printf("Unsuported type for closure environment set\n");
      return _sc->T;
    }
  }
  // shouldn't get to here 
  return _sc->F;
}



namespace extemp {
	
    EXTLLVM EXTLLVM::SINGLETON;
    int64_t EXTLLVM::LLVM_COUNT = 0l;
    bool EXTLLVM::OPTIMIZE_COMPILES = 0;
    bool EXTLLVM::VERIFY_COMPILES = 1;
	
    EXTLLVM::EXTLLVM()
    {
	//printf("making llvm !!!!!!!!!!!!!!!!!!\n");
        alloc_mutex.init();
	M = 0;
	MP = 0;
	EE = 0;
#ifdef EXT_MCJIT
  MM = 0;
#endif
	//initLLVM();
    }
	
  EXTLLVM::~EXTLLVM() {}

    llvm::Function* EXTLLVM::getFunction(std::string name) {
    //llvm::Function* cf = llvm_func_cache[name];
    //if (cf) return cf;
    for(int i=0;i<Ms.size();i++) {
      llvm::Module* m = Ms[i];
      llvm::Function* f = m->getFunction(name);
      //std::cout << "Checked " << m->getModuleIdentifier() << " for " << name << " found func " << f << std::endl;
      if(f!=NULL) {
        //if(!cf) llvm_func_cache[name] = cf;
        return f;
      }
    }
    return NULL;
    }

    llvm::GlobalVariable* EXTLLVM::getGlobalVariable(std::string name) {
    //llvm::GlobalVariable* cgv = llvm_var_cache[name];
    //if (cgv) return cgv;
    for(int i=0;i<Ms.size();i++) {
      llvm::Module* m = Ms[i];
      llvm::GlobalVariable* gv = m->getGlobalVariable(name);
      //std::cout << "Checked " << m->getModuleIdentifier() << " for " << name << " found var " << gv << std::endl;      
      if(gv!=NULL) {
        //if(!cgv) llvm_var_cache[name] = gv;
        return gv;
      }
    }
    return NULL;
  }
  
  llvm::GlobalValue* EXTLLVM::getGlobalValue(std::string name) {
    // llvm::GlobalValue* cgv = llvm_val_cache[name];
    //if (cgv) return cgv;
    for(int i=0;i<Ms.size();i++) {
      llvm::Module* m = Ms[i];
      llvm::GlobalValue* gv = m->getNamedValue(name);
      //std::cout << "Checked " << m->getModuleIdentifier() << " for " << name << " found val " << gv << std::endl;      
      if(gv!=NULL) {
        //if(!cgv) llvm_val_cache[name] = gv;        
        return gv;
      }
    }
    return NULL;
  }
  
  llvm::StructType* EXTLLVM::getNamedType(std::string name) {
    //llvm::StructType* ct = llvm_struct_cache[name];
    //if (ct) return ct;
    for(int i=0;i<Ms.size();i++) {
      llvm::Module* m = Ms[i];
      llvm::StructType* t = m->getTypeByName(name);
      //std::cout << "Checked " << m->getModuleIdentifier() << " for " << name << " found type " << t << std::endl;      
      if(t!=NULL) {
        //if(!ct) llvm_struct_cache[name];
        return t;
      }
    }
    return NULL;
  }

#ifdef EXT_MCJIT  
    uint64_t EXTLLVM::getSymbolAddress(std::string name) {      
      return MM->getSymbolAddress(name);
    }
#endif


	
    void EXTLLVM::initLLVM()
    {
	if(M == 0) { // Initalize Once Only (not per scheme process)
                     // 

    llvm::TargetOptions Opts;
	    //llvm::PerformTailCallOpt = true;    
          Opts.GuaranteedTailCallOpt = true;
          Opts.JITEmitDebugInfo = true;
          Opts.UnsafeFPMath = false;
          
  
          llvm::InitializeNativeTarget();
          llvm::InitializeNativeTargetAsmPrinter();
          llvm::LLVMContext &context = llvm::getGlobalContext();
          //llvm::IRBuilder<> theBuilder(context);
  
          // Make the module, which holds all the code.
          M = new llvm::Module("xtmmodule_0", context);

		  if (!extemp::UNIV::ARCH.empty()) M->setTargetTriple(extemp::UNIV::ARCH.front());

          Ms.push_back(M);
  
          // Build engine with JIT
          llvm::EngineBuilder factory(M);
          factory.setEngineKind(llvm::EngineKind::JIT);
          factory.setAllocateGVsWithCode(false);
          factory.setTargetOptions(Opts);
#ifdef EXT_MCJIT
          factory.setUseMCJIT(true);
          MM = new llvm::SectionMemoryManager();          
          factory.setMCJITMemoryManager(MM);
#else          
          factory.setUseMCJIT(false);
#endif
          //if(!extemp::UNIV::ARCH.empty()) factory.setMArch(extemp::UNIV::ARCH.front());
          if(!extemp::UNIV::ATTRS.empty()) factory.setMAttrs(extemp::UNIV::ATTRS);
          if(!extemp::UNIV::CPU.empty()) factory.setMCPU(extemp::UNIV::CPU.front());
          factory.setOptLevel(llvm::CodeGenOpt::Aggressive); 
          //factory.setOptLevel(llvm::CodeGenOpt::None);
          llvm::TargetMachine* tm = factory.selectTarget();          
          EE = factory.create(tm);
          EE->DisableLazyCompilation(true);

          ascii_text_color(0,7,10);
          std::cout << "ARCH           : " << std::flush;
          ascii_text_color(1,6,10);	        
          std::cout << std::string(tm->getTargetTriple()) << std::endl;
          ascii_text_color(0,7,10);	        
          std::cout << "CPU            : " << std::flush;
          ascii_text_color(1,6,10);	        
          std::cout << std::string(tm->getTargetCPU()) << std::endl;
          ascii_text_color(0,7,10);	                  
          std::cout << "ATTRS          : " << std::flush;
          ascii_text_color(1,6,10);	        
          std::cout << std::string(tm->getTargetFeatureString()) << std::endl;          
          ascii_text_color(0,7,10);
          std::cout << "MCJIT          : " << std::flush;
          ascii_text_color(1,6,10);
#ifdef EXT_MCJIT
          std::cout << "YES" << std::endl;
#else
          std::cout << "NO" << std::endl;
#endif          
          ascii_text_color(0,7,10);          
          

			
	    //EE = llvm::EngineBuilder(M).create();
	    PM = new llvm::PassManager();
	    //PM->add(new llvm::TargetData(*EE->getTargetData()));
      PM->add(new llvm::DataLayout(*(EE->getDataLayout())));

      PM->add(llvm::createBasicAliasAnalysisPass());   //new   
      // promote allocs to register
      PM->add(llvm::createPromoteMemoryToRegisterPass());
	    // Do simple "peephole" optimizations and bit-twiddling optzns.
	    PM->add(llvm::createInstructionCombiningPass());
	    // Reassociate expressions.
	    PM->add(llvm::createReassociatePass());
	    // Eliminate Common SubExpressions.
	    PM->add(llvm::createGVNPass());
	    // Function inlining
	    PM->add(llvm::createFunctionInliningPass());
	    // loop invariants
	    PM->add(llvm::createLICMPass());
	    // vars
	    PM->add(llvm::createIndVarSimplifyPass());
	    // Simplify the control flow graph (deleting unreachable blocks, etc).
	    PM->add(llvm::createCFGSimplificationPass());
      // 
	    PM->add(llvm::createPromoteMemoryToRegisterPass());

	    char fname[] = "/code.ir";
	    char load_path[256];
	    strcpy(load_path,extemp::UNIV::PWD);
	    strcat(load_path,fname);
	    FILE* fp;
	    if((fp = fopen(load_path,"rb")) == NULL)
	    {
		printf("Could not open %s\n",load_path);
		exit(1);
	    }
			
	    fseek(fp,0,SEEK_END);
	    long long size = ftell(fp);
	    fseek(fp,0,SEEK_SET);
#ifdef TARGET_OS_WINDOWS
	    char* assm = (char*) _alloca(size+1);
#else
	    char* assm = (char*) alloca(size+1);
#endif
	    size_t res = fread(assm, 1, size, fp);
            if(res != size) {
   	      printf("code.ir length(%lld) read(%lld) \n",size,res);
              if(ferror(fp)) {
    	        printf("Error reading code.ir %d\n",ferror(fp));
                exit(1);
              }else if(feof(fp)){
    	        printf("Error reading code.ir end-of-file error %d\n",feof(fp));
                exit(1);
              }else{
                printf("Length mismatch reading code.ir lgth(%lld) read(%lld)\n",size,res);
                exit(1);
	      }
	    }
	    //std::cout << "assm: " << size << " :: " << res << " ... " << std::endl << assm << std::endl;

            assm[size]=0;
	    fclose(fp);
	    llvm::SMDiagnostic pa;
	    llvm::Module* newM = ParseAssemblyString(assm, M, pa, llvm::getGlobalContext());
			
	    if(newM == 0)
	    {
     	        printf("Compiler Error: Error building code.ir\n");
		std::string errstr;
		llvm::raw_string_ostream ss(errstr);
		pa.print("Extempore",ss);
		printf(ss.str().c_str());
                exit(1);
	    }

	    llvm::GlobalValue* gv = M->getNamedValue(std::string("llvm_destroy_zone_after_delay"));
	    EE->updateGlobalMapping(gv,(void*)&llvm_destroy_zone_after_delay);			
	    gv = M->getNamedValue(std::string("free_after_delay"));
	    EE->updateGlobalMapping(gv,(void*)&free_after_delay);			
	    gv = M->getNamedValue(std::string("llvm_get_next_prime"));
	    EE->updateGlobalMapping(gv,(void*)&llvm_get_next_prime);			
	    gv = M->getNamedValue(std::string("llvm_printf"));
	    EE->updateGlobalMapping(gv,(void*)&llvm_printf);  
	    gv = M->getNamedValue(std::string("llvm_fprintf"));
	    EE->updateGlobalMapping(gv,(void*)&llvm_fprintf);	  
	    gv = M->getNamedValue(std::string("llvm_sprintf"));
	    EE->updateGlobalMapping(gv,(void*)&llvm_sprintf);	  
	    gv = M->getNamedValue(std::string("llvm_sscanf"));
	    EE->updateGlobalMapping(gv,(void*)&llvm_sscanf);	  
	    gv = M->getNamedValue(std::string("llvm_fscanf"));
	    EE->updateGlobalMapping(gv,(void*)&llvm_fscanf);	  
	    gv = M->getNamedValue(std::string("llvm_zone_create"));
	    EE->updateGlobalMapping(gv,(void*)&llvm_zone_create);   
	    gv = M->getNamedValue(std::string("llvm_zone_destroy"));
	    EE->updateGlobalMapping(gv,(void*)&llvm_zone_destroy);
	    gv = M->getNamedValue(std::string("llvm_zone_print"));
	    EE->updateGlobalMapping(gv,(void*)&llvm_zone_print);

	    gv = M->getNamedValue(std::string("llvm_runtime_error"));
	    EE->updateGlobalMapping(gv,(void*)&llvm_runtime_error);

	    gv = M->getNamedValue(std::string("llvm_send_udp"));
	    EE->updateGlobalMapping(gv,(void*)&llvm_send_udp);

	    gv = M->getNamedValue(std::string("llvm_threads_get_callback_zone"));
	    EE->updateGlobalMapping(gv,(void*)&llvm_threads_get_callback_zone); 	

	    gv = M->getNamedValue(std::string("llvm_schedule_callback"));
	    EE->updateGlobalMapping(gv,(void*)&llvm_schedule_callback); 	
	    gv = M->getNamedValue(std::string("llvm_get_function_ptr"));
	    EE->updateGlobalMapping(gv,(void*)&llvm_get_function_ptr); 	
	    // // this added for dodgy continuations
	    // gv = M->getNamedValue(std::string("llvm_make_ucontext"));
	    // EE->updateGlobalMapping(gv,(void*)&llvm_make_ucontext); 	
	    // gv = M->getNamedValue(std::string("llvm_scheme_process_ucontext"));
	    // EE->updateGlobalMapping(gv,(void*)&llvm_scheme_process_ucontext); 	


	    gv = M->getNamedValue(std::string("llvm_peek_zone_stack"));
	    EE->updateGlobalMapping(gv,(void*)&llvm_peek_zone_stack);						
	    gv = M->getNamedValue(std::string("llvm_pop_zone_stack"));
	    EE->updateGlobalMapping(gv,(void*)&llvm_pop_zone_stack);						
	    gv = M->getNamedValue(std::string("llvm_push_zone_stack"));
	    EE->updateGlobalMapping(gv,(void*)&llvm_push_zone_stack);						
						
	    // gv = M->getNamedValue(std::string("llvm_stack_alloc"));
	    // EE->updateGlobalMapping(gv,(void*)&llvm_stack_alloc);						
	    gv = M->getNamedValue(std::string("llvm_zone_malloc"));
	    EE->updateGlobalMapping(gv,(void*)&llvm_zone_malloc);						
	    gv = M->getNamedValue(std::string("get_address_table"));
	    EE->updateGlobalMapping(gv,(void*)&get_address_table);						
	    gv = M->getNamedValue(std::string("check_address_type"));
	    EE->updateGlobalMapping(gv,(void*)&check_address_type);						
	    gv = M->getNamedValue(std::string("check_address_exists"));
	    EE->updateGlobalMapping(gv,(void*)&check_address_exists);						
	    gv = M->getNamedValue(std::string("get_address_offset"));
	    EE->updateGlobalMapping(gv,(void*)&get_address_offset);  
	    gv = M->getNamedValue(std::string("add_address_table"));
	    EE->updateGlobalMapping(gv,(void*)&add_address_table);
	    gv = M->getNamedValue(std::string("new_address_table"));
	    EE->updateGlobalMapping(gv,(void*)&new_address_table);						
	    gv = M->getNamedValue(std::string("llvm_print_pointer"));
	    EE->updateGlobalMapping(gv,(void*)&llvm_print_pointer);						
	    gv = M->getNamedValue(std::string("llvm_print_i32"));
	    EE->updateGlobalMapping(gv,(void*)&llvm_print_i32);						
	    gv = M->getNamedValue(std::string("llvm_print_i64"));
	    EE->updateGlobalMapping(gv,(void*)&llvm_print_i64);						
	    gv = M->getNamedValue(std::string("llvm_print_f32"));
	    EE->updateGlobalMapping(gv,(void*)&llvm_print_f32);						
	    gv = M->getNamedValue(std::string("llvm_print_f64"));
	    EE->updateGlobalMapping(gv,(void*)&llvm_print_f64);						

	    gv = M->getNamedValue(std::string("ascii_text_color"));
	    EE->updateGlobalMapping(gv,(void*)&ascii_text_color);
	    
	    // gv = M->getNamedValue(std::string("llvm_samplerate"));
	    // EE->updateGlobalMapping(gv,(void*)&llvm_samplerate);
	    gv = M->getNamedValue(std::string("llvm_now"));
	    EE->updateGlobalMapping(gv,(void*)&llvm_now);
	    gv = M->getNamedValue(std::string("llvm_zone_reset"));
	    EE->updateGlobalMapping(gv,(void*)&llvm_zone_reset);
	    gv = M->getNamedValue(std::string("llvm_zone_copy_ptr"));
	    EE->updateGlobalMapping(gv,(void*)&llvm_zone_copy_ptr);
	    gv = M->getNamedValue(std::string("llvm_zone_mark"));
	    EE->updateGlobalMapping(gv,(void*)&llvm_zone_mark);
	    gv = M->getNamedValue(std::string("llvm_zone_mark_size"));
	    EE->updateGlobalMapping(gv,(void*)&llvm_zone_mark_size);
	    gv = M->getNamedValue(std::string("llvm_zone_ptr_set_size"));
	    EE->updateGlobalMapping(gv,(void*)&llvm_zone_ptr_set_size);
	    gv = M->getNamedValue(std::string("llvm_zone_ptr_size"));
	    EE->updateGlobalMapping(gv,(void*)&llvm_zone_ptr_size);
	    gv = M->getNamedValue(std::string("llvm_ptr_in_zone"));
	    EE->updateGlobalMapping(gv,(void*)&llvm_ptr_in_zone);
	    gv = M->getNamedValue(std::string("llvm_ptr_in_current_zone"));
	    EE->updateGlobalMapping(gv,(void*)&llvm_ptr_in_current_zone);
	    gv = M->getNamedValue(std::string("llvm_memset"));
	    EE->updateGlobalMapping(gv,(void*)&llvm_memset);
	    gv = M->getNamedValue(std::string("extitoa"));
	    EE->updateGlobalMapping(gv,(void*)&extitoa);
	    gv = M->getNamedValue(std::string("string_hash"));
	    EE->updateGlobalMapping(gv,(void*)&string_hash);

	    gv = M->getNamedValue(std::string("swap64i"));
	    EE->updateGlobalMapping(gv,(void*)&swap64i);
	    gv = M->getNamedValue(std::string("swap64f"));
	    EE->updateGlobalMapping(gv,(void*)&swap64f);
	    gv = M->getNamedValue(std::string("swap32i"));
	    EE->updateGlobalMapping(gv,(void*)&swap32i);
	    gv = M->getNamedValue(std::string("swap32f"));
	    EE->updateGlobalMapping(gv,(void*)&swap32f);			
	    gv = M->getNamedValue(std::string("unswap64i"));
	    EE->updateGlobalMapping(gv,(void*)&unswap64i);
	    gv = M->getNamedValue(std::string("unswap64f"));
	    EE->updateGlobalMapping(gv,(void*)&unswap64f);
	    gv = M->getNamedValue(std::string("unswap32i"));
	    EE->updateGlobalMapping(gv,(void*)&unswap32i);
	    gv = M->getNamedValue(std::string("unswap32f"));
	    EE->updateGlobalMapping(gv,(void*)&unswap32f);	

	    gv = M->getNamedValue(std::string("imp_randd"));
	    EE->updateGlobalMapping(gv,(void*)&imp_randd);	
	    gv = M->getNamedValue(std::string("imp_randf"));
	    EE->updateGlobalMapping(gv,(void*)&imp_randf);	
	    gv = M->getNamedValue(std::string("imp_rand1_i64"));
	    EE->updateGlobalMapping(gv,(void*)&imp_rand1_i64);	
	    gv = M->getNamedValue(std::string("imp_rand2_i64"));
	    EE->updateGlobalMapping(gv,(void*)&imp_rand2_i64);	
	    gv = M->getNamedValue(std::string("imp_rand1_i32"));
	    EE->updateGlobalMapping(gv,(void*)&imp_rand1_i32);	
	    gv = M->getNamedValue(std::string("imp_rand2_i32"));
	    EE->updateGlobalMapping(gv,(void*)&imp_rand2_i32);	
	    gv = M->getNamedValue(std::string("imp_rand1_d"));
	    EE->updateGlobalMapping(gv,(void*)&imp_rand1_d);	
	    gv = M->getNamedValue(std::string("imp_rand2_d"));
	    EE->updateGlobalMapping(gv,(void*)&imp_rand2_d);	
	    gv = M->getNamedValue(std::string("imp_rand1_f"));
	    EE->updateGlobalMapping(gv,(void*)&imp_rand1_f);	
	    gv = M->getNamedValue(std::string("imp_rand2_f"));
	    EE->updateGlobalMapping(gv,(void*)&imp_rand2_f);	


//#ifdef TARGET_OS_WINDOWS
//	    gv = M->getNamedValue(std::string("log2"));
//	    EE->updateGlobalMapping(gv,(void*)&log2);		
//#endif
	    gv = M->getNamedValue(std::string("rsplit"));
	    EE->updateGlobalMapping(gv,(void*)&rsplit);		
	    gv = M->getNamedValue(std::string("rmatch"));
	    EE->updateGlobalMapping(gv,(void*)&rmatch);		
	    gv = M->getNamedValue(std::string("rreplace"));
	    EE->updateGlobalMapping(gv,(void*)&rreplace);			    

	    gv = M->getNamedValue(std::string("base64_encode"));
	    EE->updateGlobalMapping(gv,(void*)&base64_encode);		
	    gv = M->getNamedValue(std::string("base64_decode"));
	    EE->updateGlobalMapping(gv,(void*)&base64_decode);		
	    gv = M->getNamedValue(std::string("cname_encode"));
	    EE->updateGlobalMapping(gv,(void*)&cname_encode);
	    gv = M->getNamedValue(std::string("cname_decode"));
	    EE->updateGlobalMapping(gv,(void*)&cname_decode);
      
      gv = M->getNamedValue(std::string("clock_clock"));
      EE->updateGlobalMapping(gv,(void*)&clock_clock);
      gv = M->getNamedValue(std::string("audio_clock_base"));
      EE->updateGlobalMapping(gv,(void*)&audio_clock_base);
      gv = M->getNamedValue(std::string("audio_clock_now"));
      EE->updateGlobalMapping(gv,(void*)&audio_clock_now);
      

	    // add scheme bits
	    gv = M->getNamedValue(std::string("r64value"));
	    EE->updateGlobalMapping(gv,(void*)&r64value);
	    gv = M->getNamedValue(std::string("mk_double"));
	    EE->updateGlobalMapping(gv,(void*)&mk_double);
	    gv = M->getNamedValue(std::string("r32value"));
	    EE->updateGlobalMapping(gv,(void*)&r32value);
	    gv = M->getNamedValue(std::string("mk_float"));
	    EE->updateGlobalMapping(gv,(void*)&mk_float);
	    gv = M->getNamedValue(std::string("is_real"));
	    EE->updateGlobalMapping(gv,(void*)&is_real);

	    gv = M->getNamedValue(std::string("i64value"));
	    EE->updateGlobalMapping(gv,(void*)&i64value);
	    gv = M->getNamedValue(std::string("mk_i64"));
	    EE->updateGlobalMapping(gv,(void*)&mk_i64);			    
	    gv = M->getNamedValue(std::string("i32value"));
	    EE->updateGlobalMapping(gv,(void*)&i32value);			    
	    gv = M->getNamedValue(std::string("mk_i32"));
	    EE->updateGlobalMapping(gv,(void*)&mk_i32);
	    gv = M->getNamedValue(std::string("i16value"));
	    EE->updateGlobalMapping(gv,(void*)&i16value);			    
	    gv = M->getNamedValue(std::string("mk_i16"));
	    EE->updateGlobalMapping(gv,(void*)&mk_i16);
	    gv = M->getNamedValue(std::string("i8value"));
	    EE->updateGlobalMapping(gv,(void*)&i8value);			    
	    gv = M->getNamedValue(std::string("mk_i8"));
	    EE->updateGlobalMapping(gv,(void*)&mk_i8);
	    gv = M->getNamedValue(std::string("i1value"));
	    EE->updateGlobalMapping(gv,(void*)&i1value);			    
	    gv = M->getNamedValue(std::string("mk_i1"));
	    EE->updateGlobalMapping(gv,(void*)&mk_i1);
	    gv = M->getNamedValue(std::string("is_integer"));
	    EE->updateGlobalMapping(gv,(void*)&is_integer);


	    gv = M->getNamedValue(std::string("string_value"));
	    EE->updateGlobalMapping(gv,(void*)&string_value);			    
	    gv = M->getNamedValue(std::string("mk_string"));
	    EE->updateGlobalMapping(gv,(void*)&mk_string);
	    gv = M->getNamedValue(std::string("is_string"));
	    EE->updateGlobalMapping(gv,(void*)&is_string);

	    gv = M->getNamedValue(std::string("cptr_value"));
	    EE->updateGlobalMapping(gv,(void*)&cptr_value);			    
	    gv = M->getNamedValue(std::string("mk_cptr"));
	    EE->updateGlobalMapping(gv,(void*)&mk_cptr);
	    gv = M->getNamedValue(std::string("is_cptr"));
	    EE->updateGlobalMapping(gv,(void*)&is_cptr);
	    gv = M->getNamedValue(std::string("is_cptr_or_str"));
	    EE->updateGlobalMapping(gv,(void*)&is_cptr_or_str);
            
	    gv = M->getNamedValue(std::string("malloc16"));
	    EE->updateGlobalMapping(gv,(void*)&malloc16);
	    gv = M->getNamedValue(std::string("free16"));
	    EE->updateGlobalMapping(gv,(void*)&free16);

	    gv = M->getNamedValue(std::string("list_ref"));
	    EE->updateGlobalMapping(gv,(void*)&list_ref);

	    gv = M->getNamedValue(std::string("thread_fork"));
	    EE->updateGlobalMapping(gv,(void*)&thread_fork);
	    gv = M->getNamedValue(std::string("thread_join"));
	    EE->updateGlobalMapping(gv,(void*)&thread_join);
	    gv = M->getNamedValue(std::string("thread_kill"));
	    EE->updateGlobalMapping(gv,(void*)&thread_kill);
	    gv = M->getNamedValue(std::string("thread_self"));
	    EE->updateGlobalMapping(gv,(void*)&thread_self);
	    gv = M->getNamedValue(std::string("thread_sleep"));
	    EE->updateGlobalMapping(gv,(void*)&thread_sleep);

	    gv = M->getNamedValue(std::string("mutex_create"));
	    EE->updateGlobalMapping(gv,(void*)&mutex_create);
	    gv = M->getNamedValue(std::string("mutex_destroy"));
	    EE->updateGlobalMapping(gv,(void*)&mutex_destroy);
	    gv = M->getNamedValue(std::string("mutex_lock"));
	    EE->updateGlobalMapping(gv,(void*)&mutex_lock);
	    gv = M->getNamedValue(std::string("mutex_unlock"));
	    EE->updateGlobalMapping(gv,(void*)&mutex_unlock);
	    gv = M->getNamedValue(std::string("mutex_trylock"));
	    EE->updateGlobalMapping(gv,(void*)&mutex_trylock);

            // gv = M->getNamedValue(std::string("cosd"));
            // EE->updateGlobalMapping(gv,(void*)&cos);
            // gv = M->getNamedValue(std::string("tand"));
            // EE->updateGlobalMapping(gv,(void*)&tan);
            // gv = M->getNamedValue(std::string("sind"));
            // EE->updateGlobalMapping(gv,(void*)&sin);
            // gv = M->getNamedValue(std::string("coshd"));
            // EE->updateGlobalMapping(gv,(void*)&cosh);
            // gv = M->getNamedValue(std::string("tanhd"));
            // EE->updateGlobalMapping(gv,(void*)&tanh);
            // gv = M->getNamedValue(std::string("sinhd"));
            // EE->updateGlobalMapping(gv,(void*)&sinh);
            // gv = M->getNamedValue(std::string("acosd"));
            // EE->updateGlobalMapping(gv,(void*)&acos);
            // gv = M->getNamedValue(std::string("asind"));
            // EE->updateGlobalMapping(gv,(void*)&asin);
            // gv = M->getNamedValue(std::string("atand"));
            // EE->updateGlobalMapping(gv,(void*)&atan);
            // gv = M->getNamedValue(std::string("atan2d"));
            // EE->updateGlobalMapping(gv,(void*)&atan2);
            // gv = M->getNamedValue(std::string("ceild"));
            // EE->updateGlobalMapping(gv,(void*)&ceil);
            // gv = M->getNamedValue(std::string("floord"));
            // EE->updateGlobalMapping(gv,(void*)&floor);
            // gv = M->getNamedValue(std::string("expd"));
            // EE->updateGlobalMapping(gv,(void*)&exp);
            // gv = M->getNamedValue(std::string("fmodd"));
            // EE->updateGlobalMapping(gv,(void*)&fmod);
            // gv = M->getNamedValue(std::string("powd"));
            // EE->updateGlobalMapping(gv,(void*)&pow);
            // gv = M->getNamedValue(std::string("logd"));
            // EE->updateGlobalMapping(gv,(void*)&log);
            // gv = M->getNamedValue(std::string("log2d"));
            // EE->updateGlobalMapping(gv,(void*)&log2);
            // gv = M->getNamedValue(std::string("log10d"));
            // EE->updateGlobalMapping(gv,(void*)&log10);
            // gv = M->getNamedValue(std::string("sqrtd"));
            // EE->updateGlobalMapping(gv,(void*)&sqrt);
            // gv = M->getNamedValue(std::string("fabsd"));
            // EE->updateGlobalMapping(gv,(void*)&fabs);

      // gv = M->getNamedValue(std::string("llvm_cos"));
      // EE->updateGlobalMapping(gv,(void*)&llvm_cos);
      // gv = M->getNamedValue(std::string("llvm_sin"));
      // EE->updateGlobalMapping(gv,(void*)&llvm_sin);      
      gv = M->getNamedValue(std::string("llvm_tan"));
      EE->updateGlobalMapping(gv,(void*)&llvm_tan);
      gv = M->getNamedValue(std::string("llvm_cosh"));
      EE->updateGlobalMapping(gv,(void*)&llvm_cosh);
      gv = M->getNamedValue(std::string("llvm_tanh"));
      EE->updateGlobalMapping(gv,(void*)&llvm_tanh);
      gv = M->getNamedValue(std::string("llvm_sinh"));
      EE->updateGlobalMapping(gv,(void*)&llvm_sinh);
      gv = M->getNamedValue(std::string("llvm_acos"));
      EE->updateGlobalMapping(gv,(void*)&llvm_acos);
      gv = M->getNamedValue(std::string("llvm_asin"));
      EE->updateGlobalMapping(gv,(void*)&llvm_asin);
      gv = M->getNamedValue(std::string("llvm_atan"));
      EE->updateGlobalMapping(gv,(void*)&llvm_atan);
      gv = M->getNamedValue(std::string("llvm_atan2"));
      EE->updateGlobalMapping(gv,(void*)&llvm_atan2);
      // gv = M->getNamedValue(std::string("llvm_ceil"));
      // EE->updateGlobalMapping(gv,(void*)&llvm_ceil);
      // gv = M->getNamedValue(std::string("llvm_floor"));
      // EE->updateGlobalMapping(gv,(void*)&llvm_floor);
      // gv = M->getNamedValue(std::string("llvm_exp"));
      // EE->updateGlobalMapping(gv,(void*)&llvm_exp);
      // gv = M->getNamedValue(std::string("llvm_fmod"));
      // EE->updateGlobalMapping(gv,(void*)&llvm_fmod);
      // gv = M->getNamedValue(std::string("llvm_pow"));
      // EE->updateGlobalMapping(gv,(void*)&llvm_pow);
      // gv = M->getNamedValue(std::string("llvm_log"));
      // EE->updateGlobalMapping(gv,(void*)&llvm_log);
      // gv = M->getNamedValue(std::string("llvm_log2"));
      // EE->updateGlobalMapping(gv,(void*)&llvm_log2);
      // gv = M->getNamedValue(std::string("llvm_log10"));
      // EE->updateGlobalMapping(gv,(void*)&llvm_log10);
      // gv = M->getNamedValue(std::string("llvm_sqrt"));
      // EE->updateGlobalMapping(gv,(void*)&llvm_sqrt);
      // gv = M->getNamedValue(std::string("llvm_fabs"));
      // EE->updateGlobalMapping(gv,(void*)&llvm_fabs);
            
            
	}
#ifdef EXT_MCJIT
  extemp::EXTLLVM::I()->EE->finalizeObject();
#endif
	return;
    }
}
