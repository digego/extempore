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

///////////////////
// LLVM includes //
///////////////////

// must be included before anything which pulls in <Windows.h>
#include "llvm/AsmParser/Parser.h"
#include "llvm/Config/llvm-config.h" // for LLVM_VERSION_STRING
#include "llvm/ExecutionEngine/GenericValue.h"
#include "llvm/ExecutionEngine/Interpreter.h"
#include "llvm/ExecutionEngine/MCJIT.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/IR/CallingConv.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/LinkAllPasses.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Support/MemoryObject.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCDisassembler.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstPrinter.h"
#include "llvm/MC/MCContext.h"

#include <random>
#include <fstream>
#include "stdarg.h"
#include "EXTLLVM.h"
#include "EXTThread.h"
#include "UNIV.h"
#include "SchemeFFI.h"
#include "TaskScheduler.h"
#include "Scheme.h"
#include "pcre.h"
#include "OSC.h"
#include "math.h"
#include "BranchPrediction.h"

#ifdef _WIN32
#include <malloc.h>
#else
#include <sys/types.h>
#endif

#ifdef __linux__
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

#ifdef _WIN32
#include <chrono>
#include <thread>
#elif EXT_BOOST
#include <thread>
#endif

#ifndef _WIN32
#include <unistd.h>
#endif

#include "SchemeProcess.h"

#define DEBUG_ZONE_STACK 0
#define DEBUG_ZONE_ALLOC 0
#define LEAKY_ZONES 1
#define EXTENSIBLE_ZONES 1

// llvm_scheme foreign function -> string name
// also is not thread safe!
std::map<foreign_func, std::string> LLVM_SCHEME_FF_MAP;

extemp::EXTMutex alloc_mutex("alloc mutex");

static void* malloc16(size_t Size)
{
#ifdef _WIN32
    return _aligned_malloc(Size, 16);
#else
    void* result;
    if (posix_memalign(&result, 16, Size)) {
        return nullptr;
    }
    return result;
#endif
}

static void free16(void* Ptr) {
#ifdef _WIN32
    _aligned_free(Ptr);
#else
    free(Ptr);
#endif
}

const char* llvm_scheme_ff_get_name(foreign_func ff)
{
    return LLVM_SCHEME_FF_MAP[ff].c_str();
}

void llvm_scheme_ff_set_name(foreign_func ff,const char* name)
{
    LLVM_SCHEME_FF_MAP[ff] = std::string(name);
    return;
}

// LLVM RUNTIME ERROR
void llvm_runtime_error(int error, void* arg)
{
  ascii_error();
  switch(error){
  case 1:
    printf("LLVM zptr_copy - invalid zptr! %p\n",arg);
    break;
  default:
    break;
  }
  ascii_normal();
  return;
}

THREAD_LOCAL llvm_zone_stack* tls_llvm_zone_stack = 0;
THREAD_LOCAL uint64_t tls_llvm_zone_stacksize = 0;

void llvm_zone_print(llvm_zone_t* zone)
{
  auto tmp(zone);
  auto total_size(zone->size);
  int64_t segments(1);
  while (tmp->memories) {
    tmp = tmp->memories;
    total_size += tmp->size;
    segments++;
  }
  printf("<MemZone(%p) size(%" PRId64 ") free(%" PRId64 ") segs(%" PRId64 ")>",zone,total_size,(zone->size - zone->offset),segments);
  return;
}

uint64_t llvm_zone_ptr_size(void* ptr)
{
    return *(reinterpret_cast<uint64_t*>(ptr) - 1);
}

bool llvm_zone_copy_ptr(void* ptr1, void* ptr2)
{
    uint64_t size1 = llvm_zone_ptr_size(ptr1);
    uint64_t size2 = llvm_zone_ptr_size(ptr2);

    if (unlikely(size1 != size2)) {
  //printf("Bad LLVM ptr copy - size mismatch setting %p:%lld -> %p:%lld\n",ptr1,size1,ptr2,size2);
      return 1;
    }
    if (unlikely(!size1)) {
  //printf("Bad LLVM ptr copy - size mismatch setting %p:%lld -> %p:%lld\n",ptr1,size1,ptr2,size2);
      return 1;
    }
    //printf("zone_copy_ptr: %p,%p,%lld,%lld\n",ptr2,ptr1,size1,size2);
    memcpy(ptr2, ptr1, size1);
    return 0;
}

bool llvm_ptr_in_zone(llvm_zone_t* zone, void* ptr)
{
    while (unlikely(zone && (ptr < zone->memory || ptr >= reinterpret_cast<char*>(zone->memory) + zone->size))) {
      zone = zone->memories;
    }
    return zone;
}

void llvm_schedule_callback(long long time, void* dat)
{
  //printf("scheduled callback %lld\n",time);
  extemp::SchemeProcess* proc = extemp::SchemeProcess::I(); //extemp::SchemeProcess::I()->extemporeCallback(time,dat);

  uint64_t current_time = time; //task->getStartTime();
  uint64_t duration = 1000000000; //task->getDuration();
  extemp::TaskScheduler::I()->addTask(current_time, duration, proc->getExtemporeCallback(), dat, 0, true);
  return;
}

void* llvm_get_function_ptr(char* fname)
{
  return reinterpret_cast<void*>(extemp::EXTLLVM::EE->getFunctionAddress(fname));
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
  sprintf(buf,"%" PRId64,val);
  return buf;//&buf[i+1];
}

void llvm_send_udp(char* host, int port, void* message, int message_length)
{
  int length = message_length;

#ifdef EXT_BOOST // TODO: This should use WinSock on Windows
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

// these are helpers for runtime debugging in llvm
void llvm_print_pointer(void* ptr)
{
    printf("llvm:ptr:>%p -- %" PRId64 "\n",ptr,*((int64_t*)ptr));
    return;
}

void llvm_print_i32(int32_t num)
{
    printf("llvm:i32:>%d\n",num);
    return;
}

void llvm_print_i64(int64_t num)
{
    printf("llvm:i64:>%" PRId64 "\n",num);
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

// double llvm_sin(double x) { return sin(x); }
double llvm_tan(double x) { return tan(x); }
double llvm_cosh(double x) { return cosh(x); }
double llvm_tanh(double x) { return tanh(x); }
double llvm_sinh(double x) { return sinh(x); }
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

// these shouldn't ever be large, so it should be ok to cast to signed
// int for returning into xtlang (which prefers signed ints). I hope
// this doesn't come back to bite me one day.
int32_t llvm_frames() { return (int32_t)extemp::UNIV::FRAMES; }
int32_t llvm_channels() { return (int32_t)extemp::UNIV::CHANNELS; }
int32_t llvm_in_channels() { return (int32_t)extemp::UNIV::IN_CHANNELS; }

static THREAD_LOCAL std::minstd_rand* sRandGen;

double imp_randd()
{
    if (unlikely(!sRandGen)) {
        sRandGen = new std::minstd_rand(time(nullptr));
    }
    // The existing implementation *COULD* (p = 1 / RAND_MAX) return 1!, but I don't think that was intended
    return std::uniform_real_distribution<double>()(*sRandGen);
}

float imp_randf()
{
    return imp_randd();
}

int64_t imp_rand1_i64(int64_t Limit)
{
    return imp_randd() * Limit;
}

int64_t imp_rand2_i64(int64_t Start, int64_t Limit)
{
    return imp_randd() * (Limit - Start) + Start;
}

int32_t imp_rand1_i32(int32_t Limit)
{
    return imp_randd() * Limit;
}

int32_t imp_rand2_i32(int32_t Start, int32_t Limit)
{
    return imp_randd() * (Limit - Start) + Start;
}

double imp_rand1_d(double Limit)
{
    return imp_randd() * Limit;
}

double imp_rand2_d(double Start, double Limit)
{
    return imp_randd() * (Limit - Start) + Start;
}

float imp_rand1_f(float Limit)
{
    return imp_randf() * Limit;
}

float imp_rand2_f(float Start, float Limit)
{
    return imp_randf() * (Limit - Start) + Start;
}

///////////////////////////////////

///////////////////////////////////////////////////////////////////////
// This here for Extempore Compiler Runtime.
// This is temporary and needs to replaced with something sensible!
struct closure_address_table
{
    uint64_t id;
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

uint32_t get_address_offset(uint64_t id, closure_address_table* table)
{
    while(table)
    {
      // printf("%p name: %s\ntablename: %s\n\n", name, name, table->name);
      if(table->id == id) {
        // printf("in %s returning offset %d from %s\n",table->name,table->offset,name);
        return table->offset;
      }
      table = table->next;
    }
    printf("Unable to locate %" PRIu64 " in closure environment b\n",id);
    return 0;
}

char* get_address_type(uint64_t id, closure_address_table* table)
//char* get_address_type(const char* name, closure_address_table* table)
{
    while(table)
    {
      if(table->id == id) {
        return table->type;
      }
      table = table->next;
    }
    printf("Unable to locate id in closure environment c\n");
    return 0;
}

bool check_address_exists(uint64_t id, closure_address_table* table)
{
  while(table)
    {
      if(table->id == id) {
        return true;
      }
      table = table->next;
    }
  return false;
}

bool check_address_type(uint64_t id, closure_address_table* table, const char* type)
{
  while(table)
    {
      if(table->id == id) {
        if((strcmp(table->type,type)!=0) && (strcmp("{i8*, i8*, void (i8*, i8*)*}**",type)!=0)) {
          printf("Runtime Type Error: bad type %s for %s. Should be %s\n",type,table->name,table->type);
          return 0;
        }else{
          return 1;
        }
      }
      table = table->next;
    }
  printf("Unable to locate id in closure environment type: %s d\n",type);
  return 0;
}

closure_address_table* add_address_table(llvm_zone_t* zone, char* name, uint32_t offset, char* type, int alloctype, struct closure_address_table* table)
{
    struct closure_address_table* t = NULL;
    if (alloctype == 1) {
        t = reinterpret_cast<closure_address_table*>(malloc(sizeof(struct closure_address_table)));
    } else {
        t = (struct closure_address_table*) extemp::EXTLLVM::llvm_zone_malloc(zone,sizeof(struct closure_address_table));
    }
    t->id = string_hash(name);
    t->name = name;
    t->offset = offset;
    t->type = type;
    t->next = table;
    return t;
}

bool llvm_check_valid_dot_symbol(scheme* sc, char* symbol) {
  char c[1024];
  auto pos(strchr(symbol, '.'));
  if (!pos) {
    //printf("Eval error: not valid dot syntax\n");
    return false;
  }
  strncpy(c, symbol, pos - symbol);
  c[pos - symbol] = '\0';
  pointer x = find_slot_in_env(sc, sc->envir, mk_symbol(sc, c), 1);
  if (x == sc->NIL) {
    return false;
  }
  strcat(c, "_xtlang_name");
  pointer y = find_slot_in_env(sc, sc->envir, mk_symbol(sc, c), 1);
  return y != sc->NIL;
}

#define strvalue(p)      ((p)->_object._string._svalue)
pointer llvm_scheme_env_set(scheme* _sc, char* sym)
{
  using namespace llvm;
  char fname[256];
  char tmp[256];
  char vname[256];
  char tname[256];

  char c[1024];
  c[0] = '\0';
  const char* d = "_xtlang_name";

  if(!(rsplit((char*)"\\.",sym, (char*) fname, (char*) tmp))) {
    printf("Error attempting to set environment variable in closure bad split %s\n",sym);
    return _sc->F;
  }
  if(!rsplit((char*)":",tmp, (char*) vname,(char*) tname)) {
    tname[0] = '\0';
    memcpy(vname, tmp, 256);
  }
  strcat(c,fname);
  strcat(c,d);
  pointer xtlang_f_name = find_slot_in_env(_sc,_sc->envir,mk_symbol(_sc,c),1);
  char* xtlang_name = strvalue(pair_cdr(xtlang_f_name));
  //printf("in llvm scheme env set %s.%s:%s  xtlang:%s\n",fname,vname,tname,xtlang_name);
  uint64_t id = string_hash(vname);
  // Module* M = extemp::EXTLLVM::M;
  std::string funcname(xtlang_name);
  std::string getter("_getter");
  void*(*p)() = (void*(*)()) extemp::EXTLLVM::EE->getFunctionAddress(funcname + getter);
  if (!p) {
    printf("Error attempting to set environment variable in closure %s.%s\n",fname,vname);
    return _sc->F;
  }

  size_t*** closur = (size_t***) p();
  size_t** closure = *closur;
  //uint32_t** closure = (uint32_t**) cptr_value(pair_car(args));
  closure_address_table* addy_table = (closure_address_table*) *(closure+0);
  // check address exists
  if(!check_address_exists(id, addy_table)) {
    ascii_error();
    printf("RunTime Error:");
    ascii_normal();
    printf(" slot");
    ascii_warning();
    printf(" %s.%s ",fname,vname);
    ascii_normal();
    printf("does not exist!\n");
    ascii_default();
    return _sc->F;
  }
  char* eptr = (char*) *(closure+1);
  char* type = get_address_type(id,addy_table);
  uint32_t offset = get_address_offset(id,addy_table);

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

namespace EXTLLVM {

llvm::ExecutionEngine* EE = nullptr;
llvm::legacy::PassManager* PM;
llvm::legacy::PassManager* PM_NO;
llvm::Module* M = nullptr; // TODO: obsolete?
std::vector<llvm::Module*> Ms;
int64_t LLVM_COUNT = 0l;
bool OPTIMIZE_COMPILES = true;
bool VERIFY_COMPILES = true;

static llvm::SectionMemoryManager* MM = nullptr;

uint64_t getSymbolAddress(const std::string& name) {
    return MM->getSymbolAddress(name);
}

#include "extllvm.inc"

void initLLVM()
{
    if (unlikely(EE)) {
        return;
    }
    alloc_mutex.init();
    llvm::TargetOptions Opts;
    Opts.GuaranteedTailCallOpt = true;
    Opts.UnsafeFPMath = false;
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    LLVMInitializeX86Disassembler();
    auto& context(llvm::getGlobalContext());
    auto module(llvm::make_unique<llvm::Module>("xtmmodule_0", context));
    M = module.get();
    addModule(M);
    if (!extemp::UNIV::ARCH.empty()) {
        M->setTargetTriple(extemp::UNIV::ARCH);
    }
    // Build engine with JIT
    llvm::EngineBuilder factory(std::move(module));
    factory.setEngineKind(llvm::EngineKind::JIT);
    factory.setTargetOptions(Opts);
    auto mm(llvm::make_unique<llvm::SectionMemoryManager>());
    MM = mm.get();
    factory.setMCJITMemoryManager(std::move(mm));
#ifdef _WIN32
    if (!extemp::UNIV::ATTRS.empty()) {
        factory.setMAttrs(extemp::UNIV::ATTRS);
    }
    if (!extemp::UNIV::CPU.empty()) {
        factory.setMCPU(extemp::UNIV::CPU);
    }
    llvm::TargetMachine* tm = factory.selectTarget();
#else
    factory.setOptLevel(llvm::CodeGenOpt::Aggressive);
    llvm::Triple triple(llvm::sys::getProcessTriple());
    std::string cpu;
    if (!extemp::UNIV::CPU.empty()) {
        cpu = extemp::UNIV::CPU.front();
    } else {
        cpu = llvm::sys::getHostCPUName();
    }
    llvm::SmallVector<std::string, 10> lattrs;
    if (!extemp::UNIV::ATTRS.empty()) {
        for (const auto& attr : extemp::UNIV::ATTRS) {
            lattrs.append(1, attr);
        }
    } else {
        llvm::StringMap<bool> HostFeatures;
        llvm::sys::getHostCPUFeatures(HostFeatures);
        for (auto& feature : HostFeatures) {
            std::string att = feature.getValue() ? feature.getKey().str() : std::string("-") + feature.getKey().str();
            lattrs.append(1, att);
        }
    }
    llvm::TargetMachine* tm = factory.selectTarget(triple, "", cpu, lattrs);
#endif // _WIN32
    EE = factory.create(tm);
    EE->DisableLazyCompilation(true);
    ascii_normal();
    std::cout << "ARCH           : " << std::flush;
    ascii_info();
    std::cout << std::string(tm->getTargetTriple().normalize()) << std::endl;
#ifdef _WIN32
    if (!std::string(tm->getTargetFeatureString()).empty()) {
#else
    if (!std::string(tm->getTargetCPU()).empty()) {
#endif
        ascii_normal();
        std::cout << "CPU            : " << std::flush;
        ascii_info();
        std::cout << std::string(tm->getTargetCPU()) << std::endl;
    }
    if (!std::string(tm->getTargetFeatureString()).empty()) {
        ascii_normal();
        std::cout << "ATTRS          : " << std::flush;
        auto data(tm->getTargetFeatureString().data());
        for (; *data; ++data) {
            switch (*data) {
            case '+':
                ascii_info();
                break;
            case '-':
                ascii_error();
                break;
            case ',':
                ascii_normal();
                break;
            }
            putchar(*data);
        }
        putchar('\n');
    }
    ascii_normal();
    std::cout << "LLVM           : " << std::flush;
    ascii_info();
    std::cout << LLVM_VERSION_STRING;
    std::cout << " MCJIT" << std::endl;
    ascii_normal();
    PM_NO = new llvm::legacy::PassManager();
    PM_NO->add(llvm::createAlwaysInlinerPass());
    PM = new llvm::legacy::PassManager();
    PM->add(llvm::createAggressiveDCEPass());
    PM->add(llvm::createAlwaysInlinerPass());
    PM->add(llvm::createArgumentPromotionPass());
    PM->add(llvm::createCFGSimplificationPass());
    PM->add(llvm::createDeadStoreEliminationPass());
    PM->add(llvm::createFunctionInliningPass());
    PM->add(llvm::createGVNPass(true));
    PM->add(llvm::createIndVarSimplifyPass());
    PM->add(llvm::createInstructionCombiningPass());
    PM->add(llvm::createJumpThreadingPass());
    PM->add(llvm::createLICMPass());
    PM->add(llvm::createLoopDeletionPass());
    PM->add(llvm::createLoopRotatePass());
    PM->add(llvm::createLoopUnrollPass());
    PM->add(llvm::createMemCpyOptPass());
    PM->add(llvm::createPromoteMemoryToRegisterPass());
    PM->add(llvm::createReassociatePass());
    PM->add(llvm::createScalarReplAggregatesPass());
    PM->add(llvm::createSCCPPass());
    PM->add(llvm::createTailCallEliminationPass());

    static struct {
        const char* name;
        uintptr_t   address;
    } mappingTable[] = {
        { "llvm_disassemble", uintptr_t(&llvm_disassemble) },
        { "llvm_destroy_zone_after_delay", uintptr_t(&llvm_destroy_zone_after_delay) },
        { "free_after_delay", uintptr_t(&free_after_delay) },
        // { "llvm_get_next_prime", uintptr_t(&llvm_get_next_prime) },
        { "llvm_zone_destroy", uintptr_t(&llvm_zone_destroy) },
        { "llvm_pop_zone_stack", uintptr_t(&llvm_pop_zone_stack) },
    };
    for (auto& elem : mappingTable) {
        EE->updateGlobalMapping(elem.name, elem.address);
    }

      // tell LLVM about some built-in functions
            EE->updateGlobalMapping("llvm_zone_print", (uint64_t)&llvm_zone_print);
            EE->updateGlobalMapping("llvm_runtime_error", (uint64_t)&llvm_runtime_error);
            EE->updateGlobalMapping("llvm_send_udp", (uint64_t)&llvm_send_udp);
            EE->updateGlobalMapping("llvm_schedule_callback", (uint64_t)&llvm_schedule_callback);
            EE->updateGlobalMapping("llvm_get_function_ptr", (uint64_t)&llvm_get_function_ptr);
            EE->updateGlobalMapping("llvm_zone_malloc", (uint64_t)&llvm_zone_malloc);
            EE->updateGlobalMapping("llvm_zone_malloc_from_current_zone", (uint64_t)&llvm_zone_malloc_from_current_zone);
            EE->updateGlobalMapping("get_address_table", (uint64_t)&get_address_table);
            EE->updateGlobalMapping("check_address_type", (uint64_t)&check_address_type);
            EE->updateGlobalMapping("check_address_exists", (uint64_t)&check_address_exists);
            EE->updateGlobalMapping("get_address_offset", (uint64_t)&get_address_offset);
            EE->updateGlobalMapping("add_address_table", (uint64_t)&add_address_table);
            EE->updateGlobalMapping("llvm_print_pointer", (uint64_t)&llvm_print_pointer);
            EE->updateGlobalMapping("llvm_print_i32", (uint64_t)&llvm_print_i32);
            EE->updateGlobalMapping("llvm_print_i64", (uint64_t)&llvm_print_i64);
            EE->updateGlobalMapping("llvm_print_f32", (uint64_t)&llvm_print_f32);
            EE->updateGlobalMapping("llvm_print_f64", (uint64_t)&llvm_print_f64);
            EE->updateGlobalMapping("llvm_frames", (uint64_t)&llvm_frames);
            EE->updateGlobalMapping("llvm_channels", (uint64_t)&llvm_channels);
            EE->updateGlobalMapping("llvm_in_channels", (uint64_t)&llvm_in_channels);
            EE->updateGlobalMapping("llvm_zone_copy_ptr", (uint64_t)&llvm_zone_copy_ptr);
            EE->updateGlobalMapping("llvm_zone_ptr_size", (uint64_t)&llvm_zone_ptr_size);
            EE->updateGlobalMapping("llvm_ptr_in_zone", (uint64_t)&llvm_ptr_in_zone);
            EE->updateGlobalMapping("llvm_ptr_in_current_zone", (uint64_t)&llvm_ptr_in_current_zone);
            EE->updateGlobalMapping("extitoa", (uint64_t)&extitoa);
            EE->updateGlobalMapping("string_hash", (uint64_t)&string_hash);
            EE->updateGlobalMapping("swap64i", (uint64_t)&swap64i);
            EE->updateGlobalMapping("swap64f", (uint64_t)&swap64f);
            EE->updateGlobalMapping("swap32i", (uint64_t)&swap32i);
            EE->updateGlobalMapping("swap32f", (uint64_t)&swap32f);
            EE->updateGlobalMapping("unswap64i", (uint64_t)&unswap64i);
            EE->updateGlobalMapping("unswap64f", (uint64_t)&unswap64f);
            EE->updateGlobalMapping("unswap32i", (uint64_t)&unswap32i);
            EE->updateGlobalMapping("unswap32f", (uint64_t)&unswap32f);
            EE->updateGlobalMapping("imp_randd", (uint64_t)&imp_randd);
            EE->updateGlobalMapping("imp_randf", (uint64_t)&imp_randf);
            EE->updateGlobalMapping("imp_rand1_i64", (uint64_t)&imp_rand1_i64);
            EE->updateGlobalMapping("imp_rand2_i64", (uint64_t)&imp_rand2_i64);
            EE->updateGlobalMapping("imp_rand1_i32", (uint64_t)&imp_rand1_i32);
            EE->updateGlobalMapping("imp_rand2_i32", (uint64_t)&imp_rand2_i32);
            EE->updateGlobalMapping("imp_rand1_d", (uint64_t)&imp_rand1_d);
            EE->updateGlobalMapping("imp_rand2_d", (uint64_t)&imp_rand2_d);
            EE->updateGlobalMapping("imp_rand1_f", (uint64_t)&imp_rand1_f);
            EE->updateGlobalMapping("imp_rand2_f", (uint64_t)&imp_rand2_f);
            EE->updateGlobalMapping("rsplit", (uint64_t)&rsplit);
            EE->updateGlobalMapping("rmatch", (uint64_t)&rmatch);
            EE->updateGlobalMapping("rreplace", (uint64_t)&rreplace);
            EE->updateGlobalMapping("base64_encode", (uint64_t)&base64_encode);
            EE->updateGlobalMapping("base64_decode", (uint64_t)&base64_decode);
            EE->updateGlobalMapping("cname_encode", (uint64_t)&cname_encode);
            EE->updateGlobalMapping("cname_decode", (uint64_t)&cname_decode);
            EE->updateGlobalMapping("r64value", (uint64_t)&r64value);
            EE->updateGlobalMapping("mk_double", (uint64_t)&mk_double);
            EE->updateGlobalMapping("r32value", (uint64_t)&r32value);
            EE->updateGlobalMapping("mk_float", (uint64_t)&mk_float);
            EE->updateGlobalMapping("is_real", (uint64_t)&is_real);
            EE->updateGlobalMapping("i64value", (uint64_t)&i64value);
            EE->updateGlobalMapping("mk_i64", (uint64_t)&mk_i64);
            EE->updateGlobalMapping("i32value", (uint64_t)&i32value);
            EE->updateGlobalMapping("mk_i32", (uint64_t)&mk_i32);
            EE->updateGlobalMapping("i16value", (uint64_t)&i16value);
            EE->updateGlobalMapping("mk_i16", (uint64_t)&mk_i16);
            EE->updateGlobalMapping("i8value", (uint64_t)&i8value);
            EE->updateGlobalMapping("mk_i8", (uint64_t)&mk_i8);
            EE->updateGlobalMapping("i1value", (uint64_t)&i1value);
            EE->updateGlobalMapping("mk_i1", (uint64_t)&mk_i1);
            EE->updateGlobalMapping("is_integer", (uint64_t)&is_integer);
            EE->updateGlobalMapping("string_value", (uint64_t)&string_value);
            EE->updateGlobalMapping("mk_string", (uint64_t)&mk_string);
            EE->updateGlobalMapping("is_string", (uint64_t)&is_string);
            EE->updateGlobalMapping("cptr_value", (uint64_t)&cptr_value);
            EE->updateGlobalMapping("mk_cptr", (uint64_t)&mk_cptr);
            EE->updateGlobalMapping("is_cptr", (uint64_t)&is_cptr);
            EE->updateGlobalMapping("is_cptr_or_str", (uint64_t)&is_cptr_or_str);
            EE->updateGlobalMapping("malloc16", (uint64_t)&malloc16);
            EE->updateGlobalMapping("free16", (uint64_t)&free16);
            EE->updateGlobalMapping("list_ref", (uint64_t)&list_ref);
      EE->updateGlobalMapping("llvm_tan", (uint64_t)&llvm_tan);
      EE->updateGlobalMapping("llvm_cosh", (uint64_t)&llvm_cosh);
      EE->updateGlobalMapping("llvm_tanh", (uint64_t)&llvm_tanh);
      EE->updateGlobalMapping("llvm_sinh", (uint64_t)&llvm_sinh);
      EE->updateGlobalMapping("llvm_asin", (uint64_t)&llvm_asin);
      EE->updateGlobalMapping("llvm_atan", (uint64_t)&llvm_atan);
      EE->updateGlobalMapping("llvm_atan2", (uint64_t)&llvm_atan2);
      EE->updateGlobalMapping("sys_sharedir", (uint64_t)&sys_sharedir);
      EE->updateGlobalMapping("sys_slurp_file", (uint64_t)&sys_slurp_file);
      extemp::EXTLLVM::EE->finalizeObject();
      return;
    }
  }
}

#include <unordered_map>

static std::unordered_map<std::string, const llvm::GlobalValue*> sGlobalMap;

namespace extemp {

void EXTLLVM::addModule(llvm::Module* Module)
{
    for (const auto& function : Module->getFunctionList()) {
        std::string str;
        llvm::raw_string_ostream stream(str);
        function.printAsOperand(stream, false);
        auto result(sGlobalMap.insert(std::make_pair(stream.str().substr(1), &function)));
        if (!result.second) {
            result.first->second = &function;
        }
    }
    for (const auto& global : Module->getGlobalList()) {
        std::string str;
        llvm::raw_string_ostream stream(str);
        global.printAsOperand(stream, false);
        auto result(sGlobalMap.insert(std::make_pair(stream.str().substr(1), &global)));
        if (!result.second) {
            result.first->second = &global;
        }
    }
    Ms.push_back(Module);
}

const llvm::GlobalValue* EXTLLVM::getGlobalValue(const char* Name)
{
    auto iter(sGlobalMap.find(Name));
    if (likely(iter != sGlobalMap.end())) {
        return iter->second;
    }
    return nullptr;
}

const llvm::GlobalVariable* EXTLLVM::getGlobalVariable(const char* Name)
{
    auto val(getGlobalValue(Name));
    if (likely(val)) {
        return llvm::dyn_cast<llvm::GlobalVariable>(val);
    }
    return nullptr;
}

const llvm::Function* EXTLLVM::getFunction(const char* Name) {
    auto val(getGlobalValue(Name));
    if (likely(val)) {
        return llvm::dyn_cast<llvm::Function>(val);
    }
    return nullptr;
}

}