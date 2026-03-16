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
#include "llvm/ExecutionEngine/Orc/LLJIT.h"
#include "llvm/ExecutionEngine/Orc/ThreadSafeModule.h"

#include "llvm/IR/CallingConv.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"

#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/OptimizationLevel.h"

#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Error.h"
#include "llvm/TargetParser/Host.h"

#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCDisassembler/MCDisassembler.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstPrinter.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"

#include <random>
#include <fstream>
#include <mutex>
#include <unordered_map>
#include <set>
#include <cmath>
#include <cstdlib>
#include <cstdarg>

#include <EXTLLVM.h>
#include <EXTClosureAddressTable.h>
#include <EXTThread.h>
#include <UNIV.h>
#include <SchemeFFI.h>
#include <TaskScheduler.h>
#include <Scheme.h>
#include <pcre.h>
#include <OSC.h>
#include <cmath>
#include <BranchPrediction.h>

#ifdef _WIN32
#include <malloc.h>
#else
#include <sys/types.h>
#endif

#ifdef __linux__
#include <sys/syscall.h>
#endif

#ifdef _WIN32
#include <experimental/buffer>
#include <experimental/executor>
#include <experimental/internet>
#include <experimental/io_context>
#include <experimental/net>
#include <experimental/netfwd>
#include <experimental/socket>
#include <experimental/timer>
#else
#include <sys/errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>         /* host to IP resolution       */
#include <sys/fcntl.h>
#include <arpa/inet.h>
#endif

#include <chrono>
#include <thread>

#ifndef _WIN32
#include <unistd.h>
#endif

#include "SchemeProcess.h"

// llvm_scheme foreign function -> string name
// also is not thread safe!
std::map<foreign_func, std::string> LLVM_SCHEME_FF_MAP;

EXPORT void* malloc16(size_t Size)
{
    if (!Size) {
        return nullptr;
    }
#ifdef _WIN32
    return _aligned_malloc(Size, 16);
#else
    Size = (Size + 15) & ~size_t(15);
    return std::aligned_alloc(16, Size);
#endif
}

EXPORT void free16(void* Ptr) {
#ifdef _WIN32
    _aligned_free(Ptr);
#else
    std::free(Ptr);
#endif
}

// Portable conversion from 80-bit extended precision (big-endian) to double.
// Used for reading AIFF audio files, which store sample rate in this format.
// Format: 1 sign bit, 15 exponent bits, 64 mantissa bits (with explicit integer bit)
EXPORT double fp80_to_double_portable(const unsigned char* bytes)
{
    // Read big-endian 80-bit value
    unsigned int exponent = (unsigned(bytes[0]) << 8) | bytes[1];
    uint64_t mantissa = (uint64_t(bytes[2]) << 56) | (uint64_t(bytes[3]) << 48) |
                        (uint64_t(bytes[4]) << 40) | (uint64_t(bytes[5]) << 32) |
                        (uint64_t(bytes[6]) << 24) | (uint64_t(bytes[7]) << 16) |
                        (uint64_t(bytes[8]) << 8)  | uint64_t(bytes[9]);

    // Extract sign bit
    int sign = (exponent >> 15) & 1;
    exponent &= 0x7FFF;

    // Handle special cases.
    if (exponent == 0 && mantissa == 0) {
        return sign ? -0.0 : 0.0;
    }
    if (exponent == 0x7FFF) {
        // Infinity or NaN - for audio sample rates, this shouldn't happen.
        return sign ? -INFINITY : INFINITY;
    }

    // Convert to double.
    // x86_fp80 exponent bias is 16383, double bias is 1023
    int64_t exp_unbiased = int64_t(exponent) - 16383;

    // The mantissa has an explicit integer bit (bit 63).
    // Double has implicit integer bit, so we need to handle this.
    union { uint64_t i; double d; } result;

    if (mantissa & (1ULL << 63)) {
        // Normal number - integer bit is set.
        // Remove the integer bit and shift mantissa to fit in double's 52-bit mantissa.
        uint64_t double_mantissa = (mantissa & 0x7FFFFFFFFFFFFFFFULL) >> 11;
        int64_t double_exp = exp_unbiased + 1023;

        if (double_exp >= 2047) {
            // Overflow to infinity.
            return sign ? -INFINITY : INFINITY;
        } else if (double_exp <= 0) {
            // Underflow - denormalized or zero.
            return sign ? -0.0 : 0.0;
        } else {
            // Pack into IEEE 754 double format.
            result.i = (uint64_t(sign) << 63) | (uint64_t(double_exp) << 52) | double_mantissa;
        }
    } else {
        // Denormalized or pseudo-denormalized - rare for audio sample rates.
        return sign ? -0.0 : 0.0;
    }

    return result.d;
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
EXPORT void llvm_runtime_error(int error, void* arg)
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

EXPORT void llvm_schedule_callback(long long time, void* dat)
{
  //printf("scheduled callback %lld\n",time);
  extemp::SchemeProcess* proc = extemp::SchemeProcess::I(); //extemp::SchemeProcess::I()->extemporeCallback(time,dat);

  uint64_t current_time = time; //task->getStartTime();
  uint64_t duration = 1000000000; //task->getDuration();
  extemp::TaskScheduler::I()->addTask(current_time, duration, proc->getExtemporeCallback(), dat, 0, true);
  return;
}

EXPORT void* llvm_get_function_ptr(char* fname)
{
  return reinterpret_cast<void*>(extemp::EXTLLVM::getFunctionAddress(fname));
}

EXPORT char* extitoa(int64_t val)
{
    static thread_local char buf[32];
    snprintf(buf, sizeof(buf), "%" PRId64, val);
    return buf;
}

EXPORT void llvm_send_udp(char* host, int port, void* message, int message_length)
{
  int length = message_length;

#ifdef _WIN32 // TODO: This should use WinSock on Windows
  std::experimental::net::io_context context;
  // std::experimental::net::ip::udp::resolver::iterator end;
  std::experimental::net::ip::udp::resolver resolver(context);
  std::stringstream ss;
  ss << port;
  std::experimental::net::ip::udp::resolver::results_type res = resolver.resolve(std::experimental::net::ip::udp::v4(), host, ss.str());
  auto iter = res.begin();
  auto end = res.end();
  std::experimental::net::ip::udp::endpoint sa = *iter;

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


#ifdef _WIN32
  std::experimental::net::ip::udp::socket* fd = 0;
#else
  int fd = 0;
#endif

#ifdef _WIN32
  int err = 0;
  std::experimental::net::io_context service;
  std::experimental::net::ip::udp::socket socket(service);
  socket.open(std::experimental::net::ip::udp::v4());
  socket.send_to(std::experimental::net::buffer(message, length), sa);
#else
  fd = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);

  //////// Dr Offig addition ////////
  int broadcastEnable = 1;
  int ret = setsockopt(fd, SOL_SOCKET, SO_BROADCAST, &broadcastEnable, sizeof(broadcastEnable));
  if (ret) { printf("Error: Could not open set socket to broadcast mode\n"); }
  //////////////////////////////////////

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
EXPORT void llvm_print_pointer(void* ptr)
{
    printf("llvm:ptr:>%p -- %" PRId64 "\n",ptr,*((int64_t*)ptr));
    return;
}

EXPORT void llvm_print_i32(int32_t num)
{
    printf("llvm:i32:>%d\n",num);
    return;
}

EXPORT void llvm_print_i64(int64_t num)
{
    printf("llvm:i64:>%" PRId64 "\n",num);
    return;
}

EXPORT void llvm_print_f32(float num)
{
    printf("llvm:f32:>%f\n",num);
    return;
}

EXPORT void llvm_print_f64(double num)
{
    printf("llvm:f64:>%f\n",num);
    return;
}

// these shouldn't ever be large, so it should be ok to cast to signed
// int for returning into xtlang (which prefers signed ints). I hope
// this doesn't come back to bite me one day.
static thread_local std::minstd_rand* sRandGen;

EXPORT double imp_randd()
{
    if (unlikely(!sRandGen)) {
        sRandGen = new std::minstd_rand(time(nullptr));
    }
    // The existing implementation *COULD* (p = 1 / RAND_MAX) return 1!, but I don't think that was intended
    return std::uniform_real_distribution<double>()(*sRandGen);
}

EXPORT float imp_randf()
{
    return imp_randd();
}

EXPORT int64_t imp_rand1_i64(int64_t Limit)
{
    return imp_randd() * Limit;
}

EXPORT int64_t imp_rand2_i64(int64_t Start, int64_t Limit)
{
    return imp_randd() * (Limit - Start) + Start;
}

EXPORT int32_t imp_rand1_i32(int32_t Limit)
{
    return imp_randd() * Limit;
}

EXPORT int32_t imp_rand2_i32(int32_t Start, int32_t Limit)
{
    return imp_randd() * (Limit - Start) + Start;
}

EXPORT double imp_rand1_d(double Limit)
{
    return imp_randd() * Limit;
}

EXPORT double imp_rand2_d(double Start, double Limit)
{
    return imp_randd() * (Limit - Start) + Start;
}

EXPORT float imp_rand1_f(float Limit)
{
    return imp_randf() * Limit;
}

EXPORT float imp_rand2_f(float Start, float Limit)
{
    return imp_randf() * (Limit - Start) + Start;
}

///////////////////////////////////

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

static char* get_address_type(uint64_t id, extemp::ClosureAddressTable::closure_address_table* table)
{
    while (table)
    {
        if (table->id == id) {
            return table->type;
        }
        table = table->next;
    }
    // printf("Unable to locate id in closure environment c\n");
    return nullptr;
}

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
    std::memcpy(vname, tmp, 256);
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
  void*(*p)() = (void*(*)()) extemp::EXTLLVM::getFunctionAddress(funcname + getter);
  if (!p) {
    printf("Error attempting to set environment variable in closure %s.%s\n",fname,vname);
    return _sc->F;
  }

  size_t*** closur = (size_t***) p();
  size_t** closure = *closur;
  //uint32_t** closure = (uint32_t**) cptr_value(pair_car(args));
  extemp::ClosureAddressTable::closure_address_table* addy_table = (extemp::ClosureAddressTable::closure_address_table*) *(closure+0);
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
  uint32_t offset = extemp::ClosureAddressTable::get_address_offset(id,addy_table);

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

// ORC JIT
std::unique_ptr<llvm::orc::LLJIT> JIT = nullptr;
std::unique_ptr<llvm::orc::ThreadSafeContext> TSC = nullptr;

llvm::orc::ThreadSafeContext& getThreadSafeContext() {
    // Ensure the thread-safe context exists before returning a reference
    if (!TSC) {
        TSC = std::make_unique<llvm::orc::ThreadSafeContext>(
            std::make_unique<llvm::LLVMContext>());
    }
    return *TSC;
}

std::vector<llvm::Module*> Ms;
int64_t LLVM_COUNT = 0l;
bool OPTIMIZE_COMPILES = true;
bool VERIFY_COMPILES = true;
int OPTIMIZATION_LEVEL = 2;  // Default to O2

// Map from counter-less adhoc names to their full counter-ful names.
// e.g. "foo_adhoc_W2k4K_native" -> "foo_adhoc_9_W2k4K_native"
// The xtlang get_native_fptr macro generates names without the counter,
// but the compiled functions include an adhoc counter in their names.
static std::unordered_map<std::string, std::string> sAdhocAliases;

static std::string stripAdhocCounter(std::string_view name) {
    auto pos = name.find("_adhoc_");
    if (pos == std::string_view::npos) return "";
    size_t afterAdhoc = pos + 7;
    size_t counterEnd = afterAdhoc;
    while (counterEnd < name.size() && name[counterEnd] >= '0' && name[counterEnd] <= '9') {
        counterEnd++;
    }
    if (counterEnd > afterAdhoc && counterEnd < name.size() && name[counterEnd] == '_') {
        return std::string(name.substr(0, afterAdhoc)) + std::string(name.substr(counterEnd + 1));
    }
    return "";
}

void registerAdhocAlias(std::string_view fullName) {
    auto alias = stripAdhocCounter(fullName);
    if (!alias.empty()) {
        sAdhocAliases[alias] = std::string(fullName);
    }
}

// Get function address - main lookup function
uint64_t getFunctionAddress(std::string_view name) {
    if (!JIT) {
        return 0;
    }

    auto sym = JIT->lookup(llvm::StringRef(name.data(), name.size()));
    if (!sym) {
        llvm::consumeError(sym.takeError());
        // Fall back to counter-less adhoc alias lookup
        auto it = sAdhocAliases.find(std::string(name));
        if (it != sAdhocAliases.end()) {
            auto sym2 = JIT->lookup(it->second);
            if (sym2) return sym2->getValue();
            llvm::consumeError(sym2.takeError());
        }
        return 0;
    }
    return sym->getValue();
}

// Remove a single symbol from the JIT - called from Scheme via llvm:erase-function
bool removeSymbol(const std::string& name) {
    if (!JIT) return false;

    auto& ES = JIT->getExecutionSession();
    auto& JD = JIT->getMainJITDylib();

    // Try to remove both mangled and unmangled versions
    for (const auto& tryName : {name, "_" + name}) {
        llvm::orc::SymbolNameSet toRemove;
        toRemove.insert(ES.intern(tryName));
        if (auto err = JD.remove(toRemove)) {
            llvm::consumeError(std::move(err));
        }
    }
    return true;
}

// Add a module to the JIT
// Symbol removal for redefinition is handled by Scheme calling llvm:erase-function
// BEFORE sending the IR to be compiled. This ensures symbols are only removed
// when we actually intend to redefine them.
llvm::Error addTrackedModule(llvm::orc::ThreadSafeModule TSM, const std::vector<std::string>& symbolNames) {
    if (!JIT) return llvm::make_error<llvm::StringError>("JIT not initialized", llvm::inconvertibleErrorCode());

    if (auto err = JIT->addIRModule(std::move(TSM))) {
        return err;
    }

    return llvm::Error::success();
}

EXPORT const char* llvm_disassemble(const unsigned char* Code, int syntax)
{
    size_t code_size = 1024 * 100;
    std::string Error;

    // Get target triple from host
    std::string TripleName = llvm::sys::getProcessTriple();
    llvm::Triple Triple(TripleName);

    // Look up target
    const llvm::Target* TheTarget = llvm::TargetRegistry::lookupTarget(TripleName, Error);
    if (!TheTarget) {
        std::string errMsg = "Disassembler error: " + Error;
        return strdup(errMsg.c_str());
    }

    std::unique_ptr<const llvm::MCRegisterInfo> MRI(TheTarget->createMCRegInfo(TripleName));
    if (!MRI) return strdup("Failed to create MCRegisterInfo");

    llvm::MCTargetOptions MCOptions;
    std::unique_ptr<const llvm::MCAsmInfo> AsmInfo(TheTarget->createMCAsmInfo(*MRI, TripleName, MCOptions));
    if (!AsmInfo) return strdup("Failed to create MCAsmInfo");

    std::unique_ptr<const llvm::MCSubtargetInfo> STI(TheTarget->createMCSubtargetInfo(TripleName, "", ""));
    if (!STI) return strdup("Failed to create MCSubtargetInfo");

    std::unique_ptr<const llvm::MCInstrInfo> MII(TheTarget->createMCInstrInfo());
    if (!MII) return strdup("Failed to create MCInstrInfo");

    llvm::MCContext Ctx(Triple, AsmInfo.get(), MRI.get(), STI.get());
    std::unique_ptr<llvm::MCDisassembler> DisAsm(TheTarget->createMCDisassembler(*STI, Ctx));
    if (!DisAsm) return strdup("Failed to create MCDisassembler");

    std::unique_ptr<llvm::MCInstPrinter> IP(TheTarget->createMCInstPrinter(Triple, syntax, *AsmInfo, *MII, *MRI));
    if (!IP) return strdup("Failed to create MCInstPrinter");

    IP->setPrintImmHex(true);

    std::string out_str;
    llvm::raw_string_ostream OS(out_str);
    llvm::ArrayRef<uint8_t> mem(Code, code_size);
    uint64_t size;
    uint64_t index;
    OS << "\n";
    for (index = 0; index < code_size; index += size) {
        llvm::MCInst Inst;
        if (DisAsm->getInstruction(Inst, size, mem.slice(index), index, llvm::nulls())) {
            auto instSize(*reinterpret_cast<const size_t*>(Code + index));
            if (instSize <= 0) {
                break;
            }
            OS.indent(4);
            OS.write("0x", 2);
            OS.write_hex(size_t(Code) + index);
            OS.write(": ", 2);
            OS.write_hex(instSize);
            IP->printInst(&Inst, 0, "", *STI, OS);
            OS << "\n";
        } else if (!size) {
            size = 1;
        }
    }
    return strdup(OS.str().c_str());
}

static extemp::CMG DestroyMallocZoneWithDelayCM(
        [](extemp::TaskI* Task)->void {
            extemp::EXTZones::llvm_zone_destroy(static_cast<extemp::Task<llvm_zone_t*>*>(Task)->getArg());
        });

EXPORT void llvm_destroy_zone_after_delay(llvm_zone_t* Zone, uint64_t Delay)
{
    extemp::TaskScheduler::I()->add(new extemp::Task<llvm_zone_t*>(extemp::UNIV::TIME + Delay, extemp::UNIV::SECOND(),
            &DestroyMallocZoneWithDelayCM, Zone));
}

static extemp::CMG FreeWithDelayCM(
        [](extemp::TaskI* Task)->void {
            free(static_cast<extemp::Task<char*>*>(Task)->getArg());
        });

EXPORT void free_after_delay(char* Data, double Delay)
{
    extemp::TaskScheduler::I()->add(new extemp::Task<char*>(extemp::UNIV::TIME + Delay, extemp::UNIV::SECOND(),
            &FreeWithDelayCM, Data));
}

#if 0 // TODO: What is this needed for???
static long long llvm_get_next_prime(long long start)
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
#endif

EXPORT void ascii_text_color_extern(int32_t Bold, int32_t Foreground, int32_t Background)
{
    ascii_text_color(Bold, Foreground, Background);
}

// CATEGORY: clock

EXPORT double clock_clock()
{
    return getRealTime() + extemp::UNIV::CLOCK_OFFSET;
}

EXPORT double audio_clock_base()
{
    return extemp::UNIV::AUDIO_CLOCK_BASE;
}

EXPORT double audio_clock_now()
{
    return extemp::UNIV::AUDIO_CLOCK_NOW;
}

// CATEGORY: native mutex

EXPORT void* mutex_create()
{
    auto mutex(new EXTMutex);
    return mutex;
}

EXPORT int mutex_destroy(void* Mutex)
{
    delete reinterpret_cast<EXTMutex*>(Mutex);
    return 0;
}

EXPORT int mutex_lock(void* Mutex)
{
    reinterpret_cast<EXTMutex*>(Mutex)->lock();
    return 0;
}

EXPORT int mutex_unlock(void* Mutex)
{
    reinterpret_cast<EXTMutex*>(Mutex)->unlock();
    return 0;
}

EXPORT int mutex_trylock(void* Mutex)
{
    return reinterpret_cast<EXTMutex*>(Mutex)->try_lock();
}

// CATEGORY: native thread

EXPORT void* thread_fork(EXTThread::function_type Start, void* Args) {
    auto thread(new extemp::EXTThread(Start, Args, "xt_fork"));
    thread->start();
    return thread;
}

EXPORT void thread_destroy(void* Thread)
{
    delete reinterpret_cast<EXTThread*>(Thread);
}

EXPORT int thread_join(void* Thread)
{
    return reinterpret_cast<EXTThread*>(Thread)->join();
}

EXPORT int thread_kill(void* Thread)
{
    return reinterpret_cast<EXTThread*>(Thread)->kill();
}

EXPORT int thread_equal(void* Thread1, void* Thread2)
{
    return Thread1 == Thread2;
}

EXPORT int thread_equal_self(void* Thread)
{
    return reinterpret_cast<EXTThread*>(Thread)->isCurrentThread();
}

EXPORT void* thread_self()
{
    return EXTThread::activeThread();
}

EXPORT int64_t thread_sleep(int64_t Secs, int64_t Nanosecs)
{
    std::this_thread::sleep_for(std::chrono::seconds(Secs) + std::chrono::nanoseconds(Nanosecs));
    return 0;
}


// Register a symbol with the JIT
static void registerSymbol(const char* name, void* addr) {
    if (!JIT) return;
    auto& ES = JIT->getExecutionSession();
    auto& JD = JIT->getMainJITDylib();

    llvm::orc::SymbolMap Symbols;
    Symbols[ES.intern(name)] = {
        llvm::orc::ExecutorAddr::fromPtr(addr),
        llvm::JITSymbolFlags::Exported
    };

    auto err = JD.define(llvm::orc::absoluteSymbols(std::move(Symbols)));
    if (err) {
        llvm::consumeError(std::move(err));
    }
}

void initLLVM()
{
    if (unlikely(JIT)) {
        return;
    }

    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();
    llvm::InitializeNativeTargetDisassembler();

    // Create thread-safe context
    TSC = std::make_unique<llvm::orc::ThreadSafeContext>(std::make_unique<llvm::LLVMContext>());

    // Build LLJIT
    auto JITBuilder = llvm::orc::LLJITBuilder();

    // Configure target machine
    std::string triple = llvm::sys::getProcessTriple();
    std::string cpu = extemp::UNIV::CPU.empty() ?
        std::string(llvm::sys::getHostCPUName()) : extemp::UNIV::CPU;

    // Get host features
    auto HostFeatures = llvm::sys::getHostCPUFeatures();
    std::vector<std::string> featureVec;
    std::string featureString;
        for (auto& feature : HostFeatures) {
        std::string featureStr;
        featureStr += (feature.getValue() ? "+" : "-");
        featureStr += feature.getKey().str();
        featureVec.push_back(featureStr);
        if (!featureString.empty()) featureString += ",";
        featureString += featureStr;
    }

    // Store triple for later use.
    if (extemp::UNIV::ARCH.empty()) {
        extemp::UNIV::ARCH = triple;
		  }

    // Set up target machine builder with actual CPU features.
    JITBuilder.setJITTargetMachineBuilder(
        llvm::orc::JITTargetMachineBuilder(llvm::Triple(triple))
            .setCPU(cpu)
            .addFeatures(featureVec)
            .setCodeGenOptLevel(llvm::CodeGenOptLevel::Aggressive));

    // Create the JIT.
    auto JITResult = JITBuilder.create();
    if (!JITResult) {
        std::cerr << "ERROR: Failed to create LLJIT: "
                  << llvm::toString(JITResult.takeError()) << std::endl;
        exit(1);
    }
    JIT = std::move(*JITResult);

    // Add DynamicLibrarySearchGenerator to make all process symbols available.
    auto& MainJD = JIT->getMainJITDylib();
    auto DLSGOrErr = llvm::orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(
        JIT->getDataLayout().getGlobalPrefix());
    if (!DLSGOrErr) {
        std::cerr << "ERROR: Failed to create DynamicLibrarySearchGenerator: "
                  << llvm::toString(DLSGOrErr.takeError()) << std::endl;
        exit(1);
    }
    MainJD.addGenerator(std::move(*DLSGOrErr));

    // Print configuration.
    ascii_normal();
    std::cout << "ARCH           : " << std::flush;
    ascii_info();
    std::cout << triple << std::endl;

    if (!cpu.empty()) {
        ascii_normal();
        std::cout << "CPU            : " << std::flush;
        ascii_info();
        std::cout << cpu << std::endl;
    }

    ascii_normal();
    std::cout << "LLVM           : " << std::flush;
    ascii_info();
    std::cout << LLVM_VERSION_STRING;
    std::cout << " ORC JIT" << std::endl;
    ascii_normal();

    // Register built-in symbols with the JIT.

    // Zone memory management functions
    registerSymbol("llvm_zone_destroy", (void*)&extemp::EXTZones::llvm_zone_destroy);
    registerSymbol("llvm_zone_malloc", (void*)&extemp::EXTZones::llvm_zone_malloc);
    registerSymbol("llvm_zone_malloc_from_current_zone", (void*)&extemp::EXTZones::llvm_zone_malloc_from_current_zone);
    registerSymbol("llvm_zone_print", (void*)&extemp::EXTZones::llvm_zone_print);
    registerSymbol("llvm_zone_ptr_size", (void*)&extemp::EXTZones::llvm_zone_ptr_size);
    registerSymbol("llvm_zone_copy_ptr", (void*)&extemp::EXTZones::llvm_zone_copy_ptr);
    registerSymbol("llvm_ptr_in_zone", (void*)&extemp::EXTZones::llvm_ptr_in_zone);
    registerSymbol("llvm_ptr_in_current_zone", (void*)&extemp::EXTZones::llvm_ptr_in_current_zone);
    registerSymbol("llvm_pop_zone_stack", (void*)&extemp::EXTZones::llvm_pop_zone_stack);
    registerSymbol("llvm_zone_callback_setup", (void*)&extemp::EXTZones::llvm_zone_callback_setup);
    registerSymbol("llvm_peek_zone_stack_extern", (void*)&extemp::EXTZones::llvm_peek_zone_stack_extern);
    registerSymbol("llvm_push_zone_stack_extern", (void*)&extemp::EXTZones::llvm_push_zone_stack_extern);
    registerSymbol("llvm_zone_create_extern", (void*)&extemp::EXTZones::llvm_zone_create_extern);
    registerSymbol("llvm_destroy_zone_after_delay", (void*)&llvm_destroy_zone_after_delay);

    // Closure address table functions
    registerSymbol("get_address_offset", (void*)&extemp::ClosureAddressTable::get_address_offset);
    registerSymbol("add_address_table", (void*)&extemp::ClosureAddressTable::add_address_table);
    registerSymbol("get_address_table", (void*)&extemp::ClosureAddressTable::get_address_table);
    registerSymbol("check_address_exists", (void*)&extemp::ClosureAddressTable::check_address_exists);
    registerSymbol("check_address_type", (void*)&extemp::ClosureAddressTable::check_address_type);
    registerSymbol("string_hash", (void*)&string_hash);
    registerSymbol("swap64i", (void*)&swap64i);
    registerSymbol("swap64f", (void*)&swap64f);
    registerSymbol("swap32i", (void*)&swap32i);
    registerSymbol("swap32f", (void*)&swap32f);
    registerSymbol("unswap64i", (void*)&unswap64i);
    registerSymbol("unswap64f", (void*)&unswap64f);
    registerSymbol("unswap32i", (void*)&unswap32i);
    registerSymbol("unswap32f", (void*)&unswap32f);
    registerSymbol("rsplit", (void*)&rsplit);
    registerSymbol("rmatch", (void*)&rmatch);
    registerSymbol("rreplace", (void*)&rreplace);
    registerSymbol("r64value", (void*)&r64value);
    registerSymbol("mk_double", (void*)&mk_double);
    registerSymbol("r32value", (void*)&r32value);
    registerSymbol("mk_float", (void*)&mk_float);
    registerSymbol("mk_i64", (void*)&mk_i64);
    registerSymbol("mk_i32", (void*)&mk_i32);
    registerSymbol("mk_i16", (void*)&mk_i16);
    registerSymbol("mk_i8", (void*)&mk_i8);
    registerSymbol("mk_i1", (void*)&mk_i1);
    registerSymbol("string_value", (void*)&string_value);
    registerSymbol("mk_string", (void*)&mk_string);
    registerSymbol("cptr_value", (void*)&cptr_value);
    registerSymbol("mk_cptr", (void*)&mk_cptr);
    registerSymbol("sys_sharedir", (void*)&sys_sharedir);
    registerSymbol("sys_slurp_file", (void*)&sys_slurp_file);
    registerSymbol("fp80_to_double_portable", (void*)&fp80_to_double_portable);

    return;
}

} // namespace EXTLLVM
} // namespace extemp

static std::unordered_map<std::string, const llvm::GlobalValue*> sGlobalMap;

// Cleanup handler to avoid segfaults during static destruction
static void cleanupLLVM() {
    sGlobalMap.clear();
    extemp::EXTLLVM::Ms.clear();
    // Reset the JIT to release resources.
    if (extemp::EXTLLVM::JIT) {
        extemp::EXTLLVM::JIT.reset();
    }
}

static struct EXTLLVMCleanupRegistrar {
    EXTLLVMCleanupRegistrar() {
        std::atexit(cleanupLLVM);
    }
} sCleanupRegistrar;

namespace extemp {

void EXTLLVM::addModule(llvm::Module* Module)
{
    for (const auto& function : Module->getFunctionList()) {
        std::string str;
        llvm::raw_string_ostream stream(str);
        function.printAsOperand(stream, false);
        std::string funcName = stream.str().substr(1);
        auto result(sGlobalMap.insert(std::make_pair(funcName, &function)));
        if (!result.second) {
            result.first->second = &function;
        }
    }
    for (const auto& global : Module->globals()) {
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

void EXTLLVM::removeFromGlobalMap(const std::string& name) {
    sGlobalMap.erase(name);
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
} // namespace extemp
