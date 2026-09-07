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
#include "llvm/Config/llvm-config.h"  // for LLVM_VERSION_STRING
#include "llvm/ExecutionEngine/JITLink/JITLinkMemoryManager.h"
#include "llvm/ExecutionEngine/Orc/LLJIT.h"
#include "llvm/ExecutionEngine/Orc/ObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/Orc/ThreadSafeModule.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"

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
#include "llvm/Support/Memory.h"
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

#include <bit>
#include <random>
#include <fstream>
#include <mutex>
#include <algorithm>
#include <unordered_map>
#include <unordered_set>
#include <set>
#include <cmath>
#include <cstdlib>
#include <cstdarg>
#include <ctime>

#include <EXTLLVM.h>
#include <ext/NetUtil.h>
#include <EXTClosureAddressTable.h>
#include <EXTThread.h>
#include <UNIV.h>
#include <SchemeFFI.h>
#include <TaskScheduler.h>
#include <SchemeS7.h>
#include <SchemeS7Private.h>
#include <OSC.h>

#ifdef _WIN32
#include <malloc.h>
#else
#include <sys/types.h>
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
#include <netdb.h> /* host to IP resolution       */
#include <sys/fcntl.h>
#include <arpa/inet.h>
#endif

#include <chrono>
#include <thread>

#ifndef _WIN32
#include <unistd.h>
#endif

#include "SchemeProcess.h"

EXPORT void* malloc16(size_t Size) {
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
EXPORT double fp80_to_double_portable(const unsigned char* bytes) {
    // Read big-endian 80-bit value
    unsigned int exponent = (unsigned(bytes[0]) << 8) | bytes[1];
    uint64_t mantissa = (uint64_t(bytes[2]) << 56) | (uint64_t(bytes[3]) << 48) |
                        (uint64_t(bytes[4]) << 40) | (uint64_t(bytes[5]) << 32) |
                        (uint64_t(bytes[6]) << 24) | (uint64_t(bytes[7]) << 16) |
                        (uint64_t(bytes[8]) << 8) | uint64_t(bytes[9]);

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

    // The mantissa has an explicit integer bit (bit 63); double has an implicit
    // one, so handle it explicitly.
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
            uint64_t result_bits =
                (uint64_t(sign) << 63) | (uint64_t(double_exp) << 52) | double_mantissa;
            return std::bit_cast<double>(result_bits);
        }
    } else {
        // Denormalized or pseudo-denormalized - rare for audio sample rates.
        return sign ? -0.0 : 0.0;
    }
}

// LLVM RUNTIME ERROR
EXPORT void llvm_runtime_error(int error, void* arg) {
    ascii_error();
    switch (error) {
    case 1:
        printf("LLVM zptr_copy - invalid zptr! %p\n", arg);
        break;
    default:
        break;
    }
    ascii_normal();
    return;
}

EXPORT void llvm_schedule_callback(long long time, void* dat) {
    // printf("scheduled callback %lld\n",time);
    extemp::SchemeProcess* proc =
        extemp::SchemeProcess::I();  // extemp::SchemeProcess::I()->extemporeCallback(time,dat);

    uint64_t current_time = time;    // task->getStartTime();
    uint64_t duration = 1000000000;  // task->getDuration();
    extemp::TaskScheduler::I()->addTask(current_time, duration, proc->getExtemporeCallback(), dat,
                                        0, true);
    return;
}

EXPORT void* llvm_get_function_ptr(char* fname) {
    return reinterpret_cast<void*>(extemp::EXTLLVM::getFunctionAddress(fname));
}

EXPORT char* extitoa(int64_t val) {
    static thread_local char buf[32];
    snprintf(buf, sizeof(buf), "%" PRId64, val);
    return buf;
}

EXPORT void llvm_send_udp(char* host, int port, void* message, int message_length) {
    int length = message_length;

#ifdef _WIN32  // TODO: This should use WinSock on Windows
    std::experimental::net::io_context context;
    // std::experimental::net::ip::udp::resolver::iterator end;
    std::experimental::net::ip::udp::resolver resolver(context);
    std::stringstream ss;
    ss << port;
    std::experimental::net::ip::udp::resolver::results_type res =
        resolver.resolve(std::experimental::net::ip::udp::v4(), host, ss.str());
    auto iter = res.begin();
    auto end = res.end();
    std::experimental::net::ip::udp::endpoint sa = *iter;

#else
    struct sockaddr_in sa;

    uint32_t resolved = extemp::net_util::resolve_ipv4(host);
    if (!resolved) {
        printf("OSC Error: Could not resolve host name\n");
        return;
    }

    memset(&sa, 0, sizeof(sa));
    sa.sin_family = AF_INET;
    sa.sin_port = htons(port);
    sa.sin_addr.s_addr = resolved;
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
    if (ret) {
        printf("Error: Could not open set socket to broadcast mode\n");
    }
    //////////////////////////////////////

    int err = sendto(fd, message, length, 0, (struct sockaddr*)&sa, sizeof(sa));
    close(fd);
#endif
    if (err < 0) {
        if (err == EMSGSIZE) {
            printf("Error: OSC message too large: UDP 8k message MAX\n");
        } else {
            printf("Error: Problem sending OSC message %d\n", err);
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
//   extemp::SchemeProcess* proc = extemp::SchemeProcess::I();
//   //extemp::SchemeProcess::I()->extemporeCallback(time,dat); ucontext_t* ctx =
//   proc->getContext(); return ctx;
// }
///////////////////////////////////////////////////

// these are helpers for runtime debugging in llvm
EXPORT void llvm_print_pointer(void* ptr) {
    printf("llvm:ptr:>%p -- %" PRId64 "\n", ptr, *((int64_t*)ptr));
    return;
}

EXPORT void llvm_print_i32(int32_t num) {
    printf("llvm:i32:>%d\n", num);
    return;
}

EXPORT void llvm_print_i64(int64_t num) {
    printf("llvm:i64:>%" PRId64 "\n", num);
    return;
}

EXPORT void llvm_print_f32(float num) {
    printf("llvm:f32:>%f\n", num);
    return;
}

EXPORT void llvm_print_f64(double num) {
    printf("llvm:f64:>%f\n", num);
    return;
}

// these shouldn't ever be large, so it should be ok to cast to signed
// int for returning into xtlang (which prefers signed ints). I hope
// this doesn't come back to bite me one day.
static thread_local std::minstd_rand sRandGen{
    static_cast<std::minstd_rand::result_type>(std::time(nullptr))};

EXPORT double xtc_randd() {
    // The existing implementation *COULD* (p = 1 / RAND_MAX) return 1!, but I don't think that was
    // intended
    return std::uniform_real_distribution<double>()(sRandGen);
}

EXPORT float xtc_randf() {
    return xtc_randd();
}

EXPORT int64_t xtc_rand1_i64(int64_t Limit) {
    return xtc_randd() * Limit;
}

EXPORT int64_t xtc_rand2_i64(int64_t Start, int64_t Limit) {
    return xtc_randd() * (Limit - Start) + Start;
}

EXPORT int32_t xtc_rand1_i32(int32_t Limit) {
    return xtc_randd() * Limit;
}

EXPORT int32_t xtc_rand2_i32(int32_t Start, int32_t Limit) {
    return xtc_randd() * (Limit - Start) + Start;
}

EXPORT double xtc_rand1_d(double Limit) {
    return xtc_randd() * Limit;
}

EXPORT double xtc_rand2_d(double Start, double Limit) {
    return xtc_randd() * (Limit - Start) + Start;
}

EXPORT float xtc_rand1_f(float Limit) {
    return xtc_randf() * Limit;
}

EXPORT float xtc_rand2_f(float Start, float Limit) {
    return xtc_randf() * (Limit - Start) + Start;
}

namespace extemp {
namespace EXTLLVM {

// ORC JIT
std::unique_ptr<llvm::orc::LLJIT> JIT = nullptr;
std::unique_ptr<llvm::orc::ThreadSafeContext> TSC = nullptr;

llvm::orc::ThreadSafeContext& getThreadSafeContext() {
    // Ensure the thread-safe context exists before returning a reference
    if (!TSC) {
        TSC = std::make_unique<llvm::orc::ThreadSafeContext>(std::make_unique<llvm::LLVMContext>());
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
//
// Written on the compile thread and read by llvm_get_function_ptr, which
// generated code calls from the scheduler/audio callback path, so reads take
// a shared lock. Resolving the alias at compile time would remove the lookup
// from that path altogether, but needs the runtime (xtc-codegen.xtm) to emit
// the full name.
static std::unordered_map<std::string, std::string> sAdhocAliases;
static std::shared_mutex sAdhocAliasesMutex;

static std::string stripAdhocCounter(std::string_view name) {
    auto pos = name.find("_adhoc_");
    if (pos == std::string_view::npos)
        return "";
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
        std::unique_lock<std::shared_mutex> lock(sAdhocAliasesMutex);
        sAdhocAliases[alias] = std::string(fullName);
    }
}

// Full name registered for a counter-less adhoc alias, or empty.
static std::string lookupAdhocAlias(std::string_view name) {
    std::shared_lock<std::shared_mutex> lock(sAdhocAliasesMutex);
    auto it = sAdhocAliases.find(std::string(name));
    return it == sAdhocAliases.end() ? std::string() : it->second;
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
        auto full = lookupAdhocAlias(name);
        if (!full.empty()) {
            auto sym2 = JIT->lookup(full);
            if (sym2)
                return sym2->getValue();
            llvm::consumeError(sym2.takeError());
        }
        return 0;
    }
    return sym->getValue();
}

// ---------------------------------------------------------------------------
// Module ownership
//
// Every module compiled by jitCompile is added under its own ResourceTracker
// so its machine code can be released once nothing needs it. What can go, and
// when, follows from how the xtlang driver (runtime/xtc-driver.xtm) redefines
// a closure: the first definition of `foo` produces a stub module holding
// @foo_var and the getter/native/scheme entry points, and every later
// redefinition produces a body module (functions only, e.g. @foo__12) plus a
// maker/setter module that references it. Scheme erases foo_maker and
// foo_setter before recompiling; it never erases a body by name.
//
//  * A module that defines a global variable is never removed as a unit --
//    other code may hold its addresses (foo_var, the AOT library modules, the
//    string-constant modules). Erasing one of its symbols unlinks just that
//    symbol and pins the module, which is what happened for every module
//    before trackers existed.
//  * A functions-only module is removed once all of its exported symbols have
//    been erased (the maker/setter module).
//  * A functions-only module that was referenced only by modules that have
//    since been removed (the body, once its maker goes) is an orphan. It is
//    released when a new module defines one of the symbols its referrer
//    exported -- i.e. once the redefinition that orphaned it has produced a
//    replacement maker. A redefinition that fails to compile therefore leaves
//    the old body in place, as before.
//  * Erasures are applied lazily, just before the next module is added. ORC
//    does not allow JITDylib::remove on a symbol followed by removing the
//    tracker that owns it, so a module is either removed whole or has its
//    erased symbols unlinked one by one; batching the erasures lets us tell
//    which.
//
// Removing a tracker unlinks its symbols at once, but the audio thread may be
// executing the old code at that moment (Scheme erases foo_maker before the
// new closure is installed), so the memory itself is only returned after
// kCodeGracePeriod. Closure objects that still point into a superseded
// definition after that period are dangling; that is the hazard
// llvm_destroy_zone_after_delay already accepts for zones.
// ---------------------------------------------------------------------------

namespace {

constexpr auto kCodeGracePeriod = std::chrono::seconds(10);

// Common face of the two memory-manager flavours below.
struct GraceReaper {
    virtual ~GraceReaper() = default;
    // Return memory retired more than kCodeGracePeriod ago (or all of it).
    virtual void reap(bool All) = 0;
};

// JITLink flavour (ELF and MachO): wraps the in-process manager and parks
// deallocations in a graveyard instead of returning them straight away.
class GraceJITLinkMemoryManager final : public llvm::jitlink::JITLinkMemoryManager,
                                        public GraceReaper {
    using clock = std::chrono::steady_clock;
    using Entry = std::pair<clock::time_point, std::vector<FinalizedAlloc>>;
    std::unique_ptr<llvm::jitlink::JITLinkMemoryManager> m_inner;
    std::mutex m_mutex;
    std::vector<Entry> m_graveyard;

  public:
    explicit GraceJITLinkMemoryManager(std::unique_ptr<llvm::jitlink::JITLinkMemoryManager> Inner)
        : m_inner(std::move(Inner)) {}
    ~GraceJITLinkMemoryManager() override {
        reap(true);
    }
    void allocate(const llvm::jitlink::JITLinkDylib* JD, llvm::jitlink::LinkGraph& G,
                  OnAllocatedFunction OnAllocated) override {
        m_inner->allocate(JD, G, std::move(OnAllocated));
    }
    void deallocate(std::vector<FinalizedAlloc> Allocs,
                    OnDeallocatedFunction OnDeallocated) override {
        {
            std::lock_guard<std::mutex> lock(m_mutex);
            m_graveyard.emplace_back(clock::now(), std::move(Allocs));
        }
        OnDeallocated(llvm::Error::success());
    }
    void reap(bool All) override {
        std::vector<std::vector<FinalizedAlloc>> expired;
        {
            std::lock_guard<std::mutex> lock(m_mutex);
            const auto now = clock::now();
            std::vector<Entry> kept;
            for (auto& entry : m_graveyard) {
                if (!All && now - entry.first < kCodeGracePeriod) {
                    kept.push_back(std::move(entry));
                } else {
                    expired.push_back(std::move(entry.second));
                }
            }
            m_graveyard = std::move(kept);
        }
        for (auto& allocs : expired) {
            m_inner->deallocate(std::move(allocs), [](llvm::Error Err) {
                if (Err) {
                    std::cerr << "LLVM: error releasing JIT memory: "
                              << llvm::toString(std::move(Err)) << std::endl;
                }
            });
        }
    }
};

// RuntimeDyld flavour (COFF): the mapper SectionMemoryManager allocates
// through, deferring only the release.
class GraceMemoryMapper final : public llvm::SectionMemoryManager::MemoryMapper,
                                public GraceReaper {
    using clock = std::chrono::steady_clock;
    using Entry = std::pair<clock::time_point, llvm::sys::MemoryBlock>;
    std::mutex m_mutex;
    std::vector<Entry> m_graveyard;

  public:
    ~GraceMemoryMapper() override {
        reap(true);
    }
    llvm::sys::MemoryBlock allocateMappedMemory(llvm::SectionMemoryManager::AllocationPurpose,
                                                size_t NumBytes,
                                                const llvm::sys::MemoryBlock* const NearBlock,
                                                unsigned Flags, std::error_code& EC) override {
        return llvm::sys::Memory::allocateMappedMemory(NumBytes, NearBlock, Flags, EC);
    }
    std::error_code protectMappedMemory(const llvm::sys::MemoryBlock& Block,
                                        unsigned Flags) override {
        return llvm::sys::Memory::protectMappedMemory(Block, Flags);
    }
    std::error_code releaseMappedMemory(llvm::sys::MemoryBlock& M) override {
        std::lock_guard<std::mutex> lock(m_mutex);
        m_graveyard.emplace_back(clock::now(), M);
        return std::error_code();
    }
    void reap(bool All) override {
        std::vector<llvm::sys::MemoryBlock> expired;
        {
            std::lock_guard<std::mutex> lock(m_mutex);
            const auto now = clock::now();
            std::vector<Entry> kept;
            for (auto& entry : m_graveyard) {
                if (!All && now - entry.first < kCodeGracePeriod) {
                    kept.push_back(entry);
                } else {
                    expired.push_back(entry.second);
                }
            }
            m_graveyard = std::move(kept);
        }
        for (auto& block : expired) {
            if (auto ec = llvm::sys::Memory::releaseMappedMemory(block)) {
                std::cerr << "LLVM: error releasing JIT memory: " << ec.message() << std::endl;
            }
        }
    }
};

struct TrackedModule {
    llvm::orc::ResourceTrackerSP tracker;
    std::vector<std::string> exports;        // strong symbols this module defines
    std::vector<std::string> imports;        // external symbols it references
    std::unique_ptr<llvm::Module> metadata;  // clone exposed via getModules()
    bool reclaimable = false;                // defines no global variables
    bool pinned = false;                     // a symbol was unlinked on its own
    bool everReferenced = false;             // a later module imported an export
    std::unordered_set<std::string> erased;    // exports Scheme has erased ...
    std::unordered_set<std::string> unlinked;  // ... and which are already unlinked
    // Non-empty for an orphan: exports of the module whose removal orphaned
    // it. Redefining any of them releases the orphan.
    std::unordered_set<std::string> orphanedBy;
};

std::mutex sModulesMutex;  // guards everything below
std::vector<std::unique_ptr<TrackedModule>> sModules;
std::unordered_map<std::string, TrackedModule*> sSymbolOwner;
std::unordered_map<std::string, int> sImportRefs;  // live modules importing a symbol
std::vector<std::string> sPendingErase;
GraceReaper* sGraceReaper = nullptr;
// Name -> global in a metadata clone, for getGlobalValue()/getFunction().
std::unordered_map<std::string, const llvm::GlobalValue*> sGlobalMap;

void indexMetadataModule(llvm::Module* Module) {
    for (const auto& function : Module->functions()) {
        sGlobalMap[function.getName().str()] = &function;
    }
    for (const auto& global : Module->globals()) {
        sGlobalMap[global.getName().str()] = &global;
    }
    Ms.push_back(Module);
}

void unlinkSymbol(const std::string& Name) {
    llvm::orc::SymbolNameSet names{JIT->mangleAndIntern(Name)};
    if (auto err = JIT->getMainJITDylib().remove(names)) {
        llvm::handleAllErrors(
            std::move(err), [](const llvm::orc::SymbolsNotFound&) {},  // already gone
            [&](const llvm::ErrorInfoBase& e) {
                std::cerr << "LLVM: could not remove symbol " << Name << ": " << e.message()
                          << std::endl;
            });
    }
}

bool unreferenced(const TrackedModule& M) {
    return std::none_of(M.exports.begin(), M.exports.end(),
                        [](const std::string& n) { return sImportRefs.count(n) != 0; });
}

// Unlink a module's symbols and retire its code and metadata. Modules that only
// this one referenced become orphans.
void removeModule(TrackedModule* M) {
    if (auto err = M->tracker->remove()) {
        std::cerr << "LLVM: could not remove module: " << llvm::toString(std::move(err))
                  << std::endl;
    }
    for (const auto& name : M->imports) {
        auto it = sImportRefs.find(name);
        if (it == sImportRefs.end() || --it->second > 0) {
            continue;
        }
        sImportRefs.erase(it);
        auto owner = sSymbolOwner.find(name);
        if (owner == sSymbolOwner.end()) {
            continue;
        }
        auto* B = owner->second;
        if (B->reclaimable && !B->pinned && B->everReferenced && unreferenced(*B)) {
            B->orphanedBy.insert(M->exports.begin(), M->exports.end());
        }
    }
    for (const auto& name : M->exports) {
        auto it = sSymbolOwner.find(name);
        if (it != sSymbolOwner.end() && it->second == M) {
            sSymbolOwner.erase(it);
        }
    }
    if (M->metadata) {
        for (auto it = sGlobalMap.begin(); it != sGlobalMap.end();) {
            if (it->second->getParent() == M->metadata.get()) {
                it = sGlobalMap.erase(it);
            } else {
                ++it;
            }
        }
        std::erase(Ms, M->metadata.get());
    }
    std::erase_if(sModules, [M](const std::unique_ptr<TrackedModule>& p) { return p.get() == M; });
}

void flushPendingErasures() {
    std::vector<TrackedModule*> touched;
    for (const auto& name : sPendingErase) {
        auto it = sSymbolOwner.find(name);
        if (it == sSymbolOwner.end()) {
            continue;
        }
        auto* M = it->second;
        M->erased.insert(name);
        if (std::find(touched.begin(), touched.end(), M) == touched.end()) {
            touched.push_back(M);
        }
        sSymbolOwner.erase(it);
    }
    sPendingErase.clear();
    for (auto* M : touched) {
        if (M->reclaimable && !M->pinned && M->erased.size() == M->exports.size()) {
            removeModule(M);
            continue;
        }
        for (const auto& name : M->erased) {
            if (M->unlinked.insert(name).second) {
                unlinkSymbol(name);
            }
        }
        M->pinned = true;
    }
}

// Release orphans whose referrer has just been replaced by a module defining
// one of the same symbols.
void releaseOrphans(const std::vector<std::string>& NewExports) {
    for (bool changed = true; changed;) {
        changed = false;
        for (auto& p : sModules) {
            auto* M = p.get();
            if (M->orphanedBy.empty()) {
                continue;
            }
            bool replaced = std::any_of(NewExports.begin(), NewExports.end(),
                                        [&](const std::string& n) { return M->orphanedBy.count(n); });
            if (replaced) {
                removeModule(M);  // invalidates the iteration
                changed = true;
                break;
            }
        }
    }
}

}  // namespace

ModuleSymbols collectModuleSymbols(const llvm::Module& M) {
    ModuleSymbols syms;
    auto consider = [&](const llvm::GlobalValue& GV, bool IsVariable) {
        if (GV.hasLocalLinkage()) {
            return;  // not a JIT symbol
        }
        if (GV.isDeclaration()) {
            if (!GV.use_empty()) {
                syms.imports.push_back(GV.getName().str());
            }
            return;
        }
        if (GV.hasLinkOnceLinkage() || GV.hasWeakLinkage()) {
            return;  // a bitcode.ll clone; the permanent runtime module owns the real one
        }
        syms.exports.push_back(GV.getName().str());
        if (IsVariable) {
            syms.definesGlobals = true;
        }
    };
    for (const auto& F : M.functions()) {
        if (!F.isIntrinsic()) {
            consider(F, false);
        }
    }
    for (const auto& G : M.globals()) {
        consider(G, true);
    }
    for (const auto& A : M.aliases()) {
        consider(A, true);
    }
    return syms;
}

llvm::Error addPermanentModule(llvm::orc::ThreadSafeModule TSM) {
    if (!JIT) {
        return llvm::make_error<llvm::StringError>("JIT not initialized",
                                                   llvm::inconvertibleErrorCode());
    }
    return JIT->addIRModule(std::move(TSM));
}

llvm::Error addTrackedModule(llvm::orc::ThreadSafeModule TSM, ModuleSymbols Symbols,
                             std::unique_ptr<llvm::Module> Metadata) {
    if (!JIT) {
        return llvm::make_error<llvm::StringError>("JIT not initialized",
                                                   llvm::inconvertibleErrorCode());
    }
    std::lock_guard<std::mutex> lock(sModulesMutex);
    if (sGraceReaper) {
        sGraceReaper->reap(false);
    }
    flushPendingErasures();
    auto RT = JIT->getMainJITDylib().createResourceTracker();
    if (auto err = JIT->addIRModule(RT, std::move(TSM))) {
        llvm::consumeError(RT->remove());
        return err;
    }
    auto M = std::make_unique<TrackedModule>();
    M->tracker = std::move(RT);
    M->exports = std::move(Symbols.exports);
    M->imports = std::move(Symbols.imports);
    M->metadata = std::move(Metadata);
    M->reclaimable = !Symbols.definesGlobals;
    for (const auto& name : M->exports) {
        sSymbolOwner[name] = M.get();
    }
    for (const auto& name : M->imports) {
        ++sImportRefs[name];
        auto owner = sSymbolOwner.find(name);
        if (owner != sSymbolOwner.end()) {
            owner->second->everReferenced = true;
        }
    }
    if (M->metadata) {
        indexMetadataModule(M->metadata.get());
    }
    const auto& newExports = M->exports;
    sModules.push_back(std::move(M));
    releaseOrphans(newExports);
    return llvm::Error::success();
}

llvm::Expected<llvm::orc::ResourceTrackerSP> addTransientModule(llvm::orc::ThreadSafeModule TSM) {
    if (!JIT) {
        return llvm::make_error<llvm::StringError>("JIT not initialized",
                                                   llvm::inconvertibleErrorCode());
    }
    std::lock_guard<std::mutex> lock(sModulesMutex);
    if (sGraceReaper) {
        sGraceReaper->reap(false);
    }
    auto RT = JIT->getMainJITDylib().createResourceTracker();
    if (auto err = JIT->addIRModule(RT, std::move(TSM))) {
        llvm::consumeError(RT->remove());
        return std::move(err);
    }
    return RT;
}

void removeTransientModule(llvm::orc::ResourceTrackerSP RT) {
    if (auto err = RT->remove()) {
        std::cerr << "LLVM: could not remove module: " << llvm::toString(std::move(err))
                  << std::endl;
    }
}

// Erase a symbol, called from Scheme via llvm:erase-function and friends.
// Symbols owned by a tracked module are queued and take effect when the next
// module is added (see the notes above); anything else -- an absolute symbol
// from bind-lib -- is unlinked now. Returns false if the symbol does not exist.
bool removeSymbol(const std::string& name) {
    if (!JIT) {
        return false;
    }
    std::lock_guard<std::mutex> lock(sModulesMutex);
    if (sSymbolOwner.count(name)) {
        sPendingErase.push_back(name);
        return true;
    }
    llvm::orc::SymbolNameSet names{JIT->mangleAndIntern(name)};
    if (auto err = JIT->getMainJITDylib().remove(names)) {
        bool removed = false;
        llvm::handleAllErrors(
            std::move(err), [](const llvm::orc::SymbolsNotFound&) {},
            [&](const llvm::ErrorInfoBase& e) {
                std::cerr << "LLVM: could not remove symbol " << name << ": " << e.message()
                          << std::endl;
            });
        return removed;
    }
    return true;
}

void removeFromGlobalMap(const std::string& name) {
    sGlobalMap.erase(name);
}

const llvm::GlobalValue* getGlobalValue(const char* Name) {
    auto iter(sGlobalMap.find(Name));
    if (iter != sGlobalMap.end()) [[likely]] {
        return iter->second;
    }
    return nullptr;
}

const llvm::GlobalVariable* getGlobalVariable(const char* Name) {
    auto val(getGlobalValue(Name));
    if (val) [[likely]] {
        return llvm::dyn_cast<llvm::GlobalVariable>(val);
    }
    return nullptr;
}

const llvm::Function* getFunction(const char* Name) {
    auto val(getGlobalValue(Name));
    if (val) [[likely]] {
        return llvm::dyn_cast<llvm::Function>(val);
    }
    return nullptr;
}

// Release the JIT (and the metadata clones that live in its context) before
// static destruction can get the order wrong.
static void cleanupLLVM() {
    {
        std::lock_guard<std::mutex> lock(sModulesMutex);
        sGlobalMap.clear();
        Ms.clear();
        sSymbolOwner.clear();
        sImportRefs.clear();
        sPendingErase.clear();
        sModules.clear();
    }
    sGraceReaper = nullptr;
    JIT.reset();
}

static struct EXTLLVMCleanupRegistrar {
    EXTLLVMCleanupRegistrar() {
        std::atexit(cleanupLLVM);
    }
} sCleanupRegistrar;

EXPORT const char* llvm_disassemble(const unsigned char* Code, int syntax) {
    size_t code_size = 1024 * 100;
    std::string Error;

    // Get target triple from host
    std::string TripleName = llvm::sys::getProcessTriple();
    llvm::Triple Triple(TripleName);

    // Look up target
    const llvm::Target* TheTarget = llvm::TargetRegistry::lookupTarget(Triple, Error);
    if (!TheTarget) {
        std::string errMsg = "Disassembler error: " + Error;
        return strdup(errMsg.c_str());
    }

    std::unique_ptr<const llvm::MCRegisterInfo> MRI(TheTarget->createMCRegInfo(Triple));
    if (!MRI)
        return strdup("Failed to create MCRegisterInfo");

    llvm::MCTargetOptions MCOptions;
    std::unique_ptr<const llvm::MCAsmInfo> AsmInfo(
        TheTarget->createMCAsmInfo(*MRI, Triple, MCOptions));
    if (!AsmInfo)
        return strdup("Failed to create MCAsmInfo");

    std::unique_ptr<const llvm::MCSubtargetInfo> STI(
        TheTarget->createMCSubtargetInfo(Triple, "", ""));
    if (!STI)
        return strdup("Failed to create MCSubtargetInfo");

    std::unique_ptr<const llvm::MCInstrInfo> MII(TheTarget->createMCInstrInfo());
    if (!MII)
        return strdup("Failed to create MCInstrInfo");

    llvm::MCContext Ctx(Triple, AsmInfo.get(), MRI.get(), STI.get());
    std::unique_ptr<llvm::MCDisassembler> DisAsm(TheTarget->createMCDisassembler(*STI, Ctx));
    if (!DisAsm)
        return strdup("Failed to create MCDisassembler");

    std::unique_ptr<llvm::MCInstPrinter> IP(
        TheTarget->createMCInstPrinter(Triple, syntax, *AsmInfo, *MII, *MRI));
    if (!IP)
        return strdup("Failed to create MCInstPrinter");

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
            if (instSize == 0) {
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

static extemp::CM DestroyMallocZoneWithDelayCM([](extemp::TaskI* Task) {
    extemp::EXTZones::llvm_zone_destroy(static_cast<extemp::Task<llvm_zone_t*>*>(Task)->getArg());
});

EXPORT void llvm_destroy_zone_after_delay(llvm_zone_t* Zone, uint64_t Delay) {
    extemp::TaskScheduler::I()->add(new extemp::Task<llvm_zone_t*>(
        extemp::UNIV::TIME + Delay, extemp::UNIV::SECOND(), &DestroyMallocZoneWithDelayCM, Zone));
}

static extemp::CM FreeWithDelayCM([](extemp::TaskI* Task) {
    free(static_cast<extemp::Task<char*>*>(Task)->getArg());
});

EXPORT void free_after_delay(char* Data, double Delay) {
    extemp::TaskScheduler::I()->add(new extemp::Task<char*>(
        extemp::UNIV::TIME + Delay, extemp::UNIV::SECOND(), &FreeWithDelayCM, Data));
}

#if 0  // TODO: What is this needed for???
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

EXPORT void ascii_text_color_extern(int32_t Bold, int32_t Foreground, int32_t Background) {
    ascii_text_color(Bold, Foreground, Background);
}

// CATEGORY: clock

EXPORT double clock_clock() {
    return getRealTime() + extemp::UNIV::CLOCK_OFFSET;
}

EXPORT double audio_clock_base() {
    return extemp::UNIV::AUDIO_CLOCK_BASE;
}

EXPORT double audio_clock_now() {
    return extemp::UNIV::AUDIO_CLOCK_NOW;
}

// CATEGORY: native mutex
//
// xtlang code stores the returned void* and may call lock more than once
// from the same thread without a matching unlock, so recursive semantics
// are preserved.

EXPORT void* mutex_create() {
    return new std::recursive_mutex;
}

EXPORT int mutex_destroy(void* Mutex) {
    delete reinterpret_cast<std::recursive_mutex*>(Mutex);
    return 0;
}

EXPORT int mutex_lock(void* Mutex) {
    reinterpret_cast<std::recursive_mutex*>(Mutex)->lock();
    return 0;
}

EXPORT int mutex_unlock(void* Mutex) {
    reinterpret_cast<std::recursive_mutex*>(Mutex)->unlock();
    return 0;
}

EXPORT int mutex_trylock(void* Mutex) {
    return reinterpret_cast<std::recursive_mutex*>(Mutex)->try_lock();
}

// CATEGORY: native thread

EXPORT void* thread_fork(EXTThread::function_type Start, void* Args) {
    auto thread(new extemp::EXTThread(Start, Args, "xt_fork"));
    thread->start();
    return thread;
}

EXPORT void thread_destroy(void* Thread) {
    delete reinterpret_cast<EXTThread*>(Thread);
}

EXPORT int thread_join(void* Thread) {
    return reinterpret_cast<EXTThread*>(Thread)->join();
}

EXPORT int thread_kill(void* Thread) {
    return reinterpret_cast<EXTThread*>(Thread)->kill();
}

EXPORT int thread_equal(void* Thread1, void* Thread2) {
    return Thread1 == Thread2;
}

EXPORT int thread_equal_self(void* Thread) {
    return reinterpret_cast<EXTThread*>(Thread)->isCurrentThread();
}

EXPORT void* thread_self() {
    return EXTThread::activeThread();
}

EXPORT int64_t thread_sleep(int64_t Secs, int64_t Nanosecs) {
    std::this_thread::sleep_for(std::chrono::seconds(Secs) + std::chrono::nanoseconds(Nanosecs));
    return 0;
}

// Runtime entry points generated code calls by name. They are defined in the
// main JITDylib as absolute symbols once the JIT exists; anything else
// resolves through the DynamicLibrarySearchGenerator.
static const std::pair<const char*, void*> kRuntimeSymbols[] = {
    {"llvm_zone_destroy", (void*)&extemp::EXTZones::llvm_zone_destroy},
    {"llvm_zone_malloc", (void*)&extemp::EXTZones::llvm_zone_malloc},
    {"llvm_zone_malloc_from_current_zone", (void*)&extemp::EXTZones::llvm_zone_malloc_from_current_zone},
    {"llvm_zone_print", (void*)&extemp::EXTZones::llvm_zone_print},
    {"llvm_zone_ptr_size", (void*)&extemp::EXTZones::llvm_zone_ptr_size},
    {"llvm_zone_copy_ptr", (void*)&extemp::EXTZones::llvm_zone_copy_ptr},
    {"llvm_ptr_in_zone", (void*)&extemp::EXTZones::llvm_ptr_in_zone},
    {"llvm_ptr_in_current_zone", (void*)&extemp::EXTZones::llvm_ptr_in_current_zone},
    {"llvm_pop_zone_stack", (void*)&extemp::EXTZones::llvm_pop_zone_stack},
    {"llvm_zone_callback_setup", (void*)&extemp::EXTZones::llvm_zone_callback_setup},
    {"llvm_peek_zone_stack_extern", (void*)&extemp::EXTZones::llvm_peek_zone_stack_extern},
    {"llvm_push_zone_stack_extern", (void*)&extemp::EXTZones::llvm_push_zone_stack_extern},
    {"llvm_zone_create_extern", (void*)&extemp::EXTZones::llvm_zone_create_extern},
    {"llvm_destroy_zone_after_delay", (void*)&llvm_destroy_zone_after_delay},
    {"get_address_offset", (void*)&extemp::ClosureAddressTable::get_address_offset},
    {"add_address_table", (void*)&extemp::ClosureAddressTable::add_address_table},
    {"get_address_table", (void*)&extemp::ClosureAddressTable::get_address_table},
    {"check_address_exists", (void*)&extemp::ClosureAddressTable::check_address_exists},
    {"check_address_type", (void*)&extemp::ClosureAddressTable::check_address_type},
    {"string_hash", (void*)&string_hash},
    {"swap64i", (void*)&swap64i},
    {"swap64f", (void*)&swap64f},
    {"swap32i", (void*)&swap32i},
    {"swap32f", (void*)&swap32f},
    {"unswap64i", (void*)&unswap64i},
    {"unswap64f", (void*)&unswap64f},
    {"unswap32i", (void*)&unswap32i},
    {"unswap32f", (void*)&unswap32f},
    {"rsplit", (void*)&rsplit},
    {"rmatch", (void*)&rmatch},
    {"rreplace", (void*)&rreplace},
    {"r64value", (void*)&r64value},
    {"mk_double", (void*)&mk_double},
    {"r32value", (void*)&r32value},
    {"mk_float", (void*)&mk_float},
    {"mk_i64", (void*)&mk_i64},
    {"mk_i32", (void*)&mk_i32},
    {"mk_i16", (void*)&mk_i16},
    {"mk_i8", (void*)&mk_i8},
    {"mk_i1", (void*)&mk_i1},
    {"string_value", (void*)&string_value},
    {"mk_string", (void*)&mk_string},
    {"cptr_value", (void*)&cptr_value},
    {"mk_cptr", (void*)&mk_cptr},
    {"sys_sharedir", (void*)&sys_sharedir},
    {"sys_slurp_file", (void*)&sys_slurp_file},
    {"fp80_to_double_portable", (void*)&fp80_to_double_portable},
};

// Define, or redefine, a symbol that resolves to a fixed process address
// (bind-lib and llvm:update-mapping). A symbol an earlier module defined is
// erased first, so rebinding replaces the mapping rather than silently
// keeping the old one.
llvm::Error defineAbsoluteSymbol(std::string_view Name, void* Addr) {
    if (!JIT) {
        return llvm::make_error<llvm::StringError>("JIT not initialized",
                                                   llvm::inconvertibleErrorCode());
    }
    std::lock_guard<std::mutex> lock(sModulesMutex);
    std::string name(Name);
    if (sSymbolOwner.count(name)) {
        sPendingErase.push_back(name);
        flushPendingErasures();
    }
    auto& JD = JIT->getMainJITDylib();
    auto sym = JIT->mangleAndIntern(name);
    auto define = [&]() {
        llvm::orc::SymbolMap symbols;
        symbols[sym] = {llvm::orc::ExecutorAddr::fromPtr(Addr), llvm::JITSymbolFlags::Exported};
        return JD.define(llvm::orc::absoluteSymbols(std::move(symbols)));
    };
    auto err = define();
    if (!err) {
        return llvm::Error::success();
    }
    bool duplicate = false;
    std::string message;
    llvm::handleAllErrors(
        std::move(err), [&](const llvm::orc::DuplicateDefinition&) { duplicate = true; },
        [&](const llvm::ErrorInfoBase& e) { message = e.message(); });
    if (!duplicate) {
        return llvm::make_error<llvm::StringError>(message, llvm::inconvertibleErrorCode());
    }
    if (auto rerr = JD.remove({sym})) {  // an earlier absolute definition
        return rerr;
    }
    return define();
}

void initLLVM() {
    if (JIT) [[unlikely]] {
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
    std::string cpu =
        extemp::UNIV::CPU.empty() ? std::string(llvm::sys::getHostCPUName()) : extemp::UNIV::CPU;

    // Get host features
    auto HostFeatures = llvm::sys::getHostCPUFeatures();
    std::vector<std::string> featureVec;
    std::string featureString;
    for (auto& feature : HostFeatures) {
        std::string featureStr;
        featureStr += (feature.getValue() ? "+" : "-");
        featureStr += feature.getKey().str();
        featureVec.push_back(featureStr);
        if (!featureString.empty())
            featureString += ",";
        featureString += featureStr;
    }

    // Store triple for later use.
    if (extemp::UNIV::ARCH.empty()) {
        extemp::UNIV::ARCH = triple;
    }

    // Set up target machine builder with actual CPU features.
    llvm::orc::JITTargetMachineBuilder JTMB{llvm::Triple(triple)};
    JTMB.setCPU(cpu).addFeatures(featureVec).setCodeGenOptLevel(
        llvm::CodeGenOptLevel::Aggressive);

    // Mirror LLJITBuilder's own linker choice (JITLink except on COFF, with the
    // code and relocation models it sets for JITLink), but supply memory
    // managers whose releases wait out kCodeGracePeriod -- see the module
    // ownership notes above.
    const bool isCOFF = llvm::Triple(triple).isOSBinFormatCOFF();
    const bool useJITLink = !isCOFF;
    if (useJITLink) {
        JTMB.setCodeModel(llvm::CodeModel::Small);
        JTMB.setRelocationModel(llvm::Reloc::PIC_);
    }
    JITBuilder.setJITTargetMachineBuilder(std::move(JTMB));
    JITBuilder.setObjectLinkingLayerCreator(
        [useJITLink, isCOFF](llvm::orc::ExecutionSession& ES)
            -> llvm::Expected<std::unique_ptr<llvm::orc::ObjectLayer>> {
            if (useJITLink) {
                auto inner = llvm::jitlink::InProcessMemoryManager::Create();
                if (!inner) {
                    return inner.takeError();
                }
                auto memMgr = std::make_unique<GraceJITLinkMemoryManager>(std::move(*inner));
                sGraceReaper = memMgr.get();
                return std::unique_ptr<llvm::orc::ObjectLayer>(
                    std::make_unique<llvm::orc::ObjectLinkingLayer>(ES, std::move(memMgr)));
            }
            // Leaked on purpose: it must outlive every SectionMemoryManager,
            // which the JIT destroys during static destruction.
            static auto* mapper = new GraceMemoryMapper;
            sGraceReaper = mapper;
            auto layer = std::make_unique<llvm::orc::RTDyldObjectLinkingLayer>(
                ES, [](const llvm::MemoryBuffer&) {
                    return std::make_unique<llvm::SectionMemoryManager>(mapper);
                });
            if (isCOFF) {
                layer->setOverrideObjectFlagsWithResponsibilityFlags(true);
                layer->setAutoClaimResponsibilityForObjectSymbols(true);
            }
            return std::unique_ptr<llvm::orc::ObjectLayer>(std::move(layer));
        });

    // Create the JIT.
    auto JITResult = JITBuilder.create();
    if (!JITResult) {
        std::cerr << "ERROR: Failed to create LLJIT: " << llvm::toString(JITResult.takeError())
                  << std::endl;
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

    // Register the runtime entry points with the JIT.
    llvm::orc::SymbolMap runtimeSymbols;
    for (const auto& [name, addr] : kRuntimeSymbols) {
        runtimeSymbols[JIT->mangleAndIntern(name)] = {llvm::orc::ExecutorAddr::fromPtr(addr),
                                                      llvm::JITSymbolFlags::Exported};
    }
    if (auto err = MainJD.define(llvm::orc::absoluteSymbols(std::move(runtimeSymbols)))) {
        std::cerr << "ERROR: Failed to register runtime symbols with the JIT: "
                  << llvm::toString(std::move(err)) << std::endl;
        exit(1);
    }

    return;
}

}  // namespace EXTLLVM
}  // namespace extemp
