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
#include "llvm/IR/DiagnosticInfo.h"
#include "llvm/IR/DiagnosticPrinter.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"

#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/OptimizationLevel.h"

#include "llvm/Support/CommandLine.h"
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

#include <algorithm>
#include <bit>
#include <charconv>
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
#include <ExtemporeVersion.h>
#include <ext/NetUtil.h>
#include <ext/UdpSocket.h>
#include <EXTClosureAddressTable.h>
#include <EXTThread.h>
#include <EXTRegex.h>
#include <UNIV.h>
#include <SchemeFFI.h>
#include <TaskScheduler.h>
#include <SchemeS7.h>
#include <SchemeS7Private.h>
#include <OSC.h>

#include <chrono>
#include <thread>

#include "SchemeProcess.h"

EXPORT void* malloc16(size_t Size) {
    return Size ? extemp::EXTZones::aligned_malloc(Size, 16) : nullptr;
}

EXPORT void free16(void* Ptr) {
    extemp::EXTZones::aligned_free(Ptr);
}

// Portable conversion from 80-bit extended precision (big-endian) to double.
// Used for reading AIFF audio files, which store sample rate in this format.
// Format: 1 sign bit, 15 exponent bits, 64 mantissa bits (with explicit integer bit)
EXPORT double fp80_to_double_portable(const unsigned char* bytes) {
    // Read big-endian 80-bit value
    unsigned int exponent = (unsigned(bytes[0]) << 8) | bytes[1];
    const uint64_t mantissa = (uint64_t(bytes[2]) << 56) | (uint64_t(bytes[3]) << 48) |
                              (uint64_t(bytes[4]) << 40) | (uint64_t(bytes[5]) << 32) |
                              (uint64_t(bytes[6]) << 24) | (uint64_t(bytes[7]) << 16) |
                              (uint64_t(bytes[8]) << 8) | uint64_t(bytes[9]);

    // Extract sign bit
    const int sign = (exponent >> 15) & 1;
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
    const int64_t exp_unbiased = int64_t(exponent) - 16383;

    // The mantissa has an explicit integer bit (bit 63); double has an implicit
    // one, so handle it explicitly.
    if (mantissa & (1ULL << 63)) {
        // Normal number - integer bit is set.
        // Remove the integer bit and shift mantissa to fit in double's 52-bit mantissa.
        const uint64_t double_mantissa = (mantissa & 0x7FFFFFFFFFFFFFFFULL) >> 11;
        const int64_t double_exp = exp_unbiased + 1023;

        if (double_exp >= 2047) {
            // Overflow to infinity.
            return sign ? -INFINITY : INFINITY;
        } else if (double_exp <= 0) {
            // Underflow - denormalized or zero.
            return sign ? -0.0 : 0.0;
        } else {
            // Pack into IEEE 754 double format.
            const uint64_t result_bits =
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
    switch (error) {
    case 1:
        extemp::log::error(extemp::log::jit, "zptr_copy: invalid zptr {}", arg);
        break;
    default:
        break;
    }
    return;
}

EXPORT void llvm_schedule_callback(long long time, void* dat) {
    extemp::SchemeProcess* proc =
        extemp::SchemeProcess::I();  // extemp::SchemeProcess::I()->extemporeCallback(time,dat);

    const uint64_t current_time = time;    // task->getStartTime();
    const uint64_t duration = 1000000000;  // task->getDuration();
    extemp::TaskScheduler::I()->addTask(current_time, duration, proc->getExtemporeCallback(), dat,
                                        0);
    return;
}

EXPORT void* llvm_get_function_ptr(char* fname) {
    return reinterpret_cast<void*>(extemp::EXTLLVM::getFunctionAddress(fname));
}

// Returns a per-thread buffer that the next extitoa call on the same thread
// overwrites: xtlang callers must copy or consume the result before calling
// again (two extitoa results in one expression alias each other).
EXPORT char* extitoa(int64_t val) {
    static thread_local char buf[32];
    *std::to_chars(buf, buf + sizeof(buf) - 1, val).ptr = '\0';
    return buf;
}

EXPORT void llvm_send_udp(char* host, int port, void* message, int message_length) {
    const uint32_t resolved = extemp::net_util::resolve_ipv4(host);
    if (!resolved) {
        extemp::log::error(extemp::log::osc, "could not resolve host name");
        return;
    }

    struct sockaddr_in sa;
    memset(&sa, 0, sizeof(sa));
    sa.sin_family = AF_INET;
    sa.sin_port = htons(uint16_t(port));
    sa.sin_addr.s_addr = resolved;

    extemp::UdpSocket sock;
    if (!sock.open()) {
        extemp::log::error(extemp::log::osc, "could not open UDP socket: {}",
                           extemp::UdpSocket::errorText(extemp::UdpSocket::lastError()));
        return;
    }
    if (sock.sendTo(message, size_t(message_length), sa) < 0) {
        const int err = extemp::UdpSocket::lastError();
        if (extemp::UdpSocket::messageTooBig(err)) {
            extemp::log::error(extemp::log::osc, "message too large: UDP messages are 8k at most");
        } else {
            extemp::log::error(extemp::log::osc, "problem sending message: {}",
                               extemp::UdpSocket::errorText(err));
        }
    }
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

// xtlang's side of the logger (src/Log.h): safe from the audio thread
EXPORT void log_error(const char* Message) {
    extemp::log::emit(extemp::log::Level::Error, {}, Message);
}

EXPORT void log_warn(const char* Message) {
    extemp::log::emit(extemp::log::Level::Warning, {}, Message);
}

EXPORT void log_info(const char* Message) {
    extemp::log::emit(extemp::log::Level::Info, {}, Message);
}

EXPORT void log_debug(const char* Message) {
    extemp::log::emit(extemp::log::Level::Debug, {}, Message);
}

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
//
// Each thread has its own generator. Under extempore render --seed each is
// seeded from the seed and the thread's place in the order the threads were
// made, so a render draws the same numbers every time. That happens at a
// thread's first draw, not where the thread_local is initialised: MSVC does
// that as the thread starts, which for the main thread is before the command
// line is read.
static thread_local std::minstd_rand sRandGen{std::random_device{}()};
static thread_local bool sRandSeeded = false;

static void seedRandGen() {
    const extemp::EXTThread* thread = extemp::EXTThread::activeThread();
    const uint64_t stream = thread ? thread->ordinal() + 1 : 0;  // 0: the render thread
    std::seed_seq seq{uint32_t(extemp::UNIV::SEED), uint32_t(extemp::UNIV::SEED >> 32),
                      uint32_t(stream), uint32_t(stream >> 32)};
    sRandGen.seed(seq);
    sRandSeeded = true;
}

EXPORT double xtc_randd() {
    if (extemp::UNIV::SEEDED && !sRandSeeded) [[unlikely]] {
        seedRandGen();
    }
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

// A context whose diagnostics (LLVM's warnings, remarks and errors, which it
// would otherwise print to stderr itself) go to the logger.
static std::unique_ptr<llvm::LLVMContext> makeContext() {
    auto context = std::make_unique<llvm::LLVMContext>();
    context->setDiagnosticHandlerCallBack(
        [](const llvm::DiagnosticInfo* Info, void* /*Context*/) {
            std::string text;
            llvm::raw_string_ostream stream(text);
            llvm::DiagnosticPrinterRawOStream printer(stream);
            Info->print(printer);
            switch (Info->getSeverity()) {
            case llvm::DS_Error:
                // as LLVM's own handler does: the module can't be trusted
                log::error(log::jit, "{}", text);
                log::flush();
                std::exit(1);
            case llvm::DS_Warning:
                log::warn(log::jit, "{}", text);
                break;
            case llvm::DS_Remark:
            case llvm::DS_Note:
                log::debug(log::jit, "{}", text);
                break;
            }
        },
        nullptr);
    return context;
}

llvm::orc::ThreadSafeContext& getThreadSafeContext() {
    // Ensure the thread-safe context exists before returning a reference
    if (!TSC) {
        TSC = std::make_unique<llvm::orc::ThreadSafeContext>(makeContext());
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
    const size_t afterAdhoc = pos + 7;
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
        const std::unique_lock<std::shared_mutex> lock(sAdhocAliasesMutex);
        sAdhocAliases[alias] = std::string(fullName);
    }
}

// Full name registered for a counter-less adhoc alias, or empty.
static std::string lookupAdhocAlias(std::string_view name) {
    const std::shared_lock<std::shared_mutex> lock(sAdhocAliasesMutex);
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
            const std::lock_guard<std::mutex> lock(m_mutex);
            m_graveyard.emplace_back(clock::now(), std::move(Allocs));
        }
        OnDeallocated(llvm::Error::success());
    }
    void reap(bool All) override {
        std::vector<std::vector<FinalizedAlloc>> expired;
        {
            const std::lock_guard<std::mutex> lock(m_mutex);
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
                    log::error(log::jit, "error releasing JIT memory: {}",
                               llvm::toString(std::move(Err)));
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
        const std::lock_guard<std::mutex> lock(m_mutex);
        m_graveyard.emplace_back(clock::now(), M);
        return std::error_code();
    }
    void reap(bool All) override {
        std::vector<llvm::sys::MemoryBlock> expired;
        {
            const std::lock_guard<std::mutex> lock(m_mutex);
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
                log::error(log::jit, "error releasing JIT memory: {}", ec.message());
            }
        }
    }
};

struct TrackedModule {
    llvm::orc::ResourceTrackerSP tracker;
    std::vector<std::string> exports;          // strong symbols this module defines
    std::vector<std::string> imports;          // external symbols it references
    std::unique_ptr<llvm::Module> metadata;    // clone exposed via getModules()
    bool reclaimable = false;                  // defines no global variables
    bool pinned = false;                       // a symbol was unlinked on its own
    bool everReferenced = false;               // a later module imported an export
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
    const llvm::orc::SymbolNameSet names{JIT->mangleAndIntern(Name)};
    if (auto err = JIT->getMainJITDylib().remove(names)) {
        llvm::handleAllErrors(
            std::move(err), [](const llvm::orc::SymbolsNotFound&) {},  // already gone
            [&](const llvm::ErrorInfoBase& e) {
                log::error(log::jit, "could not remove symbol {}: {}", Name, e.message());
            });
    }
}

bool unreferenced(const TrackedModule& M) {
    return std::ranges::none_of(
        M.exports, [](const std::string& n) { return sImportRefs.contains(n); });
}

// Unlink a module's symbols and retire its code and metadata. Modules that only
// this one referenced become orphans.
void removeModule(TrackedModule* M) {
    if (auto err = M->tracker->remove()) {
        log::error(log::jit, "could not remove module: {}", llvm::toString(std::move(err)));
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
        if (std::ranges::find(touched, M) == touched.end()) {
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
            const bool replaced = std::ranges::any_of(
                NewExports, [&](const std::string& n) { return M->orphanedBy.contains(n); });
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

llvm::Error addTrackedModule(llvm::orc::ThreadSafeModule TSM, ModuleSymbols Symbols,
                             std::unique_ptr<llvm::Module> Metadata) {
    if (!JIT) {
        return llvm::make_error<llvm::StringError>("JIT not initialized",
                                                   llvm::inconvertibleErrorCode());
    }
    const std::lock_guard<std::mutex> lock(sModulesMutex);
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
    const std::lock_guard<std::mutex> lock(sModulesMutex);
    if (sGraceReaper) {
        sGraceReaper->reap(false);
    }
    auto RT = JIT->getMainJITDylib().createResourceTracker();
    if (auto err = JIT->addIRModule(RT, std::move(TSM))) {
        llvm::consumeError(RT->remove());
        return err;
    }
    return RT;
}

// NOLINTNEXTLINE(performance-unnecessary-value-param): a sink, the caller hands the tracker over
void removeTransientModule(llvm::orc::ResourceTrackerSP RT) {
    if (auto err = RT->remove()) {
        log::error(log::jit, "could not remove module: {}", llvm::toString(std::move(err)));
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
    const std::lock_guard<std::mutex> lock(sModulesMutex);
    if (sSymbolOwner.contains(name)) {
        sPendingErase.push_back(name);
        return true;
    }
    const llvm::orc::SymbolNameSet names{JIT->mangleAndIntern(name)};
    if (auto err = JIT->getMainJITDylib().remove(names)) {
        const bool removed = false;
        llvm::handleAllErrors(
            std::move(err), [](const llvm::orc::SymbolsNotFound&) {},
            [&](const llvm::ErrorInfoBase& e) {
                log::error(log::jit, "could not remove symbol {}: {}", name, e.message());
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
        const std::lock_guard<std::mutex> lock(sModulesMutex);
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

// Disassemble the function at Code. The JIT does not expose symbol sizes,
// so decoding stops at the first return instruction (a function with several
// exits is shown up to its first one) or after kDisassembleLimit bytes.
std::string llvm_disassemble(const unsigned char* Code, int syntax) {
    constexpr size_t kDisassembleLimit = size_t{64} * 1024;
    std::string Error;

    // Get target triple from host
    const std::string TripleName = llvm::sys::getProcessTriple();
    const llvm::Triple Triple(TripleName);

    // Look up target
    const llvm::Target* TheTarget = llvm::TargetRegistry::lookupTarget(Triple, Error);
    if (!TheTarget) {
        return "Disassembler error: " + Error;
    }

    const std::unique_ptr<const llvm::MCRegisterInfo> MRI(TheTarget->createMCRegInfo(Triple));
    if (!MRI)
        return "Failed to create MCRegisterInfo";

    const llvm::MCTargetOptions MCOptions;
    const std::unique_ptr<const llvm::MCAsmInfo> AsmInfo(
        TheTarget->createMCAsmInfo(*MRI, Triple, MCOptions));
    if (!AsmInfo)
        return "Failed to create MCAsmInfo";

    const std::unique_ptr<const llvm::MCSubtargetInfo> STI(
        TheTarget->createMCSubtargetInfo(Triple, "", ""));
    if (!STI)
        return "Failed to create MCSubtargetInfo";

    const std::unique_ptr<const llvm::MCInstrInfo> MII(TheTarget->createMCInstrInfo());
    if (!MII)
        return "Failed to create MCInstrInfo";

    llvm::MCContext Ctx(Triple, *AsmInfo, *MRI, *STI);
    std::unique_ptr<llvm::MCDisassembler> DisAsm(TheTarget->createMCDisassembler(*STI, Ctx));
    if (!DisAsm)
        return "Failed to create MCDisassembler";

    std::unique_ptr<llvm::MCInstPrinter> IP(
        TheTarget->createMCInstPrinter(Triple, syntax, *AsmInfo, *MII, *MRI));
    if (!IP)
        return "Failed to create MCInstPrinter";

    IP->setPrintImmHex(true);

    std::string out_str;
    llvm::raw_string_ostream OS(out_str);
    const llvm::ArrayRef<uint8_t> mem(Code, kDisassembleLimit);
    OS << "\n";
    for (uint64_t index = 0; index < kDisassembleLimit;) {
        llvm::MCInst Inst;
        uint64_t size = 0;
        if (!DisAsm->getInstruction(Inst, size, mem.slice(index), index, llvm::nulls())) {
            index += size ? size : 1;  // skip an undecodable byte
            continue;
        }
        OS.indent(4);
        OS.write("0x", 2);
        OS.write_hex(size_t(Code) + index);
        OS.write(": ", 2);
        IP->printInst(&Inst, 0, "", *STI, OS);
        OS << "\n";
        if (MII->get(Inst.getOpcode()).isReturn()) {
            break;
        }
        index += size;
    }
    return OS.str();
}

static extemp::CM DestroyMallocZoneWithDelayCM([](extemp::TaskI* Task) {
    extemp::EXTZones::llvm_zone_destroy(static_cast<extemp::Task<llvm_zone_t*>*>(Task)->getArg());
});

EXPORT void llvm_destroy_zone_after_delay(llvm_zone_t* Zone, uint64_t Delay) {
    extemp::TaskScheduler::I()->addHousekeeping(new extemp::Task<llvm_zone_t*>(
        extemp::UNIV::TIME + Delay, extemp::UNIV::SECOND(), &DestroyMallocZoneWithDelayCM, Zone));
}

static extemp::CM FreeWithDelayCM([](extemp::TaskI* Task) {
    free(static_cast<extemp::Task<char*>*>(Task)->getArg());
});

EXPORT void free_after_delay(char* Data, double Delay) {
    extemp::TaskScheduler::I()->addHousekeeping(new extemp::Task<char*>(
        extemp::UNIV::TIME + Delay, extemp::UNIV::SECOND(), &FreeWithDelayCM, Data));
}

EXPORT void ascii_text_color_extern(int32_t Bold, int32_t Foreground, int32_t Background) {
    fputs(extemp::log::colour_code(stdout, Bold != 0, unsigned(Foreground), unsigned(Background))
              .c_str(),
          stdout);
}

// CATEGORY: clock

EXPORT double clock_clock() {
    return getRealTime() + extemp::UNIV::CLOCK_OFFSET;
}

// clock_clock() - clock_offset() is the clock audio_clock_now() reads
EXPORT double clock_offset() {
    return extemp::UNIV::CLOCK_OFFSET;
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
    {"llvm_zone_malloc_from_current_zone",
     (void*)&extemp::EXTZones::llvm_zone_malloc_from_current_zone},
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
    {"rmatches", (void*)&rmatches},
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
    const std::lock_guard<std::mutex> lock(sModulesMutex);
    const std::string name(Name);
    if (sSymbolOwner.contains(name)) {
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

    // LLVM cuts local value names off at 1024 characters by default, and
    // xtlang's names can be longer: a specialisation's name carries an encoding
    // of its whole type (a 128-field tuple's constructor names run to 1200
    // characters), so two locals which differ only past the cut collided as
    // "multiple definition of local value". -1 is no limit.
    {
        auto& opts = llvm::cl::getRegisteredOptions();
        auto it = opts.find("non-global-value-max-name-size");
        if (it != opts.end()) {
            static_cast<llvm::cl::opt<int>*>(it->second)->setValue(-1);
        }
    }

    // Create thread-safe context
    TSC = std::make_unique<llvm::orc::ThreadSafeContext>(makeContext());

    // Build LLJIT
    auto JITBuilder = llvm::orc::LLJITBuilder();

    // Configure target machine
    const std::string triple = llvm::sys::getProcessTriple();
    const std::string cpu =
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
    JTMB.setCPU(cpu).addFeatures(featureVec).setCodeGenOptLevel(llvm::CodeGenOptLevel::Aggressive);

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
    if (useJITLink) {
        JITBuilder.setMemoryManagerCreator(
            [](llvm::orc::ExecutionSession&)
                -> llvm::Expected<std::unique_ptr<llvm::jitlink::JITLinkMemoryManager>> {
                auto inner = llvm::jitlink::InProcessMemoryManager::Create();
                if (!inner) {
                    return inner.takeError();
                }
                auto memMgr = std::make_unique<GraceJITLinkMemoryManager>(std::move(*inner));
                sGraceReaper = memMgr.get();
                return std::unique_ptr<llvm::jitlink::JITLinkMemoryManager>(std::move(memMgr));
            });
    }
    JITBuilder.setObjectLinkingLayerCreator(
        [useJITLink, isCOFF](llvm::orc::ExecutionSession& ES,
                             llvm::jitlink::JITLinkMemoryManager& MemMgr)
            -> llvm::Expected<std::unique_ptr<llvm::orc::ObjectLayer>> {
            if (useJITLink) {
                return std::unique_ptr<llvm::orc::ObjectLayer>(
                    std::make_unique<llvm::orc::ObjectLinkingLayer>(ES, MemMgr));
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
        log::error(log::jit, "failed to create LLJIT: {}", llvm::toString(JITResult.takeError()));
        log::flush();
        exit(1);
    }
    JIT = std::move(*JITResult);
    // ORC would print these to stderr itself (an unresolved symbol, say)
    JIT->getExecutionSession().setErrorReporter([](llvm::Error Err) {
        log::error(log::jit, "{}", llvm::toString(std::move(Err)));
    });

    // Add DynamicLibrarySearchGenerator to make all process symbols available.
    auto& MainJD = JIT->getMainJITDylib();
    // The last two arguments are the defaults, spelt out. The header's own
    // default for the third is `nullptr`, which leaves unique_function's inline
    // storage unset, and GCC's -Wmaybe-uninitialized (an error here) fires when
    // the move is inlined into this function. `Fn()` zeroes the storage.
    using DLSG = llvm::orc::DynamicLibrarySearchGenerator;
    auto DLSGOrErr =
        DLSG::GetForCurrentProcess(JIT->getDataLayout().getGlobalPrefix(), DLSG::SymbolPredicate(),
                                   DLSG::AddAbsoluteSymbolsFn());
    if (!DLSGOrErr) {
        log::error(log::jit, "failed to create DynamicLibrarySearchGenerator: {}",
                   llvm::toString(DLSGOrErr.takeError()));
        log::flush();
        exit(1);
    }
    MainJD.addGenerator(std::move(*DLSGOrErr));

    if (UNIV::MODE == UNIV::Mode::Session) {
        // a release's version is its tag, vN.N.N; a dev build's says how far past
        std::string_view version = EXTEMPORE_VERSION;
        if (version.starts_with('v')) {
            version.remove_prefix(1);
        }
        log::info("{}{}", log::bold(std::format("Extempore {}", version)),
                  log::dim(std::format(", LLVM {}, {}{}{}", LLVM_VERSION_STRING, triple,
                                       cpu.empty() ? "" : " ", cpu.empty() ? "" : "(" + cpu + ")")));
    }

    // Register the runtime entry points with the JIT.
    llvm::orc::SymbolMap runtimeSymbols;
    for (const auto& [name, addr] : kRuntimeSymbols) {
        runtimeSymbols[JIT->mangleAndIntern(name)] = {llvm::orc::ExecutorAddr::fromPtr(addr),
                                                      llvm::JITSymbolFlags::Exported};
    }
    if (auto err = MainJD.define(llvm::orc::absoluteSymbols(std::move(runtimeSymbols)))) {
        log::error(log::jit, "failed to register runtime symbols with the JIT: {}",
                   llvm::toString(std::move(err)));
        log::flush();
        exit(1);
    }

    return;
}

}  // namespace EXTLLVM
}  // namespace extemp
