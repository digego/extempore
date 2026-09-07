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

#pragma once

#include <SchemeS7.h>
#include <EXTZones.h>
#include <UNIV.h>

#include <vector>
#include <string>
#include <string_view>
#include <memory>

#include "llvm/ExecutionEngine/Orc/Core.h"
#include "llvm/ExecutionEngine/Orc/ThreadSafeModule.h"
#include "llvm/Support/Error.h"

struct _llvm_callback_struct_ {
    void (*fptr)(void*, llvm_zone_t*);
    void* dat;
    llvm_zone_t* zone;
};

struct closure_address_table;

extern "C" {

EXPORT void llvm_destroy_zone_after_delay(llvm_zone_t* zone, uint64_t delay);

static inline uint64_t string_hash(const char* str) {
    uint64_t result(0);
    unsigned char c;
    while ((c = *(str++))) {
        result = result * 33 + uint8_t(c);
    }
    return result;
}

EXPORT double xtc_randd();
EXPORT int64_t xtc_rand1_i64(int64_t a);
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
class LLVMContext;

namespace orc {
class LLJIT;
class ThreadSafeContext;
}  // namespace orc

}  // namespace llvm

namespace extemp {

namespace EXTLLVM {

uint64_t getFunctionAddress(std::string_view name);
void registerAdhocAlias(std::string_view fullName);

// ORC JIT
extern std::unique_ptr<llvm::orc::LLJIT> JIT;

extern std::unique_ptr<llvm::orc::ThreadSafeContext> TSC;

llvm::orc::ThreadSafeContext& getThreadSafeContext();

// Erase a symbol defined by an earlier module (lazily, see EXTLLVM.cpp) or an
// absolute symbol registered with defineAbsoluteSymbol. False if not found.
bool removeSymbol(const std::string& name);
void removeFromGlobalMap(const std::string& name);
// Define, or redefine, a symbol resolving to a fixed process address.
llvm::Error defineAbsoluteSymbol(std::string_view Name, void* Addr);

// What a module contributes to the JIT: strong definitions it exports, external
// symbols it uses, and whether any export is a global variable.
struct ModuleSymbols {
    std::vector<std::string> exports;
    std::vector<std::string> imports;
    bool definesGlobals = false;
};
ModuleSymbols collectModuleSymbols(const llvm::Module& M);

// Add a compiled module under its own resource tracker. Metadata is the clone
// exposed through getModules()/getGlobalValue(); it is released with the code.
llvm::Error addTrackedModule(llvm::orc::ThreadSafeModule TSM, ModuleSymbols Symbols,
                             std::unique_ptr<llvm::Module> Metadata);
// Add a module for a single use (the llvm:run call stubs); pass the tracker
// back to removeTransientModule once the call has returned.
llvm::Expected<llvm::orc::ResourceTrackerSP> addTransientModule(llvm::orc::ThreadSafeModule TSM);
void removeTransientModule(llvm::orc::ResourceTrackerSP RT);

extern int64_t LLVM_COUNT;
extern bool OPTIMIZE_COMPILES;
extern bool VERIFY_COMPILES;
extern int OPTIMIZATION_LEVEL;  // 0=O0, 1=O1, 2=O2, 3=O3
extern std::vector<llvm::Module*> Ms;

void initLLVM();
const llvm::Function* getFunction(const char* name);
const llvm::GlobalVariable* getGlobalVariable(const char* name);
const llvm::GlobalValue* getGlobalValue(const char* name);
inline std::vector<llvm::Module*>& getModules() {
    return Ms;
}  // not going to protect these!!!
std::string llvm_disassemble(const unsigned char* Code, int Syntax);

}  // namespace EXTLLVM

}  // namespace extemp
