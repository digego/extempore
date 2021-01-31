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

#include <Scheme.h>
#include <EXTZones.h>
#include <EXTMutex.h>
#include <BranchPrediction.h>
#include <UNIV.h>

#include <vector>
#include <string>
#include <memory>


struct _llvm_callback_struct_ {
    void(*fptr)(void*,llvm_zone_t*);
    void* dat;
    llvm_zone_t* zone;
};

struct closure_address_table;

extern "C"
{

const char* llvm_scheme_ff_get_name(foreign_func ff);
void llvm_scheme_ff_set_name(foreign_func ff,const char* name);

void llvm_destroy_zone_after_delay(llvm_zone_t* zone, uint64_t delay);

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

EXPORT double imp_randd();
EXPORT int64_t imp_rand1_i64(int64_t a);

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
EXPORT const char* llvm_disassemble(const unsigned char*  Code, int Syntax);

}

}
