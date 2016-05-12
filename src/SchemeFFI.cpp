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

#include <fstream>

// must be included before anything which pulls in <Windows.h>
#include "llvm/ADT/StringExtras.h"
#include "llvm/AsmParser/Parser.h"
#include "llvm-c/Core.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/GenericValue.h"
#include "llvm/ExecutionEngine/Interpreter.h"
#include "llvm/IR/CallingConv.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/LinkAllPasses.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/MutexGuard.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/raw_os_ostream.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Verifier.h"

#include "SchemeFFI.h"
#include "AudioDevice.h"
#include "UNIV.h"
#include "TaskScheduler.h"
#include "SchemeProcess.h"
#include "SchemeREPL.h"
#include <unordered_set>
#include <unordered_map>

#ifdef _WIN32
#include <Windows.h>
#include <Windowsx.h>
#include <filesystem>
#include <fstream>
#else
#include <dlfcn.h>
#include <dirent.h>
#endif

// setting this define should make call_compiled thread safe BUT ...
// also extremely SLOW !

#define LLVM_EE_LOCK

#include <regex>

////////////////////////////////

#include "pcre.h"

#ifdef __APPLE__
#include <malloc/malloc.h>
#else
#include <time.h>
#endif

#ifdef _WIN32
//#include <unistd.h>
#include <malloc.h>
#elif __APPLE__
#include <Cocoa/Cocoa.h>
#include <CoreFoundation/CoreFoundation.h>
#include <AppKit/AppKit.h>
#endif

#ifdef _WIN32
#define PRINT_ERROR(format, ...)                \
    ascii_error();                   \
    printf(format , __VA_ARGS__);                       \
    ascii_normal()
#else
#define PRINT_ERROR(format, args...)            \
    ascii_error();                   \
    printf(format , ## args);                   \
    ascii_normal()
#endif

#include <queue>
//#include <unistd.h>
#include <EXTMutex.h>
#include <EXTLLVM.h>
namespace extemp { namespace SchemeFFI {
static llvm::Module* jitCompile(const std::string& String);
}}

namespace extemp {

namespace SchemeFFI {

#include "ffi/utility.inc"
#include "ffi/ipc.inc"
#include "ffi/assoc.inc"
#include "ffi/number.inc"
#include "ffi/sys.inc"
#include "ffi/sys_dsp.inc"
#include "ffi/sys_zone.inc"
#include "ffi/misc.inc"
#include "ffi/regex.inc"
#include "ffi/llvm.inc"
#include "ffi/clock.inc"

void initSchemeFFI(scheme* sc)
{
    static struct {
        const char* name;
        uint32_t    value;
    } integerTable[] = {
        { "*au:block-size*", UNIV::FRAMES },
        { "*au:samplerate*", UNIV::SAMPLE_RATE },
        { "*au:channels*", UNIV::CHANNELS },
        { "*au:in-channels*", UNIV::IN_CHANNELS },
    };
    for (auto& elem: integerTable) {
        scheme_define(sc, sc->global_env, mk_symbol(sc, elem.name), mk_integer(sc, elem.value));
    }
    static struct {
        const char*  name;
        foreign_func func;
    } funcTable[] = {
        UTILITY_DEFS,
        IPC_DEFS,
        ASSOC_DEFS,
        NUMBER_DEFS,
        SYS_DEFS,
        SYS_DSP_DEFS,
        SYS_ZONE_DEFS,
        MISC_DEFS,
        REGEX_DEFS,
        LLVM_DEFS,
        CLOCK_DEFS
    };
    for (auto& elem : funcTable) {
        scheme_define(sc, sc->global_env, mk_symbol(sc, elem.name), mk_foreign_func(sc, elem.func));
    }
}

static long long llvm_emitcounter = 0;

static std::string SanitizeType(llvm::Type* Type)
{
    std::string type;
    llvm::raw_string_ostream typeStream(type);
    Type->print(typeStream);
    auto str(typeStream.str());
    std::string::size_type pos(str.find('='));
    if (pos != std::string::npos) {
        str.erase(pos - 1);
    }
    return str;
}

static std::regex sGlobalSymRegex("[ \t]@([-a-zA-Z$._][-a-zA-Z$._0-9]*)", std::regex::optimize);
static std::regex sDefineSymRegex("define[^\\n]+@([-a-zA-Z$._][-a-zA-Z$._0-9]*)", std::regex::optimize | std::regex::ECMAScript);

static llvm::Module* jitCompile(const std::string& String)
{
    // Create some module to put our function into it.
    using namespace llvm;
    legacy::PassManager* PM = extemp::EXTLLVM::PM;
    legacy::PassManager* PM_NO = extemp::EXTLLVM::PM_NO;

    char modname[256];
    sprintf(modname, "xtmmodule_%lld", ++llvm_emitcounter);

    std::string asmcode(String);
    SMDiagnostic pa;

    static std::string sInlineString; // This is a hack for now, but it *WORKS*
    static std::string sInlineBitcode;
    static std::unordered_set<std::string> sInlineSyms;
    if (sInlineString.empty()) {
        {
            std::ifstream inStream(UNIV::SHARE_DIR + "/runtime/bitcode.ll");
            std::stringstream inString;
            inString << inStream.rdbuf();
            sInlineString = inString.str();
        }
        std::copy(std::sregex_token_iterator(sInlineString.begin(), sInlineString.end(), sGlobalSymRegex, 1),
                std::sregex_token_iterator(), std::inserter(sInlineSyms, sInlineSyms.begin()));
        {
            std::ifstream inStream(UNIV::SHARE_DIR + "/runtime/inline.ll");
            std::stringstream inString;
            inString << inStream.rdbuf();
            std::string tString = inString.str();
            std::copy(std::sregex_token_iterator(tString.begin(), tString.end(), sGlobalSymRegex, 1),
                    std::sregex_token_iterator(), std::inserter(sInlineSyms, sInlineSyms.begin()));
        }
    }
    if (sInlineBitcode.empty()) {
        // need to avoid parsing the types twice
        static bool first(true);
        if (!first) {
            auto newModule(parseAssemblyString(sInlineString, pa, getGlobalContext()));
            if (newModule) {
                std::string bitcode;
                llvm::raw_string_ostream bitstream(sInlineBitcode);
                llvm::WriteBitcodeToFile(newModule.get(), bitstream);
                std::ifstream inStream(UNIV::SHARE_DIR + "/runtime/inline.ll");
                std::stringstream inString;
                inString << inStream.rdbuf();
                sInlineString = inString.str();
            } else {
std::cout << pa.getMessage().str() << std::endl;
                abort();
            }
        } else {
            first = false;
        }
    }
    std::unique_ptr<llvm::Module> newModule;
    std::vector<std::string> symbols;
    std::copy(std::sregex_token_iterator(asmcode.begin(), asmcode.end(), sGlobalSymRegex, 1),
            std::sregex_token_iterator(), std::inserter(symbols, symbols.begin()));
    std::sort(symbols.begin(), symbols.end());
    auto end(std::unique(symbols.begin(), symbols.end()));
    std::unordered_set<std::string> ignoreSyms;
    std::copy(std::sregex_token_iterator(asmcode.begin(), asmcode.end(), sDefineSymRegex, 1),
            std::sregex_token_iterator(), std::inserter(ignoreSyms, ignoreSyms.begin()));
    std::string declarations;
    llvm::raw_string_ostream dstream(declarations);
    for (auto iter = symbols.begin(); iter != end; ++iter) {
        const char* sym(iter->c_str());
        if (sInlineSyms.find(sym) != sInlineSyms.end() || ignoreSyms.find(sym) != ignoreSyms.end()) {
            continue;
        }
        auto gv = extemp::EXTLLVM::getGlobalValue(sym);
        if (!gv) {
            continue;
        }
        auto func(llvm::dyn_cast<llvm::Function>(gv));
        if (func) {
            dstream << "declare " << SanitizeType(func->getReturnType()) << " @" << sym << " (";
            bool first(true);
            for (const auto& arg : func->getArgumentList()) {
                if (!first) {
                    dstream << ", ";
                } else {
                    first = false;
                }
                dstream << SanitizeType(arg.getType());
            }
            if (func->isVarArg()) {
                dstream << ", ...";
            }
            dstream << ")\n";
        } else {
            auto str(SanitizeType(gv->getType()));
            dstream << '@' << sym << " = external global " << str.substr(0, str.length() - 1) << '\n';
        }
    }
// std::cout << "**** DECL ****\n" << dstream.str() << "**** ENDDECL ****\n" << std::endl;
    if (!sInlineBitcode.empty()) {
        auto modOrErr(parseBitcodeFile(llvm::MemoryBufferRef(sInlineBitcode, "<string>"), getGlobalContext()));
        if (likely(modOrErr)) {
            newModule = std::move(modOrErr.get());
            asmcode = sInlineString + dstream.str() + asmcode;
            if (parseAssemblyInto(llvm::MemoryBufferRef(asmcode, "<string>"), *newModule, pa)) {
std::cout << "**** DECL ****\n" << dstream.str() << "**** ENDDECL ****\n" << std::endl;
                newModule.reset();
            }
        }
    } else {
       newModule = parseAssemblyString(asmcode, pa, getGlobalContext());
    }
    if (newModule) {
        if (unlikely(!extemp::UNIV::ARCH.empty())) {
            newModule->setTargetTriple(extemp::UNIV::ARCH);
        }
        if (EXTLLVM::OPTIMIZE_COMPILES) {
            PM->run(*newModule);
        } else {
            PM_NO->run(*newModule);
        }
    }
    //std::stringstream ss;
    if (unlikely(!newModule))
    {
// std::cout << "**** CODE ****\n" << asmcode << " **** ENDCODE ****" << std::endl;
// std::cout << pa.getMessage().str() << std::endl << pa.getLineNo() << std::endl;
        std::string errstr;
        llvm::raw_string_ostream ss(errstr);
        pa.print("LLVM IR",ss);
        printf("%s\n",ss.str().c_str());
        return nullptr;
    } else if (extemp::EXTLLVM::VERIFY_COMPILES && verifyModule(*newModule)) {
        std::cout << "\nInvalid LLVM IR\n";
        return nullptr;
    }

    llvm::Module *modulePtr = newModule.get();
    extemp::EXTLLVM::EE->addModule(std::move(newModule));
    extemp::EXTLLVM::EE->finalizeObject();
    return modulePtr;
}

}

} // end namespace

