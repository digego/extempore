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
#include <experimental/filesystem>
#include <fstream>
#else
#include <dlfcn.h>
#include <dirent.h>
#endif

#ifdef DYLIB
#include <cmrc/cmrc.hpp>
CMRC_DECLARE(xtm);
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
        { "*au:block-size*", UNIV::NUM_FRAMES },
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

static std::string fileToString(const std::string &fileName) {
    std::ifstream inStream(fileName);
    std::stringstream inString;
    inString << inStream.rdbuf();
    return inString.str();
}

static const std::string inlineDotLLString() {
#ifdef DYLIB
    auto fs = cmrc::xtm::get_filesystem();
    auto data = fs.open("runtime/inline.ll");
    static const std::string sInlineDotLLString(data.begin(), data.end());
#else
    static const std::string sInlineDotLLString(
      fileToString(UNIV::SHARE_DIR + "/runtime/inline.ll"));
#endif

    return sInlineDotLLString;
}

static const std::string bitcodeDotLLString() {
#ifdef DYLIB
    auto fs = cmrc::xtm::get_filesystem();
    auto data = fs.open("runtime/bitcode.ll");
    static const std::string sBitcodeDotLLString(data.begin(), data.end());
#else
    static const std::string sBitcodeDotLLString(
      fileToString(UNIV::SHARE_DIR + "/runtime/bitcode.ll"));
#endif

    return sBitcodeDotLLString;
}

static std::string IRToBitcode(const std::string &ir) {
    std::string bitcode;
    llvm::SMDiagnostic pa;
    auto mod(llvm::parseAssemblyString(ir, pa, llvm::getGlobalContext()));
    if (!mod) {
        pa.print("IRToBitcode", llvm::outs());
        std::abort();
    }
    llvm::raw_string_ostream bitstream(bitcode);
    llvm::WriteBitcodeToFile(mod.get(), bitstream);
    return bitcode;
}

// match @symbols @like @this_123
static const std::regex sGlobalSymRegex(
  "[ \t]@([-a-zA-Z$._][-a-zA-Z$._0-9]*)",
  std::regex::optimize);

// match "define @sym"
static const std::regex sDefineSymRegex(
  "define[^\\n]+@([-a-zA-Z$._][-a-zA-Z$._0-9]*)",
  std::regex::optimize | std::regex::ECMAScript);

// template is temporary, we'll remove this once the refactoring is done
template <class T>
static void insertMatchingSymbols(
  const std::string &code, const std::regex &regex,
  // std::unordered_set<std::string> &containingSet
  T &containingSet)
{
    std::copy(std::sregex_token_iterator(code.begin(), code.end(), regex, 1),
              std::sregex_token_iterator(),
              std::inserter(containingSet, containingSet.begin()));
}

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

static std::unordered_set<std::string> loadInlineSyms() {
    std::unordered_set<std::string> inlineSyms;
    insertMatchingSymbols(bitcodeDotLLString(), sGlobalSymRegex, inlineSyms);
    insertMatchingSymbols(inlineDotLLString(), sGlobalSymRegex, inlineSyms);
    return inlineSyms;
}

static std::string
globalDeclarations(const std::string &asmcode) {
    static std::unordered_set<std::string> sInlineSyms(loadInlineSyms());
    std::vector<std::string> symbols;
    // Copy all @symbols @like @this into symbols
    insertMatchingSymbols(asmcode, sGlobalSymRegex, symbols);
    std::sort(symbols.begin(), symbols.end());
    auto end(std::unique(symbols.begin(), symbols.end()));

    std::unordered_set<std::string> ignoreSyms;
    insertMatchingSymbols(asmcode, sDefineSymRegex, ignoreSyms);


    std::stringstream ss;

    // Iterating over all @symbols @in @asmcode matching sGlobalSymRegex
    for (auto iter = symbols.begin(); iter != end; ++iter) {
        const char* sym(iter->c_str());
        if (sInlineSyms.find(sym) != sInlineSyms.end() || ignoreSyms.find(sym) != ignoreSyms.end()) {
            continue;
        }
        auto gv = extemp::EXTLLVM::getGlobalValue(sym);
        if (!gv) {
            continue;
        }
        const llvm::Function* func(llvm::dyn_cast<llvm::Function>(gv));
        if (func) {
            ss << "declare " << SanitizeType(func->getReturnType()) << " @" << sym << " (";
            bool first(true);
            for (const auto& arg : func->getArgumentList()) {
                if (!first) {
                    ss << ", ";
                } else {
                    first = false;
                }
                ss << SanitizeType(arg.getType());
            }
            if (func->isVarArg()) {
                ss << ", ...";
            }
            ss << ")\n";
        } else {
            auto str(SanitizeType(gv->getType()));
            ss << '@' << sym << " = external global " << str.substr(0, str.length() - 1) << '\n';
        }
    }

    return ss.str();
}

static llvm::Module* jitCompile(const std::string& String)
{
    std::string asmcode(String);
    llvm::SMDiagnostic pa;

    static std::string sInlineBitcode(IRToBitcode(bitcodeDotLLString()));

    // Create some module to put our function into it.
    std::unique_ptr<llvm::Module> newModule;
    std::string declarations = globalDeclarations(asmcode);

    // std::cout << "**** DECL ****\n" << declarations << "**** ENDDECL ****\n" << std::endl;

    // The first file we compile is init.ll, and we don't want to prepend inline.ll, or any global
    // declarations to it.
    static bool shouldPrepend(false);
    if (shouldPrepend) {
        auto modOrErr(parseBitcodeFile(llvm::MemoryBufferRef(sInlineBitcode, "<string>"), llvm::getGlobalContext()));
        if (likely(modOrErr)) {
            newModule = std::move(modOrErr.get());
            asmcode = inlineDotLLString() + declarations + asmcode;
            if (parseAssemblyInto(llvm::MemoryBufferRef(asmcode, "<string>"), *newModule, pa)) {
                std::cout << "**** DECL ****\n" << declarations << "**** ENDDECL ****\n" << std::endl;
                newModule.reset();
            }
        }
    } else {
        newModule = parseAssemblyString(asmcode, pa, llvm::getGlobalContext());
        shouldPrepend = true;
    }
    if (newModule) {
        if (unlikely(!extemp::UNIV::ARCH.empty())) {
            newModule->setTargetTriple(extemp::UNIV::ARCH);
        }
        if (EXTLLVM::OPTIMIZE_COMPILES) {
            extemp::EXTLLVM::PM->run(*newModule);
        } else {
            extemp::EXTLLVM::PM_NO->run(*newModule);
        }
    }

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

