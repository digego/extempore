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
#include <iostream>
#include <sstream>
#include <string>

// must be included before anything which pulls in <Windows.h>
#include "llvm/ADT/StringExtras.h"
#include "llvm/AsmParser/Parser.h"
#include "llvm-c/Core.h"
#include "llvm/Bitcode/BitcodeWriter.h"

#include "llvm/ExecutionEngine/Orc/LLJIT.h"
#include "llvm/ExecutionEngine/Orc/ThreadSafeModule.h"

#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/OptimizationLevel.h"

#include "llvm/IR/CallingConv.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/raw_os_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/TargetParser/Host.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/Error.h"
#include "llvm/Linker/Linker.h"

// pcre2 is width-agnostic: the code unit width has to be picked before the
// header, and the 8-bit library is the one src/ffi/regex.inc uses.
#define PCRE2_CODE_UNIT_WIDTH 8
#include <pcre2.h>

#include "SchemeFFI.h"
#include "AudioDevice.h"
#include "UNIV.h"
#include "TaskScheduler.h"
#include "SchemeProcess.h"
#include "SchemeREPL.h"
#include <unordered_set>
#include <map>
#include <atomic>
#include <string_view>

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

////////////////////////////////

#ifdef __APPLE__
#include <malloc/malloc.h>
#else
#include <time.h>
#endif

#ifdef _WIN32
// #include <unistd.h>
#include <malloc.h>
#elif __APPLE__
#include <Cocoa/Cocoa.h>
#include <CoreFoundation/CoreFoundation.h>
#include <AppKit/AppKit.h>
#endif

#define PRINT_ERROR(format, ...)                                                                   \
    ascii_error();                                                                                 \
    printf(format, ##__VA_ARGS__);                                                                 \
    ascii_normal()

#include <queue>
// #include <unistd.h>
#include <EXTLLVM.h>
#include <IRPreamble.h>
namespace extemp {
namespace SchemeFFI {
static llvm::Module* jitCompile(const std::string& String);
}
}  // namespace extemp

namespace extemp {

namespace SchemeFFI {

static std::string formatLLVMType(llvm::Type* Type) {
    if (auto* ST = llvm::dyn_cast<llvm::StructType>(Type)) {
        if (ST->hasName()) {
            llvm::StringRef name = ST->getName();
            auto dotPos = name.rfind('.');
            if (dotPos != llvm::StringRef::npos) {
                llvm::StringRef suffix = name.substr(dotPos + 1);
                bool isNumericSuffix =
                    !suffix.empty() && std::all_of(suffix.begin(), suffix.end(), ::isdigit);
                if (isNumericSuffix) {
                    return "%" + name.substr(0, dotPos).str();
                }
            }
            return "%" + name.str();
        }
    }
    std::string result;
    llvm::raw_string_ostream ss(result);
    Type->print(ss);
    return ss.str();
}

#include "ffi/utility.inc"
#include "ffi/ipc.inc"
#include "ffi/number.inc"
#include "ffi/sys.inc"
#include "ffi/sys_dsp.inc"
#include "ffi/sys_zone.inc"
#include "ffi/misc.inc"
#include "ffi/regex.inc"
#include "ffi/llvm.inc"
#include "ffi/clock.inc"

// Track external library function names for calling convention (CallingConv::C).
// These are functions declared via bind-lib.
static std::unordered_set<std::string> sExternalLibFunctionNames;
static std::mutex sExternalLibFunctionNamesMutex;

// The IR preamble (see IRPreamble.h): what each compile may need to reference
// from earlier compiles and from runtime/bitcode.ll. A compile is prepended
// with just the entries its IR transitively references, so parsing cost
// scales with the IR being compiled rather than with everything compiled
// before it.
static IRPreamble::Entries sPreamble;
static std::mutex sPreambleMutex;
static bool sRuntimeHelpersLoaded = false;

void initSchemeFFI(scheme* sc) {
    static struct {
        const char* name;
        uint32_t value;
    } integerTable[] = {
        {"*au:block-size*", UNIV::NUM_FRAMES},
        {"*au:samplerate*", UNIV::SAMPLE_RATE},
        {"*au:channels*", UNIV::CHANNELS},
        {"*au:in-channels*", UNIV::IN_CHANNELS},
    };
    for (auto& elem : integerTable) {
        scheme_define(sc, sc->global_env, mk_symbol(sc, elem.name), mk_integer(sc, elem.value));
    }
    static struct {
        const char* name;
        foreign_func func;
    } funcTable[] = {UTILITY_DEFS,  IPC_DEFS,  NUMBER_DEFS, SYS_DEFS,  SYS_DSP_DEFS,
                     SYS_ZONE_DEFS, MISC_DEFS, REGEX_DEFS, LLVM_DEFS,   CLOCK_DEFS};
    for (auto& elem : funcTable) {
        scheme_define(sc, sc->global_env, mk_symbol(sc, elem.name), mk_foreign_func(sc, elem.func));
    }
}

static long long llvm_emitcounter = 0;

// Check if a symbol is an external library function (uses C calling convention).
static bool isExternalLibFunction(const std::string& name) {
    std::lock_guard<std::mutex> lock(sExternalLibFunctionNamesMutex);
    return sExternalLibFunctionNames.find(name) != sExternalLibFunctionNames.end();
}

// Register a symbol as an external library function.
static void registerExternalLibFunction(const std::string& name) {
    std::lock_guard<std::mutex> lock(sExternalLibFunctionNamesMutex);
    sExternalLibFunctionNames.insert(name);
}

// The "declare ..." line that lets a later module reference `func`.
static std::string functionDeclaration(const llvm::Function& func) {
    std::string declStr;
    llvm::raw_string_ostream ss(declStr);
    ss << "declare ";
    if (func.getCallingConv() == llvm::CallingConv::Fast) {
        ss << "fastcc ";
    } else if (func.getCallingConv() == llvm::CallingConv::C) {
        ss << "ccc ";
    }
    auto* funcType = func.getFunctionType();
    funcType->getReturnType()->print(ss, false, true);
    ss << " @" << func.getName() << "(";
    bool first = true;
    for (unsigned i = 0; i < funcType->getNumParams(); ++i) {
        if (!first)
            ss << ", ";
        first = false;
        funcType->getParamType(i)->print(ss, false, true);
    }
    if (funcType->isVarArg()) {
        if (!first)
            ss << ", ";
        ss << "...";
    }
    ss << ")";
    if (func.hasFnAttribute(llvm::Attribute::NoUnwind)) {
        ss << " nounwind";
    }
    ss << "\n";
    return ss.str();
}

// Load runtime/bitcode.ll into the preamble maps, once. The file is parsed
// with LLVM first so a mistake in it is reported here rather than by whichever
// later compile happens to reference the broken helper. Caller must hold
// sPreambleMutex.
static bool loadRuntimeHelpersLockless(llvm::LLVMContext& ctx) {
    if (sRuntimeHelpersLoaded) {
        return true;
    }
    std::ifstream inStream(UNIV::SHARE_DIR + "/runtime/bitcode.ll");
    std::stringstream ss;
    ss << inStream.rdbuf();
    const std::string text = ss.str();

    llvm::SMDiagnostic diag;
    if (!llvm::parseAssemblyString(text, diag, ctx)) {
        std::cerr << "Failed to parse bitcode.ll: " << diag.getMessage().str() << std::endl;
        return false;
    }

    IRPreamble::captureTypeDefs(sPreamble, text);
    IRPreamble::captureExternalGlobals(sPreamble, text);
    IRPreamble::captureDeclaresAndDefines(sPreamble, text);
    sRuntimeHelpersLoaded = true;
    return true;
}

static llvm::Module* jitCompile(const std::string& irString) {
    using namespace llvm;

    char modname[256];
    snprintf(modname, sizeof(modname), "xtmmodule_%lld", ++llvm_emitcounter);

    Module* modulePtr = nullptr;

    EXTLLVM::getThreadSafeContext().withContextDo([&](LLVMContext* ctx) {
        auto mod = std::make_unique<Module>(modname, *ctx);
        if (!extemp::UNIV::ARCH.empty()) {
            mod->setTargetTriple(Triple(extemp::UNIV::ARCH));
        }
        if (EXTLLVM::JIT) {
            mod->setDataLayout(EXTLLVM::JIT->getDataLayout());
        }

        // Parse the IR with the preamble it needs prepended.
        std::string fullIR;
        {
            std::lock_guard<std::mutex> lock(sPreambleMutex);
            if (!loadRuntimeHelpersLockless(*ctx)) {
                return;
            }
            fullIR = IRPreamble::select(sPreamble, irString);
        }
        fullIR += irString;
        SMDiagnostic diag;
        if (parseAssemblyInto(MemoryBufferRef(fullIR, "<user>"), mod.get(), nullptr, diag)) {
            std::string errstr;
            raw_string_ostream ss(errstr);
            diag.print("LLVM IR", ss);
            printf("%s\n", ss.str().c_str());
            return;
        }
        // Verify now, so malformed IR is a diagnostic rather than something the
        // optimiser trips over.
        if (EXTLLVM::VERIFY_COMPILES) {
            std::string verifyErrors;
            raw_string_ostream verifyStream(verifyErrors);
            if (verifyModule(*mod, &verifyStream)) {
                std::cerr << "Invalid LLVM IR for " << modname << ":\n"
                          << verifyErrors << std::endl;
                return;
            }
        }

        // Capture new declarations into the preamble so subsequent
        // compilations can reference them. This handles both individual bind-lib
        // declarations (small IR) and AOT-cached .ll files (large IR).
        // We capture before optimization because LLVM may drop unused declarations.
        bool isBindLibDeclaration = (irString.find("declare") == 0 && irString.size() < 500);
        {
            std::lock_guard<std::mutex> lock(sPreambleMutex);
            for (const auto& func : mod->functions()) {
                if (!func.isDeclaration() || func.isIntrinsic())
                    continue;

                std::string name = func.getName().str();
                if (sPreamble.funcDecls.count(name))
                    continue;

                // For bind-lib declarations, register as external library function
                // so the JIT uses C calling convention.
                if (isBindLibDeclaration) {
                    registerExternalLibFunction(name);
                }

                IRPreamble::add(sPreamble.funcDecls, name, functionDeclaration(func));
            }
        }

        // Add function/global declarations for previously compiled symbols.
        // This allows the current module to reference symbols from earlier compiles.
        for (auto& func : mod->functions()) {
            if (!func.isDeclaration())
                continue;
            if (func.isIntrinsic())
                continue;

            std::string name = func.getName().str();

            // Look up in global map of compiled functions.
            auto gv = EXTLLVM::getGlobalValue(name.c_str());
            if (!gv)
                continue;

            if (auto srcFunc = dyn_cast<Function>(gv)) {
                auto funcType = srcFunc->getFunctionType();
                auto callee = mod->getOrInsertFunction(name, funcType);
                if (auto* newFunc = dyn_cast<Function>(callee.getCallee())) {
                    if (newFunc->isDeclaration()) {
                        if (isExternalLibFunction(name)) {
                            newFunc->setCallingConv(CallingConv::C);
                        } else {
                            newFunc->setCallingConv(srcFunc->getCallingConv());
                        }
                    }
                }
            }
        }

        for (const auto& global : mod->globals()) {
            if (!global.isDeclaration())
                continue;

            std::string name = global.getName().str();

            auto gv = EXTLLVM::getGlobalValue(name.c_str());
            if (!gv)
                continue;

            if (auto srcGlobal = dyn_cast<GlobalVariable>(gv)) {
                mod->getOrInsertGlobal(name, srcGlobal->getValueType());
            }
        }

        // Optimise.
        if (EXTLLVM::OPTIMIZE_COMPILES) {
            LoopAnalysisManager LAM;
            FunctionAnalysisManager FAM;
            CGSCCAnalysisManager CGAM;
            ModuleAnalysisManager MAM;

            PassBuilder PB;
            PB.registerModuleAnalyses(MAM);
            PB.registerCGSCCAnalyses(CGAM);
            PB.registerFunctionAnalyses(FAM);
            PB.registerLoopAnalyses(LAM);
            PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

            OptimizationLevel optLevel;
            switch (EXTLLVM::OPTIMIZATION_LEVEL) {
            case 0:
                optLevel = OptimizationLevel::O0;
                break;
            case 1:
                optLevel = OptimizationLevel::O1;
                break;
            case 3:
                optLevel = OptimizationLevel::O3;
                break;
            case 2:
            default:
                optLevel = OptimizationLevel::O2;
                break;
            }
            ModulePassManager MPM = PB.buildPerModuleDefaultPipeline(optLevel);
            MPM.run(*mod, MAM);
        }

        // Work out what the module exports and imports, clone it for
        // the metadata view, and hand both to the JIT under one tracker.
        auto symbols = EXTLLVM::collectModuleSymbols(*mod);
        auto exportedNames = symbols.exports;
        auto metadataModule = CloneModule(*mod);
        llvm::Module* metadata = metadataModule.get();

        auto TSM = orc::ThreadSafeModule(std::move(mod), EXTLLVM::getThreadSafeContext());
        auto err = EXTLLVM::addTrackedModule(std::move(TSM), std::move(symbols),
                                             std::move(metadataModule));

        if (err) {
            std::cerr << "Failed to add module " << modname
                      << " to JIT: " << toString(std::move(err)) << std::endl;
            modulePtr = nullptr;
        } else {
            modulePtr = metadata;

            for (const auto& name : exportedNames) {
                EXTLLVM::registerAdhocAlias(name);
            }

            // Add declarations for newly defined symbols to the IR preamble.
            // This allows subsequent compilations to reference these symbols.
            // We also need to add any new type definitions from the module.
            // Local (private/internal) symbols are skipped: no other module can
            // reference them, and the bitcode.ll helpers among them already
            // have their definitions in the preamble.
            {
                std::lock_guard<std::mutex> lock(sPreambleMutex);

                // First, add any identified struct types from the module.
                for (auto* structType : metadata->getIdentifiedStructTypes()) {
                    if (structType->hasName() && !structType->isOpaque()) {
                        std::string name = structType->getName().str();
                        if (sPreamble.types.count(name)) {
                            continue;
                        }

                        std::string typeStr;
                        raw_string_ostream ts(typeStr);
                        ts << "%" << name << " = type ";

                        if (structType->isPacked()) {
                            ts << "<{ ";
                        } else {
                            ts << "{ ";
                        }
                        for (unsigned i = 0; i < structType->getNumElements(); ++i) {
                            if (i > 0)
                                ts << ", ";
                            structType->getElementType(i)->print(ts, false, true);
                        }
                        if (structType->isPacked()) {
                            ts << " }>";
                        } else {
                            ts << " }";
                        }
                        ts << "\n";
                        IRPreamble::add(sPreamble.types, name, ts.str());
                    }
                }

                // Now add function declarations for newly defined functions.
                for (const auto& func : metadata->functions()) {
                    if (func.isDeclaration() || func.hasLocalLinkage())
                        continue;

                    std::string name = func.getName().str();
                    if (sPreamble.funcDecls.count(name)) {
                        continue;
                    }

                    IRPreamble::add(sPreamble.funcDecls, name, functionDeclaration(func));
                }

                for (const auto& glob : metadata->globals()) {
                    if (glob.hasLocalLinkage()) {
                        continue;
                    }
                    std::string name = glob.getName().str();
                    if (sPreamble.globals.count(name)) {
                        continue;
                    }

                    std::string declStr;
                    raw_string_ostream ss(declStr);

                    ss << "@" << name << " = external ";
                    if (glob.isConstant()) {
                        ss << "constant ";
                    } else {
                        ss << "global ";
                    }
                    glob.getValueType()->print(ss, false, true);
                    ss << "\n";

                    IRPreamble::add(sPreamble.globals, name, ss.str());
                }

                // Also extract external global declarations from the original IR string.
                // LLVM drops unused external declarations during parsing, so we need to
                // capture them from the source IR to make them available to subsequent modules.
                IRPreamble::captureExternalGlobals(sPreamble, irString);
                IRPreamble::captureTypeDefs(sPreamble, irString);
            }
        }
    });

    return modulePtr;
}

}  // namespace SchemeFFI

}  // namespace extemp
