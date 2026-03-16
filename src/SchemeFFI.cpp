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

// must be included before anything which pulls in <Windows.h>
#include "llvm/ADT/StringExtras.h"
#include "llvm/AsmParser/Parser.h"
#include "llvm-c/Core.h"
#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/Bitcode/BitcodeReader.h"

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

#include "SchemeFFI.h"
#include "AudioDevice.h"
#include "UNIV.h"
#include "TaskScheduler.h"
#include "SchemeProcess.h"
#include "SchemeREPL.h"
#include <unordered_set>
#include <unordered_map>
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

#ifdef DYLIB
#include <cmrc/cmrc.hpp>
CMRC_DECLARE(xtm);
#endif

// setting this define should make call_compiled thread safe BUT ...
// also extremely SLOW !

#define LLVM_EE_LOCK

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

#define PRINT_ERROR(format, ...)            \
    ascii_error();                   \
    printf(format, ##__VA_ARGS__);           \
    ascii_normal()

#include <queue>
//#include <unistd.h>
#include <EXTMutex.h>
#include <EXTLLVM.h>
namespace extemp { namespace SchemeFFI {
static llvm::Module* jitCompile(const std::string& String);
}}

namespace extemp {

namespace SchemeFFI {

static std::string formatLLVMType(llvm::Type* Type)
{
    if (auto* ST = llvm::dyn_cast<llvm::StructType>(Type)) {
        if (ST->hasName()) {
            llvm::StringRef name = ST->getName();
            auto dotPos = name.rfind('.');
            if (dotPos != llvm::StringRef::npos) {
                llvm::StringRef suffix = name.substr(dotPos + 1);
                bool isNumericSuffix = !suffix.empty() &&
                    std::all_of(suffix.begin(), suffix.end(), ::isdigit);
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
#include "ffi/assoc.inc"
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

// Cached template module (parsed bitcode.ll) and its binary form for fast cloning.
static std::string sTemplateBitcode;
// IR declarations keyed by bare name (without % or @ prefix), prepended to every user IR.
static std::unordered_map<std::string, std::string> sTypeDefs;
static std::unordered_map<std::string, std::string> sFuncDecls;
static std::unordered_map<std::string, std::string> sGlobalDecls;
// Global/function names defined in the template module (bitcode.ll).
// Declarations for these must not be added to the maps above, since they
// already exist in every cloned template module and would cause redefinitions.
static std::unordered_set<std::string> sTemplateGlobalNames;
static std::mutex sTemplateMutex;
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

// Extract names of types, functions, and globals that are declared or defined
// in the given IR string.  This is used to avoid emitting duplicate preamble
// entries when the user IR (e.g. an AOT-compiled .ll file) already contains them.
// We scan line-by-line, which is O(n) in the IR size and done at most once per
// jitCompile call.
struct IRNames {
    std::unordered_set<std::string> types;
    std::unordered_set<std::string> funcs;
    std::unordered_set<std::string> globals;
};

static IRNames extractIRNames(const std::string& irString) {
    IRNames names;
    size_t pos = 0;
    while (pos < irString.size()) {
        size_t lineEnd = irString.find('\n', pos);
        if (lineEnd == std::string::npos) lineEnd = irString.size();
        size_t lineLen = lineEnd - pos;

        // Use a string_view bounded to this line to avoid O(n^2) scans.
        std::string_view line(irString.data() + pos, lineLen);

        // "%Name = type " at start of line
        if (line.size() > 0 && line[0] == '%') {
            auto eq = line.find(" = type ");
            if (eq != std::string_view::npos) {
                names.types.emplace(line.substr(1, eq - 1));
            }
        }
        // "declare ... @name(" or "define ... @name("
        else if ((line.size() >= 7 && line.compare(0, 7, "declare") == 0) ||
                 (line.size() >= 6 && line.compare(0, 6, "define") == 0)) {
            auto atPos = line.find('@');
            if (atPos != std::string_view::npos) {
                auto nameEnd = line.find('(', atPos);
                if (nameEnd != std::string_view::npos) {
                    names.funcs.emplace(line.substr(atPos + 1, nameEnd - atPos - 1));
                }
            }
        }
        // "@name = ..." at start of line (global definition)
        else if (line.size() > 0 && line[0] == '@') {
            auto eq = line.find(" = ");
            if (eq != std::string_view::npos) {
                names.globals.emplace(line.substr(1, eq - 1));
            }
        }

        pos = lineEnd + 1;
    }
    return names;
}

// Build the preamble string from the three maps (types, then functions, then globals).
// When irString is provided, skip any declarations already present in it to avoid
// "invalid redefinition" errors (e.g. when loading AOT-compiled .ll files that
// contain their own declarations for bind-lib functions).
// NOTE: caller must hold sTemplateMutex.
static std::string buildPreamble(const std::string& irString = "") {
    std::string preamble;
    preamble.reserve(sTypeDefs.size() * 80 + sFuncDecls.size() * 120 + sGlobalDecls.size() * 60);

    IRNames existing;
    if (!irString.empty()) {
        existing = extractIRNames(irString);
    }

    for (const auto& [name, val] : sTypeDefs) {
        if (existing.types.count(name)) continue;
        preamble += val;
    }

    for (const auto& [name, val] : sFuncDecls) {
        if (existing.funcs.count(name)) continue;
        preamble += val;
    }

    for (const auto& [name, val] : sGlobalDecls) {
        if (existing.globals.count(name)) continue;
        preamble += val;
    }

    return preamble;
}

// Extract external global declarations from IR string and add to sGlobalDecls.
// This handles globals that are declared but not defined (e.g., @SAMPLE_RATE = external global i32).
// These get dropped by LLVM if they're not used in the same module.
// NOTE: lockless version - caller must hold sTemplateMutex.
static void extractExternalGlobalsLockless(const std::string& irString) {
    std::istringstream stream(irString);
    std::string line;
    while (std::getline(stream, line)) {
        // Strip trailing CR
        if (!line.empty() && line.back() == '\r') {
            line.pop_back();
        }
        // Look for pattern: @name = external global type
        if (line.size() > 1 && line[0] == '@') {
            size_t extPos = line.find(" = external global ");
            if (extPos != std::string::npos) {
                std::string bareName = line.substr(1, extPos - 1);
                if (sTemplateGlobalNames.count(bareName)) {
                    continue;
                }
                sGlobalDecls.emplace(bareName, line + "\n");
            }
        }
    }
}

// Extract type definitions from a line (handles CRLF safely).
// Returns the type definition line if it matches "%name = type ...", empty otherwise.
static std::string extractTypeDef(const std::string& line) {
    size_t start = 0;
    size_t end = line.size();
    // Skip leading whitespace
    while (start < end && (line[start] == ' ' || line[start] == '\t')) {
        start++;
    }
    // Skip trailing whitespace and CR
    while (end > start && (line[end-1] == ' ' || line[end-1] == '\t' ||
                           line[end-1] == '\r' || line[end-1] == '\n')) {
        end--;
    }
    if (start >= end) return "";

    std::string trimmed = line.substr(start, end - start);
    // Check for type definition pattern: %name = type ...
    if (trimmed.size() > 1 && trimmed[0] == '%') {
        size_t eqPos = trimmed.find(" = type ");
        if (eqPos != std::string::npos) {
            return trimmed + "\n";
        }
    }
    return "";
}

// Initialize template module from bitcode.ll (called once, thread-safe).
static bool initializeTemplateModule(llvm::LLVMContext& ctx) {
    std::lock_guard<std::mutex> lock(sTemplateMutex);
    if (!sTemplateBitcode.empty()) {
        return true;
    }

    std::string inlineString;
#ifdef DYLIB
    auto fs = cmrc::xtm::get_filesystem();
    auto data = fs.open("runtime/bitcode.ll");
    inlineString = std::string(data.begin(), data.end());
#else
    std::ifstream inStream(UNIV::SHARE_DIR + "/runtime/bitcode.ll");
    std::stringstream ss;
    ss << inStream.rdbuf();
    inlineString = ss.str();
#endif

    // Extract type definitions and declarations (line by line to handle CRLF safely).
    // Declarations are needed so user IR can reference runtime symbols during parsing.
    std::istringstream lineStream(inlineString);
    std::string line;
    while (std::getline(lineStream, line)) {
        // Strip trailing CR if present (Windows CRLF).
        if (!line.empty() && line.back() == '\r') {
            line.pop_back();
        }
        
        std::string typeDef = extractTypeDef(line);
        if (!typeDef.empty()) {
            size_t eqPos = typeDef.find(" = type ");
            if (eqPos != std::string::npos) {
                std::string bareName = typeDef.substr(1, eqPos - 1);
                sTypeDefs.emplace(bareName, typeDef);
            }
            continue;
        }

    }

    // Parse template module to create the binary bitcode for fast cloning.
    llvm::SMDiagnostic diag;
    auto templateModule = llvm::parseAssemblyString(inlineString, diag, ctx);
    if (!templateModule) {
        std::cerr << "Failed to parse bitcode.ll: " << diag.getMessage().str() << std::endl;
        return false;
    }

    for (const auto& global : templateModule->globals()) {
        sTemplateGlobalNames.insert(global.getName().str());
    }
    for (const auto& func : templateModule->functions()) {
        sTemplateGlobalNames.insert(func.getName().str());
    }

    llvm::raw_string_ostream bitstream(sTemplateBitcode);
    llvm::WriteBitcodeToFile(*templateModule, bitstream);

    return true;
}

// Clone the template module for a new compilation.
static std::unique_ptr<llvm::Module> cloneTemplateModule(llvm::LLVMContext& ctx) {
    std::lock_guard<std::mutex> lock(sTemplateMutex);
    if (sTemplateBitcode.empty()) {
        return nullptr;
    }

    auto modOrErr = llvm::parseBitcodeFile(
        llvm::MemoryBufferRef(sTemplateBitcode, "<template>"), ctx);
    if (modOrErr) {
        return std::move(modOrErr.get());
    }
    llvm::consumeError(modOrErr.takeError());
    return nullptr;
}

static llvm::Module* jitCompile(const std::string& irString)
{
    using namespace llvm;

    char modname[256];
    snprintf(modname, sizeof(modname), "xtmmodule_%lld", ++llvm_emitcounter);

    Module* modulePtr = nullptr;

    EXTLLVM::getThreadSafeContext().withContextDo([&](LLVMContext* ctx) {
        // Step 1: Initialize template module (first time only).
        static bool templateInitialized = false;
        if (!templateInitialized) {
            if (!initializeTemplateModule(*ctx)) {
                std::cerr << "Failed to initialize template module" << std::endl;
                return;
            }
            templateInitialized = true;
        }

        // Step 2: Clone the template module for each compilation.
        // The template module contains bitcode.ll runtime helpers.
        // Use LinkOnceODR linkage so duplicate definitions are resolved by the linker.
        auto baseModule = cloneTemplateModule(*ctx);
        if (!baseModule) {
            std::cerr << "Failed to clone template module" << std::endl;
            return;
        }
        
        // Set linkage to LinkOnceODR so the linker can deduplicate across modules.
        for (auto& func : baseModule->functions()) {
            if (!func.isDeclaration() && !func.isIntrinsic()) {
                func.setLinkage(GlobalValue::LinkOnceODRLinkage);
            }
        }
        for (auto& global : baseModule->globals()) {
            if (global.hasInitializer()) {
                global.setLinkage(GlobalValue::LinkOnceODRLinkage);
            }
        }
        baseModule->setModuleIdentifier(modname);
        
        // Set target triple and data layout.
        if (!extemp::UNIV::ARCH.empty()) {
            baseModule->setTargetTriple(Triple(extemp::UNIV::ARCH));
        }
        if (EXTLLVM::JIT) {
            baseModule->setDataLayout(EXTLLVM::JIT->getDataLayout());
        }

        // Step 3: Parse user IR with type definitions prepended.
        std::string fullIR = buildPreamble(irString) + irString;
        SMDiagnostic diag;
        if (parseAssemblyInto(MemoryBufferRef(fullIR, "<user>"), baseModule.get(), nullptr, diag)) {
            std::string errstr;
            raw_string_ostream ss(errstr);
            diag.print("LLVM IR", ss);
            printf("%s\n", ss.str().c_str());
            return;
        }

        // Step 4: Capture new declarations into sFuncDecls so subsequent
        // compilations can reference them. This handles both individual bind-lib
        // declarations (small IR) and AOT-cached .ll files (large IR).
        // We capture before optimization because LLVM may drop unused declarations.
        bool isBindLibDeclaration = (irString.find("declare") == 0 && irString.size() < 500);
        {
            std::lock_guard<std::mutex> lock(sTemplateMutex);
            for (const auto& func : baseModule->functions()) {
                if (!func.isDeclaration() || func.isIntrinsic()) continue;

                std::string name = func.getName().str();
                if (sTemplateGlobalNames.count(name)) continue;
                if (sFuncDecls.count(name)) continue;

                // For bind-lib declarations, register as external library function
                // so the JIT uses C calling convention.
                if (isBindLibDeclaration) {
                    registerExternalLibFunction(name);
                }

                std::string declStr;
                raw_string_ostream ss(declStr);
                ss << "declare ";
                if (func.getCallingConv() == CallingConv::Fast) {
                    ss << "fastcc ";
                } else if (func.getCallingConv() == CallingConv::C) {
                    ss << "ccc ";
                }
                auto* funcType = func.getFunctionType();
                funcType->getReturnType()->print(ss, false, true);
                ss << " @" << name << "(";
                bool first = true;
                for (unsigned i = 0; i < funcType->getNumParams(); ++i) {
                    if (!first) ss << ", ";
                    first = false;
                    funcType->getParamType(i)->print(ss, false, true);
                }
                if (funcType->isVarArg()) {
                    if (!first) ss << ", ";
                    ss << "...";
                }
                ss << ")";
                if (func.hasFnAttribute(Attribute::NoUnwind)) {
                    ss << " nounwind";
                }
                ss << "\n";
                sFuncDecls.emplace(name, ss.str());
            }
        }

        // Step 5: Add function/global declarations for previously compiled symbols.
        // This allows the current module to reference symbols from earlier compiles.
        for (auto& func : baseModule->functions()) {
            if (!func.isDeclaration()) continue;
            if (func.isIntrinsic()) continue;

            std::string name = func.getName().str();

            // Look up in global map of compiled functions.
            auto gv = EXTLLVM::getGlobalValue(name.c_str());
            if (!gv) continue;

            if (auto srcFunc = dyn_cast<Function>(gv)) {
                auto funcType = srcFunc->getFunctionType();
                auto callee = baseModule->getOrInsertFunction(name, funcType);
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

        for (const auto& global : baseModule->globals()) {
            if (!global.isDeclaration()) continue;

            std::string name = global.getName().str();

            auto gv = EXTLLVM::getGlobalValue(name.c_str());
            if (!gv) continue;

            if (auto srcGlobal = dyn_cast<GlobalVariable>(gv)) {
                baseModule->getOrInsertGlobal(name, srcGlobal->getValueType());
            }
        }

        // Step 7: Optimize.
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
                case 0: optLevel = OptimizationLevel::O0; break;
                case 1: optLevel = OptimizationLevel::O1; break;
                case 3: optLevel = OptimizationLevel::O3; break;
                case 2:
                default: optLevel = OptimizationLevel::O2; break;
            }
            ModulePassManager MPM = PB.buildPerModuleDefaultPipeline(optLevel);
            MPM.run(*baseModule, MAM);
        }

        // Step 8: Verify.
        if (EXTLLVM::VERIFY_COMPILES) {
            std::string verifyErrors;
            raw_string_ostream verifyStream(verifyErrors);
            bool invalid = verifyModule(*baseModule, &verifyStream);
            if (invalid) {
                std::cerr << "Invalid LLVM IR for " << modname << ":\n" << verifyErrors << std::endl;
                return;
            }
        }

        modulePtr = baseModule.get();

        // Step 9: Extract symbol names (only non-declarations defined in this module).
        std::vector<std::string> symbolNames;
        for (const auto& func : baseModule->getFunctionList()) {
            if (!func.isDeclaration()) {
                symbolNames.push_back(func.getName().str());
            }
        }
        for (const auto& glob : baseModule->globals()) {
            if (!glob.isDeclaration()) {
                symbolNames.push_back(glob.getName().str());
            }
        }

        // Step 10: Clone for metadata tracking.
        auto metadataModule = CloneModule(*baseModule);

        // Step 11: Add to ORC JIT.
        auto TSM = orc::ThreadSafeModule(std::move(baseModule),
                                          EXTLLVM::getThreadSafeContext());
        auto err = EXTLLVM::addTrackedModule(std::move(TSM), symbolNames);

        if (err) {
            std::cerr << "Failed to add module " << modname << " to JIT: "
                      << toString(std::move(err)) << std::endl;
            modulePtr = nullptr;
        } else {
            modulePtr = metadataModule.get();
            EXTLLVM::addModule(metadataModule.get());

            for (const auto& name : symbolNames) {
                EXTLLVM::registerAdhocAlias(name);
            }

            // Add declarations for newly defined symbols to the IR preamble maps.
            // This allows subsequent compilations to reference these symbols.
            // We also need to add any new type definitions from the module.
            {
                std::lock_guard<std::mutex> lock(sTemplateMutex);
                
                // First, add any identified struct types from the module.
                for (auto* structType : metadataModule->getIdentifiedStructTypes()) {
                    if (structType->hasName() && !structType->isOpaque()) {
                        std::string name = structType->getName().str();
                        if (sTypeDefs.count(name)) {
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
                            if (i > 0) ts << ", ";
                            structType->getElementType(i)->print(ts, false, true);
                        }
                        if (structType->isPacked()) {
                            ts << " }>";
                        } else {
                            ts << " }";
                        }
                        ts << "\n";
                        sTypeDefs.emplace(name, ts.str());
                    }
                }
                
                // Now add function declarations for newly defined functions.
                for (const auto& func : metadataModule->functions()) {
                    if (func.isDeclaration()) continue;

                    std::string name = func.getName().str();
                    if (sFuncDecls.count(name)) {
                        continue;
                    }

                    std::string declStr;
                    raw_string_ostream ss(declStr);

                    ss << "declare ";
                    if (func.getCallingConv() == CallingConv::Fast) {
                        ss << "fastcc ";
                    } else if (func.getCallingConv() == CallingConv::C) {
                        ss << "ccc ";
                    }

                    auto* funcType = func.getFunctionType();
                    funcType->getReturnType()->print(ss, false, true);
                    ss << " @" << name << "(";

                    bool first = true;
                    for (unsigned i = 0; i < funcType->getNumParams(); ++i) {
                        if (!first) ss << ", ";
                        first = false;
                        funcType->getParamType(i)->print(ss, false, true);
                    }
                    if (funcType->isVarArg()) {
                        if (!first) ss << ", ";
                        ss << "...";
                    }
                    ss << ")";

                    if (func.hasFnAttribute(Attribute::NoUnwind)) {
                        ss << " nounwind";
                    }
                    ss << "\n";

                    sFuncDecls.emplace(name, ss.str());
                }
                
                for (const auto& glob : metadataModule->globals()) {
                    std::string name = glob.getName().str();
                    if (sTemplateGlobalNames.count(name)) {
                        continue;
                    }
                    if (sGlobalDecls.count(name)) {
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

                    sGlobalDecls.emplace(name, ss.str());
                }
                
                // Also extract external global declarations from the original IR string.
                // LLVM drops unused external declarations during parsing, so we need to
                // capture them from the source IR to make them available to subsequent modules.
                extractExternalGlobalsLockless(irString);
                
                // Extract type definitions from the user IR string.
                // LLVM's module may not preserve forward declarations or opaque types,
                // so we need to capture them from the source IR as well.
                std::istringstream irStream(irString);
                std::string irLine;
                while (std::getline(irStream, irLine)) {
                    if (!irLine.empty() && irLine.back() == '\r') {
                        irLine.pop_back();
                    }
                    std::string typeDef = extractTypeDef(irLine);
                    if (!typeDef.empty()) {
                        size_t eqPos = typeDef.find(" = type ");
                        if (eqPos != std::string::npos) {
                            std::string bareName = typeDef.substr(1, eqPos - 1);
                            sTypeDefs.emplace(bareName, typeDef);
                        }
                    }
                }
            }
            
            metadataModule.release();
        }
    });

    return modulePtr;
}

}

} // end namespace
