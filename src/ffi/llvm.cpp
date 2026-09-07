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
#include "llvm/ADT/StringExtras.h"
#include "llvm/AsmParser/Parser.h"
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
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/TargetParser/Host.h"
#include "llvm/TargetParser/Triple.h"
#include "llvm/Transforms/Utils/Cloning.h"

#include "SchemeFFIRegistry.h"

#include "EXTLLVM.h"
#include "IRPreamble.h"
#include "UNIV.h"

#include <algorithm>
#include <atomic>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <memory>
#include <mutex>
#include <optional>
#include <sstream>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

// UNIV.h supplies <Windows.h> (behind WIN32_LEAN_AND_MEAN) for GetProcAddress;
// the POSIX build needs dlfcn itself.
#ifndef _WIN32
#include <dlfcn.h>
#endif

namespace extemp {

namespace SchemeFFI {

static llvm::Module* jitCompile(const std::string& String);

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

static pointer optimizeCompiles(scheme* Scheme, pointer Args)
{
    EXTLLVM::OPTIMIZE_COMPILES = (argAt(Scheme, Args, 1) == Scheme->T);
    return Scheme->T;
}

static pointer optimizationLevel(scheme* Scheme, pointer Args)
{
    if (Args == Scheme->NIL) {
        return mk_integer(Scheme, EXTLLVM::OPTIMIZATION_LEVEL);
    }
    // Set optimization level (0-3)
    int level = int(argInt(Scheme, Args, 1));
    if (level < 0) level = 0;
    if (level > 3) level = 3;
    EXTLLVM::OPTIMIZATION_LEVEL = level;
    return mk_integer(Scheme, level);
}

static pointer jitCompileIRString(scheme* Scheme, pointer Args)
{
    auto modulePtr(jitCompile(argString(Scheme, Args, 1)));
    if (!modulePtr) {
        return Scheme->F;
    }
    return mk_cptr(Scheme, modulePtr);
}

static pointer ff_set_name(scheme* Scheme, pointer Args)
{
   pointer x = argAt(Scheme, Args, 1);
   // In the s7 adapter, foreign functions don't expose their raw function pointer.
   // This is a no-op for now — ff_set_name/ff_get_name are used for debugging only.
   (void)x;
   return Scheme->T;
}

static pointer ff_get_name(scheme* Scheme, pointer Args)
{
   pointer x = argAt(Scheme, Args, 1);
   // Return the s7 function name if available
   if (s7_is_function(x)) {
       char* name = s7_object_to_c_string(Scheme->sc, x);
       pointer result = mk_string(Scheme, name ? name : "#<unknown>");
       free(name);
       return result;
   }
   return mk_string(Scheme, "#<unknown>");
}

static pointer get_function(scheme* Scheme, pointer Args)
{
    auto func(extemp::EXTLLVM::getFunction(argString(Scheme, Args, 1)));
    if (!func) {
        return Scheme->F;
    }
    return mk_cptr(Scheme, const_cast<llvm::Function*>(func));
}

static pointer get_globalvar(scheme* Scheme, pointer Args)
{
    auto var(extemp::EXTLLVM::getGlobalVariable(argString(Scheme, Args, 1)));
    if (!var) {
        return Scheme->F;
    }
    return mk_cptr(Scheme, const_cast<llvm::GlobalVariable*>(var));
}

static pointer get_struct_size(scheme* Scheme, pointer Args)
{
    const char* struct_type_str = argString(Scheme, Args, 1);
    unsigned long long hash = string_hash(struct_type_str);
    char name[128];
    snprintf(name, sizeof(name), "_xtmT%llu", hash);
    char assm[1024];
    snprintf(assm, sizeof(assm), "%%%s = type %s", name, struct_type_str);

    long size = -1;
    EXTLLVM::getThreadSafeContext().withContextDo([&](llvm::LLVMContext* ctx) {
    llvm::SMDiagnostic pa;
        auto newM(llvm::parseAssemblyString(assm, pa, *ctx));
    if (!newM) {
            return;
    }
        auto type(llvm::StructType::getTypeByName(*ctx, name));
    if (!type) {
            return;
        }
        const auto& layout = newM->getDataLayout();
        size = layout.getStructLayout(type)->getSizeInBytes();
    });

    if (size < 0) {
        return Scheme->F;
    }
    return mk_integer(Scheme, size);
}

static llvm::StructType* getNamedType(const char* name) {
    llvm::StructType* result = nullptr;
    EXTLLVM::getThreadSafeContext().withContextDo([&](llvm::LLVMContext* ctx) {
        result = llvm::StructType::getTypeByName(*ctx, name);
    });
    return result;
}

static pointer get_named_struct_size(scheme* Scheme, pointer Args)
{
    auto type(getNamedType(argString(Scheme, Args, 1)));
    if (!type) {
        return Scheme->F;
    }
    auto& DL = EXTLLVM::JIT->getDataLayout();
    long size = DL.getStructLayout(type)->getSizeInBytes();
    return mk_integer(Scheme, size);
}

static pointer get_function_args(scheme* Scheme, pointer Args)
{
    auto func(extemp::EXTLLVM::getFunction(argString(Scheme, Args, 1)));
    if (!func) {
        return Scheme->F;
    }
    pointer str = mk_string(Scheme, formatLLVMType(func->getReturnType()).c_str());
    pointer p = cons(Scheme, str, Scheme->NIL);
    for (const auto& arg : func->args()) {
        // the injector must span the cons too: cons can itself trigger GC,
        // and p is only referenced from this frame
        EnvInjector injector(Scheme, p);
        str = mk_string(Scheme, formatLLVMType(arg.getType()).c_str());
        EnvInjector injector2(Scheme, str);
        p = cons(Scheme, str, p);
    }
    return reverse_in_place(Scheme, Scheme->NIL, p);
}

static pointer get_function_varargs(scheme* Scheme, pointer Args)
{
    auto func(extemp::EXTLLVM::getFunction(argString(Scheme, Args, 1)));
    return (func && func->isVarArg()) ? Scheme->T : Scheme->F;
}

static pointer get_function_type(scheme* Scheme, pointer Args)
{
    auto func(extemp::EXTLLVM::getFunction(argString(Scheme, Args, 1)));
    if (!func) {
        return Scheme->F;
    }
    std::string typestr;
    llvm::raw_string_ostream ss(typestr);
    func->getFunctionType()->print(ss);
    return mk_string(Scheme, ss.str().c_str());
}

static pointer get_function_calling_conv(scheme* Scheme, pointer Args)
{
    auto func(extemp::EXTLLVM::getFunction(argString(Scheme, Args, 1)));
    if (!func) {
        return Scheme->F;
    }
    return mk_integer(Scheme, func->getCallingConv());
}

static pointer get_global_variable_type(scheme* Scheme, pointer Args)
{
    using namespace llvm;
    auto var(extemp::EXTLLVM::getGlobalVariable(argString(Scheme, Args, 1)));
    if (!var) {
        return Scheme->F;
    }
    std::string typestr;
    llvm::raw_string_ostream ss(typestr);
    var->getType()->print(ss);
    return mk_string(Scheme, ss.str().c_str());
}

static pointer get_function_pointer(scheme* Scheme, pointer Args)
{
    auto name(argString(Scheme, Args, 1));
    auto addr = EXTLLVM::getFunctionAddress(name);
    if (!addr) {
        return Scheme->F;
    }
    return mk_cptr(Scheme, reinterpret_cast<void*>(addr));
}

// llvm:remove-function, llvm:remove-globalvar and llvm:erase-function are all
// the same operation --- remove a JIT symbol and drop its global-map entry ---
// so one body is registered under all three names. They share an identity, so
// an argument error from any of them reports the first.
static pointer remove_symbol_and_global(scheme* Scheme, pointer Args)
{
    const char* name = argString(Scheme, Args, 1);
    if (EXTLLVM::removeSymbol(name)) {
        EXTLLVM::removeFromGlobalMap(name);
        return Scheme->T;
    }
    return Scheme->F;
}

static pointer llvm_call_void_native(scheme* Scheme, pointer Args)
{
    std::string name(argString(Scheme, Args, 1));

    auto addr = EXTLLVM::getFunctionAddress(name + "_native");
    if (!addr) {
        // Try without _native suffix.
        addr = EXTLLVM::getFunctionAddress(name);
        if (!addr) {
            return Scheme->F;
        }
    }
    auto p = reinterpret_cast<void(*)(void)>(addr);
    p();
    return Scheme->T;
}

static std::atomic<uint64_t> CALL_STUB_COUNTER{0};

static pointer call_compiled(scheme* Scheme, pointer Args)
{
    // Get the llvm::Function* from the cptr argument
    auto func = reinterpret_cast<llvm::Function*>(cptr_value(pair_car(Args)));
    if (unlikely(!func)) {
        printf("No such function\n");
        return Scheme->F;
    }

    auto funcType = func->getFunctionType();
    Args = pair_cdr(Args);

    unsigned lgth = list_length(Scheme, Args);
    if (unlikely(lgth != funcType->getNumParams())) {
        printf("Wrong number of arguments for function!\n");
        return Scheme->F;
    }

    if (!extemp::EXTLLVM::JIT) {
        printf("LLVM JIT not initialized\n");
        return Scheme->F;
    }

    // Resolve the compiled function address (try with and without _native suffix)
    std::string name = func->getName().str();
    auto addr = EXTLLVM::getFunctionAddress(name);
    if (!addr) {
        addr = EXTLLVM::getFunctionAddress(name + "_native");
        if (!addr) {
            return Scheme->F;
        }
    }

    // Marshal arguments into raw values first (outside the JIT context).
    struct ArgValue {
        enum Kind { Int, Float, Double, Ptr } kind;
        llvm::Type* ty;
        uint64_t intVal;
        double doubleVal;
        float floatVal;
        void* ptrVal;
    };

    std::vector<ArgValue> argValues;
    argValues.reserve(lgth);

    pointer argList = Args;
    for (unsigned i = 0; i < lgth; ++i) {
        auto argTy = funcType->getParamType(i);
        pointer p = pair_car(argList);
        argList = pair_cdr(argList);

        switch (argTy->getTypeID()) {
        case llvm::Type::IntegerTyID: {
            if (unlikely(!is_integer(p))) {
                printf("Bad argument type %u\n", i);
                return Scheme->F;
            }
            argValues.push_back({ArgValue::Int, argTy, static_cast<uint64_t>(ivalue(p)), 0.0, 0.0f, nullptr});
            break;
        }
        case llvm::Type::FloatTyID: {
            if (unlikely(!is_real(p))) {
                printf("Bad argument type %u\n", i);
                return Scheme->F;
            }
            float f = static_cast<float>(rvalue(p));
            argValues.push_back({ArgValue::Float, argTy, 0u, 0.0, f, nullptr});
            break;
        }
        case llvm::Type::DoubleTyID: {
            if (unlikely(!is_real(p))) {
                printf("Bad argument type %u\n", i);
                return Scheme->F;
            }
            argValues.push_back({ArgValue::Double, argTy, 0u, rvalue(p), 0.0f, nullptr});
            break;
        }
        case llvm::Type::PointerTyID: {
            void* rawPtr = nullptr;
            if (is_string(p)) {
                rawPtr = static_cast<void*>(string_value(p));
            } else if (is_cptr(p)) {
                rawPtr = cptr_value(p);
            } else {
                printf("Bad argument type %u\n", i);
                return Scheme->F;
            }
            argValues.push_back({ArgValue::Ptr, argTy, 0u, 0.0, 0.0f, rawPtr});
            break;
        }
        default:
            printf("Unsupported argument type at index %u\n", i);
            return Scheme->F;
        }
    }

    const auto& DL = extemp::EXTLLVM::JIT->getDataLayout();
    std::string stubName = "__extemp_call_stub_" + std::to_string(CALL_STUB_COUNTER.fetch_add(1));
    std::unique_ptr<llvm::Module> module;

    // Build a tiny stub inside the thread-safe context.
    extemp::EXTLLVM::getThreadSafeContext().withContextDo([&](llvm::LLVMContext* ctx) {
        auto ptrIntTy = llvm::Type::getIntNTy(*ctx, DL.getPointerSizeInBits());

        std::vector<llvm::Value*> argConsts;
        argConsts.reserve(lgth);

        for (const auto& av : argValues) {
            switch (av.kind) {
            case ArgValue::Int:
                argConsts.push_back(llvm::ConstantInt::get(av.ty, av.intVal));
                break;
            case ArgValue::Float:
                argConsts.push_back(llvm::ConstantFP::get(av.ty, av.floatVal));
                break;
            case ArgValue::Double:
                argConsts.push_back(llvm::ConstantFP::get(av.ty, av.doubleVal));
                break;
            case ArgValue::Ptr: {
                auto ptrAsInt = llvm::ConstantInt::get(ptrIntTy, reinterpret_cast<uintptr_t>(av.ptrVal));
                argConsts.push_back(llvm::ConstantExpr::getIntToPtr(ptrAsInt, av.ty));
                break;
            }
            }
        }

        module = std::make_unique<llvm::Module>(stubName + "_module", *ctx);
        module->setDataLayout(DL);

        auto stubType = llvm::FunctionType::get(funcType->getReturnType(), false);
        auto stubFunc = llvm::Function::Create(stubType, llvm::Function::ExternalLinkage, stubName, module.get());
        stubFunc->setCallingConv(func->getCallingConv());

        auto entryBB = llvm::BasicBlock::Create(*ctx, "entry", stubFunc);
        llvm::IRBuilder<> builder(entryBB);

        auto targetAddrConst = llvm::ConstantInt::get(ptrIntTy, addr);
        auto targetPtr = llvm::ConstantExpr::getIntToPtr(targetAddrConst, llvm::PointerType::get(*ctx, 0));
        llvm::FunctionCallee callee(funcType, targetPtr);

        auto callInst = builder.CreateCall(callee, argConsts);
        callInst->setCallingConv(func->getCallingConv());

        if (funcType->getReturnType()->isVoidTy()) {
            builder.CreateRetVoid();
        } else {
            builder.CreateRet(callInst);
        }
    });

    if (!module) {
        return Scheme->F;
    }

    // JIT the stub; it is removed again once the call has returned.
    auto TSM = llvm::orc::ThreadSafeModule(std::move(module), extemp::EXTLLVM::getThreadSafeContext());
    auto stubTracker = extemp::EXTLLVM::addTransientModule(std::move(TSM));
    if (!stubTracker) {
        std::cerr << "Failed to JIT call stub: " << llvm::toString(stubTracker.takeError()) << std::endl;
        return Scheme->F;
    }

    pointer result = Scheme->F;
    auto stubAddr = EXTLLVM::getFunctionAddress(stubName);
    if (!stubAddr) {
        printf("Failed to resolve call stub\n");
    } else {
        // Call the stub and marshal the result back to Scheme.
        auto retTy = funcType->getReturnType();
        switch (retTy->getTypeID()) {
        case llvm::Type::FloatTyID: {
            using StubFn = float(*)();
            float res = reinterpret_cast<StubFn>(stubAddr)();
            result = mk_real(Scheme, res);
            break;
        }
        case llvm::Type::DoubleTyID: {
            using StubFn = double(*)();
            double res = reinterpret_cast<StubFn>(stubAddr)();
            result = mk_real(Scheme, res);
            break;
        }
        case llvm::Type::IntegerTyID: {
            auto intTy = llvm::cast<llvm::IntegerType>(retTy);
            unsigned width = intTy->getBitWidth();
            uint64_t mask = (width >= 64) ? ~uint64_t(0) : ((uint64_t(1) << width) - 1);

            using StubFn = uint64_t(*)();
            uint64_t res = reinterpret_cast<StubFn>(stubAddr)() & mask;
            result = mk_integer(Scheme, res);
            break;
        }
        case llvm::Type::PointerTyID: {
            using StubFn = void*(*)();
            void* res = reinterpret_cast<StubFn>(stubAddr)();
            result = mk_cptr(Scheme, res);
            break;
        }
        case llvm::Type::VoidTyID: {
            using StubFn = void(*)();
            reinterpret_cast<StubFn>(stubAddr)();
            result = Scheme->T;
            break;
        }
        default:
            printf("Unsupported return type for compiled call\n");
        }
    }
    extemp::EXTLLVM::removeTransientModule(std::move(*stubTracker));
    return result;
}

static pointer llvm_convert_float_constant(scheme* Scheme, pointer Args)
{
    const char* floatin = argString(Scheme, Args, 1);
    if (floatin[0] && floatin[1] == 'x') {
        return argAt(Scheme, Args, 1);
    }
    llvm::APFloat apf(llvm::APFloat::IEEEsingle(), llvm::StringRef(floatin));
    // TODO: if necessary, checks for inf/nan can be done here
    auto ival(llvm::APInt::doubleToBits(apf.convertToFloat()));
    return mk_string(Scheme, (std::string("0x") + llvm::utohexstr(ival.getLimitedValue(), true)).c_str());
}

static pointer llvm_convert_double_constant(scheme* Scheme, pointer Args)
{
    static_assert(sizeof(double) == sizeof(uint64_t), "sizeof(double) must be 8 bytes");
    const char* floatin = argString(Scheme, Args, 1);
    if (floatin[0] && floatin[1] == 'x') {
        return argAt(Scheme, Args, 1);
    }
    llvm::APFloat apf(llvm::APFloat::IEEEdouble(), llvm::StringRef(floatin));
    // TODO: if necessary, checks for inf/nan can be done here
    // convertToDouble, not convertToFloat: this APFloat is an IEEEdouble, and
    // asking for the float value of one is a hard error in LLVM
    auto ival(llvm::APInt::doubleToBits(apf.convertToDouble()));
    return mk_string(Scheme, (std::string("0x") + llvm::utohexstr(ival.getLimitedValue(), true)).c_str());
}

static pointer llvm_count(scheme* Scheme, pointer Args)
{
    return mk_integer(Scheme, EXTLLVM::LLVM_COUNT);
}

static pointer llvm_count_set(scheme* Scheme, pointer Args)
{
    EXTLLVM::LLVM_COUNT = argInt(Scheme, Args, 1);
    return llvm_count(Scheme, Args);
}

static pointer llvm_count_inc(scheme* Scheme, pointer Args)
{
    ++EXTLLVM::LLVM_COUNT;
    return llvm_count(Scheme, Args);
}

static pointer callClosure(scheme* Scheme, pointer Args)
{
    uint32_t** closure = reinterpret_cast<uint32_t**>(argCptr(Scheme, Args, 1));
    if (!closure) {
        throw ScmRuntimeError("llvm:call-closure: null closure pointer", Args);
    }
    auto fptr(reinterpret_cast<int64_t (*)(void*, int64_t)>(closure[0]));
    return mk_integer(Scheme, (*fptr)(closure[0], argInt(Scheme, Args, 2)));
}

static pointer printLLVMFunction(scheme* Scheme, pointer Args)
{
    const char* name = argString(Scheme, Args, 1);
    auto func(extemp::EXTLLVM::getFunction(name));
    if (!func) {
        throw ScmRuntimeError(std::string("llvm:print-function: no such function: ") + name, Args);
    }
    std::string str;
    llvm::raw_string_ostream ss(str);
    ss << *func;
    puts(ss.str().c_str());
    return Scheme->T;
}

static pointer llvm_print_all_closures(scheme* Scheme, pointer Args) // TODO
{
    std::string rgx(argString(Scheme, Args, 1));
    rgx += "_.*";
    for (auto module : EXTLLVM::getModules()) {
        for (const auto& func : module->getFunctionList()) {
            if (func.hasName() && rmatch(rgx.data(), func.getName().data())) {
                std::string str;
                llvm::raw_string_ostream ss(str);
                ss << func;
                printf("\n---------------------------------------------------\n%s", ss.str().c_str());
            }
        }
    }
    return Scheme->T;
}

static pointer llvm_print_all_modules(scheme* Scheme, pointer Args) // TODO
{
    for (auto module : EXTLLVM::getModules()) {
        std::string str;
        llvm::raw_string_ostream ss(str);
        ss << *module;
        printf("\n---------------------------------------------------\n%s", ss.str().c_str());
    }
    return Scheme->T;
}

static pointer llvm_print_closure(scheme* Scheme, pointer Args) // TODO
{
    auto fname(argString(Scheme, Args, 1));
    for (auto module : EXTLLVM::getModules()) {
        for (const auto& func : module->getFunctionList()) {
            if (func.hasName() && !strcmp(func.getName().data(), fname)) {
                std::string str;
                llvm::raw_string_ostream ss(str);
                ss << func;
                if (ss.str().find_first_of("{") != std::string::npos) {
                    std::cout << str << std::endl;
                }
            }
        }
    }
    return Scheme->T;
}

static pointer llvm_closure_last_name(scheme* Scheme, pointer Args)
{
    std::string rgx(argString(Scheme, Args, 1));
    rgx += "__[0-9]*";
    std::string last_name;
    bool found = false;
    for (auto module : EXTLLVM::getModules()) {
        for (const auto& func : module->getFunctionList()) {
            if (func.hasName() && rmatch(rgx.data(), func.getName().data())) {
                last_name = func.getName().str();
                found = true;
            }
        }
    }
    if (found) {
        return mk_string(Scheme, last_name.c_str());
    }
    return Scheme->F;
}

static pointer llvm_disasm(scheme* Scheme, pointer Args)
{
    int lgth = list_length(Scheme, Args);
    int syntax = (lgth > 1) ? int(argInt(Scheme, Args, 2)) : 1;
    if (syntax > 1) {
      std::cout << "Syntax argument must be either 0: at&t or 1: intel" << std::endl;
      std::cout << "The default is 1: intel" << std::endl;
      syntax = 1;
    }
    pointer name(llvm_closure_last_name(Scheme, Args));
    if (!is_string(name)) {
        throw ScmRuntimeError(std::string("llvm:disassemble: no compiled closure named ") +
                              argString(Scheme, Args, 1), Args);
    }
    EnvInjector injector(Scheme, name);
    pointer lookupArgs = cons(Scheme, name, pair_cdr(Args));
    EnvInjector injector2(Scheme, lookupArgs);
    pointer fp = get_function_pointer(Scheme, lookupArgs);
    if (!is_cptr(fp)) {
        throw ScmRuntimeError(std::string("llvm:disassemble: no function pointer for ") +
                              string_value(name), Args);
    }
    auto fptr(reinterpret_cast<unsigned char*>(cptr_value(fp)));
    return mk_string(Scheme, extemp::EXTLLVM::llvm_disassemble(fptr, syntax).c_str());
}

// Map a symbol name to a process address for future compiles, replacing any
// earlier mapping. Reports failure to the console and returns false.
static bool defineJITSymbol(const char* name, void* addr) {
    if (auto err = EXTLLVM::defineAbsoluteSymbol(name, addr)) {
        std::cerr << "Error: could not bind symbol " << name << ": " << llvm::toString(std::move(err))
                  << std::endl;
        return false;
    }
    return true;
}

static pointer bind_symbol(scheme* Scheme, pointer Args)
{
    auto library(argCptr(Scheme, Args, 1));
    auto sym(argString(Scheme, Args, 2));

#ifdef _WIN32
    auto ptr(reinterpret_cast<void*>(GetProcAddress(reinterpret_cast<HMODULE>(library), sym)));
#else
    auto ptr(dlsym(library, sym));
#endif
    if (ptr && defineJITSymbol(sym, ptr)) [[likely]] {
        return Scheme->T;
    }
    return Scheme->F;
}

static pointer update_mapping(scheme* Scheme, pointer Args)
{
    auto sym(argString(Scheme, Args, 1));
    auto ptr(argCptr(Scheme, Args, 2));

    // The previous address is not tracked under ORC; "old" is always 0.
    defineJITSymbol(sym, ptr);
    return mk_cptr(Scheme, nullptr);
}

// Written from whichever scheme process thread is compiling, so both ends take
// the lock and the reader copies out.
static std::mutex LLVM_ALIAS_TABLE_MUTEX;
static std::unordered_map<std::string, std::string> LLVM_ALIAS_TABLE;

static pointer add_llvm_alias(scheme* Scheme, pointer Args)
{
    std::string alias(argString(Scheme, Args, 1));
    std::string target(argString(Scheme, Args, 2));
    std::lock_guard<std::mutex> lock(LLVM_ALIAS_TABLE_MUTEX);
    LLVM_ALIAS_TABLE[std::move(alias)] = std::move(target);
    return Scheme->T;
}

static pointer get_llvm_alias(scheme* Scheme, pointer Args)
{
    std::string alias(argString(Scheme, Args, 1));
    std::string target;
    {
        std::lock_guard<std::mutex> lock(LLVM_ALIAS_TABLE_MUTEX);
        auto iter(LLVM_ALIAS_TABLE.find(alias));
        if (iter == LLVM_ALIAS_TABLE.end()) {
            return Scheme->F;
        }
        target = iter->second;
    }
    return mk_string(Scheme, target.c_str());
}

static pointer get_named_type(scheme* Scheme, pointer Args)
{
    const char* name = argString(Scheme, Args, 1);
    if (name[0] == '%') {
        ++name;
    }
    int ptrDepth = 0;
    int len = strlen(name) - 1;
    while (len >= 0 && name[len] == '*') {
        ++ptrDepth;
        --len;
    }
    auto st = getNamedType(std::string(name, len + 1).c_str());
    if (st) {
        std::string body;
        llvm::raw_string_ostream ss(body);
        ss << "{ ";
        bool first = true;
        for (auto* elem : st->elements()) {
            if (!first) ss << ", ";
            first = false;
            elem->print(ss);
        }
        ss << " }";
        return mk_string(Scheme, (ss.str() + std::string(ptrDepth, '*')).c_str());
    }
    return Scheme->NIL;
}

static pointer list_modules(scheme* Scheme, pointer Args)
{
    // ORC keeps multiple modules. Expose the stored metadata clones.
    pointer p = Scheme->NIL;
    for (auto module : EXTLLVM::getModules()) {
        EnvInjector injector(Scheme, p);
        pointer c = mk_cptr(Scheme, module);
        EnvInjector injector2(Scheme, c);
        p = cons(Scheme, c, p);
    }
    return reverse_in_place(Scheme, Scheme->NIL, p);
}

static pointer export_llvmmodule_bitcode(scheme* Scheme, pointer Args)
{
    auto m(reinterpret_cast<llvm::Module*>(argCptr(Scheme, Args, 1)));
    if (!m) {
        return Scheme->F;
    }
    auto filename(argString(Scheme, Args, 2));
#ifdef _WIN32
    std::string str;
    std::ofstream fout(filename);
    llvm::raw_string_ostream ss(str);
    ss << *m;
    std::string irStr = ss.str();
    // add dllimport (otherwise global variables won't work)
    std::string oldStr(" external global ");
    std::string newStr(" external dllimport global ");
    size_t pos = 0;
    while ((pos = irStr.find(oldStr, pos)) != std::string::npos) {
        irStr.replace(pos, oldStr.length(), newStr);
        pos += newStr.length();
    }
    // LLVM can't handle guaranteed tail call under win64 yet
    oldStr = std::string(" tail call ");
    newStr = std::string(" call ");
    pos = 0;
    while ((pos = irStr.find(oldStr, pos)) != std::string::npos) {
        irStr.replace(pos, oldStr.length(), newStr);
        pos += newStr.length();
    }
    fout << irStr; //ss.str();
    fout.close();
#else
    std::error_code errcode;
    llvm::raw_fd_ostream ss(filename, errcode, llvm::sys::fs::OF_None);
    if (errcode) {
      std::cout << errcode.message() << std::endl;
      return Scheme->F;
    }
    llvm::WriteBitcodeToFile(*m, ss);
#endif
    return Scheme->T;
}

static pointer emitModuleToObjectFile(scheme* Scheme, pointer Args)
{
    auto m = reinterpret_cast<llvm::Module*>(argCptr(Scheme, Args, 1));
    if (!m) {
        return Scheme->F;
    }
    auto filename = argString(Scheme, Args, 2);

    // Get target triple from module or use host
    std::string triple = m->getTargetTriple().str();
    if (triple.empty()) {
        triple = llvm::sys::getProcessTriple();
    }

    // Look up the target
    std::string error;
    llvm::Triple tripleObj(triple);
    auto target = llvm::TargetRegistry::lookupTarget(tripleObj, error);
    if (!target) {
        std::cerr << "llvm:emit-object-file: " << error << std::endl;
        return Scheme->F;
    }

    // Get CPU and features
    std::string cpu = extemp::UNIV::CPU.empty() ?
        std::string(llvm::sys::getHostCPUName()) : extemp::UNIV::CPU;

    std::string features;
    auto hostFeatures = llvm::sys::getHostCPUFeatures();
    for (auto& feature : hostFeatures) {
        if (!features.empty()) features += ",";
        features += (feature.getValue() ? "+" : "-");
        features += feature.getKey().str();
    }

    // Create target machine
    llvm::TargetOptions opt;
    opt.GuaranteedTailCallOpt = true;  // equivalent to llc -tailcallopt
    std::unique_ptr<llvm::TargetMachine> targetMachine(target->createTargetMachine(
        llvm::Triple(triple), cpu, features, opt,
        llvm::Reloc::PIC_,
        std::nullopt,
        llvm::CodeGenOptLevel::Aggressive));

    if (!targetMachine) {
        std::cerr << "llvm:emit-object-file: failed to create target machine" << std::endl;
        return Scheme->F;
    }

    // Set up data layout
    m->setDataLayout(targetMachine->createDataLayout());

    // Open output file
    std::error_code ec;
    llvm::raw_fd_ostream dest(filename, ec, llvm::sys::fs::OF_None);
    if (ec) {
        std::cerr << "llvm:emit-object-file: " << ec.message() << std::endl;
        return Scheme->F;
    }

    // Emit object file
    llvm::legacy::PassManager pass;
    if (targetMachine->addPassesToEmitFile(pass, dest, nullptr,
            llvm::CodeGenFileType::ObjectFile)) {
        std::cerr << "llvm:emit-object-file: target machine can't emit object files" << std::endl;
        return Scheme->F;
    }

    pass.run(*m);
    dest.flush();

    return Scheme->T;
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

std::span<const FFIEntry> llvmDefs()
{
    static const FFIEntry defs[] = {
        FFI_DEF("llvm:optimize", optimizeCompiles, 1, 0, false),
        FFI_DEF("llvm:optimization-level", optimizationLevel, 0, 1, false),
        FFI_DEF("llvm:jit-compile-ir-string", jitCompileIRString, 1, 0, false),
        FFI_DEF("llvm:ffi-set-name", ff_set_name, 1, 1, false),
        FFI_DEF("llvm:ffi-get-name", ff_get_name, 1, 0, false),
        FFI_DEF("llvm:get-function", get_function, 1, 0, false),
        FFI_DEF("llvm:get-globalvar", get_globalvar, 1, 0, false),
        FFI_DEF("llvm:get-struct-size", get_struct_size, 1, 0, false),
        FFI_DEF("llvm:get-named-struct-size", get_named_struct_size, 1, 0, false),
        FFI_DEF("llvm:get-function-args", get_function_args, 1, 0, false),
        FFI_DEF("llvm:get-function-varargs", get_function_varargs, 1, 0, false),
        FFI_DEF("llvm:get-function-type", get_function_type, 1, 0, false),
        FFI_DEF("llvm:get-function-calling-conv", get_function_calling_conv, 1, 0, false),
        FFI_DEF("llvm:get-global-variable-type", get_global_variable_type, 1, 0, false),
        FFI_DEF("llvm:get-function-pointer", get_function_pointer, 1, 0, false),
        FFI_DEF("llvm:remove-function", remove_symbol_and_global, 1, 0, false),
        FFI_DEF("llvm:remove-globalvar", remove_symbol_and_global, 1, 0, false),
        FFI_DEF("llvm:erase-function", remove_symbol_and_global, 1, 0, false),
        FFI_DEF("llvm:call-void-func", llvm_call_void_native, 1, 0, false),
        FFI_DEF("llvm:run", call_compiled, 1, 0, true),
        FFI_DEF("llvm:convert-float", llvm_convert_float_constant, 1, 0, false),
        FFI_DEF("llvm:convert-double", llvm_convert_double_constant, 1, 0, false),
        FFI_DEF("llvm:list-modules", list_modules, 0, 0, false),
        FFI_DEF("llvm:count", llvm_count, 0, 0, false),
        FFI_DEF("llvm:count-set", llvm_count_set, 1, 0, false),
        FFI_DEF("llvm:count++", llvm_count_inc, 0, 0, false),
        FFI_DEF("llvm:call-closure", callClosure, 2, 0, false),
        FFI_DEF("llvm:print", llvm_print_all_modules, 0, 0, false),
        FFI_DEF("llvm:print-function", printLLVMFunction, 1, 0, false),
        FFI_DEF("llvm:print-all-closures", llvm_print_all_closures, 1, 0, false),
        FFI_DEF("llvm:print-closure", llvm_print_closure, 1, 0, false),
        FFI_DEF("llvm:get-closure-work-name", llvm_closure_last_name, 1, 0, false),
        FFI_DEF("llvm:disassemble", llvm_disasm, 1, 1, false),
        FFI_DEF("llvm:bind-symbol", bind_symbol, 2, 0, false),
        FFI_DEF("llvm:update-mapping", update_mapping, 2, 0, false),
        FFI_DEF("llvm:add-llvm-alias", add_llvm_alias, 2, 0, false),
        FFI_DEF("llvm:get-llvm-alias", get_llvm_alias, 1, 0, false),
        FFI_DEF("llvm:get-named-type", get_named_type, 1, 0, false),
        FFI_DEF("llvm:export-module", export_llvmmodule_bitcode, 2, 0, false),
        FFI_DEF("llvm:emit-object-file", emitModuleToObjectFile, 2, 0, false),
    };
    return defs;
}

}  // namespace SchemeFFI

}  // namespace extemp
