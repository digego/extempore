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
#include "llvm/Target/TargetOptions.h"
#ifdef EXT_MCJIT
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Verifier.h"
#else
#include "llvm/Analysis/Verifier.h"
#include "llvm/ExecutionEngine/JIT.h"
#include "llvm/PassManager.h"
#endif

#include "SchemeFFI.h"
#include "AudioDevice.h"
#include "UNIV.h"
#include "TaskScheduler.h"
#include "SchemeProcess.h"
#include "SchemeREPL.h"
//#include "sys/mman.h"

#ifdef _WIN32
#include <Windows.h>
#include <Windowsx.h>
#include <filesystem>
#include <fstream>
#else
#include <dlfcn.h>
#include <dirent.h>
#endif

#include <sstream>
#include <string.h>

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

#ifdef _WIN32
#define PRINT_ERROR(format, ...)		\
    ascii_text_color(1,1,10);			\
    printf(format , __VA_ARGS__);			\
    ascii_text_color(0,7,10)
#else
#define PRINT_ERROR(format, args...)		\
    ascii_text_color(1,1,10);			\
    printf(format , ## args);			\
    ascii_text_color(0,7,10)
#endif

char* cstrstrip (char* inputStr)
{
    char *start, *end;
    size_t len;

    /* Strip leading whitespace */
    start = inputStr;
    while(*start == ' ') start++;
    len = strlen(start);
    memmove(inputStr, start, len + 1);

    /* Strip trailing whitespace */
    end = inputStr + len - 1;
    while(end >= inputStr && *end == ' ') end--;
    *(end + 1) = '\0';

    return inputStr;
}

static inline std::string xtm_ftostr(double V) {
  char Buffer[200];
  sprintf(Buffer, "%20.6e", V);
  char *B = Buffer;
  while (*B == ' ') ++B;
  return B;
}

static inline std::string xtm_ftostr(const llvm::APFloat& V) {
  if (&V.getSemantics() == &llvm::APFloat::IEEEdouble)
    return xtm_ftostr(V.convertToDouble());
  else if (&V.getSemantics() == &llvm::APFloat::IEEEsingle)
    return xtm_ftostr((double)V.convertToFloat());
  return "<unknown format in ftostr>"; // error
}




#define nelem(table) sizeof(table) / sizeof(table[0])

namespace extemp {
	
    SchemeFFI SchemeFFI::SINGLETON;
    std::map<std::string,std::pair<std::string,std::string> > SchemeFFI::IMPCIR_DICT;  
    char* SchemeFFI::tmp_str_a = (char*) malloc(1024);
    char* SchemeFFI::tmp_str_b = (char*) malloc(4096);
  std::map<std::string,std::string> SchemeFFI::LLVM_ALIAS_TABLE;

    void SchemeFFI::initSchemeFFI(scheme* sc)
    {
	int i;

	static struct {
	    const char *name;
	    uint32_t    value;
	} integerTable[] = {
	    { "*au:block-size*",	UNIV::FRAMES },
	    { "*au:samplerate*",	UNIV::SAMPLERATE },
	    { "*au:channels*",	        UNIV::CHANNELS },
	    { "*au:in-channels*",	UNIV::IN_CHANNELS },
	};

	static struct {
	    const char * name;
	    foreign_func func;
	} funcTable[] = {
	    { "ascii-print-color",	       	&SchemeFFI::asciiColor },
	    { "emit",                       &SchemeFFI::emit },
      { "quit",                       &SchemeFFI::exit_extempore },
	    //IPC stuff
	    { "ipc:new",		              	&SchemeFFI::newSchemeProcess },
	    { "ipc:connect",	            	&SchemeFFI::connectToProcess },
	    { "ipc:call-async",	          	&SchemeFFI::ipcCall },
	    { "ipc:define",		             	&SchemeFFI::ipcDefine },
	    { "ipc:eval-string",	        	&SchemeFFI::ipcEval },
	    { "ipc:load",			              &SchemeFFI::ipcLoad },
	    { "ipc:set-priority",           &SchemeFFI::ipcSetPriority },
	    { "ipc:get-priority",           &SchemeFFI::ipcGetPriority },
	    { "ipc:get-process-name",      	&SchemeFFI::getNameOfCurrentProcess },
	    // misc scheme ties
	    { "assoc-strcmp",               &SchemeFFI::assocstrcmp },
	    { "assoc-strcmp-all",           &SchemeFFI::assocstrcmpall },
	    // number stuff
	    { "random-real",		            &SchemeFFI::randomReal },
	    { "random-int",			            &SchemeFFI::randomInt },
	    { "real->integer",		          &SchemeFFI::realToInteger },
	    { "real->rational",		          &SchemeFFI::realToRational },
	    { "rational->real",		          &SchemeFFI::rationalToReal },
	    { "integer->real",		          &SchemeFFI::integerToReal },
      { "rational->n",                &SchemeFFI::rationalToNumerator },
      { "rational->d",                &SchemeFFI::rationalToDenominator },
	    // sys stuff
	    { "sys:pointer-size",		        &SchemeFFI::pointerSize },
      { "sys:mcjit-enabled",          &SchemeFFI::mcjitEnabled },
	    { "sys:platform",	  	          &SchemeFFI::platform },
	    { "sys:share-dir",	       	    &SchemeFFI::getShareDir },
	    { "sys:cmdarg",    		          &SchemeFFI::cmdarg },
	    { "sys:open-dylib",		          &SchemeFFI::openDynamicLib },
	    { "sys:close-dylib",	          &SchemeFFI::closeDynamicLib },
	    { "sys:symbol-cptr",	          &SchemeFFI::symbol_pointer },
	    { "sys:make-cptr",		          &SchemeFFI::makeCptr },
	    { "sys:slurp-file",             &SchemeFFI::slurpFile },
	    { "sys:directory-list",         &SchemeFFI::dirlist },
      { "sys:expand-path",            &SchemeFFI::pathExpansion },
      { "sys:command",                &SchemeFFI::command },
      { "sys:command-output",         &SchemeFFI::commandOutput },
      { "sys:set-env",                &SchemeFFI::setEnv },
      { "sys:set-default-timeout",    &SchemeFFI::setDefaultTimeout },
      { "sys:get-default-timeout",    &SchemeFFI::getDefaultTimeout },
	    // DSP sys stuff
	    { "sys:set-dsp-closure",	      &SchemeFFI::setDSPClosure },
	    { "sys:set-dspmt-closure",	    &SchemeFFI::setDSPMTClosure },
	    { "sys:set-dsp-wrapper",	      &SchemeFFI::setDSPWrapper },
	    { "sys:set-dspmt-wrapper",	    &SchemeFFI::setDSPMTWrapper },
      { "sys:init-mt-audio",          &SchemeFFI::initMTAudio },
      { "sys:init-mt-audio-buf",      &SchemeFFI::initMTAudioBuf },
      { "sys:audio-load",             &SchemeFFI::getAudioLoad },
	    { "sys:set-dsp-wrapper-array",	&SchemeFFI::setDSPWrapperArray },
	    { "sys:set-dspmt-wrapper-array",&SchemeFFI::setDSPMTWrapperArray },
	    // memory zone stuff
      { "sys:create-mzone",	   	      &SchemeFFI::createMallocZone },
	    { "sys:default-mzone",		      &SchemeFFI::defaultMallocZone },
	    { "sys:destroy-mzone",		      &SchemeFFI::destroyMallocZone },
	    { "sys:copy-to-dmzone",		      &SchemeFFI::copyToDefaultZone },
	    { "sys:reset-mzone",		        &SchemeFFI::resetMallocZone },
	    { "sys:peek-memzone",           &SchemeFFI::peekMemoryZone },
	    { "sys:pop-memzone",            &SchemeFFI::popMemoryZone },
	    { "sys:push-memzone",           &SchemeFFI::pushMemoryZone },
	    // misc stuff
	    { "cptr:get-i64",               &SchemeFFI::dataGETi64 },
	    { "cptr:get-double",            &SchemeFFI::dataGETdouble },
	    { "cptr:get-float",             &SchemeFFI::dataGETfloat },
	    { "cptr:set-i64",               &SchemeFFI::dataSETi64 },
	    { "cptr:set-double",            &SchemeFFI::dataSETdouble },
	    { "cptr:set-float",             &SchemeFFI::dataSETfloat },
	    { "cptr->string",               &SchemeFFI::cptrToString },
	    { "cptr:get-string",            &SchemeFFI::cptrToString },
	    { "string->cptr",               &SchemeFFI::stringToCptr },
	    { "string-strip",		            &SchemeFFI::stringStrip },
	    { "string-hash",		            &SchemeFFI::stringHash },
	    { "base64-encode",	            &SchemeFFI::Base64Encode },
	    { "base64-decode",	            &SchemeFFI::Base64Decode },
	    { "cname-encode",		            &SchemeFFI::CNameEncode },
	    { "cname-decode",		            &SchemeFFI::CNameDecode },
	    { "string-join",		            &SchemeFFI::stringJoin },
	    { "call-cpp-at-time",	         	&SchemeFFI::callCPPAtTime },
	    { "now",			                  &SchemeFFI::getTime },
	    { "sexpr->string",		          &SchemeFFI::sexprToString },
	    { "println",	  		            &SchemeFFI::print },
	    { "print",		     	            &SchemeFFI::print_no_new_line },
	    { "print-full",			            &SchemeFFI::printFull },
	    { "print-full-nq",		          &SchemeFFI::printFullNoQuotes },
	    { "pprint-error",		            &SchemeFFI::printError }, // pprint-error is pprint for a reason!
	    { "print-notification",	       	&SchemeFFI::printNotification },
	    { "get-closure-env",         		&SchemeFFI::getClosureEnv },
      { "mk-ff",                      &SchemeFFI::scmAddForeignFunc },
	    // regex stuff
	    { "regex:match?",		            &SchemeFFI::regex_match },
	    { "regex:matched",		          &SchemeFFI::regex_matched },
	    { "regex:match-all",		        &SchemeFFI::regex_match_all },
	    { "regex:split",		            &SchemeFFI::regex_split },
	    { "regex:replace",		          &SchemeFFI::regex_replace },
	    // llvm stuff
	    { "llvm:optimize",			        &SchemeFFI::optimizeCompiles },
	    { "llvm:jit-compile-ir-string", &SchemeFFI::jitCompileIRString},
      { "llvm:ffi-set-name",          &SchemeFFI::ff_set_name },
      { "llvm:ffi-get-name",          &SchemeFFI::ff_get_name },
	    { "llvm:get-function",		     	&SchemeFFI::get_function },
	    { "llvm:get-globalvar",		     	&SchemeFFI::get_globalvar },
      { "llvm:get-struct-size",       &SchemeFFI::get_struct_size },
      { "llvm:get-named-struct-size", &SchemeFFI::get_named_struct_size },
	    { "llvm:get-function-args",		  &SchemeFFI::get_function_args },
	    { "llvm:get-function-varargs",	&SchemeFFI::get_function_varargs },
	    { "llvm:get-function-type",		  &SchemeFFI::get_function_type },
	    { "llvm:get-function-calling-conv",	&SchemeFFI::get_function_calling_conv },
	    { "llvm:get-global-variable-type",	&SchemeFFI::get_global_variable_type },
	    { "llvm:get-function-pointer",	&SchemeFFI::get_function_pointer },
	    { "llvm:jit-compile-function",	&SchemeFFI::recompile_and_link_function },
	    { "llvm:remove-function",		    &SchemeFFI::remove_function },
	    { "llvm:remove-globalvar",		  &SchemeFFI::remove_global_var },
	    { "llvm:erase-function",		    &SchemeFFI::erase_function },
	    { "llvm:call-void-func",        &SchemeFFI::llvm_call_void_native },
	    { "llvm:run",				            &SchemeFFI::call_compiled },
	    { "llvm:convert-float",			    &SchemeFFI::llvm_convert_float_constant },
 	    { "llvm:convert-double",			  &SchemeFFI::llvm_convert_double_constant },
	    { "llvm:count",				          &SchemeFFI::llvm_count },
	    { "llvm:count-set",				      &SchemeFFI::llvm_count_set },
	    { "llvm:count++",		        	  &SchemeFFI::llvm_count_inc },
	    { "llvm:call-closure",			    &SchemeFFI::callClosure },
	    { "llvm:print",				          &SchemeFFI::printLLVMModule },
	    { "llvm:print-function",		    &SchemeFFI::printLLVMFunction },
      { "llvm:print-all-closures",         &SchemeFFI::llvm_print_all_closures },
      { "llvm:print-closure",    &SchemeFFI::llvm_print_closure },
      { "llvm:get-closure-work-name", &SchemeFFI::llvm_closure_last_name },
      { "llvm:disassemble",           &SchemeFFI::llvm_disasm },      
	    { "llvm:bind-symbol",			      &SchemeFFI::bind_symbol },
	    { "llvm:update-mapping",			  &SchemeFFI::update_mapping },
	    { "llvm:add-llvm-alias",        &SchemeFFI::add_llvm_alias },
	    { "llvm:get-llvm-alias",        &SchemeFFI::get_llvm_alias },
	    { "llvm:get-named-type",        &SchemeFFI::get_named_type },
	    { "llvm:get-global-module",     &SchemeFFI::get_global_module},
      { "llvm:export-module",         &SchemeFFI::export_llvmmodule_bitcode },
	    { "impc:ir:getname",			      &SchemeFFI::impcirGetName },
	    { "impc:ir:gettype",			      &SchemeFFI::impcirGetType },		
	    { "impc:ir:addtodict",			    &SchemeFFI::impcirAdd },
	    //#ifdef EXT_BOOST
	    //#else
	    //CLOCK STUFF
	    { "clock:set-offset",           &SchemeFFI::setClockOffset},
	    { "clock:get-offset",           &SchemeFFI::getClockOffset},
      { "clock:adjust-offset",        &SchemeFFI::adjustClockOffset},
	    { "clock:clock",                &SchemeFFI::getClockTime},
      { "clock:ad:clock",             &SchemeFFI::lastSampleBlockClock},    
	    //#endif
           

	    		
	};

	for (i = 0; i < nelem(integerTable); i++) {
	    scheme_define(
		sc, sc->global_env,
		mk_symbol(sc, integerTable[i].name),
		mk_integer(sc, integerTable[i].value)
		);
	}

	for (i = 0; i < nelem(funcTable); i++) {
	    scheme_define(
		sc, sc->global_env,
		mk_symbol(sc, funcTable[i].name),
		mk_foreign_func(sc, funcTable[i].func)
		);
	}

    }

    //////////////////// helper functions ////////////////////////
    void SchemeFFI::addGlobal(scheme* sc, char* symbol_name, pointer arg)
    {
	scheme_define(sc, sc->global_env, mk_symbol(sc, symbol_name), arg);
    }
	
    void SchemeFFI::addForeignFunc(scheme* sc, char* symbol_name, foreign_func func)
    {
	scheme_define(sc, sc->global_env, mk_symbol(sc, symbol_name), mk_foreign_func(sc, func));
    }

    // pointer SchemeFFI::scmAddForeignFunc(scheme* sc, pointer args) {
    //   //char* sym_name = string_value(pair_car(args));
    //   foreign_func func = (foreign_func) cptr_value(pair_car(args));
    //   //scheme_define(sc, sc->global_env, mk_symbol(sc, symbol_name), mk_foreign_func(sc, func));
    //   return mk_foreign_func(sc,func); //sc->T;
    // }

  pointer SchemeFFI::scmAddForeignFunc(scheme* sc, pointer args) {
    char* symbol_name = string_value(pair_car(args));
    foreign_func func = (foreign_func) cptr_value(pair_cadr(args));
    pointer ffunc = mk_foreign_func(sc,func);
    scheme_define(sc, sc->global_env, mk_symbol(sc, symbol_name), ffunc);
    return ffunc;
  }

    void SchemeFFI::addGlobalCptr(scheme* sc, char* symbol_name, void* ptr)
    {
	scheme_define(sc, sc->global_env, mk_symbol(sc, symbol_name), mk_cptr(sc, ptr));		
    }
	
    ///////////////////////////////////////////////////////
    //
    // MISC STUFF
    //
    //////////////////////////////////////////////////////



    pointer SchemeFFI::exit_extempore(scheme* _sc, pointer args)
    {
      int rc = (int)ivalue(pair_car(args));
#ifdef _WIN32
	  std::exit(rc);
#else
      std::_Exit(rc);
#endif
    }

    pointer SchemeFFI::dataGETi64(scheme* _sc, pointer args)
    {       
        char* cptr = (char*) cptr_value(pair_car(args));
	int64_t offset = ivalue(pair_cadr(args));
	int64_t* ptr = (int64_t*) (cptr+offset); 
	return mk_integer(_sc,ptr[0]);
    }

    pointer SchemeFFI::dataGETdouble(scheme* _sc, pointer args)
    {       
        char* cptr = (char*) cptr_value(pair_car(args));
	int64_t offset = ivalue(pair_cadr(args));
	double* ptr = (double*) (cptr+offset); 
	return mk_real(_sc,ptr[0]);
    }

    pointer SchemeFFI::dataGETfloat(scheme* _sc, pointer args)
    {       
        char* cptr = (char*) cptr_value(pair_car(args));
	int64_t offset = ivalue(pair_cadr(args));
	float* ptr = (float*) (cptr+offset); 
	return mk_real(_sc,ptr[0]);
    }

    pointer SchemeFFI::dataSETi64(scheme* _sc, pointer args)
    {       
        char* cptr = (char*) cptr_value(pair_car(args));
	int64_t offset = ivalue(pair_cadr(args));
	int64_t* ptr = (int64_t*) (cptr+offset);
	ptr[0] = (int64_t) ivalue(pair_caddr(args));
	return _sc->T;
    }

    pointer SchemeFFI::dataSETdouble(scheme* _sc, pointer args)
    {       
        char* cptr = (char*) cptr_value(pair_car(args));
	int64_t offset = ivalue(pair_cadr(args));
	double* ptr = (double*) (cptr+offset);
	ptr[0] = (double) rvalue(pair_caddr(args));
	return _sc->T;
    }

    pointer SchemeFFI::dataSETfloat(scheme* _sc, pointer args)
    {       
        char* cptr = (char*) cptr_value(pair_car(args));
	int64_t offset = ivalue(pair_cadr(args));
	float* ptr = (float*) (cptr+offset);
	ptr[0] = (float) rvalue(pair_caddr(args));
	return _sc->T;
    }

    pointer SchemeFFI::cptrToString(scheme* _sc, pointer args)
    {
        char* cptr = (char*) cptr_value(pair_car(args));
	char* cstr = (char*) cptr;
	return mk_string(_sc, cstr);
    }

    pointer SchemeFFI::stringToCptr(scheme* _sc, pointer args)
    {
        char* cstr = (char*) cptr_value(pair_car(args));
	char* cptr = (char*) malloc(strlen(cstr) + 1);
        strcpy(cptr, cstr);
	return mk_cptr(_sc, cptr);
    }

    pointer SchemeFFI::asciiColor(scheme* _sc, pointer args)
    {
	ascii_text_color(ivalue(pair_car(args)),ivalue(pair_cadr(args)),ivalue(pair_caddr(args)));
	return _sc->T;
    }

    pointer SchemeFFI::emit(scheme* _sc, pointer args)
    {
	std::stringstream ss;
	int lgth = list_length(_sc,args);
	pointer io = list_ref(_sc,lgth-1,args);
	if(!is_string(io)) {
	    PRINT_ERROR("Emit accepts only string arguments!\n");
	    return _sc->F;
	}
	ss << string_value(io);
	pointer arg = 0;
	for(int i=0;i<lgth-1;i++) {
	    arg = pair_car(args);
	    if(!is_string(arg)) {
		PRINT_ERROR("Emit accepts only string arguments!\n");
		return _sc->F;
	    }
	    ss << string_value(arg);
	    args = pair_cdr(args);
	}

	std::string tmp = ss.str();
	// replace io string in place	
	int l = tmp.length();
	char* s = (char*) malloc(l+1);
	strcpy(s,tmp.c_str());
	free(io->_object._string._svalue);
	io->_object._string._svalue = s;
	io->_object._string._length = l;
	// return io string
	return io;
    }

    pointer SchemeFFI::makeCptr(scheme* _sc, pointer args)
    {                                                     
         long num_bytes = ivalue(pair_car(args));            
         void* ptr = malloc(num_bytes);                      
         memset(ptr,0,num_bytes);                            
         return mk_cptr(_sc, ptr);                           
    }                                                     

  pointer SchemeFFI::pathExpansion(scheme* _sc, pointer args) {    
  #ifdef _WIN32
    char* path = string_value(pair_car(args));
    char* exp_path = path;
  #else
	char exp_path[8192];
	memset(exp_path,0,8192);
    char* path = string_value(pair_car(args));
    if(path[0] == '~') {
      char* h = getenv("HOME");      
      strcpy(exp_path,h);
      strcat(exp_path,&path[1]);
    }else{
      realpath(path,exp_path);
    }
    
  #endif  
    return mk_string(_sc,exp_path);
  }
  
  pointer SchemeFFI::command(scheme* _sc, pointer args) {    
    // NOTE: doesn't work for Windows yet
    int res = system(string_value(pair_car(args)));
    return mk_integer(_sc,res);
  }

  pointer SchemeFFI::commandOutput(scheme* _sc, pointer args) {    

    char outbuf[8192];
    memset(outbuf,0,8192);
#ifdef _WIN32
    FILE *stream = _popen(string_value(pair_car(args)), "r");
#else
    FILE *stream = popen(string_value(pair_car(args)), "r");
#endif
    if (stream && fgets(outbuf, 8192, stream))
      {
#ifdef _WIN32
        _pclose(stream);
#else
        pclose(stream);
#endif
        // get rid of the final newline
        size_t len = strnlen(outbuf, 8192);
        if(len < 8192)
          outbuf[len-1] = '\0';
        return mk_string(_sc,outbuf);
      }
    else
      return _sc->F;     
  }

  pointer SchemeFFI::setEnv(scheme* _sc, pointer args) {
    char* var = string_value(pair_car(args));
    char* val = string_value(pair_cadr(args));

    int res;
#ifdef _WIN32
    res = _putenv_s(var, val);
#else
    res = setenv(var, val, 1); // overwrite = TRUE
#endif
    return mk_integer(_sc,res);
  }

  pointer SchemeFFI::setDefaultTimeout(scheme* _sc, pointer args)
  {
    long long timeout = ivalue(pair_car(args));
    SchemeProcess::I(_sc)->setMaxDuration(timeout);
    return _sc->T;
  }

  pointer SchemeFFI::getDefaultTimeout(scheme* _sc, pointer args)
  {
    return mk_integer(_sc,SchemeProcess::I(_sc)->getMaxDuration());
  }

#ifdef _WIN32
	pointer SchemeFFI::dirlist(scheme* _sc, pointer args)
	{
	  char* path = string_value(pair_car(args));
	  std::tr2::sys::path bpath(path);
	  if(!std::tr2::sys::exists(bpath)) {
		  return _sc->NIL;
	  }
	  if(!std::tr2::sys::is_directory(bpath)) {
		  return _sc->NIL;
	  }

	  std::tr2::sys::directory_iterator end_it;

	  pointer list = _sc->NIL;
	  for(std::tr2::sys::directory_iterator it(bpath); it != end_it; ++it) {
	    _sc->imp_env->insert(list);
		pointer tlist = 0;
		//tlist = cons(_sc,mk_string(_sc,it->path().leaf().string().c_str()),list);
		tlist = cons(_sc,mk_string(_sc,it->path().string().c_str()),list);
	    _sc->imp_env->erase(list);
	    list = tlist;	    
	  }
	  return reverse(_sc,list);
	}
#else
  pointer SchemeFFI::dirlist(scheme* _sc, pointer args)
  {    

    DIR *dp;
    struct dirent *ep;     
    dp = opendir (string_value(pair_car(args)));
    
    pointer list = _sc->NIL;
    if (dp != NULL)
      {
    while ((ep = readdir (dp))) {
	  _sc->imp_env->insert(list);
	  pointer tlist = cons(_sc,mk_string(_sc,ep->d_name),list);
	  _sc->imp_env->erase(list);
	  list = tlist;
	}
	
	(void) closedir (dp);
      }
    else {
      perror ("Couldn't open the directory");
    }
    
    return reverse(_sc,list);
  }
#endif

  pointer SchemeFFI::slurpFile(scheme* _sc, pointer args)
  {
    std::string filename(string_value(pair_car(args)));
    std::string sharedir_filename(UNIV::SHARE_DIR + "/" + filename);

    // check raw path first, then prepend SHARE_DIR
    std::FILE *fp = std::fopen(filename.c_str(), "rb");
    if (!fp) {
      fp = std::fopen(sharedir_filename.c_str(), "rb");
    }

    if(fp){
      std::string contents;
      std::fseek(fp, 0, SEEK_END);
      contents.resize(std::ftell(fp));
      std::rewind(fp);
      std::fread(&contents[0], 1, contents.size(), fp);
      std::fclose(fp);

      return mk_string(_sc,contents.c_str());
    }
    return _sc->F;
  }

    pointer SchemeFFI::impcirGetType(scheme* _sc, pointer args)
    {
		std::string key(string_value(pair_car(args)));
		return mk_string(_sc, IMPCIR_DICT[key].second.c_str());	
    }

    pointer SchemeFFI::impcirGetName(scheme* _sc, pointer args)
    {
		std::string key(string_value(pair_car(args)));
		return mk_string(_sc, IMPCIR_DICT[key].first.c_str());	
    }

    pointer SchemeFFI::impcirAdd(scheme* _sc, pointer args)
    {
		std::string current("current");
		std::string previous("previous");
		std::string key(string_value(pair_car(args)));
		std::string name(string_value(pair_cadr(args)));
		std::string type(string_value(pair_caddr(args)));
		//std::cout << "ADDING IN C++ "  << key << " " << name << " " << type << std::endl; 
		std::pair<std::string,std::string> p(name,type);
		IMPCIR_DICT[previous] = IMPCIR_DICT[current];
		IMPCIR_DICT[current] = p;
		IMPCIR_DICT[key] = p;
		return _sc->T;
    }

    // ipc stuff
    pointer SchemeFFI::newSchemeProcess(scheme* _sc, pointer args)
    {
	std::string host_name("localhost");
	std::string proc_name(string_value(pair_car(args)));
	int port = ivalue(pair_cadr(args));
	SchemeProcess* sp = new SchemeProcess(UNIV::SHARE_DIR, proc_name, port, 0);
	sp->start();
	SchemeREPL* repl = new SchemeREPL(proc_name);

	if(repl->connectToProcessAtHostname(host_name, port)) {
	    return _sc->T;
	} else {
	    sp->stop();
	    delete sp;
	    delete repl;
	    return _sc->F;
	}
    }

    pointer SchemeFFI::connectToProcess(scheme* _sc, pointer args)
    {
	std::string host_name(string_value(pair_car(args)));
	std::string proc_name(string_value(pair_cadr(args)));
	int port = ivalue(pair_caddr(args));
	SchemeREPL* repl = new SchemeREPL(proc_name);
	if(repl->connectToProcessAtHostname(host_name, port)) {
	    return _sc->T;
	} else {
	    delete repl;
	    return _sc->F;
	}
    }

  pointer SchemeFFI::ipcCall(scheme* _sc, pointer args)
  {
    std::string process(string_value(pair_car(args)));
    SchemeREPL* repl = SchemeREPL::I(process);
    if(!repl) {
      std::cout << "Error: unknown scheme process '" << process << "'" << std::endl;
      return _sc->F;
    }
    std::stringstream ss;
    pointer sym = pair_cadr(args);
    args = pair_cddr(args);
    for(; is_pair(args); args = pair_cdr(args)) {
	    ss << " ";
	    if(is_pair(pair_car(args)) || is_vector(pair_car(args)) || is_symbol(pair_car(args))) {
        ss << "'";
        UNIV::printSchemeCell(_sc, ss, pair_car(args),true);
	    }
	    else if(_sc->NIL == pair_car(args)) {
        ss << "'()";
	    }
	    else if(pair_car(args) == _sc->F) {
        ss << "#f";
	    }
	    else if(pair_car(args) == _sc->T) {
        ss << "#t";
	    }
      else if(pair_car(args) == _sc->EOF_OBJ) {
        // igore end of file
      }
	    else if(is_closure(pair_car(args))) {
        std::stringstream tmp;
        UNIV::printSchemeCell(_sc, tmp, closure_code(pair_car(args)), true);
        std::string sss = "(lambda "+tmp.str().substr(1);
        ss << sss;
	    }
	    else if(is_string(pair_car(args)) || is_number(pair_car(args)) || is_symbol(pair_car(args))){
        UNIV::printSchemeCell(_sc, ss, pair_car(args), true);
	    }
	    else {
        PRINT_ERROR("Extempore's IPC mechanism cannot serialise this type - this maybe related to the return type as well as the arguments.\n");
        return _sc->F;
	    }
    }
    std::string str = "("+std::string(symname(sym))+ss.str()+")";
    repl->writeString(str);
    return _sc->T;
  }
    
  pointer SchemeFFI::ipcDefine(scheme* _sc, pointer args)
  {
    std::string process(string_value(pair_car(args)));
    SchemeREPL* repl = SchemeREPL::I(process);
    if(!repl) {
      std::cout << "Error: unknown scheme process '" << process << "'" << std::endl;
      return _sc->F;
    }
    std::stringstream ss;
    pointer sym = pair_cadr(args);
    pointer value = pair_caddr(args);
    ss << " ";
    if(is_pair(value) || is_vector(value) || is_symbol(value)) {
	    ss << "'";
	    UNIV::printSchemeCell(_sc, ss, value,true);
    }
    else if(_sc->NIL == value) {
	    ss << "'()";
    }
    else if(value == _sc->F) {
	    ss << "#f";
    }
    else if(value == _sc->T) {
	    ss << "#t";
    }
    else if(value == _sc->EOF_OBJ) {
      // ignore eof
    }
    else if(is_closure(value)) {
	    std::stringstream tmp;
	    UNIV::printSchemeCell(_sc, tmp, closure_code(value), true);
	    std::string sss = "(lambda "+tmp.str().substr(1);
	    ss << sss;
    }
    else if(is_string(value) || is_number(value)) {
	    UNIV::printSchemeCell(_sc, ss,value, true);
    }
    else {
      PRINT_ERROR("Extempore's IPC mechanism cannot serialise this type - this maybe related to the return type as well as the arguments.\n");
	    return _sc->F;
    }
    std::string str = "(define "+std::string(symname(sym))+ss.str()+")";
    repl->writeString(str);
    return _sc->T;
  }

  pointer SchemeFFI::ipcEval(scheme* _sc, pointer args)
  {
    std::string process(string_value(pair_car(args)));
    SchemeREPL* repl = SchemeREPL::I(process);
    if(!repl) {
      std::cout << "Error: unknown scheme process '" << process << "'" << std::endl;
      return _sc->F;
    }
    std::string expr(string_value(pair_cadr(args)));
    SchemeREPL::I(process)->writeString(expr);
    return _sc->T;
    }
    
  pointer SchemeFFI::ipcLoad(scheme* _sc, pointer args)
  {
    std::string process(string_value(pair_car(args)));
    SchemeREPL* repl = SchemeREPL::I(process);
    if(!repl) {
      std::cout << "Error: unknown scheme process '" << process << "'" << std::endl;
      return _sc->F;
    }
    std::string path(string_value(pair_cadr(args)));
    std::string str = "(sys:load \""+std::string(path)+"\")";
    SchemeREPL::I(process)->writeString(str);
    return _sc->T;
  }

  pointer SchemeFFI::ipcSetPriority(scheme* _sc, pointer args)
  {
    std::string process(string_value(pair_car(args)));
    SchemeREPL* repl = SchemeREPL::I(process);
    if(!repl) {
      std::cout << "Error: unknown scheme process '" << process << "'" << std::endl;
      return _sc->F;
    }
    int priority = ivalue(pair_cadr(args));
    SchemeProcess::I(process)->setPriority(priority);
    return _sc->T;
  }

  pointer SchemeFFI::ipcGetPriority(scheme* _sc, pointer args)
  {
    std::string process(string_value(pair_car(args)));
    SchemeREPL* repl = SchemeREPL::I(process);
    if(!repl) {
      std::cout << "Error: unknown scheme process '" << process << "'" << std::endl;
      return _sc->F;
    }
    int priority = SchemeProcess::I(process)->getPriority();
    return mk_integer(_sc, priority);
  }

    pointer SchemeFFI::getNameOfCurrentProcess(scheme* _sc, pointer args)
    {
	    const char* name = SchemeProcess::I(_sc)->getName().c_str();
		return mk_string(_sc,name);
	//if(args == _sc->NIL) return mk_string(_sc, name);
	//else { printf("Error getting name of current process\n"); return _sc->F; }
	/*
	  NSDictionary* dict = (NSDictionary*) objc_value(pair_car(args));
	  NSString* alias_name = [dict objectForKey:[NSString stringWithCString:name]];
	  if(alias_name == NULL) return mk_string(_sc, name);
	  return mk_string(_sc, [alias_name UTF8String]);
	*/
    }

    pointer SchemeFFI::assocstrcmp(scheme* _sc, pointer args)
    {
      pointer key = pair_car(args);
      pointer alist = pair_cadr(args);
      return assoc_strcmp(_sc,key,alist);
    }

    pointer SchemeFFI::assocstrcmpall(scheme* _sc, pointer args)
    {
      pointer key = pair_car(args);
      pointer alist = pair_cadr(args);
      return assoc_strcmp_all(_sc,key,alist);
    }

    // number stuff	
    pointer SchemeFFI::randomReal(scheme* _sc, pointer args)
    {
	return mk_real(_sc,UNIV::random());
    }
	
    pointer SchemeFFI::randomInt(scheme* _sc, pointer args)
    {
	return mk_integer(_sc,UNIV::random(ivalue(pair_car(args))));
    }
	
    pointer SchemeFFI::integerToReal(scheme* _sc, pointer args)
    {
	double val = (double) ivalue(pair_car(args));
	return mk_real(_sc,val);
    }
	
    pointer SchemeFFI::rationalToReal(scheme* _sc, pointer args)
    {
	return mk_real(_sc, rvalue(pair_car(args)));
    }

    pointer SchemeFFI::realToRational(scheme* _sc, pointer args)
    {
	double val = rvalue(pair_car(args));
	long long vali = (long long) val;
	double remain = val - (double)vali;
	return mk_rational(_sc, ((long long)(remain*10000000.0))+(vali*10000000ll), 10000000ll);
    }

    pointer SchemeFFI::rationalToNumerator(scheme* _sc, pointer args)
    {
        pointer rat = pair_car(args);
        if(!is_rational(rat)) 
          return mk_integer(_sc, ivalue(rat));
        return mk_integer(_sc,rat->_object._number.value.ratvalue.n);
    }

    pointer SchemeFFI::rationalToDenominator(scheme* _sc, pointer args)
    {
        pointer rat = pair_car(args);
        if(!is_rational(rat)) return mk_integer(_sc,1);
        return mk_integer(_sc,rat->_object._number.value.ratvalue.d);
    }
	
    pointer SchemeFFI::realToInteger(scheme* _sc, pointer args)
    {
	long long int val = (long long int) rvalue(pair_car(args));
	return mk_integer(_sc,val);
    }
	
  pointer SchemeFFI::openDynamicLib(scheme* _sc, pointer args)
  {
    //void* lib_handle = dlopen(string_value(pair_car(args)), RTLD_GLOBAL); //LAZY);
#ifdef _WIN32
    void* lib_handle = LoadLibraryA(string_value(pair_car(args)));
#else
    void* lib_handle = dlopen(string_value(pair_car(args)), RTLD_LAZY|RTLD_GLOBAL);
#endif
    if (!lib_handle)
      {
        // if an optional second argument is non-nil, print the error
        if(pair_cdr(args) != _sc->NIL && pair_cadr(args) != _sc->F)
          {
#ifdef _WIN32
            std::cout << "LoadLibrary error:" << GetLastError() << std::endl;
#else
            printf("%s\n", dlerror());
#endif
          }
        return _sc->F;
      }
    return mk_cptr(_sc,lib_handle);
  }

    pointer SchemeFFI::closeDynamicLib(scheme* _sc, pointer args)
    {
#ifdef _WIN32
      FreeLibrary((HMODULE)cptr_value(pair_car(args))) ;
#else
	dlclose(cptr_value(pair_car(args)));
#endif
	return _sc->T;
    }
	
	
    pointer SchemeFFI::pointerSize(scheme* _sc, pointer args)
    {
#ifdef TARGET_64BIT
	return mk_integer(_sc, 64);
#else
	return mk_integer(_sc, 32);
#endif
    }

    pointer SchemeFFI::mcjitEnabled(scheme* _sc, pointer args)
    {
#ifdef EXT_MCJIT
      return _sc->T;
#else
      return _sc->F;
#endif
    }  
  
    pointer SchemeFFI::cmdarg(scheme* _sc, pointer args)
    {
      char* key = string_value(pair_car(args));
      std::string val = UNIV::CMDPARAMS[std::string(key)];
      return mk_string(_sc,val.c_str());
    }

    pointer SchemeFFI::platform(scheme* _sc, pointer args)
    {
#ifdef __APPLE__
      return mk_string(_sc, "OSX");
#elif __linux__
      return mk_string(_sc, "Linux");
#elif _WIN32
	  return mk_string(_sc, "Windows");
#else
	  return mk_string(_sc, "");
#endif
    }

  pointer SchemeFFI::getShareDir(scheme* _sc, pointer args)
  {
		return mk_string(_sc,UNIV::SHARE_DIR.c_str());
  }

    // positions base64[62] = '+' and base64[63] == '/'
    pointer SchemeFFI::Base64Encode(scheme* _sc, pointer args)
    {
      const unsigned char* dat = (const unsigned char*) cptr_value(pair_car(args));
      size_t datlength = ivalue(pair_cadr(args));
      size_t lth = 0;
      char* res = base64_encode(dat,datlength,&lth);
      return mk_string(_sc,res);
    }
    // positions base64[62] = '+' and base64[63] == '/'
    pointer SchemeFFI::Base64Decode(scheme* _sc, pointer args)
    {
      char* str = string_value(pair_car(args));
      size_t lth = 0;
      unsigned char* res = base64_decode(str,strlen(str),&lth);      
      return mk_string(_sc,(char*)res);
    }

    // positions base64[62] = '_' and base64[63] == '-'
    pointer SchemeFFI::CNameEncode(scheme* _sc, pointer args)
    {
      char* str = string_value(pair_car(args));
      size_t lth = 0;
      char* res = cname_encode(str,strlen(str),&lth);
      return mk_string(_sc,res);
    }

    // positions base64[62] = '_' and base64[63] == '-'
    pointer SchemeFFI::CNameDecode(scheme* _sc, pointer args)
    {
      char* str = string_value(pair_car(args));
      size_t lth = 0;
      char* res = (char*) cname_decode(str,strlen(str),&lth);
      return mk_string(_sc,res);
    }

    pointer SchemeFFI::stringHash(scheme* _sc, pointer args)
    {
      char* str = string_value(pair_car(args));
      // unsigned long hash = 0;
      // int c;
  
  //     while ((c = *str++))
	// hash = c + (hash << 6) + (hash << 16) - hash;
  
      return mk_integer(_sc,string_hash((unsigned char*) str));
    }
	
    pointer SchemeFFI::stringStrip(scheme* _sc, pointer args)
    {
	return mk_string(_sc,cstrstrip(string_value(pair_car(args))));
    }
	
    pointer SchemeFFI::stringJoin(scheme* _sc, pointer args)
    {
	pointer array = pair_car(args);
	if(array == _sc->NIL) return mk_string(_sc,"");
	char* joinstr = string_value(pair_cadr(args));
	int len = 0;
	for(pointer x=pair_car(array);array != _sc->NIL;array=pair_cdr(array))
	{
	    x = pair_car(array);
	    len += strlen(string_value(x));
	}
	array = pair_car(args); // reset array
	len += (list_length(_sc,array)-1)*(strlen(joinstr));

	char* result = (char*) alloca(len+1);

	memset(result,0,len+1);

	for(pointer x=pair_car(array);array != _sc->NIL;array=pair_cdr(array))
	{
	    x=pair_car(array);
	    char* str = string_value(x);
	    strcat(result,str);
	    if(pair_cdr(array) != _sc->NIL) strcat(result,joinstr);
	}
	return mk_string(_sc,result);
    }	
		
    pointer SchemeFFI::getClosureEnv(scheme* _sc, pointer args)
    {
	return closure_env(pair_car(args));
    }
	
    pointer SchemeFFI::getTime(scheme* _sc, pointer args)
    {
	return mk_integer(_sc, extemp::UNIV::TIME);
    }	
	
    pointer SchemeFFI::sexprToString(scheme* _sc, pointer args)
    {
	std::stringstream ss;
	UNIV::printSchemeCell(_sc, ss, args, true);
	std::string s = ss.str();
	s.erase(0,1);
	s.erase(s.size()-1,1);
	return mk_string(_sc, s.c_str()); 		
    }
	
    pointer SchemeFFI::print(scheme* _sc, pointer args)
    {
	if(args == _sc->NIL) {
	    printf("\r\n");
	    fflush(stdout);
	    return _sc->T;
	}	    
	std::stringstream ss;
	UNIV::printSchemeCell(_sc, ss, args);
	std::string s = ss.str();
	s.erase(0,1);
	s.erase(s.size()-1,1);
	printf("%s\r\n",s.c_str());
	fflush(stdout);
	return _sc->T; 
    }
	
    pointer SchemeFFI::print_no_new_line(scheme* _sc, pointer args)
    {
	if(args == _sc->NIL) {
	    printf("\r\n");
	    fflush(stdout);
	    return _sc->T;
	}	    
	std::stringstream ss;
	UNIV::printSchemeCell(_sc, ss, args, false, false);
	std::string s = ss.str();
	s.erase(0,1);
	s.erase(s.size()-1,1);
	printf("%s",s.c_str());
	fflush(stdout);
	return _sc->T; 
    }

    pointer SchemeFFI::printFull(scheme* _sc, pointer args)
    {
	std::stringstream ss;
	UNIV::printSchemeCell(_sc, ss, args, true);
	std::string s = ss.str();
	s.erase(0,1);
	s.erase(s.size()-1,1);
	printf("%s\n",s.c_str());		
	return _sc->T; 
    }	
	
    pointer SchemeFFI::printFullNoQuotes(scheme* _sc, pointer args)
    {
	std::stringstream ss;
	UNIV::printSchemeCell(_sc, ss, args, true, false);
	std::string s = ss.str();
	s.erase(0,1);
	s.erase(s.size()-1,1);
	printf("%s\n",s.c_str());		
	return _sc->T; //mk_string(_sc, s.c_str()); 
    }		
	
    pointer SchemeFFI::printError(scheme* _sc, pointer args)
    {	
	std::stringstream ss;
	UNIV::printSchemeCell(_sc, ss, args, true);
	std::string s = ss.str();
	s.erase(0,1);
	s.erase(s.size()-1,1);
	ascii_text_color(1,1,10);
	printf("%s\n",s.c_str());	
	ascii_text_color(0,7,10);
        fflush(stdout);	
	return _sc->T; //mk_string(_sc, s.c_str()); 
    }

    pointer SchemeFFI::printNotification(scheme* _sc, pointer args)
    {
	std::stringstream ss;
	UNIV::printSchemeCell(_sc, ss, args);
	std::string s = ss.str();
	s.erase(0,1);
	s.erase(s.size()-1,1);
	ascii_text_color(1,3,10);
	printf("%s\n",s.c_str());
	ascii_text_color(0,7,10);		
        fflush(stdout);	
	return _sc->T; //mk_string(_sc, s.c_str()); 
    }
	
    pointer SchemeFFI::callCPPAtTime(scheme* _sc, pointer args)
    {	
	if(is_cptr(pair_caddr(args)) == false) {
	    printf("Bad task needs valid CM ptr");
	    return _sc->F;
	}
		
	int task_type = ivalue(pair_cadr(args));
		
	bool is_callback = (task_type == 2) ? true : false;
		
	// is_render_thread_type are for tasks that must be called in the render thread
	// primarily for midi calls to an AU that must occur on the audio thread
	bool is_render_thread_type = (task_type == 1) ? true : false;

	if(is_render_thread_type) {
	    if(is_pair(pair_car(args))) { //check for tag based call
		std::cout << "NO RENDER THREAD AVAILABLE FOR CALLBACKS" << std::endl;
	    }else{ 
		std::cout << "NO RENDER THREAD AVAILABLE FOR CALLBACKS" << std::endl;				
	    }		
	}else{
	    if(is_pair(pair_car(args))) { 
		extemp::TaskScheduler::I()->addTask(ivalue(pair_caar(args)), ivalue(pair_cdar(args)), (extemp::CM*)(cptr_value(pair_caddr(args))), new SchemeObj(_sc, pair_cadddr(args), pair_caddddr(args)), 0, is_callback);
	    }else{ 
		extemp::TaskScheduler::I()->addTask(ivalue(pair_car(args)), ivalue(pair_car(args))+_sc->call_default_time, (extemp::CM*)(cptr_value(pair_caddr(args))), new SchemeObj(_sc, pair_cadddr(args), pair_caddddr(args)), 0, is_callback);
	    }		
	}
	return _sc->T;
    }	
	
	
    ///////////////////////////////////////////////////////
    //
    // REGEX STUFF
    //
    //////////////////////////////////////////////////////
	
    pointer SchemeFFI::regex_match(scheme* _sc, pointer args)	
    {
	char* data = string_value(pair_car(args));
	char* pattern = string_value(pair_cadr(args));		
	
	pcre *re; 
	const char *error; 
	int erroffset;

	re = pcre_compile(	pattern, /* the pattern */ 
			0, /* default options */ 
			&error, /* for error message */ 
			&erroffset, /* for error offset */ 
			NULL); /* use default character tables */
	
	int rc;
	int ovector[30]; 
	rc = pcre_exec(	re, /* result of pcre_compile() */ 
			NULL, /* we didn’t study the pattern */ 
			data, /* the subject string */ 
			strlen(data), /* the length of the subject string */ 
			0, /* start at offset 0 in the subject */ 
			0, /* default options */ 
			ovector, /* vector of integers for substring information */ 
			30); /* number of elements (NOT size in bytes) */
					
	return (rc>=0) ? _sc->T : _sc->F;
    }	

    pointer SchemeFFI::regex_matched(scheme* _sc, pointer args)	
    {
	char* data = string_value(pair_car(args));
	char* pattern = string_value(pair_cadr(args));		
		
	pcre *re; 
	const char *error; 
	int erroffset; 
	re = pcre_compile(	pattern, /* the pattern */ 
				0, /* default options */ 
				&error, /* for error message */ 
				&erroffset, /* for error offset */ 
				NULL); /* use default character tables */
		
	int rc; 
	int ovector[60]; 
	rc = pcre_exec(	re, /* result of pcre_compile() */ 
			NULL, /* we didn’t study the pattern */ 
			data, /* the subject string */ 
			strlen(data), /* the length of the subject string */ 
			0, /* start at offset 0 in the subject */ 
			0, /* default options */ 
			ovector, /* vector of integers for substring information */ 
			60); /* number of elements (NOT size in bytes) */
						
	// make pointer to hold return results
	pointer list = _sc->NIL;
		
	// if failed to match return empty list
	if(rc<0) return list;
		
	for(int i=0, p=0;i<rc;i++)
	{
	    //std::cout << "RC: " << rc << " " << ovector[p] << "::" << ovector[p+1] << std::endl;
	    p=i*2;
	    if(ovector[p]==-1) {
		_sc->imp_env->insert(list);
		pointer tlist = cons(_sc,mk_string(_sc,""),list);
		_sc->imp_env->erase(list);
		list = tlist;
	    }else{
		int range = ovector[p+1] - ovector[p];				
		char* b = (char*) alloca(range+1);
		memset(b,0,range+1);
		char* a = data+ovector[p];
		char* substring = strncpy(b, a, range);
		_sc->imp_env->insert(list);
		pointer tlist = cons(_sc,mk_string(_sc,substring),list);
		_sc->imp_env->erase(list);
		list = tlist;
	    }
	}
		
	return reverse(_sc,list);
    }	
	
    pointer SchemeFFI::regex_match_all(scheme* _sc, pointer args)	
    {
	char* data = string_value(pair_car(args));
	char* pattern = string_value(pair_cadr(args));		
		
	pcre *re; 
	const char *error; 
	int erroffset; 
	re = pcre_compile(	pattern, /* the pattern */ 
				0, /* default options */ 
				&error, /* for error message */ 
				&erroffset, /* for error offset */ 
				NULL); /* use default character tables */
		
	// pointer to hold return results		
	pointer list = _sc->NIL;
	int rc; 
	int ovector[60];			
		
	while(true) {
	    rc = pcre_exec(	re, /* result of pcre_compile() */ 
				NULL, /* we didn’t study the pattern */ 
				data, /* the subject string */ 
				strlen(data), /* the length of the subject string */ 
				0, /* start at offset 0 in the subject */ 
				0, /* default options */ 
				ovector, /* vector of integers for substring information */ 
				60); /* number of elements (NOT size in bytes) */
							
	    //std::cout << data << " RC: " << rc << " " << ovector[0] << "::" << ovector[1] << std::endl;
	    if(rc<1) {
		return reverse(_sc,list);
	    }
	    int range = ovector[1] - ovector[0];
	    char* b = (char*) alloca(range+1);
	    memset(b,0,range+1);
	    char* a = data+ovector[0];
	    char* substring = strncpy(b, a, range);
	    _sc->imp_env->insert(list);
	    pointer tlist = cons(_sc,mk_string(_sc,substring),list);
	    _sc->imp_env->erase(list);
	    list = tlist;
	    data = data+range+ovector[0];
	}
		
	// shouldn't ever reach here!
	return _sc->NIL;
    }
	
    pointer SchemeFFI::regex_split(scheme* _sc, pointer args)	
    {
	char* data = string_value(pair_car(args));
	char* pattern = string_value(pair_cadr(args));
		
	pcre *re; 
	const char *error; 
	int erroffset; 
	re = pcre_compile(	pattern, /* the pattern */ 
				0, /* default options */ 
				&error, /* for error message */ 
				&erroffset, /* for error offset */ 
				NULL); /* use default character tables */
		
	// pointer to hold return results		
	pointer list = _sc->NIL;
	int rc; 
	int ovector[60];			
		
	while(true) {
	    rc = pcre_exec(	re, /* result of pcre_compile() */ 
				NULL, /* we didn’t study the pattern */ 
				data, /* the subject string */ 
				strlen(data), /* the length of the subject string */ 
				0, /* start at offset 0 in the subject */ 
				0, /* default options */ 
				ovector, /* vector of integers for substring information */ 
				60); /* number of elements (NOT size in bytes) */
							
	    //std::cout << data << " RC: " << rc << " " << ovector[0] << "::" << ovector[1] << std::endl;
	    if(rc<1) {
		if(strlen(data)>0) // append remaining chars if any left
		{
		    list = cons(_sc,mk_string(_sc,data),list);
		}
		return reverse(_sc,list);
	    }
	    int range = ovector[0];
	    char* b = (char*) alloca(range+1);
	    memset(b,0,range+1);
	    char* substring = strncpy(b, data, range);
	    _sc->imp_env->insert(list);
	    pointer tlist = cons(_sc,mk_string(_sc,substring),list);
	    _sc->imp_env->erase(list);
	    list = tlist;
	    data = data+ovector[1];
	}
		
	// shouldn't ever reach here!
	return _sc->NIL;
    }
	
    pointer SchemeFFI::regex_replace(scheme* _sc, pointer args)	
    {
	char* data = string_value(pair_car(args));
	char* pattern = string_value(pair_cadr(args));
	char* replace = string_value(pair_caddr(args));
		
	pcre *re;
	const char *error; 
	int erroffset; 
	re = pcre_compile(	pattern, /* the pattern */ 
				0, /* default options */ 
				&error, /* for error message */ 
				&erroffset, /* for error offset */ 
				NULL); /* use default character tables */
		
	int rc; 
	int ovector[60];
		
	rc = pcre_exec(	re, /* result of pcre_compile() */ 
			NULL, /* we didn’t study the pattern */ 
			data, /* the subject string */ 
			strlen(data), /* the length of the subject string */ 
			0, /* start at offset 0 in the subject */ 
			0, /* default options */ 
			ovector, /* vector of integers for substring information */ 
			60); /* number of elements (NOT size in bytes) */

	// no match found return original string
	if(rc<1) return mk_string(_sc,data);
					
	// ok we have a match
	// first replace any groups in replace string (i.e. $1 $2 ...)
	char* res = (char*) "";
	char* sep = (char*) "$";
	char* tmp = 0;
	int pos,range,size = 0;
 	char* p = strtok(replace,sep);
	do{
	    char* cc;
	    pos = strtol(p,&cc,10);
	    range = (pos>0) ? ovector[(pos*2)+1] - ovector[pos*2] : 0;
	    size = strlen(res);
	    tmp = (char*) alloca(size+range+strlen(cc)+1);
	    memset(tmp,0,size+range+strlen(cc)+1);
	    memcpy(tmp,res,size);
	    memcpy(tmp+size,data+ovector[pos*2],range);
	    memcpy(tmp+size+range,cc,strlen(cc));
	    res = tmp;
	    p = strtok(NULL, sep);
	}while(p);
		
	// now we can use "rep" to replace the original regex match (i.e. ovector[0]-ovector[1])
	int lgth = (strlen(data)-range)+strlen(res)+1;
	range = ovector[1] - ovector[0];
	char* result = (char*) alloca(lgth);
	memset(result,0,lgth);
	memcpy(result,data,ovector[0]);
	memcpy(result+ovector[0],res,strlen(res));
	memcpy(result+ovector[0]+strlen(res),data+ovector[1],strlen(data)-ovector[1]);		
		
	return mk_string(_sc,result);
    }

    ///////////////////////////////////////////////////////
    //
    // MEMORY ZONE STUFF
    //
    //////////////////////////////////////////////////////
    void SchemeFFI::freeWithDelay(TaskI* task)
    {
	Task<char*>* t = static_cast<Task<char*>*>(task);
        char* dat = t->getArg();
	free(dat);
    }

    void SchemeFFI::destroyMallocZoneWithDelay(TaskI* task)
    {
	Task<llvm_zone_t*>* t = static_cast<Task<llvm_zone_t*>*>(task);
	llvm_zone_t* zone = t->getArg();
	llvm_zone_destroy(zone);
    }

    pointer SchemeFFI::createMallocZone(scheme* _sc, pointer args)
    {
	if(args == _sc->NIL)
	    return mk_cptr(_sc,llvm_zone_create(1024*100));
	else
	    return mk_cptr(_sc,llvm_zone_create(ivalue(pair_car(args))));
    }

    pointer SchemeFFI::defaultMallocZone(scheme* _sc, pointer args)
    {
	return mk_cptr(_sc,SchemeProcess::I(_sc)->getDefaultZone()); //llvm_zone_default());
    }
	
    pointer SchemeFFI::destroyMallocZone(scheme* _sc, pointer args)
    {		
	llvm_zone_t* ptr = (llvm_zone_t*) cptr_value(pair_car(args));
	if(pair_cdr(args) != _sc->NIL)
	{
	  llvm_destroy_zone_after_delay(ptr, ivalue(pair_cadr(args)));
	}
        else
	{
	  llvm_zone_destroy(ptr);
	}
	return _sc->T;
    }

    pointer SchemeFFI::resetMallocZone(scheme* _sc, pointer args)
    {		
	llvm_zone_t* zone = (llvm_zone_t*) cptr_value(pair_car(args));
	llvm_zone_reset(zone);
	return _sc->T;
    }

    pointer SchemeFFI::copyToDefaultZone(scheme* _sc, pointer args)
    {		
	return _sc->NIL;
    }

    pointer SchemeFFI::peekMemoryZone(scheme* _sc, pointer args)
    {
        return mk_cptr(_sc,llvm_peek_zone_stack());
    }

    pointer SchemeFFI::popMemoryZone(scheme* _sc, pointer args)
    {
        return mk_cptr(_sc,llvm_pop_zone_stack());
    }

    pointer SchemeFFI::pushMemoryZone(scheme* _sc, pointer args)
    {
        llvm_push_zone_stack((llvm_zone_t*)cptr_value(pair_car(args)));
        return _sc->T;
    }
	
    ////////////////////////////////////////////
    //
    // LLVM STUFF
    //
    /////////////////////////////////////////////

    pointer SchemeFFI::optimizeCompiles(scheme* _sc, pointer args)
    {
        EXTLLVM::OPTIMIZE_COMPILES = (pair_car(args) == _sc->T) ? 1 : 0; 
        return _sc->T;
    }

    pointer SchemeFFI::verifyCompiles(scheme* _sc, pointer args)
    {
        EXTLLVM::VERIFY_COMPILES = (pair_car(args) == _sc->T) ? 1 : 0; 
        return _sc->T;
    }

#ifdef EXT_MCJIT
  static long long llvm_emitcounter = 0;
#endif
  pointer SchemeFFI::jitCompileIRString(scheme* _sc, pointer args)
  {
    // Create some module to put our function into it.
    using namespace llvm;
    Module* M = EXTLLVM::I()->M;
    legacy::PassManager* PM = extemp::EXTLLVM::I()->PM;

#ifdef EXT_MCJIT
    char modname[256];
    sprintf(modname, "xtmmodule_%lld", ++llvm_emitcounter);
    char tmpbuf[1024];
#endif

    char* assm = string_value(pair_car(args));
    SMDiagnostic pa;
    //ParseError pa;
    long long num_of_funcs = M->getFunctionList().size();

#ifndef EXT_MCJIT
    std::unique_ptr<llvm::Module> newModule = ParseAssemblyString(assm, M, pa, getGlobalContext());

    if(EXTLLVM::OPTIMIZE_COMPILES)
      {
        PM->run(*M);
      }

#else
    std::string asmcode(assm);
    int cnt = 0;

    std::unique_ptr<llvm::Module> newModule;
    do {
      newModule = parseAssemblyString(asmcode, pa, getGlobalContext());

      if(newModule != NULL) break;
      std::string err = pa.getMessage().str();
      if(cnt > 1000) {
        std::cout << "MCJIT Compiler Error: could not resolve all external dependencies" << std::endl;
        break;
      }
      if(err.find("use of undefined value") != std::string::npos) {
        char symname[1024];
        const char* undef_value_string = "use of undefined value '@(.*)'";
        const char* match1_string = "$1";
        const char* eq_type_string = " = type ";
        rreplace((char*)undef_value_string,(char*)err.c_str(),(char*)match1_string,symname);
        GlobalValue* gv = extemp::EXTLLVM::I()->getGlobalValue(symname);
        if(gv == NULL){
          std::cout << "MCJIT compiler error: \"" << symname << "\" not defined in LLVM" << std::endl;
          return _sc->F;
        }
        Function* func = extemp::EXTLLVM::I()->getFunction(symname);
        const char* tmp_name = NULL;
      
        if(func != NULL) {
          Function::ArgumentListType::iterator funcargs = func->getArgumentList().begin();
          int cntt = 0;
          while(funcargs != func->getArgumentList().end())
            {			
              Argument* a = funcargs;
              std::string typestr2;
              llvm::raw_string_ostream ss2(typestr2);
              llvm::Type* t = a->getType();
              t->print(ss2);
              tmp_name = ss2.str().c_str();
              if(a->getType()->isStructTy()) {	      
                rsplit((char*)eq_type_string,(char*)tmp_name,tmp_str_a,tmp_str_b);
              }
              funcargs++;
              cntt++;
            }          
        }
      
        GlobalValue* vvv = gv;
        std::string typestr;
        llvm::raw_string_ostream ss(typestr);
        vvv->getType()->print(ss);
        std::string stype = ss.str();

        stype.resize(stype.length()-1); // drop last '*'
        std::string exprr("");
        if(func) {
          Type* rettype = func->getReturnType();
          typestr.clear();
          llvm::raw_string_ostream sts(typestr);
          rettype->print(sts);
          std::size_t pos = sts.str().find(" = type");
          std::string rettypestr = sts.str();                    
          if (pos > 0) {
            rettypestr = sts.str().substr(0, pos);
          }
          std::string argstypestr = stype.substr(rettypestr.length()+1,stype.length());
          sprintf(tmpbuf,"declare %s @%s%s",rettypestr.c_str(),symname,argstypestr.c_str());
          
          exprr.append(tmpbuf);
        } else {
          exprr.append("@");
          exprr.append(symname);
          exprr.append(" = external global ");
          exprr.append(stype);          
        }
        exprr.append("\n\n");        
        exprr.append(asmcode);        
        asmcode.clear();
        asmcode.append(exprr);        
        cnt++;
      }else{
        break;
      }
    }while (newModule == 0);

	if (!extemp::UNIV::ARCH.empty()) newModule->setTargetTriple(extemp::UNIV::ARCH.front());

    if(EXTLLVM::OPTIMIZE_COMPILES)
      {
        PM->run(*newModule);
      }

#endif
  
    //std::stringstream ss;
    if(newModule == 0)
      {
        std::string errstr;
        llvm::raw_string_ostream ss(errstr);
        pa.print("LLVM IR",ss);
        printf("%s\n",ss.str().c_str());
#ifndef EXT_MCJIT    
        // if the number of functions in module has changed when
        // calling runFunction then we assume a stub was made and
        // appended to the end of the modules function list. we remove
        // this function now that we no longer need it!
        if(num_of_funcs != M->getFunctionList().size()) {
          iplist<Function>::iterator iter = M->getFunctionList().end();
          Function* func = dyn_cast<Function>(--iter);
          //std::cout << "REMOVING ON FAIL: " << *func << std::endl;
          func->dropAllReferences();
          func->removeFromParent();
        }
#endif
        return _sc->F;
      }else{
      if (extemp::EXTLLVM::VERIFY_COMPILES) {
        if (verifyModule(*M)) {
          std::cout << "\nInvalid LLVM IR\n";
#ifndef EXT_MCJIT        
          if(num_of_funcs != M->getFunctionList().size()) {
            iplist<Function>::iterator iter = M->getFunctionList().end();
            Function* func = dyn_cast<Function>(--iter);
            //std::cout << "REMOVING ON FAIL: " << *func << std::endl;
            func->dropAllReferences();
            func->removeFromParent();
          }
#endif        
          return _sc->F;
        } 
      }
#ifdef EXT_MCJIT
      llvm::Module *modulePtr = newModule.get();
      extemp::EXTLLVM::I()->EE->addModule(std::move(newModule));
      extemp::EXTLLVM::I()->addModule(modulePtr);
      extemp::EXTLLVM::I()->EE->finalizeObject();

      // when using MCJIT, return a pointer to the module with the new
      // functions in it - which we'll use later to export the bitcode
      // during AOT-compilation
      return mk_cptr(_sc, modulePtr);
#else
      return _sc->T;
#endif
    }
  }
	
    pointer SchemeFFI::ff_set_name(scheme* _sc, pointer args)
    {
       pointer x = pair_car(args);
       foreign_func ff = x->_object._ff;
       char* name = string_value(pair_cadr(args));
       llvm_scheme_ff_set_name(ff,name);
       return _sc->T;
    }

    pointer SchemeFFI::ff_get_name(scheme* _sc, pointer args)
    {
       pointer x = pair_car(args);
       foreign_func ff = x->_object._ff;
       const char* name = llvm_scheme_ff_get_name(ff);
       return mk_string(_sc,name);
    }

  pointer SchemeFFI::get_function(scheme* _sc, pointer args)
  {
    using namespace llvm;
    //Module* M = EXTLLVM::I()->M;
    llvm::Function* func = extemp::EXTLLVM::I()->getFunction(std::string(string_value(pair_car(args))));
    //llvm::Function* func = M->getFunction(std::string(string_value(pair_car(args))));
    if(func == 0)
      {
        return _sc->F;
      }
    return mk_cptr(_sc, func); 				
  }

  pointer SchemeFFI::get_globalvar(scheme* _sc, pointer args)
  {
    using namespace llvm;

    //Module* M = EXTLLVM::I()->M;
    //llvm::GlobalVariable* var = M->getGlobalVariable(std::string(string_value(pair_car(args))));
    llvm::GlobalVariable* var = extemp::EXTLLVM::I()->getGlobalVariable(std::string(string_value(pair_car(args))));   
    if(var == 0)
      {
        return _sc->F;
      }				
    return mk_cptr(_sc, var); 				
  }


  pointer SchemeFFI::get_function_calling_conv(scheme* _sc, pointer args)
  {
    using namespace llvm;

    //Module* M = EXTLLVM::I()->M;
    //llvm::Function* func = M->getFunction(std::string(string_value(pair_car(args))));
    llvm::Function* func = extemp::EXTLLVM::I()->getFunction(std::string(string_value(pair_car(args))));    
    if(func == 0)
      {
        return _sc->F;
      }			

    int cc = func->getCallingConv();
    return mk_integer(_sc, cc);
  }

  pointer SchemeFFI::get_function_varargs(scheme* _sc, pointer args)
  {
    using namespace llvm;

    //Module* M = EXTLLVM::I()->M;
    //llvm::Function* func = M->getFunction(std::string(string_value(pair_car(args))));
    llvm::Function* func = extemp::EXTLLVM::I()->getFunction(std::string(string_value(pair_car(args))));  
    if(func == 0)
      {
        return _sc->F;
      }	
    return func->isVarArg() ? _sc->T : _sc->F;
  }

  pointer SchemeFFI::llvm_print_all_closures(scheme* _sc, pointer args)
  {
    using namespace llvm;
    char* x = string_value(pair_car(args));
    char rgx[1024];
    memset((void*)&rgx[0],0,1024);
    memcpy((void*)&rgx[0],x,strlen(x));
    strcat((char*)&rgx[0],"_.*");
    // printf("check regex: %s\n",(char*)&rgx[0]);

    Module* M = NULL;
    std::vector<llvm::Module*> Ms = EXTLLVM::I()->getModules();
    for (int i=0;i<Ms.size();i++) {
      M = Ms[i];    
      for (Module::const_iterator GI = M->begin(), GE = M->end(); GI != GE; ++GI) {
        const llvm::Function* func = GI;
        if (func->hasName() && rmatch((char*)&rgx[0],(char*)func->getName().data())) {
          //printf("HIT %s\n",func->getName().data());
          std::string str;
          llvm::raw_string_ostream ss(str);
          ss << *func;
          printf("\n---------------------------------------------------\n%s",str.c_str());        
        }
      }
    }
    return _sc->T;
  }

  pointer SchemeFFI::llvm_closure_last_name(scheme* _sc, pointer args)
  {
    using namespace llvm;
    char* x = string_value(pair_car(args));
    char rgx[1024];
    memset((void*)&rgx[0],0,1024);
    memcpy((void*)&rgx[0],x,strlen(x));
    strcat((char*)&rgx[0],"__[0-9]*");
    // printf("check regex: %s\n",(char*)&rgx[0]);
    char* last_name = NULL;

    Module* M = NULL;
    std::vector<llvm::Module*> Ms = EXTLLVM::I()->getModules();
    for (int i=0;i<Ms.size();i++) {
      M = Ms[i];    
      for (Module::const_iterator GI = M->begin(), GE = M->end(); GI != GE; ++GI) {
        const llvm::Function* func = GI;
        if (func->hasName() && rmatch((char*)&rgx[0],(char*)func->getName().data())) {
          last_name = (char*)func->getName().data();
        }
      }
    }
    //std::cout << "fullname:" << last_name << std::endl;
    if(last_name) return mk_string(_sc,last_name);
    else return _sc->F;
  }


    pointer SchemeFFI::llvm_print_closure(scheme* _sc, pointer args)
  {
    using namespace llvm;
    char* fname = string_value(pair_car(args));
    
    Module* M = NULL;
    std::vector<llvm::Module*> Ms = EXTLLVM::I()->getModules();
    for (int i=0;i<Ms.size();i++) {
      M = Ms[i];
      for (Module::const_iterator GI = M->begin(), GE = M->end(); GI != GE; ++GI) {
        const llvm::Function* func = GI;
        if (func->hasName() && strcmp(func->getName().data(),fname)==0) {
          std::string str;
          llvm::raw_string_ostream ss(str);
          ss << *func;
          if(str.find_first_of("{") != std::string::npos) {
            std::cout << str << std::endl;
          }
          //printf("\n---------------------------------------------------\n%s",str.c_str());
        }
      }
    }
    return _sc->T;
  }
  

  pointer SchemeFFI::llvm_disasm(scheme* _sc, pointer args)
  {
    //using namespace llvm;
    //long bytes = ivalue(pair_cadr(args));
    //int x64 = (pair_caddr(args) == _sc->T) ? 1 : 0;
    int lgth = list_length(_sc, args);
    int syntax = 1;
    if(lgth > 1) {
      syntax = ivalue(pair_cadr(args));
    }
    if (syntax > 1) {
      std::cout << "Syntax argument must be either 0: at&t or 1: intel" << std::endl;
      std::cout << "The default is 1: intel" << std::endl;      
      syntax = 1;
    }
    pointer name = SchemeFFI::llvm_closure_last_name(_sc, args);
    unsigned char* fptr = (unsigned char*) cptr_value(SchemeFFI::get_function_pointer(_sc,cons(_sc,name,pair_cdr(args))));
    char* dasm = llvm_disassemble(fptr,syntax); //,bytes,x64);
    return mk_string(_sc,dasm);
  }  
  
  pointer SchemeFFI::get_struct_size(scheme* _sc, pointer args)
  {
    using namespace llvm;

#ifdef EXT_MCJIT
    legacy::PassManager* PM = extemp::EXTLLVM::I()->PM;
#else
    PassManager* PM = extemp::EXTLLVM::I()->PM;
#endif
    char* struct_type_str = string_value(pair_car(args));
    unsigned long long hash = string_hash((unsigned char*)struct_type_str);
    char name[128];
    sprintf(name,"_xtmT%lld",hash);
    char assm[1024];
    sprintf(assm,"%%%s = type %s",name,struct_type_str);
    //printf("parse this! %s\n",assm);
    SMDiagnostic pa;
    // Don't!! write this into the default module!
#ifdef EXT_MCJIT
    std::unique_ptr<llvm::Module> newM = parseAssemblyString(assm, pa, getGlobalContext());
#else
    const Module* newM = ParseAssemblyString(assm, NULL, pa, getGlobalContext());
#endif
    if(newM == 0)
      {
        return _sc->F;
      }
    StructType* type = newM->getTypeByName(std::string(name));
    if(type == 0)
      {
        return _sc->F;
      }
    DataLayout* layout = new DataLayout(newM.get());
    const StructLayout* sl = layout->getStructLayout(type);
    long size = sl->getSizeInBytes();
    delete layout;
    return mk_integer(_sc,size);
  }

  pointer SchemeFFI::get_named_struct_size(scheme* _sc, pointer args)
  {
    using namespace llvm;

    Module* M = EXTLLVM::I()->M;
    //StructType* type = M->getTypeByName(std::string(string_value(pair_car(args))));
    StructType* type = extemp::EXTLLVM::I()->getNamedType(std::string(string_value(pair_car(args))));
    if(type == 0)
      {
        return _sc->F;
      }
    DataLayout* layout = new DataLayout(M);
    const StructLayout* sl = layout->getStructLayout(type);
    long size = sl->getSizeInBytes();
    delete layout;
    return mk_integer(_sc,size);       
  }

  pointer SchemeFFI::get_function_type(scheme* _sc, pointer args)
  {
    using namespace llvm;

    //Module* M = EXTLLVM::I()->M;
    //llvm::Function* func = M->getFunction(std::string(string_value(pair_car(args))));
    llvm::Function* func = extemp::EXTLLVM::I()->getFunction(std::string(string_value(pair_car(args))));  
    if(func == 0)
      {
        return _sc->F;
      }			

    std::string typestr;
    llvm::raw_string_ostream ss(typestr);
    func->getFunctionType()->print(ss);
    //printf("%s\n",ss.str().c_str());
    pointer str = mk_string(_sc, ss.str().c_str()); //func->getFunctionType()->getDescription().c_str());
    return str;
  }


  pointer SchemeFFI::get_global_module(scheme* _sc, pointer args)
  {
    using namespace llvm;

    Module* M = EXTLLVM::I()->M;
    if(M == NULL)
      {
        return _sc->F;
      }
    return mk_cptr(_sc, M);
  }


  pointer SchemeFFI::export_llvmmodule_bitcode(scheme* _sc, pointer args)
  {
    using namespace llvm;

#ifdef EXT_MCJIT
    Module* m = (Module *)cptr_value(pair_car(args));
#else
    Module* m = EXTLLVM::I()->M;
#endif // EXT_MCJIT
    if(m == 0)
      {
        return _sc->F;
      }			
        
    char* filename = string_value(pair_cadr(args));
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

    while((pos = irStr.find(oldStr, pos)) != std::string::npos)
      {
        irStr.replace(pos, oldStr.length(), newStr);
        pos += newStr.length();
      }

    // LLVM can't handle guaranteed tail call under win64 yet
    oldStr = std::string(" tail call ");
    newStr = std::string(" call ");
    pos = 0;

    while((pos = irStr.find(oldStr, pos)) != std::string::npos)
      {
        irStr.replace(pos, oldStr.length(), newStr);
        pos += newStr.length();
      }

    fout << irStr; //ss.str();
    fout.close();
#else
    std::error_code errcode;
    llvm::raw_fd_ostream ss(filename, errcode, llvm::sys::fs::F_RW);
    if(errcode) {
      std::cout << errcode.message() << std::endl;
      return _sc->F;
    }
    llvm::WriteBitcodeToFile(m,ss);
#endif
    return _sc->T;    
  }

  pointer SchemeFFI::get_function_args(scheme* _sc, pointer args)
  {
    using namespace llvm;

    //Module* M = EXTLLVM::I()->M;
    //llvm::Function* func = M->getFunction(std::string(string_value(pair_car(args))));
    llvm::Function* func = extemp::EXTLLVM::I()->getFunction(std::string(string_value(pair_car(args))));    
    if(func == 0)
      {
        return _sc->F;
      }

    std::string typestr;
    llvm::raw_string_ostream ss(typestr);
    func->getReturnType()->print(ss);

    const char* tmp_name = ss.str().c_str();
    const char* eq_type_string = " = type ";

    if(func->getReturnType()->isStructTy()) {
      rsplit((char*)eq_type_string,(char*)tmp_name,tmp_str_a,tmp_str_b);
      tmp_name = tmp_str_a;
    }

    pointer str = mk_string(_sc, tmp_name); //_sc, ss.str().c_str()); //func->getReturnType()->getDescription().c_str());
    pointer p = cons(_sc, str, _sc->NIL); 

    Function::ArgumentListType::iterator funcargs = func->getArgumentList().begin();
    while(funcargs != func->getArgumentList().end())
      {			
        Argument* a = funcargs;
        _sc->imp_env->insert(p);					
        std::string typestr2;
        llvm::raw_string_ostream ss2(typestr2);
        a->getType()->print(ss2);

        tmp_name = ss2.str().c_str();

        if(a->getType()->isStructTy()) {	      
          rsplit((char*)eq_type_string,(char*)tmp_name,tmp_str_a,tmp_str_b);
          //printf("tmp:%s  a:%s  b:%s\n",(char*)tmp_name,tmp_str_a,tmp_str_b);
          tmp_name = tmp_str_a;
        }

        pointer str = mk_string(_sc, tmp_name); //_sc, ss2.str().c_str()); //a->getType()->getDescription().c_str());
        _sc->imp_env->erase(p);
        p = cons(_sc, str, p);			
        funcargs++;
      }
    return reverse(_sc, p);				
  }

    pointer SchemeFFI::remove_global_var(scheme* _sc, pointer args)
    {
	// Create some module to put our function into it.
	using namespace llvm;

	// Module* M = EXTLLVM::I()->M;  
	// llvm::GlobalVariable* var = M->getGlobalVariable(std::string(string_value(pair_car(args))));
	llvm::GlobalVariable* var = extemp::EXTLLVM::I()->getGlobalVariable(std::string(string_value(pair_car(args))));  
	if(var == 0)
	{
	    return _sc->F;
	}		
	var->dropAllReferences();		
	var->removeFromParent();
	return _sc->T;
    }	

  pointer SchemeFFI::remove_function(scheme* _sc, pointer args)
  {
    // Create some module to put our function into it.
    using namespace llvm;

    //Module* M = EXTLLVM::I()->M;		
    //llvm::Function* func = M->getFunction(std::string(string_value(pair_car(args))));
    llvm::Function* func = extemp::EXTLLVM::I()->getFunction(std::string(string_value(pair_car(args))));    
    if(func == 0)
      {
        return _sc->F;
      }		
    if(func->mayBeOverridden()) {
	    func->dropAllReferences();		
	    func->removeFromParent();
	    return _sc->T;
    }else{
	    printf("Cannot remove function with dependencies\n");
	    return _sc->F;
    }
  }		

  pointer SchemeFFI::erase_function(scheme* _sc, pointer args)
  {
    // Create some module to put our function into it.
    using namespace llvm;		
    //Module* M = EXTLLVM::I()->M;
    //llvm::Function* func = M->getFunction(std::string(string_value(pair_car(args))));
    llvm::Function* func = extemp::EXTLLVM::I()->getFunction(std::string(string_value(pair_car(args))));      
    if(func == 0)
      {
        return _sc->F;
      }
#ifndef EXT_MCJIT    
    extemp::EXTLLVM::I()->EE->lock.acquire();
    extemp::EXTLLVM::I()->EE->freeMachineCodeForFunction(func);
    extemp::EXTLLVM::I()->EE->lock.release();
#endif    
    func->deleteBody();
    func->eraseFromParent();

    return _sc->T;
  }

    pointer SchemeFFI::callClosure(scheme* _sc, pointer args)
    {
	using namespace llvm;
	uint32_t** closure = (uint32_t**) cptr_value(pair_car(args));
	void* eptr = (void*) *(closure+0);		
	int64_t (*fptr)(void*, int64_t) = (int64_t (*)(void*, int64_t)) *(closure+1);
	return mk_integer(_sc, (*fptr)(eptr,ivalue(pair_cadr(args))));
    }
	
  pointer SchemeFFI::get_global_variable_type(scheme* _sc, pointer args)
  {
    using namespace llvm;

    //Module* M = EXTLLVM::I()->M;
    //Module::global_iterator i = M->global_begin();
    //GlobalVariable* var = M->getNamedGlobal(std::string(string_value(pair_car(args))));
    llvm::GlobalVariable* var = extemp::EXTLLVM::I()->getGlobalVariable(std::string(string_value(pair_car(args))));      
    if(var == 0)
      {
        return _sc->F;
      }				
    std::string typestr;
    llvm::raw_string_ostream ss(typestr);
    var->getType()->print(ss);
    return mk_string(_sc, ss.str().c_str()); //var->getType()->getDescription().c_str());
  }	

  pointer SchemeFFI::get_function_pointer(scheme* _sc, pointer args)
  {
    using namespace llvm;

    llvm::Function* func = EXTLLVM::I()->getFunction(std::string(string_value(pair_car(args))));        
    if(func == 0)
      {
        return _sc->F;
      }

    void* p;

    // has the function been loaded somewhere else, e.g. dlsym
      p = EXTLLVM::I()->EE->getPointerToGlobalIfAvailable(func);
    if(p==NULL) // look for it as a JIT-compiled function
      p = EXTLLVM::I()->EE->getPointerToFunction(func);
    if(p==NULL) {
      return _sc->F;
    }

    return mk_cptr(_sc, p);
  }		

  pointer SchemeFFI::llvm_call_void_native(scheme* _sc, pointer args)
  {
    using namespace llvm;

    //Module* M = EXTLLVM::I()->M;
    char name[1024];
    sprintf(name,"%s_native",string_value(pair_car(args)));
    //llvm::Function* func = M->getFunction(std::string(name));
    llvm::Function* func = extemp::EXTLLVM::I()->getFunction(std::string(name));     
    //llvm::Function* func = M->getFunction(std::string(string_value(pair_car(args))));
    //func->setCallingConv(CallingConv::C); //kCStackBased);
    if(func == 0)
      {
        return _sc->F;
      }
    // this should be safe without a lock
    void* p = EXTLLVM::I()->EE->getPointerToFunction(func);

    if(p==NULL) {
	    //[[LogView sharedInstance] error:@"LLVM: Bad Function Ptr\n"];
	    return _sc->F;
    }

    void(*f)(void) = (void(*)(void)) p;
    f();
        
    return _sc->T;
  }		
	
  pointer SchemeFFI::recompile_and_link_function(scheme* _sc, pointer args)
  {
    using namespace llvm;
		
    std::string fname(string_value(pair_car(args)));
    //Module* M = EXTLLVM::I()->M;
    llvm::Function* func = extemp::EXTLLVM::I()->getFunction(fname);    
    //llvm::Function* func = M->getFunction(fname); //std::string(string_value(pair_car(args))));
    //func->setCallingConv(CallingConv::C); //kCStackBased);
    if(func == 0)
      {
        return _sc->F;
      }
    //std::cout << "fname: " << fname << std::endl;
    //std::cout << "M: " << M << std::endl;
    //std::cout << "FUNC: " << func << std::endl;
    //std::cout << "EE: " << EXTLLVM::I()->EE << std::endl;
    void* p = 0;
    try{
      EXTLLVM* xll = EXTLLVM::I();
      ExecutionEngine* EE = xll->EE;
      
#ifdef EXT_MCJIT
      void* p = NULL;
#else      
      EE->lock.acquire();
      void* p = EE->recompileAndRelinkFunction(func);
      EE->lock.release();
#endif      
    }catch(std::exception& e) {
      std::cout << "EXCEPT: " << e.what() << std::endl;
    }
		
    if(p==NULL) {
	    return _sc->F;
    }
		
    //const llvm::GlobalValue* f2 = NativeScheme::I()->EE->getGlobalValueAtAddress(p);
    //std::cout << "FUNC: " << func << "  fptr: " << p << "   f2: " << f2 << std::endl;
		
    return mk_cptr(_sc, p);
  }

    //
    // This will not be threadsafe whenever a bind-func is done!
    // 
    pointer SchemeFFI::call_compiled(scheme* _sc, pointer args)
    {
	using namespace llvm;

	ExecutionEngine* EE = EXTLLVM::I()->EE;

#ifdef LLVM_EE_LOCK	
  llvm::MutexGuard locked(EE->lock);
#endif
	
        Module* M = EXTLLVM::I()->M;
	llvm::Function* func = (Function*) cptr_value(pair_car(args));
	if(func == 0)
	{
	    //std::cout << "no such function\n" << std::endl;
	    printf("No such function\n");
	    return _sc->F;
	}				
	func->getArgumentList();
	args = pair_cdr(args);

	//llvm::Function* func = NativeScheme::LLVM_JIT->getFunction(std::string("testplusthree"));
	//llvm::Argument* arg = func->arg_begin();
	int lgth = list_length(_sc, args);
	Function::ArgumentListType::iterator funcargs = func->getArgumentList().begin();
	if(lgth != func->getArgumentList().size())
	{
	    printf("Wrong number of arguments for function!\n");
	    return _sc->F;			
	}
	std::vector<llvm::GenericValue> fargs(lgth);
	//std::cout << "ARGS: " << lgth << std::endl;
	for(int i=0;i<lgth;i++,++funcargs)
	{
	    Argument* a = funcargs;
	    pointer p = list_ref(_sc, i, args);
	    if(is_integer(p)) {
		if(a->getType()->getTypeID() != Type::IntegerTyID)
		{
		    printf("Bad argument type %i\n",i);
		    return _sc->F;
		}
		int width = a->getType()->getPrimitiveSizeInBits();
		//std::cout << "TYPE: " << a->getType()->getTypeID() << std::endl;				
		fargs[i].IntVal = APInt(width,ivalue(p));
	    }			
	    else if(is_real(p))
	    {

		if(a->getType()->getTypeID() == Type::FloatTyID)
		{
		    fargs[i].FloatVal = (float) rvalue(p);
		}
		else if(a->getType()->getTypeID() == Type::DoubleTyID)
		{
		    fargs[i].DoubleVal = rvalue(p);
		}
		else
		{
		    printf("Bad argument type %i\n",i);
		    return _sc->F;
		}
	    }
	    else if(is_string(p))
	    {
		if(a->getType()->getTypeID() != Type::PointerTyID)
		{
		    printf("Bad argument type %i\n",i);
		    return _sc->F;					
		}
		//std::cout << "PTRVALUE: " << cptr_value(p) << std::endl; 				
		fargs[i].PointerVal = string_value(p);
	    }						
	    else if(is_cptr(p))
	    {
		if(a->getType()->getTypeID() != Type::PointerTyID)
		{
		    printf("Bad argument type %i\n",i);
		    return _sc->F;
		}
		fargs[i].PointerVal = cptr_value(p);
		//fargs[i].PointerVal = (void*)p;
	    }
	    else if(is_closure(p))
	    {			
		//ascii_print_color(1,1,10); // error color
		printf("Bad argument at index %i you can't pass in a scheme closure.\n",i);
		//ascii_print_color(0,9,10);
		return _sc->F;
	    }
	    else {
		//ascii_print_color(1,1,10); // error color
		printf("Bad argument at index %i\n",i);
		//ascii_print_color(0,9,10); // default
		return _sc->F;
	    }
		    
	}
  GenericValue gv = EE->runFunction(func,fargs);

	//std::cout << "GV: " << gv.DoubleVal << " " << gv.FloatVal << " " << gv.IntVal.getZExtValue() << std::endl;
	switch(func->getReturnType()->getTypeID())
	{
	case Type::FloatTyID:
	    return mk_real(_sc, gv.FloatVal);
	case Type::DoubleTyID:
	    return mk_real(_sc, gv.DoubleVal);
	case Type::IntegerTyID:
	    return mk_integer(_sc, gv.IntVal.getZExtValue()); //  getRawData());
	case Type::PointerTyID:
	    return mk_cptr(_sc, gv.PointerVal);
	case Type::VoidTyID:
	    return _sc->T;
	default:
	    return _sc->F;
	}
    }
	
    // this all here to conver 32bit floats (as a string) into llvms hex float 32 notation :(
    pointer SchemeFFI::llvm_convert_float_constant(scheme* _sc, pointer args)
    {		
	char* floatin = string_value(pair_car(args));
	char floatout[256];
        // if already converted to hex value just return Hex String Unchanged
        if(floatin[1]=='x') return pair_car(args); 
#ifdef _WIN32
	float f = (float) strtod(floatin, (char**) &floatout);
#else        
	float f = strtof(floatin, (char**) &floatout);
#endif
	llvm::APFloat apf(f);
		
	bool ignored;
	bool isDouble = false; // apf.getSemantics() == &llvm::APFloat::IEEEdouble;
	double Val = isDouble ? apf.convertToDouble() :
	apf.convertToFloat();
        // char hexstr[128];
        // apf.convertToHexString(hexstr,0,false,llvm::APFloat::rmTowardZero);
        // std::string StrVal(hexstr);
	std::string StrVal = xtm_ftostr(apf);
		
	// Check to make sure that the stringized number is not some string like
	// "Inf" or NaN, that atof will accept, but the lexer will not.  Check
	// that the string matches the "[-+]?[0-9]" regex.
	//
	if ((StrVal[0] >= '0' && StrVal[0] <= '9') ||
	    ((StrVal[0] == '-' || StrVal[0] == '+') &&
	     (StrVal[1] >= '0' && StrVal[1] <= '9'))) {
	    // Reparse stringized version!
	    if (atof(StrVal.c_str()) == Val) {
	      return mk_string(_sc, StrVal.c_str());
	    }
	}
		
	// Otherwise we could not reparse it to exactly the same value, so we must
	// output the string in hexadecimal format!  Note that loading and storing
	// floating point types changes the bits of NaNs on some hosts, notably
	// x86, so we must not use these types.
	assert(sizeof(double) == sizeof(uint64_t) && "assuming that double is 64 bits!");
	char Buffer[40];
	//APFloat apf = CFP->getValueAPF();
	// Floats are represented in ASCII IR as double, convert.
	//if (!isDouble) apf.convert(llvm::APFloat::IEEEdouble, llvm::APFloat::rmNearestTiesToEven, &ignored);
	apf.convert(llvm::APFloat::IEEEdouble, llvm::APFloat::rmNearestTiesToEven, &ignored);
		
	char tmpstr[256];
	tmpstr[0] = '0';
	tmpstr[1] = 'x';
	tmpstr[2] = 0;
	char* v = llvm::utohex_buffer(uint64_t(apf.bitcastToAPInt().getZExtValue()), Buffer+40);
	strcat(tmpstr, v);
	//std::cout << "STR: " << tmpstr << "  v: " << v <<  std::endl;
	return mk_string(_sc, tmpstr);
    }	


     // this all here to conver 64bit floats (as a string) into llvms hex floating point notation :(
     pointer SchemeFFI::llvm_convert_double_constant(scheme* _sc, pointer args)
     {		
 	char* floatin = string_value(pair_car(args));
 	char floatout[256];
        // if already converted to hex value just return Hex String Unchanged
        if(floatin[1]=='x') return pair_car(args); 
 #ifdef _WIN32
 	double f = strtod(floatin, (char**) &floatout);
 #else
 	double f = strtod(floatin, (char**) &floatout);
 #endif
 	llvm::APFloat apf(f); 
 		
	bool ignored;
	bool isDouble = true; // apf.getSemantics() == &llvm::APFloat::IEEEdouble;
 	double Val = isDouble ? apf.convertToDouble() : apf.convertToFloat();

        // char hexstr[128];
        // apf.convertToHexString(hexstr,0,false,llvm::APFloat::rmTowardZero);
        // std::string StrVal(hexstr);
 	std::string StrVal = xtm_ftostr(apf);
 		
 	// Check to make sure that the stringized number is not some string like
 	// "Inf" or NaN, that atof will accept, but the lexer will not.  Check
 	// that the string matches the "[-+]?[0-9]" regex.
 	//
 	if ((StrVal[0] >= '0' && StrVal[0] <= '9') ||
 	    ((StrVal[0] == '-' || StrVal[0] == '+') &&
 	     (StrVal[1] >= '0' && StrVal[1] <= '9'))) {
 	    // Reparse stringized version!
 	    if (atof(StrVal.c_str()) == Val) {
 		return mk_string(_sc, StrVal.c_str());
 	    }
 	}
 		
 	// Otherwise we could not reparse it to exactly the same value, so we must
 	// output the string in hexadecimal format!  Note that loading and storing
 	// floating point types changes the bits of NaNs on some hosts, notably
 	// x86, so we must not use these types.
 	assert(sizeof(double) == sizeof(uint64_t) && "assuming that double is 64 bits!");
 	char Buffer[40];
 	//APFloat apf = CFP->getValueAPF();
 	// Floats are represented in ASCII IR as double, convert.
 	//if (!isDouble) apf.convert(llvm::APFloat::IEEEdouble, llvm::APFloat::rmNearestTiesToEven, &ignored);
 	//apf.convert(llvm::APFloat::IEEEdouble, llvm::APFloat::rmNearestTiesToEven, &ignored);
 		
 	char tmpstr[256];
 	tmpstr[0] = '0';
 	tmpstr[1] = 'x';
 	tmpstr[2] = 0;
 	char* v = llvm::utohex_buffer(uint64_t(apf.bitcastToAPInt().getZExtValue()), Buffer+40);
 	strcat(tmpstr, v);
 	//std::cout << "STR: " << tmpstr << "  v: " << v <<  std::endl;
 	return mk_string(_sc, tmpstr);
     }	
 

    pointer SchemeFFI::llvm_count_set(scheme* _sc, pointer args)
    {
        EXTLLVM::LLVM_COUNT = ivalue(pair_car(args));
	return mk_integer(_sc, EXTLLVM::LLVM_COUNT);		
    }


    pointer SchemeFFI::llvm_count_inc(scheme* _sc, pointer args)
    {
	EXTLLVM::LLVM_COUNT++;
	return mk_integer(_sc, EXTLLVM::LLVM_COUNT);		
    }

    pointer SchemeFFI::llvm_count(scheme* _sc, pointer args)
    {
	return mk_integer(_sc, EXTLLVM::LLVM_COUNT);
    }
	
    pointer SchemeFFI::printLLVMModule(scheme* _sc, pointer args)
    {
	llvm::Module* M = EXTLLVM::I()->M;
	std::string str;
	llvm::raw_string_ostream ss(str);
		
	if(list_length(_sc, args) > 0) {
    llvm::GlobalValue* val = extemp::EXTLLVM::I()->getGlobalValue(std::string(string_value(pair_car(args))));    
    //llvm::GlobalValue* val = M->getNamedValue(std::string(string_value(pair_car(args))));
	    if(val == NULL) {
		std::cerr << "No such value found in LLVM Module" << std::endl;
		return _sc->F;
	    }
	    ss << *val;
	    printf("At address: %p\n%s\n",val,str.c_str());
	} else {
	    ss << *M;
	}
		
	printf("%s",str.c_str());
	return _sc->T;
    }

pointer SchemeFFI::printLLVMFunction(scheme* _sc, pointer args)
{
  //llvm::Module* M = EXTLLVM::I()->M;
  //llvm::Function* func = M->getFunction(std::string(string_value(pair_car(args))));
  llvm::Function* func = extemp::EXTLLVM::I()->getFunction(std::string(string_value(pair_car(args))));      
	std::string str;
	llvm::raw_string_ostream ss(str);
	ss << *func;
	printf("%s",str.c_str());
	return _sc->T;		
}

    pointer SchemeFFI::symbol_pointer(scheme* _sc, pointer args)
    {
	void* library = cptr_value(pair_car(args));
	char* symname = string_value(pair_cadr(args));
		
#ifdef _WIN32
        void* ptr = (void*) GetProcAddress((HMODULE)library, symname);
#else
	void* ptr = dlsym(library, symname);
#endif
	if(!ptr) {
	    return _sc->F;
	}
	return mk_cptr(_sc,ptr);
    }
	
  pointer SchemeFFI::bind_symbol(scheme* _sc, pointer args)
  {
    void* library = cptr_value(pair_car(args));
    char* symname = string_value(pair_cadr(args));

    llvm::Module* M = EXTLLVM::I()->M;
    llvm::ExecutionEngine* EE = EXTLLVM::I()->EE;

    llvm::MutexGuard locked(EE->lock);
		
#ifdef _WIN32
    void* ptr = (void*) GetProcAddress((HMODULE)library, symname);
#else
    void* ptr = dlsym(library, symname);
#endif
    if(ptr) {
      EE->updateGlobalMapping(symname, (uint64_t)ptr);
      return _sc->T;
    }else{
      // printf("Could not find symbol named %s\n",symname);
      return _sc->F;
    }
  }

  pointer SchemeFFI::update_mapping(scheme* _sc, pointer args)
  {
    char* symname = string_value(pair_car(args));
    void* ptr = cptr_value(pair_cadr(args));

    llvm::Module* M = EXTLLVM::I()->M;
    llvm::ExecutionEngine* EE = EXTLLVM::I()->EE;

    llvm::MutexGuard locked(EE->lock);

    // returns previous value of the mapping, or NULL if not set
    uint64_t oldval = EE->updateGlobalMapping(symname, (uint64_t)ptr);
    return mk_cptr(_sc, (void*)oldval);
  }

    // For simple preprocessor alias's
    pointer SchemeFFI::add_llvm_alias(scheme* _sc, pointer args)
    {
	char* name = string_value(pair_car(args));
	char* type = string_value(pair_cadr(args));
        LLVM_ALIAS_TABLE[std::string(name)] = std::string(type);
	return _sc->T;
    }

    pointer SchemeFFI::get_llvm_alias(scheme* _sc, pointer args)
    {
	char* name = string_value(pair_car(args));
	if (LLVM_ALIAS_TABLE.find(std::string(name)) != LLVM_ALIAS_TABLE.end())
	  return mk_string(_sc,LLVM_ALIAS_TABLE[std::string(name)].c_str());
	else
	  return _sc->F; 
    }


pointer SchemeFFI::get_named_type(scheme* _sc, pointer args)
{
	char* n = string_value(pair_car(args));
	char nk[256];
	char* name = nk;
	strcpy(name,n);        
	if (name[0] == '%') name = name+1;	

	int ptrdepth = 0;
	while(name[strlen(name)-1] == '*') {
	  name[strlen(name)-1]='\0';
    ptrdepth++;
	}

	//llvm::Module* M = EXTLLVM::I()->M;	
	//const llvm::Type* tt = M->getTypeByName(name);
  const llvm::Type* tt = extemp::EXTLLVM::I()->getNamedType(name);  

	if(tt) {
	  //return mk_string(_sc,M->getTypeName(tt).c_str());
	  std::string typestr;
	  llvm::raw_string_ostream ss(typestr);
	  tt->print(ss);
	  

	  const char* tmp_name = ss.str().c_str();
	  if(tt->isStructTy()) {
      const char* eq_type_string = " = type ";
	    rsplit((char*)eq_type_string,(char*)tmp_name,tmp_str_a,tmp_str_b);
	    tmp_name = tmp_str_b;
	  }
	  
	  //add back any requried '*'s
	  if(ptrdepth>0) {
	    char tmpstr[256];
	    memset(tmpstr,0,256);
      strcpy(tmpstr,tmp_name);
	    for( ;ptrdepth>0;ptrdepth--) {
	      tmpstr[strlen(tmpstr)]='*';
	    }
	    tmp_name = tmpstr;
	  }
	  return mk_string(_sc,tmp_name);
	} else {
	  return _sc->NIL; 
	}
}
	
    ////////////////////////////////////////////////////////////
    //
    //  DSP BITS
    //
    ////////////////////////////////////////////////////////////
	
    pointer SchemeFFI::setDSPClosure(scheme* _sc, pointer args)
    {
	AudioDevice::I()->setDSPClosure(cptr_value(pair_car(args)));
	return _sc->T;
    }

    pointer SchemeFFI::setDSPMTClosure(scheme* _sc, pointer args)
    {
      AudioDevice::I()->setDSPMTClosure(cptr_value(pair_car(args)),ivalue(pair_cadr(args)));
	return _sc->T;
    }
	
    pointer SchemeFFI::setDSPWrapper(scheme* _sc, pointer args)
    {
	AudioDevice::I()->setDSPWrapper((dsp_f_ptr)cptr_value(pair_car(args)));
	return _sc->T;
    }
	
    pointer SchemeFFI::setDSPWrapperArray(scheme* _sc, pointer args)
    {
	AudioDevice::I()->setDSPWrapperArray((dsp_f_ptr_array)cptr_value(pair_car(args)));
	return _sc->T;
    }

    pointer SchemeFFI::setDSPMTWrapper(scheme* _sc, pointer args)
    {
      AudioDevice::I()->setDSPMTWrapper((dsp_f_ptr_sum)cptr_value(pair_car(args)),
                                         (dsp_f_ptr)cptr_value(pair_cadr(args)));
      return _sc->T;
    }

    pointer SchemeFFI::setDSPMTWrapperArray(scheme* _sc, pointer args)
    {
      AudioDevice::I()->setDSPMTWrapperArray((dsp_f_ptr_sum_array)cptr_value(pair_car(args)),
                                             (dsp_f_ptr_array)cptr_value(pair_cadr(args)));
	return _sc->T;
    }

    pointer SchemeFFI::initMTAudio(scheme* _sc, pointer args)
    {
      pointer val = pair_cadr(args);
      bool zerolatency = (val == _sc->T) ? true : false;
      AudioDevice::I()->initMTAudio(ivalue(pair_car(args)),zerolatency);
      return _sc->T;
    }

    pointer SchemeFFI::initMTAudioBuf(scheme* _sc, pointer args)
    {
      pointer val = pair_cadr(args);
      bool zerolatency = (val == _sc->T) ? true : false;
      AudioDevice::I()->initMTAudioBuf(ivalue(pair_car(args)),zerolatency);
      return _sc->T;
    }

    pointer SchemeFFI::getAudioLoad(scheme* _sc, pointer args)
    {
      double load = AudioDevice::getCPULoad();
      return mk_real(_sc,load);
    }

  pointer SchemeFFI::getClockTime(scheme* _sc, pointer args)
  {
    return mk_real(_sc, getRealTime()+UNIV::CLOCK_OFFSET);
  }

  pointer SchemeFFI::adjustClockOffset(scheme* _sc, pointer args)
  {
    UNIV::CLOCK_OFFSET = rvalue(pair_car(args)) + UNIV::CLOCK_OFFSET;
    return mk_real(_sc,UNIV::CLOCK_OFFSET);
  }

  pointer SchemeFFI::setClockOffset(scheme* _sc, pointer args)
  {
    UNIV::CLOCK_OFFSET = rvalue(pair_car(args));
    return pair_car(args);
  }

  pointer SchemeFFI::getClockOffset(scheme* _sc, pointer args)
  {
    return mk_real(_sc, UNIV::CLOCK_OFFSET);
  }

  pointer SchemeFFI::lastSampleBlockClock(scheme* _sc, pointer args)
  {
    pointer p1 = mk_integer(_sc,UNIV::TIME);
    _sc->imp_env->insert(p1);
    pointer p2 = mk_real(_sc,AudioDevice::REALTIME + UNIV::CLOCK_OFFSET);
    _sc->imp_env->insert(p2);
    pointer p3 = cons(_sc, p1, p2);
    _sc->imp_env->erase(p1);
    _sc->imp_env->erase(p2);
    return p3;
  }

} // end namespace

