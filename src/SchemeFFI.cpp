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
 * ARE DISCLEXTD. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE 
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
 * POSSIBILITY OF SUCH DAMAGE.
 *
 */


#include "SchemeFFI.h"
#include "AudioDevice.h"
#include "UNIV.h"
#include "TaskScheduler.h"
#include "SchemeProcess.h"
#include "SchemeREPL.h"
//#include "sys/mman.h"

#ifdef TARGET_OS_WINDOWS
#include <Windows.h>
#include <Windowsx.h>
#else
#include <dlfcn.h>
#endif

#ifdef EXT_BOOST
#include <boost/filesystem.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#endif

#include <sstream>
#include <string.h>


#ifdef TARGET_OS_WINDOWS
#else
#include <dirent.h>
#endif


// setting this define should make call_compiled_closure
// and call_compiled thread safe 
// BUT ... also extremely SLOW !

#define LLVM_EE_LOCK

////////////////////////////////


#define PCRE_REGEX

#ifdef PCRE_REGEX
#include <pcre.h>
#else
#include <oniguruma.h>
#endif

#ifdef TARGET_OS_MAC
#include <malloc/malloc.h>
#else
#include <time.h>
#endif

/////////////////////// llvm includes
#include "llvm/Assembly/Parser.h"
#include "llvm/LLVMContext.h"
#include "llvm/CallingConv.h"
#include "llvm/Module.h"
#include "llvm/Constants.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Instructions.h"
//#include "llvm/ModuleProvider.h"
#include "llvm/ExecutionEngine/JIT.h"
#include "llvm/ExecutionEngine/Interpreter.h"
#include "llvm/ExecutionEngine/GenericValue.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/Target/TargetData.h"
#include "llvm/LinkAllPasses.h"
#include "llvm/PassManager.h"
#include "llvm/ADT/StringExtras.h"
///////////////////////////////////////

#ifdef TARGET_OS_WINDOWS
//#include <unistd.h>
#include <malloc.h>
#include <gl/GL.h>
#elif TARGET_OS_LINUX
#include <GL/glx.h>
#include <GL/gl.h>
#elif TARGET_OS_MAC
#include <Cocoa/Cococa.h>
#include <CoreFoundation/CoreFoundation.h>
#include <AppKit/AppKit.h>
#include <OpenGL/OpenGL.h>
#include <OpenGL/gl.h>
#endif

#ifdef TARGET_OS_WINDOWS
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


#define nelem(table) sizeof(table) / sizeof(table[0])

namespace extemp {
	
    SchemeFFI SchemeFFI::SINGLETON;
    double SchemeFFI::CLOCK_OFFSET = 0.0;
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
	    { "ascii-print-color",		&SchemeFFI::asciiColor },
	    { "emit",                       &SchemeFFI::emit },
            { "quit",                       &SchemeFFI::exit_extempore },

	    //IPC stuff
	    { "ipc:new",			&SchemeFFI::newSchemeProcess },
	    { "ipc:connect",		&SchemeFFI::connectToProcess },
	    { "ipc:call-async",		&SchemeFFI::ipcCall },
	    { "ipc:define",			&SchemeFFI::ipcDefine },
	    { "ipc:eval-string",		&SchemeFFI::ipcEval },
	    { "ipc:load",			&SchemeFFI::ipcLoad },
	    { "ipc:set-priority",       &SchemeFFI::ipcSetPriority },
	    { "ipc:get-priority",       &SchemeFFI::ipcGetPriority },
	    { "ipc:get-process-name",	&SchemeFFI::getNameOfCurrentProcess },

	    // misc scheme ties
	    { "assoc-strcmp",            &SchemeFFI::assocstrcmp },
	    
	    // number stuff
	    { "random-real",		&SchemeFFI::randomReal },
	    { "random-int",			&SchemeFFI::randomInt },
	    { "real->integer",		&SchemeFFI::realToInteger },
	    { "real->rational",		&SchemeFFI::realToRational },
	    { "rational->real",		&SchemeFFI::rationalToReal },
	    { "integer->real",		&SchemeFFI::integerToReal },

	    // sys stuff
	    { "sys:pointer-size",		&SchemeFFI::pointerSize },
	    { "sys:platform",		&SchemeFFI::platform },
	    { "sys:cmdarg",		&SchemeFFI::cmdarg },
	    { "sys:open-dylib",		&SchemeFFI::openDynamicLib },
	    { "sys:close-dylib",		&SchemeFFI::closeDynamicLib },
	    { "sys:make-cptr",		&SchemeFFI::makeCptr },
	    { "sys:directory-list",     &SchemeFFI::dirlist },

	    // DSP sys stuff
	    { "sys:set-dsp-closure",	&SchemeFFI::setDSPClosure },
	    { "sys:set-dsp-wrapper",	&SchemeFFI::setDSPWrapper },
	    { "sys:set-dsp-wrapper-array",	&SchemeFFI::setDSPWrapperArray },

	    // memory zone stuff
    	    { "sys:create-mzone",		&SchemeFFI::createMallocZone },
	    { "sys:default-mzone",		&SchemeFFI::defaultMallocZone },
	    { "sys:destroy-mzone",		&SchemeFFI::destroyMallocZone },
	    { "sys:copy-to-dmzone",		&SchemeFFI::copyToDefaultZone },
	    { "sys:reset-mzone",		&SchemeFFI::resetMallocZone },
	    { "sys:peek-memzone",               &SchemeFFI::peekMemoryZone },
	    { "sys:pop-memzone",               &SchemeFFI::popMemoryZone },
	    { "sys:push-memzone",               &SchemeFFI::pushMemoryZone },


	    // misc stuff
	    { "cptr:get-i64",            &SchemeFFI::dataGETi64 },
	    { "cptr:get-double",            &SchemeFFI::dataGETdouble },
	    { "cptr:set-i64",            &SchemeFFI::dataSETi64 },
	    { "cptr:set-double",            &SchemeFFI::dataSETdouble },
	    { "cptr->string",            &SchemeFFI::cptrToString },
	    { "cptr:get-string",            &SchemeFFI::cptrToString },
	    { "string-strip",		&SchemeFFI::stringStrip },
	    { "string-hash",		&SchemeFFI::stringHash },
	    { "string-join",		&SchemeFFI::stringJoin },
	    { "call-cpp-at-time",		&SchemeFFI::callCPPAtTime },
	    { "now",			&SchemeFFI::getTime },
	    { "sexpr->string",		&SchemeFFI::sexprToString },
	    { "println",			&SchemeFFI::print },
	    { "print",			&SchemeFFI::print_no_new_line },
	    { "print-full",			&SchemeFFI::printFull },
	    { "print-full-nq",		&SchemeFFI::printFullNoQuotes },
	    { "pprint-error",		&SchemeFFI::printError }, // pprint-error is pprint for a reason!
	    { "print-notification",		&SchemeFFI::printNotification },
	    { "get-closure-env",		&SchemeFFI::getClosureEnv },
            { "mk-ff",               &SchemeFFI::scmAddForeignFunc },

	    // regex stuff
	    { "regex:match?",		&SchemeFFI::regex_match },
	    { "regex:matched",		&SchemeFFI::regex_matched },
	    { "regex:match-all",		&SchemeFFI::regex_match_all },
	    { "regex:split",		&SchemeFFI::regex_split },
	    { "regex:replace",		&SchemeFFI::regex_replace },

	    // llvm stuff
	    { "llvm:optimize",			&SchemeFFI::optimizeCompiles },
	    { "llvm:compile",			&SchemeFFI::compile },
	    { "llvm:bind-global-var",		&SchemeFFI::bind_global_var },
	    { "llvm:get-function",			&SchemeFFI::get_function },
	    { "llvm:get-globalvar",			&SchemeFFI::get_globalvar },
	    { "llvm:get-function-args",		&SchemeFFI::get_function_args },
	    { "llvm:get-function-type",		&SchemeFFI::get_function_type },
	    { "llvm:get-function-calling-conv",	&SchemeFFI::get_function_calling_conv },
	    { "llvm:get-global-variable-type",	&SchemeFFI::get_global_variable_type },
	    { "llvm:get-function-pointer",		&SchemeFFI::get_function_pointer },
	    { "llvm:jit-compile-function",		&SchemeFFI::recompile_and_link_function },
	    { "llvm:remove-function",		&SchemeFFI::remove_function },
	    { "llvm:remove-globalvar",		&SchemeFFI::remove_global_var },
	    { "llvm:erase-function",		&SchemeFFI::erase_function },
	    { "llvm:call-void-func",          &SchemeFFI::llvm_call_void_native },
	    { "llvm:run",				&SchemeFFI::call_compiled },
	    { "llvm:run-closure",			&SchemeFFI::call_compiled_closure },
	    { "llvm:convert-float",			&SchemeFFI::llvm_convert_float_constant },
 	    { "llvm:convert-double",			&SchemeFFI::llvm_convert_double_constant },
	    { "llvm:count",				&SchemeFFI::llvm_count },
	    { "llvm:count-set",				&SchemeFFI::llvm_count_set },
	    { "llvm:count++",			&SchemeFFI::llvm_count_inc },
	    { "llvm:call-closure",			&SchemeFFI::callClosure },
	    { "llvm:print",				&SchemeFFI::printLLVMModule },
	    { "llvm:print-function",		&SchemeFFI::printLLVMFunction },
	    { "llvm:bind-symbol",			&SchemeFFI::bind_symbol },
	    { "llvm:add-llvm-alias",                          &SchemeFFI::add_llvm_alias },
	    { "llvm:get-llvm-alias",                          &SchemeFFI::get_llvm_alias },
	    { "llvm:get-named-type",                          &SchemeFFI::get_named_type },
	    { "impc:ir:getname",			&SchemeFFI::impcirGetName },
	    { "impc:ir:gettype",			&SchemeFFI::impcirGetType },		
	    { "impc:ir:addtodict",			&SchemeFFI::impcirAdd },
#if defined (TARGET_OS_LINUX)
	    { "gl:get-event",			&SchemeFFI::getEvent },
#endif
#if defined (TARGET_OS_WINDOWS)
	    { "gl:get-event",			&SchemeFFI::getEvent },
	    { "gl:add-extension",           &SchemeFFI::addGLExtension },
#endif
	    { "gl:make-ctx",			    &SchemeFFI::makeGLContext },	    
	    { "gl:set-context",             &SchemeFFI::glMakeContextCurrent },
	    { "gl:swap-buffers",			&SchemeFFI::glSwapBuffers },

	    //#ifdef EXT_BOOST
	    //#else
	    //CLOCK STUFF
	    { "clock:set-offset",                           &SchemeFFI::setClockOffset},
	    { "clock:get-offset",                           &SchemeFFI::getClockOffset},
            { "clock:adjust-offset",                        &SchemeFFI::adjustClockOffset},
	    { "clock:clock",                                &SchemeFFI::getClockTime},
            { "clock:ad:clock",                             &SchemeFFI::lastSampleBlockClock},    
	    { "ad:clock:set-offset",                        &SchemeFFI::ad_setClockOffset},
            { "ad:clock:get-offset",                       &SchemeFFI::ad_getClockOffset},
	    { "ad:clock:adjust-offset",                    &SchemeFFI::ad_adjustClockOffset},
	    { "ad:clock:clock",                            &SchemeFFI::ad_getClockTime},
            { "ad:clock",                                  &SchemeFFI::ad_getClockTime},
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

    pointer SchemeFFI::scmAddForeignFunc(scheme* sc, pointer args) {
      //char* sym_name = string_value(pair_car(args));
        foreign_func func = (foreign_func) cptr_value(pair_car(args));       
        //scheme_define(sc, sc->global_env, mk_symbol(sc, symbol_name), mk_foreign_func(sc, func));
        return mk_foreign_func(sc,func); //sc->T;
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
        // This is a seriously nasty HACK!!
        // PLEASE FIX ME!
        exit(1);
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

    pointer SchemeFFI::cptrToString(scheme* _sc, pointer args)
    {       
        char* cptr = (char*) cptr_value(pair_car(args));
	char* cstr = (char*) cptr;
	return mk_string(_sc, cstr);
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
        void* ptr = malloc(ivalue(pair_car(args)));
	    return mk_cptr(_sc, ptr);
    }

#ifdef EXT_BOOST
	pointer SchemeFFI::dirlist(scheme* _sc, pointer args)
	{
	  char* path = string_value(pair_car(args));
	  boost::filesystem::path bpath(path);
	  if(!boost::filesystem::exists(bpath)) {
		  return _sc->NIL;
	  }
	  if(!boost::filesystem::is_directory(bpath)) {
		  return _sc->NIL;
	  }

	  boost::filesystem::directory_iterator end_it;

	  pointer list = _sc->NIL;
	  for(boost::filesystem::directory_iterator it(bpath); it != end_it; ++it) {
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
	while (ep = readdir (dp)) {
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
	SchemeProcess* sp = new SchemeProcess(std::string(UNIV::PWD),proc_name, port, 0);
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
	        PRINT_ERROR("IPC does not support type.\nThis maybe related to the return type as well as the arguments if calling from ipc:call.\nIn particular remember that objc objects cannot be passed natively (you can turn them into strings though by calling objc:string-encode and then on the other end use objc:string-decode to reconstitute an object\n");
		return _sc->F;
	    }
	}
	std::string str = "("+std::string(symname(sym))+ss.str()+")";
	SchemeREPL::I(process)->writeString(str);
	return _sc->T;
    }
    
    pointer SchemeFFI::ipcDefine(scheme* _sc, pointer args)
    {
	std::string process(string_value(pair_car(args)));
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
	    PRINT_ERROR("IPC does not support type.\nThis maybe related to the return type as well as the arguments if calling from ipc:call.\nIn particular remember that objc objects cannot be passed natively (you can turn them into strings though by calling objc:string-encode and then on the other end use objc:string-decode to reconstitute an object\n");
	    return _sc->F;
	}
	std::string str = "(define "+std::string(symname(sym))+ss.str()+")";
	SchemeREPL::I(process)->writeString(str);
	return _sc->T;
    }

    pointer SchemeFFI::ipcEval(scheme* _sc, pointer args)
    {
	std::string process(string_value(pair_car(args)));
	std::string expr(string_value(pair_cadr(args)));
	SchemeREPL::I(process)->writeString(expr);
	return _sc->T;
    }
    
    pointer SchemeFFI::ipcLoad(scheme* _sc, pointer args)
    {
	std::string process(string_value(pair_car(args)));
	std::string path(string_value(pair_cadr(args)));
	std::string str = "(load \""+std::string(path)+"\")";
	SchemeREPL::I(process)->writeString(str);
	return _sc->T;
    }

    pointer SchemeFFI::ipcSetPriority(scheme* _sc, pointer args)
    {
	std::string process(string_value(pair_car(args)));
	int priority = ivalue(pair_cadr(args));
	SchemeProcess::I(process)->setPriority(priority);
	return _sc->T;
    }

    pointer SchemeFFI::ipcGetPriority(scheme* _sc, pointer args)
    {
	std::string process(string_value(pair_car(args)));
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
	
    pointer SchemeFFI::realToInteger(scheme* _sc, pointer args)
    {
	long long int val = (long long int) rvalue(pair_car(args));
	return mk_integer(_sc,val);
    }
	
    pointer SchemeFFI::openDynamicLib(scheme* _sc, pointer args)
    {
	//void* lib_handle = dlopen(string_value(pair_car(args)), RTLD_GLOBAL); //LAZY);
#ifdef TARGET_OS_WINDOWS
	  char* ccc = string_value(pair_car(args));
	  size_t ccc_size = (strlen(ccc)+1) * 2; //2 for 16 bytes (wide_char)
	  size_t converted_chars = 0;
	  wchar_t* wstr = (wchar_t*) _alloca(ccc_size);
	  mbstowcs_s(&converted_chars,wstr,ccc_size, ccc, _TRUNCATE);
      void* lib_handle = LoadLibrary(wstr);
#else
	void* lib_handle = dlopen(string_value(pair_car(args)), RTLD_LAZY);
#endif
	if (!lib_handle)
	{
#ifdef TARGET_OS_WINDOWS
	  std::cout << "Error loading library" << GetLastError() << std::endl;
	  printf("For Library Path: %ls\n",wstr);
	  return _sc->F;
#else                       
	  fprintf(stderr, "%s\n", dlerror());
#endif
	    return _sc->F;
	}
	return mk_cptr(_sc,lib_handle);
    }

    pointer SchemeFFI::closeDynamicLib(scheme* _sc, pointer args)
    {
#ifdef TARGET_OS_WINDOWS
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

    pointer SchemeFFI::cmdarg(scheme* _sc, pointer args)
    {
      char* key = string_value(pair_car(args));
      std::string val = UNIV::CMDPARAMS[std::string(key)];
      return mk_string(_sc,val.c_str());
    }

    pointer SchemeFFI::platform(scheme* _sc, pointer args)
    {
#ifdef TARGET_OS_MAC
      return mk_string(_sc, "OSX");
#elif TARGET_OS_LINUX
      return mk_string(_sc, "Linux");
#elif TARGET_OS_WINDOWS
	  return mk_string(_sc, "Windows");
#else
	  return mk_string(_sc, "");
#endif
    }

    pointer SchemeFFI::stringHash(scheme* _sc, pointer args)
    {
      char* str = string_value(pair_car(args));
      unsigned long hash = 0;
      int c;
  
      while (c = *str++)
	hash = c + (hash << 6) + (hash << 16) - hash;
  
      return mk_integer(_sc,hash);
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
	
#ifdef PCRE_REGEX
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
	
	
#else // ONIGURUMA NOT COMPLETE!!
    pointer SchemeFFI::regex_match(scheme* _sc, pointer args)	
    {
	int r;
	unsigned char *start, *range, *end;
	regex_t* reg;
	OnigErrorInfo einfo;
	OnigRegion *region;
	
	UChar* pattern = (UChar*) string_value(pair_cadr(args)); 
	UChar* str     = (UChar*) string_value(pair_car(args));
	
	r = onig_new(&reg, pattern, pattern + strlen((char* )pattern), ONIG_OPTION_DEFAULT, ONIG_ENCODING_ASCII, ONIG_SYNTAX_DEFAULT, &einfo);
	if (r != ONIG_NORMAL) {
	    UChar s[ONIG_MAX_ERROR_MESSAGE_LEN];
	    onig_error_code_to_str(s, r, &einfo);
	    fprintf(stderr, "ERROR: %s\n", s);
	    return _sc->F;
	}
	
	region = onig_region_new();
	
	bool matched = false;
	end   = str + strlen((char* )str);
	start = str;
	range = end;
	r = onig_search(reg, str, end, start, range, region, ONIG_OPTION_NONE);
	if (r >= 0) {
	    int i;
	    matched = true;
	    fprintf(stderr, "match at %d\n", r);
	    for (i = 0; i < region->num_regs; i++) {
		fprintf(stderr, "%d: (%d-%d)\n", i, region->beg[i], region->end[i]);
	    }
	}
	else if (r == ONIG_MISMATCH) {
	    fprintf(stderr, "search fail\n");
	}
	else { /* error */
	    UChar s[ONIG_MAX_ERROR_MESSAGE_LEN];
	    onig_error_code_to_str(s, r);
	    fprintf(stderr, "ERROR: %s\n", s);
	    return _sc->F;
	}
	
	onig_region_free(region, 1 /* 1:free self, 0:free contents only */);
	onig_free(reg);
	onig_end();
	return (matched) ? _sc->T : _sc->F;		
    }
	
    pointer SchemeFFI::regex_match_all(scheme* _sc, pointer args)	
    {
	int r;
	unsigned char *start, *range, *end;
	regex_t* reg;
	OnigErrorInfo einfo;
	OnigRegion *region;
	
	UChar* pattern = (UChar*) string_value(pair_cadr(args)); 
	UChar* str     = (UChar*) string_value(pair_car(args));
	
	r = onig_new(&reg, pattern, pattern + strlen((char* )pattern), ONIG_OPTION_DEFAULT, ONIG_ENCODING_ASCII, ONIG_SYNTAX_DEFAULT, &einfo);
	if (r != ONIG_NORMAL) {
	    UChar s[ONIG_MAX_ERROR_MESSAGE_LEN];
	    onig_error_code_to_str(s, r, &einfo);
	    fprintf(stderr, "ERROR: %s\n", s);
	    return _sc->F;
	}
	
	region = onig_region_new();
	pointer list = _sc->NIL;
	  
	while(true) {
	    bool matched = false;
	    end   = str + strlen((char* )str);
	    start = str;
	    range = end;
	    r = onig_search(reg, str, end, start, range, region, ONIG_OPTION_NONE);
	    if (r >= 0) {
		int i;
		matched = true;
		fprintf(stderr, "match at %d\n", r);
		for (i = 0; i < region->num_regs; i++) {
		    if(region->beg[i] == -1) continue;
		    int range = region->end[i] - region->beg[i];
		    char* substr = (char*) alloca(range+1);
		    memset(substr,0,range+1);
		    strncpy(substr, ((char*)str)+region->beg[i], range);
		    _sc->imp_env->insert(list); 			
		    pointer tlist = cons(_sc,mk_string(_sc,substr),list);
		    _sc->imp_env->erase(list);
		    list = tlist;
		    //set_vector_elem(_sc,v,vpos++,mk_string(_sc,substr));
		    fprintf(stderr, "%d: (%d-%d)\n", i, region->beg[i], region->end[i]);
		    str = str+region->end[i];		
		}
	    }
	    else if (r == ONIG_MISMATCH) {
		break;
	    }
	    else { /* error */
		UChar s[ONIG_MAX_ERROR_MESSAGE_LEN];
		onig_error_code_to_str(s, r);
		fprintf(stderr, "ERROR: %s\n", s);
		return _sc->F;
	    }
	}
	
	onig_region_free(region, 1 /* 1:free self, 0:free contents only */);
	onig_free(reg);
	onig_end();
	return reverse(_sc, list);
    }	
#endif

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
	
    pointer SchemeFFI::compile(scheme* _sc, pointer args)
    {
	// Create some module to put our function into it.
	using namespace llvm;
	Module* M = EXTLLVM::I()->M;
	PassManager* PM = extemp::EXTLLVM::I()->PM;

	char* assm = string_value(pair_car(args));
	SMDiagnostic pa;
	//ParseError pa;
	long long num_of_funcs = M->getFunctionList().size();		
	const Module* newM = ParseAssemblyString(assm, M, pa, getGlobalContext());
	if(EXTLLVM::OPTIMIZE_COMPILES)
	{
	    PM->run(*M);
	}

	//std::stringstream ss;
	if(newM == 0)
	{
	    std::string errstr;
	    llvm::raw_string_ostream ss(errstr);
	    pa.Print("Impromptu",ss);
	    printf("%s\n",ss.str().c_str());
	    // if the number of functions in module has changed when calling runFunction 
	    // then we assume a stub was made and appended to the end of the modules function list.
	    // we remove this function now that we no longer need it!
	    if(num_of_funcs != M->getFunctionList().size()) {
		iplist<Function>::iterator iter = M->getFunctionList().end();
		Function* func = dyn_cast<Function>(--iter);
		//std::cout << "REMOVING ON FAIL: " << *func << std::endl;
		func->dropAllReferences();
		func->removeFromParent();
	    }			
	    return _sc->F;
	}else{
	    std::string Err;
	    if (verifyModule(*M, ReturnStatusAction, &Err)) {
		printf("%s\n%s","Parsed, but not valid!\n",Err.c_str());
		if(num_of_funcs != M->getFunctionList().size()) {
		    iplist<Function>::iterator iter = M->getFunctionList().end();
		    Function* func = dyn_cast<Function>(--iter);
		    //std::cout << "REMOVING ON FAIL: " << *func << std::endl;
		    func->dropAllReferences();
		    func->removeFromParent();
		}							
		return _sc->F;
	    } 
	    return _sc->T;
	}
    }
	
    pointer SchemeFFI::bind_global_var(scheme* _sc, pointer args)
    {
	using namespace llvm;

	Module* M = EXTLLVM::I()->M;		
	llvm::GlobalValue* gv = M->getNamedValue(std::string(string_value(pair_car(args))));
	if(gv == 0) {
	    printf("Attempting to bind to a non-existant global LLVM variable\n");
	    return _sc->F;
	}
	void* ptr = cptr_value(pair_cadr(args));
	void** ptrptr = (void**) malloc(sizeof(void*));
	ptrptr[0] = ptr;
        EXTLLVM::I()->EE->lock.acquire();
	EXTLLVM::I()->EE->updateGlobalMapping(gv,ptrptr);
	EXTLLVM::I()->EE->lock.release();
	return _sc->T;
    }
	
    pointer SchemeFFI::get_function(scheme* _sc, pointer args)
    {
	using namespace llvm;
	Module* M = EXTLLVM::I()->M;
	llvm::Function* func = M->getFunction(std::string(string_value(pair_car(args))));
	if(func == 0)
	{
	    return _sc->F;
	}
	return mk_cptr(_sc, func); 				
    }

    pointer SchemeFFI::get_globalvar(scheme* _sc, pointer args)
    {
	using namespace llvm;

	Module* M = EXTLLVM::I()->M;
	llvm::GlobalVariable* var = M->getGlobalVariable(std::string(string_value(pair_car(args)))); 
	if(var == 0)
	{
	    return _sc->F;
	}				
	return mk_cptr(_sc, var); 				
    }


    pointer SchemeFFI::get_function_calling_conv(scheme* _sc, pointer args)
    {
	using namespace llvm;

	Module* M = EXTLLVM::I()->M;
	llvm::Function* func = M->getFunction(std::string(string_value(pair_car(args))));		
	if(func == 0)
	{
	    return _sc->F;
	}			

	int cc = func->getCallingConv();
	return mk_integer(_sc, cc);
    }

    pointer SchemeFFI::get_function_type(scheme* _sc, pointer args)
    {
	using namespace llvm;

	Module* M = EXTLLVM::I()->M;
	llvm::Function* func = M->getFunction(std::string(string_value(pair_car(args))));
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


    pointer SchemeFFI::get_function_args(scheme* _sc, pointer args)
    {
	using namespace llvm;

	Module* M = EXTLLVM::I()->M;
	llvm::Function* func = M->getFunction(std::string(string_value(pair_car(args))));
	if(func == 0)
	{
	    return _sc->F;
	}			

        std::string typestr;
	llvm::raw_string_ostream ss(typestr);
        func->getReturnType()->print(ss);

	const char* tmp_name = ss.str().c_str();

	if(func->getReturnType()->isStructTy()) {	      
	  rsplit(" = type ",(char*)tmp_name,tmp_str_a,tmp_str_b);
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
	      rsplit(" = type ",(char*)tmp_name,tmp_str_a,tmp_str_b);
	      //printf("tmp:%s  a:%s  b:%s\n",(char*)tmp_name,tmp_str_a,tmp_str_b);
	      //tmp_name = tmp_str_b;
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

	Module* M = EXTLLVM::I()->M;		
	llvm::GlobalVariable* var = M->getGlobalVariable(std::string(string_value(pair_car(args)))); 
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

	Module* M = EXTLLVM::I()->M;		
	llvm::Function* func = M->getFunction(std::string(string_value(pair_car(args))));
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
	Module* M = EXTLLVM::I()->M;
	llvm::Function* func = M->getFunction(std::string(string_value(pair_car(args))));
	if(func == 0)
	{
	    return _sc->F;
	}		
	extemp::EXTLLVM::I()->EE->lock.acquire();
	extemp::EXTLLVM::I()->EE->freeMachineCodeForFunction(func);
	extemp::EXTLLVM::I()->EE->lock.release();
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

	Module* M = EXTLLVM::I()->M;
	Module::global_iterator i = M->global_begin();
	GlobalVariable* var = M->getNamedGlobal(std::string(string_value(pair_car(args))));
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

	Module* M = EXTLLVM::I()->M;
	llvm::Function* func = M->getFunction(std::string(string_value(pair_car(args))));
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

	return mk_cptr(_sc, p);
    }		

    pointer SchemeFFI::llvm_call_void_native(scheme* _sc, pointer args)
    {
	using namespace llvm;

	Module* M = EXTLLVM::I()->M;
        char name[1024];
        sprintf(name,"%s_native",string_value(pair_car(args)));
	llvm::Function* func = M->getFunction(std::string(name));
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
	Module* M = EXTLLVM::I()->M;
	llvm::Function* func = M->getFunction(fname); //std::string(string_value(pair_car(args))));
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
	  EE->lock.acquire();
	  void* p = EE->recompileAndRelinkFunction(func);
	  EE->lock.release();
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
    // This will not be threadsafe whenever a definec is done!
    // 
    pointer SchemeFFI::call_compiled_closure(scheme* _sc, pointer args)
    {
	using namespace llvm;
	uint32_t** closure = (uint32_t**) cptr_value(pair_car(args));
	char* fname = (char*) *(closure+0);
	void* eptr = (void*) *(closure+1);		

	//std::cout << "CALL COMPILED CLOSURE: " << fname << std::endl;

	ExecutionEngine* EE = EXTLLVM::I()->EE;	

#ifdef LLVM_EE_LOCK
        EE->lock.acquire();	
#endif

	Module* M = EXTLLVM::I()->M;
	llvm::Function* func = M->getFunction(std::string(fname));
	if(func == 0)
	{
	    printf("No such function\n");
#ifdef LLVM_EE_LOCK
	    EE->lock.release();	
#endif
	    return _sc->F;
	}				
	//std::cout << "FUNC: " << *func << std::endl;

	int lgth = list_length(_sc, args);
	if(lgth != func->arg_size()) {
	    printf("Wrong number of arguments supplied to native closure - should take: %d\n",(func->arg_size()-1));
#ifdef LLVM_EE_LOCK
	    EE->lock.release();	
#endif
	    return _sc->F;			
	}
	Function::ArgumentListType::iterator funcargs = func->getArgumentList().begin();		
	std::vector<llvm::GenericValue> fargs(lgth);
	// first add the environment
	// which must always go first for a closure
	if(((Argument*)funcargs)->getType()->getTypeID() != Type::PointerTyID)
	{
	    printf("Error: closure must have an environment!\n");
#ifdef LLVM_EE_LOCK
	    EE->lock.release();	
#endif
	    return _sc->F;			
	}
	fargs[0].PointerVal = eptr;	
	++funcargs; // and increment funcargs past eptr

	//std::cout << "ARGS: " << lgth << std::endl;
	for(int i=1;i<lgth;i++,++funcargs)
	{
	    Argument* a = funcargs;
	    pointer p = list_ref(_sc, i, args);
	    if(is_integer(p)) {			
		if(a->getType()->getTypeID() != Type::IntegerTyID)
		{
		    printf("Bad argument type %i\n",i);
#ifdef LLVM_EE_LOCK
		    EE->lock.release();	
#endif
		    return _sc->F;
		}
		int width = a->getType()->getPrimitiveSizeInBits();				
		//std::cout << "IVALUE: " << ivalue(p) << std::endl; 				
		fargs[i].IntVal = APInt(width,ivalue(p));				
	    }			
	    else if(is_real(p))
	    {

		if(a->getType()->getTypeID() == Type::FloatTyID)
		{
		    //std::cout << "RVALUE: " << rvalue(p) << std::endl; 					
		    fargs[i].FloatVal = (float) rvalue(p);
		}
		else if(a->getType()->getTypeID() == Type::DoubleTyID)
		{
		    //std::cout << "RVALUE: " << rvalue(p) << std::endl; 
		    fargs[i].DoubleVal = rvalue(p);
		}
		else
		{
		    printf("Bad argument type %i\n",i);
#ifdef LLVM_EE_LOCK
		    EE->lock.release();	
#endif
		    return _sc->F;
		}
	    }
	    else if(is_string(p))
	    {
		if(a->getType()->getTypeID() != Type::PointerTyID)
		{
		    printf("Bad argument type %i\n",i);
#ifdef LLVM_EE_LOCK
		    EE->lock.release();	
#endif
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
#ifdef LLVM_EE_LOCK
		    EE->lock.release();	
#endif
		    return _sc->F;					
		}
		//std::cout << "PTRVALUE: " << cptr_value(p) << std::endl; 				
		fargs[i].PointerVal = cptr_value(p);
	    }
	    else 
	    {
		printf("Bad scheme argument\n");
#ifdef LLVM_EE_LOCK
		EE->lock.release();	
#endif
		return _sc->F;
	    }
	}


	//long long num_of_funcs = M->getFunctionList().size();
	//EE->lock.release();	

	/*
#ifdef LLVM_EE_LOCK	
	EE->lock.release();	
#endif
	*/
	GenericValue gv = EE->runFunction(func,fargs);

#ifdef LLVM_EE_LOCK	
	EE->lock.release();	
#endif

	/*
	if(num_of_funcs != M->getFunctionList().size()) {
	  std::cout << "KICK(ccc): " << num_of_funcs << "  id:" << boost::this_thread::get_id() << std::endl;
	    iplist<Function>::iterator iter = M->getFunctionList().end();
	    Function* stub = dyn_cast<Function>(--iter);
	    //			std::stringstream ss;
	    //			ss << *stub << std::endl;
	    EE->freeMachineCodeForFunction(stub);
	    stub->deleteBody();
	    stub->eraseFromParent();
	}
	*/

	//EE->lock.release();	

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
	default:
	    return _sc->F;
	}		
    }

    //
    // This will not be threadsafe whenever a definec is done!
    // 
    pointer SchemeFFI::call_compiled(scheme* _sc, pointer args)
    {
	using namespace llvm;

	ExecutionEngine* EE = EXTLLVM::I()->EE;

#ifdef LLVM_EE_LOCK	
	EE->lock.acquire();
#endif
	
        Module* M = EXTLLVM::I()->M;
	llvm::Function* func = (Function*) cptr_value(pair_car(args));
	if(func == 0)
	{
	    //std::cout << "no such function\n" << std::endl;
	    printf("No such function\n");
#ifdef LLVM_EE_LOCK
	    EE->lock.release();	
#endif
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
#ifdef LLVM_EE_LOCK
	    EE->lock.release();	
#endif
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
#ifdef LLVM_EE_LOCK
		    EE->lock.release();	
#endif
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
#ifdef LLVM_EE_LOCK
		    EE->lock.release();	
#endif
		    return _sc->F;
		}
	    }
	    else if(is_string(p))
	    {
		if(a->getType()->getTypeID() != Type::PointerTyID)
		{
		    printf("Bad argument type %i\n",i);
#ifdef LLVM_EE_LOCK
		    EE->lock.release();	
#endif
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
#ifdef LLVM_EE_LOCK
		    EE->lock.release();	
#endif
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
#ifdef LLVM_EE_LOCK
		EE->lock.release();	
#endif
		return _sc->F;
	    }
	    else {
		//ascii_print_color(1,1,10); // error color
		printf("Bad argument at index %i\n",i);
		//ascii_print_color(0,9,10); // default
#ifdef LLVM_EE_LOCK
		EE->lock.release();	
#endif
		return _sc->F;
	    }
		    
	}

	//		double(*fp)(double	,double) = (double(*)(double,double)) EE->getPointerToFunction(func);
	//		double v = fp(rvalue(pair_car(args)),rvalue(pair_cadr(args)));


	//long long num_of_funcs = M->getFunctionList().size();
	//std::cout << "llvm funcs: " << num_of_funcs << "  id:" << boost::this_thread::get_id() << std::endl;
	
	//        EE->lock.release();	


/*
	#ifdef LLVM_EE_LOCK
	EE->lock.release();
	#endif
*/
	
        GenericValue gv = EE->runFunction(func,fargs);
#ifdef LLVM_EE_LOCK		
	EE->lock.release();
#endif

        //EE->lock.acquire();	
	//long long num_of_funcs = M->getFunctionList().size();

	// if the number of functions in module has changed when calling runFunction 
	// then we assume a stub was made and appended to the end of the modules function list.
	// we remove this function now that we no longer need it!

	/*
	if(num_of_funcs != M->getFunctionList().size()) {
	    std::cout << "KICK(cc): " << num_of_funcs << "  id:" << boost::this_thread::get_id() << std::endl;
	    iplist<Function>::iterator iter = M->getFunctionList().end();
	    Function* stub = dyn_cast<Function>(--iter);
	    EE->freeMachineCodeForFunction(stub);
	    stub->deleteBody();
	    stub->eraseFromParent();
	}
*/
	//	EE->lock.release();	


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
#ifdef TARGET_OS_WINDOWS
	float f = (float) strtod(floatin, (char**) &floatout);
#else        
	float f = strtof(floatin, (char**) &floatout);
#endif
	llvm::APFloat apf(f);
		
	bool ignored;
	bool isDouble = false; // apf.getSemantics() == &llvm::APFloat::IEEEdouble;
	double Val = isDouble ? apf.convertToDouble() :
	apf.convertToFloat();
	std::string StrVal = llvm::ftostr(apf);
		
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
 #ifdef TARGET_OS_WINDOWS
 	double f = strtod(floatin, (char**) &floatout);
 #else
 	double f = strtod(floatin, (char**) &floatout);
 #endif
 	llvm::APFloat apf(f); 
 		
 	bool ignored;
 	bool isDouble = true; // apf.getSemantics() == &llvm::APFloat::IEEEdouble;
 	double Val = isDouble ? apf.convertToDouble() : apf.convertToFloat();
 	std::string StrVal = llvm::ftostr(apf);
 		
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
	    llvm::GlobalValue* val = M->getNamedValue(std::string(string_value(pair_car(args))));
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
	llvm::Module* M = EXTLLVM::I()->M;
	llvm::Function* func = M->getFunction(std::string(string_value(pair_car(args))));
	std::string str;
	llvm::raw_string_ostream ss(str);
	ss << *func;
	printf("%s",str.c_str());
	return _sc->T;		
    }
	
    pointer SchemeFFI::bind_symbol(scheme* _sc, pointer args)
    {
	void* library = cptr_value(pair_car(args));
	char* symname = string_value(pair_cadr(args));

	llvm::Module* M = EXTLLVM::I()->M;
	llvm::ExecutionEngine* EE = EXTLLVM::I()->EE;	

	EE->lock.acquire();
		
#ifdef TARGET_OS_WINDOWS
        void* ptr = (void*) GetProcAddress((HMODULE)library, symname);
#else
	void* ptr = dlsym(library, symname);
#endif
	if(!ptr) {
	    printf("Could not find symbol named %s!\n",symname);
	    return _sc->F;
	}
	llvm::GlobalValue* gv = M->getNamedValue(std::string(symname));
	EE->updateGlobalMapping(gv,ptr);

	EE->lock.release();		

	return _sc->T;
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
	  return _sc->NIL;		    
    }


    pointer SchemeFFI::get_named_type(scheme* _sc, pointer args)
    {
	char* n = string_value(pair_car(args));
	char nk[256];
	char* name = nk;
	strcpy(name,n);
	if (name[0] == '%') name = name++;	

	int ptrdepth = 0;
	while(name[strlen(name)-1] == '*') {
	  name[strlen(name)-1]=NULL;
          ptrdepth++;
	}

	llvm::Module* M = EXTLLVM::I()->M;
	
	const llvm::Type* tt = M->getTypeByName(name);

	if(tt) {
	  //return mk_string(_sc,M->getTypeName(tt).c_str());
	  std::string typestr;
	  llvm::raw_string_ostream ss(typestr);
	  tt->print(ss);
	  

	  const char* tmp_name = ss.str().c_str();
	  if(tt->isStructTy()) {
	    rsplit("= type ",(char*)tmp_name,tmp_str_a,tmp_str_b);
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


  ////////////////////////////////////////////////////////////////
  //  SOME XWindows guff
  ////////////////////////////////////////////////////////////////
#if defined (TARGET_OS_LINUX)

  int singleBufferAttributess[] = {
    GLX_DRAWABLE_TYPE, GLX_WINDOW_BIT,
    GLX_RENDER_TYPE,   GLX_RGBA_BIT,
    GLX_RED_SIZE,      1,   /* Request a single buffered color buffer */
    GLX_GREEN_SIZE,    1,   /* with the maximum number of color bits  */
    GLX_BLUE_SIZE,     1,   /* for each component                     */
    None
  };

  int doubleBufferAttributes[] = {
    GLX_DRAWABLE_TYPE, GLX_WINDOW_BIT,
    GLX_RENDER_TYPE,   GLX_RGBA_BIT,
    GLX_DOUBLEBUFFER,  True,  /* Request a double-buffered color buffer with */
    GLX_RED_SIZE,      1,     /* the maximum number of bits per component    */
    GLX_GREEN_SIZE,    1, 
    GLX_BLUE_SIZE,     1,
    None
  };

  static Bool EXTGLWaitForNotify( Display *dpy, XEvent *event, XPointer arg ) {
    return (event->type == MapNotify) && (event->xmap.window == (Window) arg);
  }

  pointer SchemeFFI::glSwapBuffers(scheme* _sc, pointer args)
  {
    args = pair_car(args);
    glXSwapBuffers((Display*) cptr_value(pair_car(args)), (GLXDrawable) cptr_value(pair_cadr(args)));
    return _sc->T;
  }

  pointer SchemeFFI::glMakeContextCurrent(scheme* _sc, pointer args)
  {
    args = pair_car(args);
    Display* dpy = (Display*) cptr_value(pair_car(args));
    GLXDrawable glxWin = (GLXDrawable) cptr_value(pair_cadr(args));
    GLXContext context = (GLXContext) cptr_value(pair_caddr(args));
    /* Bind the GLX context to the Window */
    glXMakeContextCurrent( (Display*) dpy, (GLXDrawable) glxWin, (GLXDrawable) glxWin, (GLXContext) context);    
    return _sc->T;
  }

  pointer SchemeFFI::getEvent(scheme* _sc, pointer args)
  {
    args = pair_car(args);
    Display* dpy = (Display*) cptr_value(pair_car(args));
    XEvent event;
    if(XPending(dpy) == 0) return _sc->NIL;
    //only return the LATEST event. DROP eveything earlier
    while(XPending(dpy)) XNextEvent(dpy, &event);
    switch(event.type){
    case ButtonPress: {
      XButtonEvent be = event.xbutton;
      pointer list = _sc->NIL;
      _sc->imp_env->insert(list);
      pointer tlist = cons(_sc,mk_integer(_sc,be.y),list);
      _sc->imp_env->erase(list);
      list = tlist;
      _sc->imp_env->insert(list);
      tlist = cons(_sc,mk_integer(_sc,be.x),list);
      _sc->imp_env->erase(list);
      list = tlist;
      tlist = cons(_sc,mk_integer(_sc,be.button),list);
      _sc->imp_env->erase(list);
      list = tlist;
      tlist = cons(_sc,mk_integer(_sc,be.state),list);
      _sc->imp_env->erase(list);
      list = tlist;
      tlist = cons(_sc,mk_integer(_sc,0),list);
      _sc->imp_env->erase(list);
      list = tlist;
      return list;
    }
    case MotionNotify: {
      XMotionEvent me = event.xmotion;
      pointer list = _sc->NIL;
      _sc->imp_env->insert(list);
      pointer tlist = cons(_sc,mk_integer(_sc,me.y),list);
      _sc->imp_env->erase(list);
      list = tlist;
      _sc->imp_env->insert(list);
      tlist = cons(_sc,mk_integer(_sc,me.x),list);
      _sc->imp_env->erase(list);
      list = tlist;
      tlist = cons(_sc,mk_integer(_sc,me.state),list);
      _sc->imp_env->erase(list);
      list = tlist;
      tlist = cons(_sc,mk_integer(_sc,1),list);
      _sc->imp_env->erase(list);
      list = tlist;
      return list;
    }
    case KeyPress: {
      XKeyEvent ke = event.xkey;
      pointer list = _sc->NIL;
      _sc->imp_env->insert(list);
      pointer tlist = cons(_sc,mk_integer(_sc,ke.y),list);
      _sc->imp_env->erase(list);
      list = tlist;
      _sc->imp_env->insert(list);
      tlist = cons(_sc,mk_integer(_sc,ke.x),list);
      _sc->imp_env->erase(list);
      list = tlist;
      tlist = cons(_sc,mk_integer(_sc,ke.keycode),list);
      _sc->imp_env->erase(list);
      list = tlist;
      tlist = cons(_sc,mk_integer(_sc,ke.state),list);
      _sc->imp_env->erase(list);
      list = tlist;
      tlist = cons(_sc,mk_integer(_sc,2),list);
      _sc->imp_env->erase(list);
      list = tlist;
      return list;
    }
    default:
      return _sc->NIL;
    }
  }

  bool checkGLXExtension(Display* dpy,const char* extName)
   {
     /*
          Search for extName in the extensions string.  Use of strstr()
          is not sufficient because extension names can be prefixes of
          other extension names.  Could use strtok() but the constant
          string returned by glGetString can be in read-only memory.
     */
     char* list = (char*) glXQueryExtensionsString(dpy, DefaultScreen(dpy));
     char* end;
     int extNameLen;
     extNameLen = strlen(extName);
     end = list + strlen(list);
     while (list < end)
       {
	 int n = strcspn(list, " ");
	 if ((extNameLen == n) && (strncmp(extName, list, n) == 0))
	   return true;
	 list += (n + 1);
       };
     return false;
   }; // bool checkGLXExtension(const char* extName)

  
  void* opengl_render_callback(void* a)
  {
    Display              *dpy;
    Window                xWin;
    XEvent                event;
    XVisualInfo          *vInfo;
    XSetWindowAttributes  swa;
    GLXFBConfig          *fbConfigs;
    GLXContext            context;
    GLXContext            sharedContext;
    GLXWindow             glxWin;
    int                   swaMask;
    int                   numReturned;
    int                   swapFlag = True;


    pointer args = (pointer) ((void**)a)[0];
    scheme* _sc = (scheme*) ((void**)a)[1];

    long(*callback)(void) = (long(*)(void)) cptr_value(pair_caddr(pair_cddddr(args)));

    //GLXContext util_glctx;     
    dpy = XOpenDisplay (string_value(pair_car(args)));
    if (dpy == NULL) {
      printf("No such X display\n");
      return _sc->F;     
    }

    /* Request a suitable framebuffer configuration - try for a double buffered configuration first */
    fbConfigs = glXChooseFBConfig( dpy, DefaultScreen(dpy), doubleBufferAttributes, &numReturned );

    if ( fbConfigs == NULL ) {  /* no double buffered configs available */
      fbConfigs = glXChooseFBConfig( dpy, DefaultScreen(dpy), singleBufferAttributess, &numReturned );
      swapFlag = False;
    }

    /* Create an X colormap and window with a visual matching the first
    ** returned framebuffer config */
    vInfo = glXGetVisualFromFBConfig( dpy, fbConfigs[0] );

    // attrList[indx] = GLX_USE_GL; indx++; 
    // attrList[indx] = GLX_DEPTH_SIZE; indx++; 
    // attrList[indx] = 1; indx++; 
    // attrList[indx] = GLX_RGBA; indx++; 
    // attrList[indx] = GLX_RED_SIZE; indx++; 
    // attrList[indx] = 1; indx++; 
    // attrList[indx] = GLX_GREEN_SIZE; indx++; 
    // attrList[indx] = 1; indx++; 
    // attrList[indx] = GLX_BLUE_SIZE; indx++; 
    // attrList[indx] = 1; indx++;     
    // attrList[indx] = None;     
    //vinfo = glXChooseVisual(display, DefaultScreen(display), attrList);     
    if (vInfo == NULL) {
      printf ("ERROR: Can't open window\n"); 
      return _sc->F;
    }    
 

    swa.border_pixel = 0;
    swa.override_redirect = (pair_cadr(args) == _sc->T) ? True : False; 
    swa.event_mask = StructureNotifyMask | KeyPressMask | ButtonPressMask | ButtonMotionMask;
    swa.colormap = XCreateColormap( dpy, RootWindow(dpy, vInfo->screen),
                                    vInfo->visual, AllocNone );

    //swaMask = CWBorderPixel | CWColormap | CWEventMask;
    swaMask = CWBorderPixel | CWColormap | CWEventMask | CWOverrideRedirect;


    //xWin = XCreateWindow( dpy, RootWindow(dpy, vInfo->screen), 0, 0, 1024, 768,
    //                      0, vInfo->depth, InputOutput, vInfo->visual,
    //                      swaMask, &swa );

    xWin = XCreateWindow( dpy, RootWindow(dpy, vInfo->screen), ivalue(pair_caddr(args)), ivalue(pair_cadddr(args)), ivalue(pair_car(pair_cddddr(args))), ivalue(pair_cadr(pair_cddddr(args))),
                          0, vInfo->depth, InputOutput, vInfo->visual,
                          swaMask, &swa );

    // if we are sharing a context
    if(sharedContext) {
      /* Create a GLX context for OpenGL rendering */
      context = glXCreateNewContext( dpy, fbConfigs[0], GLX_RGBA_TYPE, sharedContext, True );    
    }else{ // if we aren't sharing a context
      /* Create a GLX context for OpenGL rendering */
      context = glXCreateNewContext( dpy, fbConfigs[0], GLX_RGBA_TYPE, NULL, True );
    }

    /* Create a GLX window to associate the frame buffer configuration with the created X window */
    glxWin = glXCreateWindow( dpy, fbConfigs[0], xWin, NULL );
    
    /* Map the window to the screen, and wait for it to appear */
    XMapWindow( dpy, xWin );
    XIfEvent( dpy, &event, EXTGLWaitForNotify, (XPointer) xWin );

    /* Bind the GLX context to the Window */
    glXMakeContextCurrent( dpy, glxWin, glxWin, context );

    void (*swapInterval)(int) = 0;

    if (checkGLXExtension(dpy,"GLX_MESA_swap_control")) {
      swapInterval = (void (*)(int)) glXGetProcAddress((const GLubyte*) "glXSwapIntervalMESA");
    } else if (checkGLXExtension(dpy,"GLX_SGI_swap_control")) {
      swapInterval = (void (*)(int)) glXGetProcAddress((const GLubyte*) "glXSwapIntervalSGI");
    } else {
      printf("no vsync?!\n");
    }

    printf("Is Direct:%d\n",glXIsDirect(dpy,context));

    //glxSwapIntervalSGI(1);
    //glXSwapIntervalMESA(1);

    /* OpenGL rendering ... */
    glClearColor( 0.0, 0.0, 0.0, 1.0 );
    glClear( GL_COLOR_BUFFER_BIT );

    glFlush();

    if ( swapFlag )
      glXSwapBuffers(dpy, glxWin);

    printf("Using OPENGL callback render loop at refresh rate!\n");
    if(pair_cdddr(pair_cddddr(args)) != _sc->NIL) {
      long(*glinit)(void) = (long(*)(void)) cptr_value(pair_cadddr(pair_cddddr(args)));
      glinit();
    }
    
    glXSwapBuffers(dpy, glxWin);
    glFlush();
    swapInterval(1);
    
    while(true) {
      callback();
      glXSwapBuffers(dpy, glxWin);
    }
  }

  
  pointer SchemeFFI::makeGLContext(scheme* _sc, pointer args)
  {
    Display              *dpy;
    Window                xWin;
    XEvent                event;
    XVisualInfo          *vInfo;
    XSetWindowAttributes  swa;
    GLXFBConfig          *fbConfigs;
    GLXContext            context;
    GLXContext            sharedContext;
    GLXWindow             glxWin;
    int                   swaMask;
    int                   numReturned;
    int                   swapFlag = True;

    if(pair_cddr(pair_cddddr(args)) != _sc->NIL) {
      EXTThread* render_thread = new EXTThread();
      void* v[2];
      v[0] = args;
      v[1] = _sc;
      render_thread->create(&opengl_render_callback,v);
      return _sc->T;
    }

    sharedContext = NULL;
    //if(pair_cdr(args) != _sc->NIL) sharedContext = (GLXContext) cptr_value(pair_cadr(args));

    //GLXContext util_glctx;     
    dpy = XOpenDisplay (string_value(pair_car(args)));
    if (dpy == NULL) {
      printf("No such X display\n");
      return _sc->F;     
    }

    /* Request a suitable framebuffer configuration - try for a double buffered configuration first */
    fbConfigs = glXChooseFBConfig( dpy, DefaultScreen(dpy), doubleBufferAttributes, &numReturned );

    if ( fbConfigs == NULL ) {  /* no double buffered configs available */
      fbConfigs = glXChooseFBConfig( dpy, DefaultScreen(dpy), singleBufferAttributess, &numReturned );
      swapFlag = False;
    }

    /* Create an X colormap and window with a visual matching the first
    ** returned framebuffer config */
    vInfo = glXGetVisualFromFBConfig( dpy, fbConfigs[0] );

    // attrList[indx] = GLX_USE_GL; indx++; 
    // attrList[indx] = GLX_DEPTH_SIZE; indx++; 
    // attrList[indx] = 1; indx++; 
    // attrList[indx] = GLX_RGBA; indx++; 
    // attrList[indx] = GLX_RED_SIZE; indx++; 
    // attrList[indx] = 1; indx++; 
    // attrList[indx] = GLX_GREEN_SIZE; indx++; 
    // attrList[indx] = 1; indx++; 
    // attrList[indx] = GLX_BLUE_SIZE; indx++; 
    // attrList[indx] = 1; indx++;     
    // attrList[indx] = None;     
    //vinfo = glXChooseVisual(display, DefaultScreen(display), attrList);     
    if (vInfo == NULL) {
      printf ("ERROR: Can't open window\n"); 
      return _sc->F;
    }    
 

    swa.border_pixel = 0;
    swa.override_redirect = (pair_cadr(args) == _sc->T) ? True : False; 
    swa.event_mask = StructureNotifyMask | KeyPressMask | ButtonPressMask | ButtonMotionMask;
    swa.colormap = XCreateColormap( dpy, RootWindow(dpy, vInfo->screen),
                                    vInfo->visual, AllocNone );

    //swaMask = CWBorderPixel | CWColormap | CWEventMask;
    swaMask = CWBorderPixel | CWColormap | CWEventMask | CWOverrideRedirect;


    //xWin = XCreateWindow( dpy, RootWindow(dpy, vInfo->screen), 0, 0, 1024, 768,
    //                      0, vInfo->depth, InputOutput, vInfo->visual,
    //                      swaMask, &swa );

    xWin = XCreateWindow( dpy, RootWindow(dpy, vInfo->screen), ivalue(pair_caddr(args)), ivalue(pair_cadddr(args)), ivalue(pair_car(pair_cddddr(args))), ivalue(pair_cadr(pair_cddddr(args))),
                          0, vInfo->depth, InputOutput, vInfo->visual,
                          swaMask, &swa );

    // if we are sharing a context
    if(sharedContext) {
      /* Create a GLX context for OpenGL rendering */
      context = glXCreateNewContext( dpy, fbConfigs[0], GLX_RGBA_TYPE, sharedContext, True );    
    }else{ // if we aren't sharing a context
      /* Create a GLX context for OpenGL rendering */
      context = glXCreateNewContext( dpy, fbConfigs[0], GLX_RGBA_TYPE, NULL, True );
    }

    /* Create a GLX window to associate the frame buffer configuration with the created X window */
    glxWin = glXCreateWindow( dpy, fbConfigs[0], xWin, NULL );
    
    /* Map the window to the screen, and wait for it to appear */
    XMapWindow( dpy, xWin );
    XIfEvent( dpy, &event, EXTGLWaitForNotify, (XPointer) xWin );

    /* Bind the GLX context to the Window */
    glXMakeContextCurrent( dpy, glxWin, glxWin, context );

    void (*swapInterval)(int) = 0;

    if (checkGLXExtension(dpy,"GLX_MESA_swap_control")) {
      swapInterval = (void (*)(int)) glXGetProcAddress((const GLubyte*) "glXSwapIntervalMESA");
    } else if (checkGLXExtension(dpy,"GLX_SGI_swap_control")) {
      swapInterval = (void (*)(int)) glXGetProcAddress((const GLubyte*) "glXSwapIntervalSGI");
    } else {
      printf("no vsync?!\n");
    }

    printf("Is Direct:%d\n",glXIsDirect(dpy,context));

    //glxSwapIntervalSGI(1);
    //glXSwapIntervalMESA(1);

    /* OpenGL rendering ... */
    glClearColor( 0.0, 0.0, 0.0, 1.0 );
    glClear( GL_COLOR_BUFFER_BIT );

    glFlush();

    if ( swapFlag )
      glXSwapBuffers(dpy, glxWin);

    swapInterval(0);    

    pointer list = _sc->NIL;
    _sc->imp_env->insert(list);
    pointer tlist = cons(_sc,mk_cptr(_sc,(void*)context),list);
    _sc->imp_env->erase(list);
    list = tlist;
    _sc->imp_env->insert(list);
    tlist = cons(_sc,mk_cptr(_sc,(void*)glxWin),list);
    _sc->imp_env->erase(list);
    list = tlist;
    tlist = cons(_sc,mk_cptr(_sc,(void*)dpy),list);
    _sc->imp_env->erase(list);
    list = tlist;
		
    return list; //_cons(_sc, mk_cptr(_sc, (void*)dpy),mk_cptr(_sc,(void*)glxWin),1);
  }

#elif TARGET_OS_WINDOWS

  //int singleBufferAttributess[] = {
  //  GLX_DRAWABLE_TYPE, GLX_WINDOW_BIT,
  //  GLX_RENDER_TYPE,   GLX_RGBA_BIT,
  //  GLX_RED_SIZE,      1,   /* Request a single buffered color buffer */
  //  GLX_GREEN_SIZE,    1,   /* with the maximum number of color bits  */
  //  GLX_BLUE_SIZE,     1,   /* for each component                     */
  //  None
  //};

  //int doubleBufferAttributes[] = {
  //  GLX_DRAWABLE_TYPE, GLX_WINDOW_BIT,
  //  GLX_RENDER_TYPE,   GLX_RGBA_BIT,
  //  GLX_DOUBLEBUFFER,  True,  /* Request a double-buffered color buffer with */
  //  GLX_RED_SIZE,      1,     /* the maximum number of bits per component    */
  //  GLX_GREEN_SIZE,    1, 
  //  GLX_BLUE_SIZE,     1,
  //  None
  //};

  //static Bool EXTGLWaitForNotify( Display *dpy, XEvent *event, XPointer arg ) {
  //  return (event->type == MapNotify) && (event->xmap.window == (Window) arg);
  //}

  pointer SchemeFFI::addGLExtension(scheme* _sc, pointer args)
  {
    using namespace llvm;
    char* ext_name = string_value(pair_car(args));
    Module* M = EXTLLVM::I()->M;		
    llvm::GlobalValue* gv = M->getNamedValue(std::string(ext_name));
    if( gv == 0 ) {
      printf("Attempting to bind to a non-existant global LLVM variable\n");
      return _sc->F;
    }
    void* ptr = (void*) wglGetProcAddress(ext_name); //cptr_value(pair_cadr(args));
    if( ptr ) {
      EXTLLVM::I()->EE->lock.acquire();	
      EXTLLVM::I()->EE->updateGlobalMapping(gv,ptr); 
      EXTLLVM::I()->EE->lock.release();	
      printf("successfully bound extension:%s\n",ext_name);
      return _sc->T;
    } else {
      printf("Cannot find opengl extension %s\n",ext_name);
      return _sc->F;
    }
  }

  int EXT_WIN_MSG_MASK = PM_REMOVE;
 
  pointer SchemeFFI::getEvent(scheme* _sc, pointer args)
  {
    //std::cout << "GET EVENTS!" << std::endl;
    // this here to stop opengl swap buffer code from pinching out input events before we get to them    
    if(EXT_WIN_MSG_MASK==PM_REMOVE) { // this is a temporary hack!
      EXT_WIN_MSG_MASK = PM_REMOVE | PM_QS_PAINT | PM_QS_POSTMESSAGE | PM_QS_SENDMESSAGE;
    }
     
    MSG msg;
 
    // std::cout << "GOING INTO GET MESSAGES" << std::endl;
    // int res = PeekMessage(&msg,NULL,0,0,PM_REMOVE);
    // std::cout << "MSG RES: " << res <<  std::endl;
    while(PeekMessage(&msg, NULL, 0, 0, PM_REMOVE | PM_QS_INPUT)) {
      //if(res){      
      //std::cout << "GRABBED MSG" << std::endl;
      WPARAM wParam = msg.wParam;
      LPARAM lParam = msg.lParam;
      //std::cout << "GOT MESSAGE: " << &msg << std::endl;
      switch(msg.message) {
      case WM_CHAR: {
   	pointer list = _sc->NIL;
   	_sc->imp_env->insert(list);
   	pointer tlist = cons(_sc,mk_integer(_sc,wParam),list);
   	_sc->imp_env->erase(list);
   	list = tlist;
   	return list;
      }       
      case WM_MOUSEMOVE: {
   	pointer list = _sc->NIL;
   	_sc->imp_env->insert(list);
   	pointer tlist = cons(_sc,mk_integer(_sc,GET_X_LPARAM(lParam)),list);
   	_sc->imp_env->erase(list);
   	list = tlist;
   	_sc->imp_env->insert(list);
   	tlist = cons(_sc,mk_integer(_sc,GET_Y_LPARAM(lParam)),list);
   	_sc->imp_env->erase(list);
   	list = tlist;
   	return list;       
      } 
      default:
   	TranslateMessage(&msg);
   	DispatchMessage(&msg);
   	return _sc->NIL;
      }
    }
    return _sc->NIL;
  }
 
    
  pointer SchemeFFI::glSwapBuffers(scheme* _sc, pointer args)
  {
    args = pair_car(args);
    SwapBuffers((HDC)cptr_value(pair_car(args)));
    //EXT_WIN_MSG_MASK = PM_REMOVE | PM_QS_PAINT | PM_QS_POSTMESSAGE | PM_QS_SENDMESSAGE;  
    MSG msg;
  
    //std::cout << "GOING INTO GET MESSAGES" << std::endl;
    while(PeekMessage(&msg, NULL, 0, 0, EXT_WIN_MSG_MASK)) {
      //std::cout << "GOT MESSAGE" << std::endl;
      TranslateMessage(&msg);
      DispatchMessage(&msg);
      //std::cout << "msg: " << &msg << std::endl;
    }
    //std::cout << "DONE PROCESSING WIN MESSAGES" << std::endl;
    return _sc->T;
  }

  
  // pointer SchemeFFI::glSwapBuffers(scheme* _sc, pointer args)
  // {
  //   args = pair_car(args);
  //   SwapBuffers((HDC)cptr_value(pair_car(args)));

  //   MSG msg;

  //   //std::cout << "GOING INTO GET MESSAGES" << std::endl;
  //   while(PeekMessage(&msg, NULL, 0, 0, PM_REMOVE)) {
  //     //std::cout << "GOT MESSAGE" << std::endl;
  //     TranslateMessage(&msg);
  //     DispatchMessage(&msg);
  //     //std::cout << "msg: " << &msg << std::endl;
  //   }
  //   //std::cout << "DONE PROCESSING WIN MESSAGES" << std::endl;

  //   //glXSwapBuffers((Display*) cptr_value(pair_car(args)), (GLXDrawable) cptr_value(pair_cadr(args)));
  //   return _sc->T;
  // }

  pointer SchemeFFI::glMakeContextCurrent(scheme* _sc, pointer args)
  {
    wglMakeCurrent ( (HDC)cptr_value(pair_car(args)), (HGLRC)cptr_value(pair_cadr(args)) );
    return _sc->T;
  }

 // window handler
 
// Step 4: the Window Procedure
LRESULT CALLBACK WinProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
    switch(msg)
    {
        case WM_CLOSE:
            DestroyWindow(hwnd);
        break;
        case WM_DESTROY:
            PostQuitMessage(0);
        break;
        default:
            return DefWindowProc(hwnd, msg, wParam, lParam);
    }
    return 0;
}


  pointer SchemeFFI::makeGLContext(scheme* _sc, pointer args)
  {
    PIXELFORMATDESCRIPTOR pfd = { 
      sizeof(PIXELFORMATDESCRIPTOR),  //  size of this pfd  
      1,                     // version number  
      PFD_DRAW_TO_WINDOW |   // support window  
      PFD_SUPPORT_OPENGL |   // support OpenGL  
      PFD_DOUBLEBUFFER,      // double buffered  
      PFD_TYPE_RGBA,         // RGBA type  
      32,                    // 32-bit color depth  
      0, 0, 0, 0, 0, 0,      // color bits ignored  
      0,                     // no alpha buffer  
      0,                     // shift bit ignored  
      0,                     // no accumulation buffer  
      0, 0, 0, 0,            // accum bits ignored  
      24,                    // 24-bit z-buffer      
      8,                     // no stencil buffer  
      0,                     // no auxiliary buffer  
      PFD_MAIN_PLANE,        // main layer  
      0,                     // reserved  
      0, 0, 0                // layer masks ignored  
    }; 
    
    HDC  hdc;
    HGLRC hglrc;
    int  iPixelFormat; 
    int  posx = ivalue(pair_caddr(args));
    int  posy = ivalue(pair_cadddr(args));
    int  width = ivalue(pair_car(pair_cddddr(args)));
    int  height = ivalue(pair_cadr(pair_cddddr(args)));
    bool  forcetop = (pair_cddr(pair_cddddr(args))==_sc->NIL) ? 0 : 1;    
    
    MSG msg;
    WNDCLASSEX ex;
    HINSTANCE hinstance;
    
    LPCWSTR WNDCLASSNAME = L"GLClass";
    LPCWSTR WNDNAME = L"OpenGL base code";
    
    ex.cbSize = sizeof(WNDCLASSEX);
    ex.style = CS_HREDRAW|CS_VREDRAW|CS_OWNDC;
    ex.lpfnWndProc = WinProc;
    ex.cbClsExtra = 0;
    ex.cbWndExtra = 0;
    ex.hInstance = hinstance;
    ex.hIcon = LoadIcon(NULL, IDI_APPLICATION);
    ex.hCursor = LoadCursor(NULL, IDC_ARROW);
    ex.hbrBackground = NULL;
    ex.lpszMenuName = NULL;
    ex.lpszClassName = (LPCWSTR)WNDCLASSNAME;
    ex.hIconSm = NULL;

    RegisterClassEx(&ex);
    
	      // center position of the window
    //int posx = (GetSystemMetrics(SM_CXSCREEN) / 2) - (width / 2);
    //int posy = (GetSystemMetrics(SM_CYSCREEN) / 2) - (height / 2);

    // set up the window for a windowed application by default
    long wndStyle; // = WS_OVERLAPPEDWINDOW;
    long dwExStyle; // = WS_OVERLAPPEDWINDOW;
    int screenmode = 0; // NOT FULLSCREEN
    bool fullScreen = false;

    if (pair_cadr(args) == _sc->T){ // if fullscreen
        screenmode = 1;
        fullScreen = true;
    }

    /*      Check if fullscreen is on*/
    if (fullScreen) {
	DEVMODE dmScreenSettings;
	memset(&dmScreenSettings, 0, sizeof(dmScreenSettings));
	dmScreenSettings.dmSize = sizeof(dmScreenSettings);
	dmScreenSettings.dmPelsWidth = width;   //screen width
	dmScreenSettings.dmPelsHeight = height; //screen height
	dmScreenSettings.dmBitsPerPel = 32; //bits;   //bits per pixel
	dmScreenSettings.dmFields = DM_BITSPERPEL | DM_PELSWIDTH | DM_PELSHEIGHT;
	
	if (ChangeDisplaySettings(&dmScreenSettings, CDS_FULLSCREEN != DISP_CHANGE_SUCCESSFUL)) {
	  /*      Setting display mode failed, switch to windowed*/
	  //MessageBox(NULL, (LPCWSTR)L"Display mode failed", NULL, MB_OK);
	  fullScreen = true; //false; 
	  screenmode = 1;
	}
    }


    if (fullScreen){ // if fullscreen
        dwExStyle = WS_EX_APPWINDOW | WS_EX_TOPMOST;
        wndStyle = WS_POPUP;
        screenmode = 1; //FULLSCREEN
        posx = 0;
        posy = 0;
       
        ShowCursor(FALSE);
        // change resolution before the window is created
        //SysSetDisplayMode(screenw, screenh, SCRDEPTH);
    }else{
        dwExStyle = WS_EX_APPWINDOW | WS_EX_WINDOWEDGE | WS_EX_TOPMOST;
        wndStyle = WS_OVERLAPPEDWINDOW; //windows style         
        screenmode = 0; //not fullscreen
    }

    RECT    windowRect;

    windowRect.left =(long)0;               //set left value to 0
    windowRect.right =(long)posx;  //set right value to requested width
    windowRect.top =(long)0;                //set top value to 0
    windowRect.bottom =(long)posy;//set bottom value to requested height

    AdjustWindowRectEx(&windowRect, wndStyle, FALSE, dwExStyle);	

    // create the window
    HWND hwnd = CreateWindowEx(NULL,
                  WNDCLASSNAME, //"GLClass",
                  WNDNAME, //"Extempore OpenGL",
                  wndStyle|WS_CLIPCHILDREN|WS_CLIPSIBLINGS,
                  posx, posy,
                  width, height,
                  NULL,
                  NULL,
                  hinstance,
                  NULL);

    hdc = GetDC(hwnd);
    if(hdc == NULL) {
      std::cout << "Failed to Get the Window Device Context" << std::endl;
      return _sc->F;
    }
    
	// set pixel format
    iPixelFormat = ChoosePixelFormat(hdc, &pfd);
    SetPixelFormat(hdc, iPixelFormat, &pfd);

    // create a rendering context  
    hglrc = wglCreateContext (hdc); 
    if(hglrc == NULL) {
      std::cout << "Failed to create an OpenGL rendering Context" << std::endl;
      return _sc->F;
    }
    
    // make it the calling thread's current rendering context 
    wglMakeCurrent (hdc, hglrc);
    
    ShowWindow(hwnd, SW_SHOW);		// everything went OK, show the window
    UpdateWindow(hwnd);
    if (forcetop) SetWindowPos(hwnd,HWND_TOPMOST,posx,posy,width,height,SWP_NOOWNERZORDER);

    
    /* Bind the GLX context to the Window */
    wglMakeCurrent( hdc,hglrc );

    std::cout << "hwnd:" << hwnd << " hdc:" << hdc << " hglrc:" << hglrc << std::endl;

    /* OpenGL rendering ... */
    glClearColor( 0.0, 0.0, 0.0, 1.0 );
    glClear( GL_COLOR_BUFFER_BIT );

    glFlush();

    //if ( swapFlag )
	  SwapBuffers(hdc);

    //swapInterval(0);    

    pointer list = _sc->NIL;
    _sc->imp_env->insert(list);
    pointer tlist = cons(_sc,mk_cptr(_sc,(void*)hglrc),list);
    _sc->imp_env->erase(list);
    list = tlist;
    _sc->imp_env->insert(list);
    tlist = cons(_sc,mk_cptr(_sc,(void*)hdc),list);
    _sc->imp_env->erase(list);
    list = tlist;
		
    return list; //_cons(_sc, mk_cptr(_sc, (void*)dpy),mk_cptr(_sc,(void*)glxWin),1);
  }


#elif TARGET_OS_MAC

  pointer SchemeFFI::glSwapBuffers(scheme* _sc, pointer args)
  {    
    //return objc_glSwapBuffers(_sc, args);
    NSOpenGLView* view = (NSOpenGLView*) cptr_value(pair_car(args));
    CGLContextObj ctx = (CGLContextObj) [[view openGLContext] CGLContextObj];
    CGLLockContext(ctx);
    [[view openGLContext] flushBuffer];
    CGLUnlockContext(ctx);
    //CGLContextObj ctx = CGLGetCurrentContext();
    //CGLFlushDrawable(ctx);
    return _sc->T;
  }

  pointer SchemeFFI::glMakeContextCurrent(scheme* _sc, pointer args)
  {
    //    return objc_glMakeContextCurrent(_sc, args);
    CGLContextObj ctx = CGLGetCurrentContext();
    NSOpenGLView* view = (NSOpenGLView*) cptr_value(pair_car(args));
    ctx = (CGLContextObj) [[view openGLContext] CGLContextObj];
    //CGLLockContext(ctx);
    CGLSetCurrentContext(ctx);
    //CGLUnlockContext(ctx);    
    return _sc->T;
  }


  pointer SchemeFFI::makeGLContext(scheme* _sc, pointer args)
  {
    //return objc_makeGLContext(_sc, args);
     
    bool fullscrn = (pair_cadr(args) == _sc->T) ? 1 : 0; 
    int  posx = ivalue(pair_caddr(args));
    int  posy = ivalue(pair_cadddr(args));
    int  width = ivalue(pair_car(pair_cddddr(args)));
    int  height = ivalue(pair_cadr(pair_cddddr(args)));

    NSAutoreleasePool* pool = [[NSAutoreleasePool alloc] init];

    NSRect frameRect = NSMakeRect(posx,posy,width,height);

    // Get the screen rect of our main display
    NSArray* screens = [NSScreen screens];
    NSScreen* scrn = [NSScreen mainScreen]; // [screens objectAtIndex:0];
    
    NSRect screenRect = frameRect;
    if(fullscrn) screenRect = [scrn frame];
    
    //NSSize size = screenRect.size;
    NSPoint position = screenRect.origin;	
    NSSize size = screenRect.size;
    
    if(fullscrn) {
      position.x = 0;
      position.y = 0;      
      screenRect.origin = position;
    }
      
    NSOpenGLPixelFormatAttribute array[] = 		
      {
    	NSOpenGLPFAWindow,
    	NSOpenGLPFACompliant,
    	NSOpenGLPFANoRecovery,

        // Add these back for multsampling	
    	// NSOpenGLPFAMultisample,
    	// NSOpenGLPFASampleBuffers, (NSOpenGLPixelFormatAttribute)1,
    	// NSOpenGLPFASamples, (NSOpenGLPixelFormatAttribute)4,							
	
    	NSOpenGLPFAColorSize, (NSOpenGLPixelFormatAttribute)24,			
    	NSOpenGLPFADepthSize, (NSOpenGLPixelFormatAttribute)24,
    	NSOpenGLPFAAccumSize, (NSOpenGLPixelFormatAttribute)24,
    	NSOpenGLPFAAlphaSize, (NSOpenGLPixelFormatAttribute)8,
    	NSOpenGLPFAStencilSize, (NSOpenGLPixelFormatAttribute)8,
	
    	NSOpenGLPFADoubleBuffer,
    	NSOpenGLPFAAccelerated,
    	(NSOpenGLPixelFormatAttribute)0				
      };

    NSOpenGLPixelFormat* fmt = [[NSOpenGLPixelFormat alloc] initWithAttributes: (NSOpenGLPixelFormatAttribute*) array]; 
    //NSOpenGLContext *ctx = _openGLContext = [[NSOpenGLContext alloc] initWithFormat:fmt shareContext:nil];
    NSOpenGLView* view = [[NSOpenGLView alloc] initWithFrame:screenRect pixelFormat:fmt];
    
    int windowStyleMask;
    if(fullscrn){
      windowStyleMask = NSBorderlessWindowMask;
    }else{
      windowStyleMask = NSTitledWindowMask | NSClosableWindowMask | NSMiniaturizableWindowMask;
    }
    NSWindow* window = [[NSWindow alloc] initWithContentRect:screenRect
    	      styleMask:windowStyleMask
    	      backing:NSBackingStoreBuffered
    	      defer:YES screen:scrn];
    
    [window setContentView:view];
    [window useOptimizedDrawing:YES];
    [window setOpaque:YES];
    [window setBackgroundColor:[NSColor colorWithDeviceRed:0.0 green:0.0 blue:0.0 alpha:1.0]];
    if(fullscrn) {
      [window setHasShadow:NO];
      [window makeKeyAndOrderFront:nil];
    }else{	
      [window setTitle:@"Extempore OpenGL Window"];
      [window makeKeyAndOrderFront:nil];				
    }	
    
    [window display];	
  
    GLint swapInt = 1;
    [[view openGLContext] setValues:&swapInt forParameter:NSOpenGLCPSwapInterval];
  
    CGLContextObj ctx = (CGLContextObj) [[view openGLContext] CGLContextObj];
    
    CGLSetCurrentContext(ctx);
    CGLLockContext(ctx);
    
    glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
    CGLEnable( ctx, kCGLCEMPEngine);				
    
    CGLUnlockContext(ctx);
    
    // pointer list = _sc->NIL;
    // _sc->imp_env->insert(list);
    // pointer tlist = cons(_sc,mk_cptr(_sc,(void*)v),list);
    // _sc->imp_env->erase(list);
    // list = tlist;
    // _sc->imp_env->insert(list);
    // tlist = cons(_sc,mk_cptr(_sc,(void*)hdc),list);
    // _sc->imp_env->erase(list);
    // list = tlist;
    [pool release];
    
    return mk_cptr(_sc, view); //list; //_cons(_sc, mk_cptr(_sc, (void*)dpy),mk_cptr(_sc,(void*)glxWin),1);
  }


#endif


  //////////////////////////////////////////////////////////////////
  //  CLOCK STUFF

#ifdef EXT_BOOST
 boost::posix_time::ptime EXT_BOOST_JAN1970(boost::gregorian::date(1970,boost::gregorian::Jan,1));

  double time_to_double(boost::posix_time::ptime& time) {
	 boost::posix_time::time_period period(EXT_BOOST_JAN1970,time);
	 return period.length().total_microseconds()/D_MILLION;
  }
 
  struct boost::posix_time::ptime& double_to_time(double tm) {
     using namespace boost::posix_time;
     ptime p;
     int64_t seconds = (int64_t)tm;
     int64_t fractional = (tm-seconds)*time_duration::ticks_per_second();
     p = EXT_BOOST_JAN1970+time_duration(0,0,seconds,fractional);

     return p;
  }
  pointer SchemeFFI::adjustClockOffset(scheme* _sc, pointer args)
  {
    SchemeFFI::CLOCK_OFFSET = rvalue(pair_car(args)) + SchemeFFI::CLOCK_OFFSET;
    return mk_real(_sc,SchemeFFI::CLOCK_OFFSET);
  }

  pointer SchemeFFI::setClockOffset(scheme* _sc, pointer args)
  {
    SchemeFFI::CLOCK_OFFSET = rvalue(pair_car(args));
    return pair_car(args);
  }

  pointer SchemeFFI::getClockOffset(scheme* _sc, pointer args)
  {
    return mk_real(_sc, SchemeFFI::CLOCK_OFFSET);
  }

  pointer SchemeFFI::lastSampleBlockClock(scheme* _sc, pointer args)
  {
    pointer p1 = mk_integer(_sc,UNIV::TIME);
    _sc->imp_env->insert(p1);
    pointer p2 = mk_real(_sc,AudioDevice::REALTIME + SchemeFFI::CLOCK_OFFSET);
    _sc->imp_env->insert(p2);
    pointer p3 = cons(_sc, p1, p2);
    _sc->imp_env->erase(p1);
    _sc->imp_env->erase(p2);
    return p3;
  }

  pointer SchemeFFI::getClockTime(scheme* _sc, pointer args)
  {
	boost::posix_time::ptime pt = boost::posix_time::microsec_clock::local_time();
	return mk_real(_sc, time_to_double(pt) + SchemeFFI::CLOCK_OFFSET);
    //return mk_real(_sc, CFAbsoluteTimeGetCurrent() + kCFAbsoluteTimeIntervalSince1970 + SchemeFFI::CLOCK_OFFSET);
  }

  //audiodevice clock stuff
  pointer SchemeFFI::ad_adjustClockOffset(scheme* _sc, pointer args)
  {
    AudioDevice::CLOCKOFFSET = rvalue(pair_car(args)) + AudioDevice::CLOCKOFFSET;
    return mk_real(_sc,AudioDevice::CLOCKOFFSET);
  }

  pointer SchemeFFI::ad_setClockOffset(scheme* _sc, pointer args)
  {
    AudioDevice::CLOCKOFFSET = rvalue(pair_car(args));
    return pair_car(args);
  }

  pointer SchemeFFI::ad_getClockOffset(scheme* _sc, pointer args)
  {
    return mk_real(_sc, AudioDevice::CLOCKOFFSET);
  }

  pointer SchemeFFI::ad_getClockTime(scheme* _sc, pointer args)
  {
    return mk_real(_sc,AudioDevice::CLOCKBASE + AudioDevice::CLOCKOFFSET + ((double)UNIV::TIME/(double)UNIV::SAMPLERATE));
  }

  pointer SchemeFFI::ad_setTime(scheme* _sc, pointer args)
  {
    if(pair_cdr(args) == _sc->NIL) {
		//AudioDevice::CLOCKBASE = CFAbsoluteTimeGetCurrent() + kCFAbsoluteTimeIntervalSince1970;
		boost::posix_time::ptime pt = boost::posix_time::microsec_clock::local_time();
		AudioDevice::CLOCKBASE = time_to_double(pt); /* + SchemeFFI::CLOCK_OFFSET) */
    }else{
      AudioDevice::CLOCKBASE = rvalue(pair_cadr(args));
    }
    UNIV::TIME = ivalue(pair_car(args));
    return _sc->T;
  } 
#else
double time_to_double(struct timespec t) {
    return t.tv_sec + t.tv_nsec/D_BILLION;
}
 
struct timespec double_to_time(double tm) {
  struct timespec t;
 
  t.tv_sec = (long)tm;
  t.tv_nsec = (tm - t.tv_sec)*BILLION;
  if (t.tv_nsec == BILLION) {
    t.tv_sec++;
    t.tv_nsec = 0;
  }
  return t;
}

  pointer SchemeFFI::adjustClockOffset(scheme* _sc, pointer args)
  {
    SchemeFFI::CLOCK_OFFSET = rvalue(pair_car(args)) + SchemeFFI::CLOCK_OFFSET;
    return mk_real(_sc,SchemeFFI::CLOCK_OFFSET);
  }

  pointer SchemeFFI::setClockOffset(scheme* _sc, pointer args)
  {
    SchemeFFI::CLOCK_OFFSET = rvalue(pair_car(args));
    return pair_car(args);
  }

  pointer SchemeFFI::getClockOffset(scheme* _sc, pointer args)
  {
    return mk_real(_sc, SchemeFFI::CLOCK_OFFSET);
  }

  pointer SchemeFFI::lastSampleBlockClock(scheme* _sc, pointer args)
  {
    pointer p1 = mk_integer(_sc,UNIV::TIME);
    _sc->imp_env->insert(p1);
    pointer p2 = mk_real(_sc,AudioDevice::REALTIME + SchemeFFI::CLOCK_OFFSET);
    _sc->imp_env->insert(p2);
    pointer p3 = cons(_sc, p1, p2);
    _sc->imp_env->erase(p1);
    _sc->imp_env->erase(p2);
    return p3;
  }

#ifdef TARGET_OS_MAC
  pointer SchemeFFI::getClockTime(scheme* _sc, pointer args)
  {
    return mk_real(_sc, CFAbsoluteTimeGetCurrent() + kCFAbsoluteTimeIntervalSince1970 + SchemeFFI::CLOCK_OFFSET);
  } 
#else
  pointer SchemeFFI::getClockTime(scheme* _sc, pointer args)
  {
    struct timespec t;
    clock_gettime(CLOCK_REALTIME, &t);
    return mk_real(_sc, time_to_double(t)+SchemeFFI::CLOCK_OFFSET);
  }
#endif

  //audiodevice clock stuff
  pointer SchemeFFI::ad_adjustClockOffset(scheme* _sc, pointer args)
  {
    AudioDevice::CLOCKOFFSET = rvalue(pair_car(args)) + AudioDevice::CLOCKOFFSET;
    return mk_real(_sc,AudioDevice::CLOCKOFFSET);
  }

  pointer SchemeFFI::ad_setClockOffset(scheme* _sc, pointer args)
  {
    AudioDevice::CLOCKOFFSET = rvalue(pair_car(args));
    return pair_car(args);
  }

  pointer SchemeFFI::ad_getClockOffset(scheme* _sc, pointer args)
  {
    return mk_real(_sc, AudioDevice::CLOCKOFFSET);
  }

  pointer SchemeFFI::ad_getClockTime(scheme* _sc, pointer args)
  {
    return mk_real(_sc,AudioDevice::CLOCKBASE + AudioDevice::CLOCKOFFSET + ((double)UNIV::TIME/(double)UNIV::SAMPLERATE));
  }

  pointer SchemeFFI::ad_setTime(scheme* _sc, pointer args)
  {
    if(pair_cdr(args) == _sc->NIL) {
#ifdef TARGET_OS_MAC
      AudioDevice::CLOCKBASE = CFAbsoluteTimeGetCurrent() + kCFAbsoluteTimeIntervalSince1970;
#else
      struct timespec t;
      clock_gettime(CLOCK_REALTIME, &t);
      AudioDevice::CLOCKBASE = time_to_double(t);
#endif
    }else{
      AudioDevice::CLOCKBASE = rvalue(pair_cadr(args));
    }
    UNIV::TIME = ivalue(pair_car(args));
    return _sc->T;
  } 
#endif

} // end namespace

