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
#include "sys/mman.h"


#include <sstream>
#include <string.h>
#include <dlfcn.h>

#define PCRE_REGEX

#ifdef PCRE_REGEX
#include <pcre.h>
#else
#include <oniguruma.h>
#endif

#ifdef TARGET_OS_MAC
#include <malloc/malloc.h>
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
#include "llvm/Target/TargetSelect.h"
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

#define PRINT_ERROR(format, args...) \
    ascii_text_color(1,1,10); \
    printf(format , ## args); \
    ascii_text_color(0,9,10)


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

void ascii_text_color(int attr, int fg, int bg)
{
    char command[13];
    /* Command is the control command to the terminal */
    sprintf(command, "%c[%d;%d;%dm", 0x1B, attr, fg + 30, bg + 40);
    printf("%s", command);
}

#define nelem(table) sizeof(table) / sizeof(table[0])

namespace extemp {
	
    SchemeFFI SchemeFFI::SINGLETON;

    void SchemeFFI::initSchemeFFI(scheme* sc)
    {
	int i;

	static struct {
		const char *name;
		uint32_t    value;
	} integerTable[] = {
		{ "*au:block-size*",	UNIV::FRAMES },
		{ "*au:samplerate*",	UNIV::SAMPLERATE },
		{ "*au:channels*",	UNIV::CHANNELS },
	};

	static struct {
		const char * name;
		foreign_func func;
	} funcTable[] = {
		{ "ascii-print-color",		&SchemeFFI::asciiColor },

		//IPC stuff
		{ "ipc:new",			&SchemeFFI::newSchemeProcess },
		{ "ipc:connect",		&SchemeFFI::connectToProcess },
		{ "ipc:call-async",		&SchemeFFI::ipcCall },
		{ "ipc:define",			&SchemeFFI::ipcDefine },
		{ "ipc:eval-string",		&SchemeFFI::ipcEval },
		{ "ipc:load",			&SchemeFFI::ipcLoad },
		{ "ipc:get-process-name",	&SchemeFFI::getNameOfCurrentProcess },

		// number stuff
		{ "random-real",		&SchemeFFI::randomReal },
		{ "random-int",			&SchemeFFI::randomInt },
		{ "real->integer",		&SchemeFFI::realToInteger },
		{ "real->rational",		&SchemeFFI::realToRational },
		{ "rational->real",		&SchemeFFI::rationalToReal },
		{ "integer->real",		&SchemeFFI::integerToReal },

		// sys stuff
		{ "sys:pointer-size",		&SchemeFFI::pointerSize },
		{ "sys:open-dylib",		&SchemeFFI::openDynamicLib },
		{ "sys:close-dylib",		&SchemeFFI::closeDynamicLib },

		// DSP sys stuff
		{ "sys:set-dsp-closure",	&SchemeFFI::setDSPClosure },
		{ "sys:set-dsp-wrapper",	&SchemeFFI::setDSPWrapper },
		{ "sys:set-dsp-wrapper-array",	&SchemeFFI::setDSPWrapperArray },

		// memory zone stuff
		{ "sys:create-mzone",		&SchemeFFI::createMallocZone },
		{ "sys:default-mzone",		&SchemeFFI::defaultMallocZone },
		{ "sys:destroy-mzone",		&SchemeFFI::destroyMallocZone },
		{ "sys:copy-to-dmzone",		&SchemeFFI::copyToDefaultZone },

		// misc stuff
		{ "string-strip",		&SchemeFFI::stringStrip },
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

		// regex stuff
		{ "regex:match?",		&SchemeFFI::regex_match },
		{ "regex:matched",		&SchemeFFI::regex_matched },
		{ "regex:match-all",		&SchemeFFI::regex_match_all },
		{ "regex:split",		&SchemeFFI::regex_split },
		{ "regex:replace",		&SchemeFFI::regex_replace },

		// llvm stuff
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
		{ "llvm:run",				&SchemeFFI::call_compiled },
		{ "llvm:run-closure",			&SchemeFFI::call_compiled_closure },
		{ "llvm:convert-float",			&SchemeFFI::llvm_convert_float_constant },
		{ "llvm:count",				&SchemeFFI::llvm_count },
		{ "llvm:count++",			&SchemeFFI::llvm_count_inc },
		{ "llvm:call-closure",			&SchemeFFI::callClosure },
		{ "llvm:print",				&SchemeFFI::printLLVMModule },
		{ "llvm:print-function",		&SchemeFFI::printLLVMFunction },
		{ "llvm:bind-symbol",			&SchemeFFI::bind_symbol },
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
	
    void SchemeFFI::addGlobalCptr(scheme* sc, char* symbol_name, void* ptr)
    {
	scheme_define(sc, sc->global_env, mk_symbol(sc, symbol_name), mk_cptr(sc, ptr));		
    }
	
    ///////////////////////////////////////////////////////
    //
    // MISC STUFF
    //
    //////////////////////////////////////////////////////

    pointer SchemeFFI::asciiColor(scheme* _sc, pointer args)
    {
	ascii_text_color(ivalue(pair_car(args)),ivalue(pair_cadr(args)),ivalue(pair_caddr(args)));
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
	    else if(pair_car(args) == _sc->F) {
		ss << "#f";
	    }
	    else if(pair_car(args) == _sc->T) {
		ss << "#t";
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

    pointer SchemeFFI::getNameOfCurrentProcess(scheme* _sc, pointer args)
    {
	const char* name = SchemeProcess::I(_sc)->getName().c_str();
	if(args == _sc->NIL) return mk_string(_sc, name);
	else { printf("Error getting name of current process\n"); return _sc->F; }
	/*
	NSDictionary* dict = (NSDictionary*) objc_value(pair_car(args));
	NSString* alias_name = [dict objectForKey:[NSString stringWithCString:name]];
	if(alias_name == NULL) return mk_string(_sc, name);
	return mk_string(_sc, [alias_name UTF8String]);
	*/
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
	void* lib_handle = dlopen(string_value(pair_car(args)), RTLD_LAZY);
	if (!lib_handle)
	    {
		fprintf(stderr, "%s\n", dlerror());
		return _sc->F;
	    }
	return mk_cptr(_sc,lib_handle);
    }

    pointer SchemeFFI::closeDynamicLib(scheme* _sc, pointer args)
    {
	dlclose(cptr_value(pair_car(args)));
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
	len += (list_length(_sc,array)-1)*(strlen(joinstr));
	char* result = (char*) alloca(len+1);
	memset(result,0,len+1);
	array = pair_car(args); // reset array
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
	ascii_text_color(0,9,10);	
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
	ascii_text_color(0,9,10);		
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
		    list = cons(_sc,mk_string(_sc,""),list);
		    _sc->imp_env->erase(list);
		}else{
		    int range = ovector[p+1] - ovector[p];				
		    char* b = (char*) alloca(range+1);
		    memset(b,0,range+1);
		    char* a = data+ovector[p];
		    char* substring = strncpy(b, a, range);
		    _sc->imp_env->insert(list);
		    list = cons(_sc,mk_string(_sc,substring),list);
		    _sc->imp_env->erase(list);
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
	    list = cons(_sc,mk_string(_sc,substring),list);
	    _sc->imp_env->erase(list);
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
	    list = cons(_sc,mk_string(_sc,substring),list);
	    _sc->imp_env->erase(list);
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
		    list = cons(_sc,mk_string(_sc,substr),list);
		    _sc->imp_env->erase(list);
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
    void SchemeFFI::destroyMallocZoneWithDelay(TaskI* task)
    {
	Task<llvm_zone_t*>* t = static_cast<Task<llvm_zone_t*>*>(task);
	llvm_zone_t* zone = t->getArg();
	llvm_zone_destroy(zone);
    }

    pointer SchemeFFI::createMallocZone(scheme* _sc, pointer args)
    {
	return mk_cptr(_sc,llvm_zone_create(1024*10));
    }

    pointer SchemeFFI::defaultMallocZone(scheme* _sc, pointer args)
    {
	return mk_cptr(_sc,SchemeProcess::I(_sc)->getDefaultZone()); //llvm_zone_default());
    }
	
    pointer SchemeFFI::destroyMallocZone(scheme* _sc, pointer args)
    {		
	llvm_zone_t* ptr = (llvm_zone_t*) cptr_value(pair_car(args));
	llvm_zone_destroy(ptr);
	return _sc->T;
    }

    pointer SchemeFFI::copyToDefaultZone(scheme* _sc, pointer args)
    {		
	return _sc->NIL;
    }
	
    ////////////////////////////////////////////
    //
    // LLVM STUFF
    //
    /////////////////////////////////////////////
	
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
	EXTLLVM::I()->EE->updateGlobalMapping(gv,ptrptr);
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

	pointer str = mk_string(_sc, func->getFunctionType()->getDescription().c_str());
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

	pointer str = mk_string(_sc, func->getReturnType()->getDescription().c_str());
	pointer p = cons(_sc, str, _sc->NIL); 

	Function::ArgumentListType::iterator funcargs = func->getArgumentList().begin();
	while(funcargs != func->getArgumentList().end())
	    {			
		Argument* a = funcargs;
		_sc->imp_env->insert(p);						
		pointer str = mk_string(_sc, a->getType()->getDescription().c_str());
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
	extemp::EXTLLVM::I()->EE->freeMachineCodeForFunction(func);
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
	return mk_string(_sc, var->getType()->getDescription().c_str());
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

	void* p = EXTLLVM::I()->EE->getPointerToFunction(func);

	if(p==NULL) {
	    //[[LogView sharedInstance] error:@"LLVM: Bad Function Ptr\n"];
	    return _sc->F;
	}

	return mk_cptr(_sc, p);
    }		
	
    pointer SchemeFFI::recompile_and_link_function(scheme* _sc, pointer args)
    {
	using namespace llvm;
		
	Module* M = EXTLLVM::I()->M;
	llvm::Function* func = M->getFunction(std::string(string_value(pair_car(args))));
	//func->setCallingConv(CallingConv::C); //kCStackBased);
	if(func == 0)
	    {
		return _sc->F;
	    }
		
	void* p = EXTLLVM::I()->EE->recompileAndRelinkFunction(func);
		
	if(p==NULL) {
	    return _sc->F;
	}
		
	//const llvm::GlobalValue* f2 = NativeScheme::I()->EE->getGlobalValueAtAddress(p);
	//std::cout << "FUNC: " << func << "  fptr: " << p << "   f2: " << f2 << std::endl;
		
	return mk_cptr(_sc, p);
    }

    pointer SchemeFFI::call_compiled_closure(scheme* _sc, pointer args)
    {
	using namespace llvm;
	uint32_t** closure = (uint32_t**) cptr_value(pair_car(args));
	char* fname = (char*) *(closure+0);
	void* eptr = (void*) *(closure+1);		

	//std::cout << "CALL COMPILED CLOSURE: " << fname << std::endl;

	ExecutionEngine* EE = EXTLLVM::I()->EE;		
	Module* M = EXTLLVM::I()->M;
	llvm::Function* func = M->getFunction(std::string(fname));
	if(func == 0)
	    {
		printf("No such function\n");
		return _sc->F;
	    }				
	//std::cout << "FUNC: " << *func << std::endl;

	int lgth = list_length(_sc, args);
	if(lgth != func->arg_size()) {
	    printf("Wrong number of arguments supplied to native closure - should take: %d\n",(func->arg_size()-1));
	    return _sc->F;			
	}
	Function::ArgumentListType::iterator funcargs = func->getArgumentList().begin();		
	std::vector<llvm::GenericValue> fargs(lgth);
	// first add the environment
	// which must always go first for a closure
	if(((Argument*)funcargs)->getType()->getTypeID() != Type::PointerTyID)
	    {
		printf("Error: closure must have an environment!\n");
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
			//std::cout << "PTRVALUE: " << cptr_value(p) << std::endl; 				
			fargs[i].PointerVal = cptr_value(p);
		    }
		else 
		    {
			printf("Bad scheme argument\n");
			return _sc->F;
		    }
	    }

	long long num_of_funcs = M->getFunctionList().size();
	GenericValue gv = EE->runFunction(func,fargs);
	if(num_of_funcs != M->getFunctionList().size()) {
	    iplist<Function>::iterator iter = M->getFunctionList().end();
	    Function* stub = dyn_cast<Function>(--iter);
	    //			std::stringstream ss;
	    //			ss << *stub << std::endl;
	    EE->freeMachineCodeForFunction(stub);
	    stub->deleteBody();
	    stub->eraseFromParent();
	}

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

    pointer SchemeFFI::call_compiled(scheme* _sc, pointer args)
    {
	using namespace llvm;

	ExecutionEngine* EE = EXTLLVM::I()->EE;	
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

	//		double(*fp)(double	,double) = (double(*)(double,double)) EE->getPointerToFunction(func);
	//		double v = fp(rvalue(pair_car(args)),rvalue(pair_cadr(args)));

	long long num_of_funcs = M->getFunctionList().size();
	GenericValue gv = EE->runFunction(func,fargs);
	// if the number of functions in module has changed when calling runFunction 
	// then we assume a stub was made and appended to the end of the modules function list.
	// we remove this function now that we no longer need it!
	if(num_of_funcs != M->getFunctionList().size()) {
	    iplist<Function>::iterator iter = M->getFunctionList().end();
	    Function* stub = dyn_cast<Function>(--iter);
	    EE->freeMachineCodeForFunction(stub);
	    stub->deleteBody();
	    stub->eraseFromParent();
	}

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
	float f = strtof(floatin, (char**) &floatout);
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
		
	void* ptr = dlsym(library, symname);
	if(!ptr) {
	    printf("Could not find symbol named %s!\n",symname);
	    return _sc->F;
	}
	llvm::GlobalValue* gv = M->getNamedValue(std::string(symname));
	EE->updateGlobalMapping(gv,ptr);
		
	return _sc->T;
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
		
} // end namespace

