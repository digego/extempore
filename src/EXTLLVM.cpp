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

#include "stdarg.h"
#include "EXTLLVM.h"
#include "UNIV.h"
#include "SchemeFFI.h"
#include "TaskScheduler.h"

#include "llvm/Assembly/Parser.h"
#include "llvm/LLVMContext.h"
#include "llvm/CallingConv.h"
#include "llvm/Module.h"
#include "llvm/Constants.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Instructions.h"

#include "llvm/Target/TargetSelect.h"

//#include "llvm/ModuleProvider.h"

#include "llvm/ExecutionEngine/JIT.h"
// #include "llvm/ExecutionEngine/Interpreter.h"
// #include "llvm/ExecutionEngine/GenericValue.h"
#include "llvm/Target/TargetOptions.h"
// #include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/SourceMgr.h"
// #include "llvm/Analysis/Verifier.h"
#include "llvm/Target/TargetData.h"
#include "llvm/LinkAllPasses.h"
#include "llvm/PassManager.h"

#include "llvm/ExecutionEngine/JIT.h"
#include "llvm/ExecutionEngine/Interpreter.h"
#include "llvm/ExecutionEngine/GenericValue.h"

#include "SchemeProcess.h"

llvm_zone_t* llvm_zone_create(uint64_t size)
{
	llvm_zone_t* zone = (llvm_zone_t*) malloc(sizeof(llvm_zone_t));
	zone->memory = malloc((size_t) size);
	zone->offset = 0;
	zone->size = size;
	//printf("CreateZone: %p:%p:%lld:%lld\n",zone,zone->memory,zone->offset,zone->size);
	return zone;
}

void llvm_zone_destroy(llvm_zone_t* zone)
{
	//printf("DestroyZone: %p:%p:%lld:%lld\n",zone,zone->memory,zone->offset,zone->size);	
	free(zone->memory);
	free(zone);
	return;
}

void* llvm_zone_malloc(llvm_zone_t* zone, uint64_t size)
{
	//printf("MallocZone: %p:%p:%lld:%lld\n",zone,zone->memory,zone->offset,zone->size);
	if(zone->offset+size >= zone->size)
	{
		// for the moment print a warning and just leak the memory
		printf("Zone full ... leaking %lld bytes\n",size);
		return malloc((size_t)size);
	}
	void* newptr = (void*)(((char*)zone->memory)+zone->offset);
	zone->offset += size; 
	return newptr;
}

extemp::CM* DestroyMallocZoneWithDelayCM = mk_cb(extemp::SchemeFFI::I(),extemp::SchemeFFI,destroyMallocZoneWithDelay);
void llvm_destroy_zone_after_delay(llvm_zone_t* zone, double delay)
{
	//printf("destroyWithDelay %p\n",zone);
	extemp::CM* cb = DestroyMallocZoneWithDelayCM; //mk_cb(extemp::SchemeFFI::I(),extemp::SchemeFFI,destroyMallocZoneWithDelay);
	extemp::Task<llvm_zone_t*>* task = new extemp::Task<llvm_zone_t*>(extemp::UNIV::TIME,44100,cb,zone);
	extemp::TaskScheduler::I()->add(task);
}

int llvm_printf(char* format, ...)
{
	va_list ap;
	va_start(ap,format);
	char* ret = (char*) alloca(2048);
	int returnval = vasprintf(&ret, format, ap);
	printf("%s\n",ret);
	va_end(ap);
	return returnval;
}

long long llvm_get_next_prime(long long start)
{
	long long  how_many = start+100000;
	long long  *array = (long long*) calloc(how_many, sizeof(long long));
	long long  i, prime, multiple;
	/*  mark each int as potentially prime */
	for (i=0; i<how_many; i++)
		array[i] = 1;
	/* special cases: 0, 1 not considered prime */
	array[0] = array[1] = 0;
	/* foreach starting prime, mark every multiple as non-prime */
	prime = 0;
	while (1) {
		/* skip non-primes to find first prime */
		for (; (prime < how_many) && (!array[prime]); ++prime)
			continue;
		if (prime >= how_many)
			break;
		for (multiple=2*prime; multiple<how_many; multiple+=prime) {
			array[multiple] = 0;
		}
		++prime;
	}
	/* Now that we have marked all multiple of primes as non-prime, */
	/* print the remaining numbers that fell through the sieve, and */
	/* are thus prime */
	for (i=start+1; i<how_many; i++) {
		if(array[i]) return i;
	}
	return -1;
}


namespace extemp {
	
	EXTLLVM EXTLLVM::SINGLETON;
	int64_t EXTLLVM::LLVM_COUNT = 0l;
	bool EXTLLVM::OPTIMIZE_COMPILES = 0;
	
	EXTLLVM::EXTLLVM()
	{
		//printf("making llvm !!!!!!!!!!!!!!!!!!\n");
		M = 0;
		MP = 0;
		EE = 0;
		//initLLVM();
	}
	
	EXTLLVM::~EXTLLVM() {}
	
	void EXTLLVM::initLLVM()
	{
		if(M == 0) { // Initalize Once Only (not per scheme process)			
			bool result = llvm::InitializeNativeTarget();			
			M = new llvm::Module("JIT",llvm::getGlobalContext());
			// Create the JIT.
			std::string ErrStr;
		  	EE = llvm::EngineBuilder(M).setErrorStr(&ErrStr).create();
		  	if (!EE) {
		    	fprintf(stderr, "Could not create ExecutionEngine: %s\n", ErrStr.c_str());
		    	exit(1);
		  	}
			
			//EE = llvm::EngineBuilder(M).create();
			PM = new llvm::PassManager();
			PM->add(new llvm::TargetData(*EE->getTargetData()));
			// Do simple "peephole" optimizations and bit-twiddling optzns.
			PM->add(llvm::createInstructionCombiningPass());
			// Reassociate expressions.
			PM->add(llvm::createReassociatePass());
			// Eliminate Common SubExpressions.
			PM->add(llvm::createGVNPass());
			// Simplify the control flow graph (deleting unreachable blocks, etc).
			PM->add(llvm::createCFGSimplificationPass());
			// Function inlining
			PM->add(llvm::createFunctionInliningPass());
			// loop invariants
			PM->add(llvm::createLICMPass());
			// vars
			PM->add(llvm::createIndVarSimplifyPass());
			
			//llvm::PerformTailCallOpt = true;
			llvm::GuaranteedTailCallOpt = true;
			llvm::llvm_start_multithreaded();
			
			char fname[] = "/code.ir";
			char load_path[256];
			strcpy(load_path,extemp::UNIV::PWD);
			strcat(load_path,fname);
			FILE* fp;
			if((fp = fopen(load_path,"r")) == NULL)
			{
				printf("Could not open %s",load_path);
				exit(1);
			}
			
			fseek(fp,0,SEEK_END);
			int size = ftell(fp);
			fseek(fp,0,SEEK_SET);
			char* assm = (char*) alloca(size+1);
			int res = fread(assm, 1, size, fp);
			fclose(fp);
			llvm::SMDiagnostic pa;
			llvm::Module* newM = ParseAssemblyString(assm, M, pa, llvm::getGlobalContext());
			
			if(newM == 0)
			{
				std::string errstr;
				llvm::raw_string_ostream ss(errstr);
				pa.Print("Extempore",ss);
				printf(ss.str().c_str());
			}

			llvm::GlobalValue* gv = M->getNamedValue(std::string("llvm_destroy_zone_after_delay"));
			EE->updateGlobalMapping(gv,(void*)&llvm_destroy_zone_after_delay);			
			gv = M->getNamedValue(std::string("next_prime"));
			EE->updateGlobalMapping(gv,(void*)&llvm_get_next_prime);			
			gv = M->getNamedValue(std::string("llvm_printf"));
			EE->updateGlobalMapping(gv,(void*)&llvm_printf);									
			gv = M->getNamedValue(std::string("llvm_zone_create"));
			EE->updateGlobalMapping(gv,(void*)&llvm_zone_create);						
			gv = M->getNamedValue(std::string("llvm_zone_destroy"));
			EE->updateGlobalMapping(gv,(void*)&llvm_zone_destroy);						
			gv = M->getNamedValue(std::string("llvm_zone_malloc"));
			EE->updateGlobalMapping(gv,(void*)&llvm_zone_malloc);						
			
		}	
	 	return;
	}
}
