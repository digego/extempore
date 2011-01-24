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

#ifndef _EXTLLVM_H
#define _EXTLLVM_H

#include "Scheme.h"

typedef struct _llvm_zone_t {
	void* memory;
	uint64_t offset;
	uint64_t size;
} llvm_zone_t;

llvm_zone_t* llvm_zone_create(uint64_t size);
llvm_zone_t* llvm_zone_default();
void llvm_zone_destroy(llvm_zone_t* zone);
void* llvm_zone_malloc(llvm_zone_t* zone, uint64_t size);


namespace llvm {
	class Module;
	class ModuleProvider;
	class ExecutionEngine;
	class PassManager;
} // end llvm namespace

namespace extemp {

	class EXTLLVM {
	public:
		EXTLLVM();
		~EXTLLVM();
		static EXTLLVM* I() { return &SINGLETON; }	
	
		void initLLVM();
	
		static int64_t LLVM_COUNT;
		static bool OPTIMIZE_COMPILES;	
		
		llvm::Module* M;
		llvm::ModuleProvider* MP;
		llvm::ExecutionEngine* EE;
		llvm::PassManager* PM;		

	private:
		static EXTLLVM SINGLETON;
	};

} // end extemp namespace

#endif