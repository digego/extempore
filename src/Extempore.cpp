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

#include "UNIV.h"
#include "SchemeProcess.h"
#include "AudioDevice.h"
#include "TaskScheduler.h"
#include "EXTLLVM.h"

// need to init glut on linux
#ifdef TARGET_OS_LINUX
#include "GL/glut.h"
#endif

int main(int argv, char** args)
{
	if(argv<2) {
		printf("Must include inital path to read libs from\n");
		return -1;
	}
	
	bool with_banner = 0;
	
	if(argv>2) {
		with_banner = 1;
	}
	
#ifdef TARGET_OS_LINUX
	glutInit(&argv,args);
#endif
	extemp::UNIV::PWD = args[1];
	extemp::EXTLLVM::I()->initLLVM();
	extemp::SchemeProcess* sp = new extemp::SchemeProcess(std::string(args[1]), std::string("primary process"), 7099, with_banner);
	extemp::AudioDevice* dev = extemp::AudioDevice::I();

	sp->start();
	dev->start();
	
	// sleep indefiniately let server process do the work
	while(1) {
		printf("TIME: %lld\n",extemp::UNIV::TIME);
		sleep(5000);
	}
	return 0;
}