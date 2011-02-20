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
#include "SchemeREPL.h"
#include "EXTLLVM.h"
#include <string>

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

    std::string host("localhost");
    std::string primary_name("primary");
    std::string utility_name("utility");
    int primary_port = 7099;
    int utility_port = 7098;
	

    extemp::UNIV::PWD = args[1];
    extemp::EXTLLVM::I()->initLLVM();
    extemp::SchemeProcess* primary = new extemp::SchemeProcess(std::string(args[1]), primary_name, primary_port, with_banner);
    extemp::SchemeProcess* utility = new extemp::SchemeProcess(std::string(args[1]), utility_name, utility_port, 0);
    extemp::AudioDevice* dev = extemp::AudioDevice::I();

    primary->start();
    utility->start();
    dev->start();

    extemp::SchemeREPL* primary_repl = new extemp::SchemeREPL(primary_name);
    primary_repl->connectToProcessAtHostname(host,primary_port);
    extemp::SchemeREPL* utility_repl = new extemp::SchemeREPL(utility_name);
    utility_repl->connectToProcessAtHostname(host,utility_port);
	
    // sleep indefiniately let server process do the work
    while(1) {
	// put printf here as a reminder that something is happending
	printf("PING: %lld\n",extemp::UNIV::TIME);
	sleep(5000);
    }
    return 0;
}
