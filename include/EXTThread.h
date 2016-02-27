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

#ifndef EXT_THREAD
#define EXT_THREAD

#ifdef _WIN32
#include <thread>
#include <functional>
#else
#include "pthread.h"
#endif


namespace extemp
{
    class EXTThread
    {
    public:
	EXTThread();
#ifdef _WIN32
	EXTThread(std::thread&& _bthread);
#else
	EXTThread(pthread_t _pthread);
#endif
	~EXTThread();

	int create(void *(*start_routine)(void *), void *arg);
	int kill();
	int detach();
	int join();
	bool isRunning();
	bool isCurrentThread();
	int setPriority(int, bool);
	int getPriority(); //doesn't say if it's realtime or not
	bool isEqualTo(EXTThread* other_thread);
#ifdef _WIN32
	std::thread& getBthread();
#else
	pthread_t getPthread();
#endif

    protected:
	bool initialised;
	bool detached;
	bool joined;
	bool cancelled;			
#ifdef _WIN32
	std::thread bthread;
#else
	pthread_t pthread;
#endif
    };
} //End Namespace

#endif
