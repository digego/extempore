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

#include <iostream>

#include "pthread.h"

#include "EXTThread.h"


#define _EXTTHREAD_DEBUG_


namespace extemp
{
    EXTThread::EXTThread() : initialised(false), detached(false), joined(false)
    {
//		initialised = false;
//		detached = false;
//		joined = false;
    }

    EXTThread::~EXTThread()
    {		
#ifdef _EXTTHREAD_DEBUG_
	if (initialised && (! (detached || joined)))
	{
	    std::cerr << "Resource leak destroying EXTThread: creator has not joined nor detached thread." << std::endl;
	}
#endif
    }

    int EXTThread::create(void *(*start_routine)(void *), void *arg)
    {
	int result = 22; //EINVAL;

	if (! initialised)
	{
	    result = pthread_create(&pthread, NULL, start_routine, arg);
	    initialised = ! result;
	}

#ifdef _EXTTHREAD_DEBUG_
	if (result)
	{
	    std::cerr << "Error creating thread: " << result << std::endl;
	}
#endif

	return result;
    }    


    int EXTThread::detach()
    {
	int result = 22; //EINVAL;

	if (initialised)
	{
	    result = pthread_detach(pthread);
	    detached = ! result;
	}

#ifdef _EXTTHREAD_DEBUG_
	if (result)
	{
	    std::cerr << "Error detaching thread: " << result << std::endl;
	}
#endif

	return result;
    }

    int EXTThread::join()
    {
	int result = 22; //EINVAL;

	if (initialised)
	{
	    result = pthread_join(pthread, NULL);
	    joined = ! result;
	}

#ifdef _EXTTHREAD_DEBUG_
	if (result)
	{
	    std::cerr << "Error joining thread: " << result << std::endl;
	}
#endif

	return result;
    }
	
    //BE AWARE THAT THIS WILL ONLY TERMINATE THE THREAD AT CERTAIN PREDETEMINED POINTS (LIKE WAIT POINTS)
    //READ pthread_cancel BEFORE MAKING ASSUMPTIONS.  (usually need a pthread_testcancel or pthread_cond_(timed)wait
    int EXTThread::cancel()
    {
	int result = 22; //EINVAL
		
	if(initialised)
	{
	    result = pthread_cancel(pthread);
	    cancelled = ! result;
	}
		
#ifdef _EXTTHREAD_DEBUG_
	if (result)
	{
	    std::cerr << "Error joining thread: " << result << std::endl;
	}
#endif
	return result;
		
    }
	
    bool EXTThread::isRunning() 
    { 
	return 0 != pthread; 
    }	
	
    bool EXTThread::isCurrentThread()
    {
	return pthread_equal(pthread_self(), pthread);
    }
	
    pthread_t EXTThread::getPthread()
    {
	return pthread;
    }
}
