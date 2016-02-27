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

#include <iostream>
#include <stdio.h>
#include <string.h>

#include "UNIV.h"
#include "EXTThread.h"

#ifdef _WIN32
#include <Windows.h>
#elif __APPLE__
#include <mach/thread_policy.h>
#include <mach/thread_act.h>
#endif

#define _EXTTHREAD_DEBUG_


namespace extemp
{
    EXTThread::EXTThread() : initialised(false), detached(false), joined(false)
    {       
    }

#ifdef _WIN32
	EXTThread::EXTThread(std::thread&& _bthread) : initialised(false), detached(false), joined(false), bthread{ std::move(_bthread) }
	{
	}
#else
    EXTThread::EXTThread(pthread_t _pthread) : initialised(false), detached(false), joined(false), pthread{ _pthread }
    {
    }
#endif

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
#ifdef _WIN32
    // std::function<void*(void*)> fn = static_cast<std::function<void*(void*)> >(start_routine);
    std::function<void*()> fn = [start_routine,arg]()->void* { return start_routine(arg); };
    bthread = std::thread(fn);
    result = 0;
#else
	    result = pthread_create(&pthread, NULL, start_routine, arg);
#endif
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

	int EXTThread::kill()
	{
#ifdef _WIN32
		return 0;
#else
		return pthread_cancel(pthread);
#endif
	}


    int EXTThread::detach()
    {
	int result = 22; //EINVAL;

	if (initialised)
	{
#ifdef _WIN32
	    bthread.detach();
	    result = 0;
#else
	    result = pthread_detach(pthread);
#endif
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
#ifdef _WIN32
	    bthread.join();
            result = 0;
#else
	    result = pthread_join(pthread, NULL);
#endif
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

  int EXTThread::setPriority(int priority, bool realtime)
  {
#ifdef _WIN32
    auto thread = bthread.native_handle();
#else
	pthread_t thread = pthread;
#endif
#ifdef __linux__
    sched_param param;
    int policy;
    pthread_getschedparam(thread,&policy,&param);
    param.sched_priority = priority;

    // for realtime threads, use SCHED_RR policy
    if(realtime)
      policy = SCHED_RR;
    
    int result = pthread_setschedparam(thread,policy,&param);
    if(result != 0) {
      fprintf(stderr, "Error: failed to set thread priority: %s\n", strerror(result));
      return 0;
    }else{
      return 1;
    }
#elif __APPLE__    
    struct thread_time_constraint_policy ttcpolicy;
    int result;
    
    // OSX magic numbers
    ttcpolicy.period=(uint32_t)(UNIV::SAMPLERATE/100); // HZ/160
      ttcpolicy.computation=(uint32_t)(UNIV::SAMPLERATE/143); // HZ/3300;
    ttcpolicy.constraint=(uint32_t)(UNIV::SAMPLERATE/143); // HZ/2200;
    ttcpolicy.preemptible=1; // 1 

    result = thread_policy_set(pthread_mach_thread_np(thread),
                               THREAD_TIME_CONSTRAINT_POLICY,
                               (thread_policy_t)&ttcpolicy,
                               THREAD_TIME_CONSTRAINT_POLICY_COUNT);
    if (result != KERN_SUCCESS)
      {
        fprintf(stderr, "Error: failed to set thread priority: %s\n", strerror(result));
        return 0;
      }else{
        return 1;
      }
#else
    fprintf(stderr, "Error: cannot set thread priority on Windows\n");
    return 0;
#endif
  }

  int EXTThread::getPriority()
  {
#ifdef __linux__ 
    int policy;
    sched_param param;
    pthread_getschedparam(pthread,&policy,&param);
    return param.sched_priority;
#endif
    // fprintf(stderr, "Error: thread priority only available Linux\n");    
    return 0;
  }
  
    bool EXTThread::isRunning() 
    { 
#ifdef _WIN32
      return initialised;
#else
	return 0 != pthread; 
#endif
    }	
	
    bool EXTThread::isCurrentThread()
    {
#ifdef _WIN32
      return (bthread.get_id() == std::this_thread::get_id());
#else
	return pthread_equal(pthread_self(), pthread);
#endif
    }

	bool EXTThread::isEqualTo(EXTThread* other_thread)
	{
#ifdef _WIN32
		return bthread.get_id() == other_thread->getBthread().get_id();		
#else
		return pthread_equal(pthread, other_thread->getPthread());
#endif  
	}

#ifdef _WIN32
  std::thread& EXTThread::getBthread()
    {
	return bthread;
    }
#else
    pthread_t EXTThread::getPthread()
    {
	return pthread;
    }
#endif
}
