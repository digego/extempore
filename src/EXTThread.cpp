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

#include "EXTThread.h"

#ifdef TARGET_OS_WINDOWS
#include <Windows.h>
#endif

#define _EXTTHREAD_DEBUG_


namespace extemp
{
    EXTThread::EXTThread() : initialised(false), detached(false), joined(false)
    {       
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
#ifdef EXT_BOOST
            bthread = boost::thread(start_routine, arg);
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


    int EXTThread::detach()
    {
	int result = 22; //EINVAL;

	if (initialised)
	{
#ifdef EXT_BOOST
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
#ifdef EXT_BOOST
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
	
    //BE AWARE THAT THIS WILL ONLY TERMINATE THE THREAD AT CERTAIN PREDETEMINED POINTS (LIKE WAIT POINTS)
    //READ pthread_cancel BEFORE MAKING ASSUMPTIONS.  (usually need a pthread_testcancel or pthread_cond_(timed)wait
    int EXTThread::cancel()
    {
	int result = 22; //EINVAL
		
	if(initialised)
	{
#ifdef EXT_BOOST
	    bthread.interrupt();
            result = 0;
#else
	    result = pthread_cancel(pthread);
#endif
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

    int EXTThread::setPriority(int priority)
    {
#ifdef EXT_BOOST
#ifdef TARGET_OS_WINDOWS
        return (int) SetThreadPriority(bthread.native_handle(),priority);
#else
	return 0;
#endif
	return 0;
#else

#endif
    }

    int EXTThread::getPriority()
    {
#ifdef EXT_BOOST
#ifdef TARGET_OS_WINDOWS
        return (int) GetThreadPriority(bthread.native_handle());
#else
	return 0;
#endif
	return 0;
#else

#endif
    }
	
    bool EXTThread::isRunning() 
    { 
#ifdef EXT_BOOST
      return initialised;
#else
	return 0 != pthread; 
#endif
    }	
	
    bool EXTThread::isCurrentThread()
    {
#ifdef EXT_BOOST
        return (bthread.get_id() == boost::this_thread::get_id());
#else
	return pthread_equal(pthread_self(), pthread);
#endif
    }
	
#ifdef EXT_BOOST
  boost::thread& EXTThread::getBthread()
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
