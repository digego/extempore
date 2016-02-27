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
#include <stdlib.h>
#include "EXTMutex.h"
#include <cstring>


#define _EXTMUTEX_DEBUG_
#define EXT_MUTEX_RECURSIVE 0

namespace extemp
{
    EXTMutex::EXTMutex(std::string _name) :
	initialised(false),
	name(_name)
    {
    }
    
    EXTMutex::~EXTMutex()
    {
        destroy();
    }
    

    int EXTMutex::init()
    {

#ifdef EXT_BOOST  // START BOOST
        int result = 0;
#else // START POSIX
        pthread_mutexattr_t pthread_mutex_attr;
	pthread_mutexattr_init(&pthread_mutex_attr);
     
#ifdef _EXTMUTEX_DEBUG_
        pthread_mutexattr_settype(&pthread_mutex_attr, PTHREAD_MUTEX_ERRORCHECK);
#else
        pthread_mutexattr_settype(&pthread_mutex_attr, PTHREAD_MUTEX_NORMAL);
#endif

#if EXT_MUTEX_RECURSIVE
	pthread_mutexattr_settype(&pthread_mutex_attr, PTHREAD_MUTEX_RECURSIVE);
#endif
	int result = pthread_mutex_init(&pthread_mutex, &pthread_mutex_attr);
#endif // END POSIX

        initialised = ! result;

#ifdef _EXTMUTEX_DEBUG_
        if (result)
        {
            std::cerr << "Error initialising mutex: " << name << " err: " << result << std::endl;
        }
#endif   
        return result;
    }

    
    void EXTMutex::destroy()
    {
        int result = 0;
        
        if (initialised)
        {
            initialised = false;
#ifdef EXT_BOOST
            result = 0;
#else
            result = pthread_mutex_destroy(&pthread_mutex);
#endif
        }
		
#ifdef _EXTMUTEX_DEBUG_
        if (result)
        {
            std::cerr << "Error destroying mutex: " << name << " err: " << result << std::endl;
        }
#endif
    }


    bool EXTMutex::isOwnedByCurrentThread()
    {
#ifdef EXT_BOOST
	return false;
#else
	return pthread_equal(pthread_self(), owner);
#endif 

    }

#ifdef EXT_BOOST
	int EXTMutex::lock()
	{  
	  try{
	    bmutex.lock();
	    return true;
	  }catch(std::exception& e){
	    std::cout << "Problem locking mutex: " << name << " " << e.what() << std::endl;
	    return false;
	  }
	}
	
    int EXTMutex::unlock()
    {
		
      try{
	bmutex.unlock();
	return 0;
      }catch(std::exception& e){
	std::cout << "Problem unlocking mutex: " << name << " " << e.what() << std::endl;
	return 0;
      }
    }
#elif EXT_MUTEX_RECURSIVE
    int EXTMutex::unlock()
    {
	int result = pthread_mutex_unlock(&pthread_mutex);
#ifdef _EXTMUTEX_DEBUG_
        if (result)
        {
            std::cerr << "Error unlocking mutex: " << name << " err: " << result << std::endl;
        }
#endif
	return result;
    }
	
    int EXTMutex::lock()
    {
	int result = pthread_mutex_lock(&pthread_mutex);
#ifdef _EXTMUTEX_DEBUG_
        if (result)
        {
            std::cerr << "Error locking mutex: " << name << " err: " << result << std::endl;
        }
#endif
	return true;		
    }
#else
    int EXTMutex::lock()
    {
	pthread_t current = pthread_self();
	if(!pthread_equal(current,owner)) {
	    int result = pthread_mutex_lock(&pthread_mutex);
	    if(result==0) owner = current; 
#ifdef _EXTMUTEX_DEBUG_
	    if (result)
	    {
		std::cerr << "Error locking mutex: " << name << " err: " << result << std::endl;
	    }
#endif
	    return true;
	}else{
	    return false;
	}
    }
	
    int EXTMutex::unlock()
    {
	if(pthread_equal(pthread_self(),owner)) 
	{
	    owner = 0;
	    int result = pthread_mutex_unlock(&pthread_mutex);
#ifdef _EXTMUTEX_DEBUG_
	    if (result)
	    {
		std::cerr << "Error unlocking mutex: " << name << " err: " << result << std::endl;
	    }
#endif
	    return result;
	}else{
	    std::cerr << "Error attempting to unlock a mutex from a thread which does not own it!: " << name << std::endl << std::flush;
	    std::cerr << "Caller(" << pthread_self() << ")  Owner(" << owner << ")" << " " << this << std::endl << std::flush;
	    std::cerr << "Either track this down or change EXTMutex to be reursive! (i.e. set EXT_MUTEX_RECURSIVE=1)" << std::endl << std::flush;
	    exit(1);
	}
    }

#endif	
}
