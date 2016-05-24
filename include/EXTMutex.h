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

#ifndef EXT_MUTEX
#define EXT_MUTEX

#ifdef EXT_BOOST
#include <mutex>
#else
#include "pthread.h"
#endif

#include <string>

namespace extemp
{

class EXTMutex
{
public:
    class ScopedLock
    {
    private:
        EXTMutex& m_mutex;
    public:
        ScopedLock(EXTMutex& Mutex): m_mutex(Mutex) {
            m_mutex.lock();
        }
        ~ScopedLock() {
            m_mutex.unlock();
        }
    };
private:
    std::string          m_name;
    bool                 m_initialised;
#ifdef _WIN32
    std::recursive_mutex m_mutex;
#else
    pthread_mutex_t      m_mutex;
#endif
public:
    EXTMutex(const std::string& Name = std::string()): m_name(Name), m_initialised(false) {
    }
    ~EXTMutex() {
        destroy();
    }

    void init(bool Recursive = true);
    void destroy();

    void lock();
    void unlock();
    bool try_lock();

    friend class EXTCondition;
};

#ifdef _WIN32
#include <exception>

inline void EXTMutex::init(bool Recursive)
{
    m_initialised = true;
}

inline void EXTMutex::destroy()
{
    m_initialised = false;
}

inline void EXTMutex::lock()
{
    try {
        m_mutex.lock();
    } catch(std::exception& e) {
        fprintf(stderr, "Problem locking mutex: %s\n", e.what());
    }
}

inline void EXTMutex::unlock()
{
    try {
        m_mutex.unlock();
    } catch(std::exception& e){
        fprintf(stderr, "Problem unlocking mutex: %s\n", e.what());
    }
}

inline bool EXTMutex::try_lock()
{
    return m_mutex.try_lock();
}

#else // begin POSIX

inline void EXTMutex::init(bool Recursive)
{
    pthread_mutexattr_t attr;
    pthread_mutexattr_init(&attr);
#ifdef _EXTMUTEX_DEBUG_
    pthread_mutexattr_settype(&attr, (!Recursive) ? PTHREAD_MUTEX_ERRORCHECK : PTHREAD_MUTEX_NORMAL);
#else
    pthread_mutexattr_settype(&attr, (Recursive) ? PTHREAD_MUTEX_RECURSIVE : PTHREAD_MUTEX_NORMAL);
#endif
    auto result(pthread_mutex_init(&m_mutex, &attr));
    m_initialised = !result;
#ifdef _EXTMUTEX_DEBUG_
    if (result)
    {
        dprintf(2, "Error initialising mutex: %s err: %d", m_name, result);
    }
#endif
}

inline void EXTMutex::destroy()
{
    if (m_initialised)
    {
        m_initialised = false;
        auto __attribute__((unused)) result(pthread_mutex_destroy(&m_mutex));
#ifdef _EXTMUTEX_DEBUG_
        if (result)
        {
            dprintf(2, "Error destroying mutex: %s err: %d\n", m_name, result);
        }
#endif
    }
}

inline void EXTMutex::lock()
{
    auto __attribute__((unused)) result(pthread_mutex_lock(&m_mutex));
#ifdef _EXTMUTEX_DEBUG_
    if (result)
    {
        dprintf(2, "Error locking mutex: %s err: %d\n", name, result;
    }
#endif
}

inline void EXTMutex::unlock()
{
    auto __attribute__((unused)) result(pthread_mutex_unlock(&m_mutex));
#ifdef _EXTMUTEX_DEBUG_
    if (result)
    {
        dprintf(2, "Error unlocking mutex: %s err: %d\n", name, result);
    }
#endif
}

inline bool EXTMutex::try_lock()
{
    return !pthread_mutex_trylock(&m_mutex);
}

#endif // end POSIX

} //End Namespace

#endif
