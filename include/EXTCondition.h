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

#ifndef EXT_CONDITION
#define EXT_CONDITION

#ifdef EXT_BOOST
#include <condition_variable>
#else
#include "pthread.h"
#endif

#include "EXTMutex.h"

namespace extemp
{

class EXTCondition
{
private:
    bool                        m_initialised;
#ifdef EXT_BOOST
    std::condition_variable_any m_cond;
#else
    pthread_cond_t              m_cond;
#endif
public:
    EXTCondition(): m_initialised(false) {
    }
    ~EXTCondition() {
        destroy();
    }

    void init();
    void destroy();

    void wait(EXTMutex *mutex);
    void signal();
};

#ifdef _WIN32

inline void EXTCondition::init()
{
    m_initialised = true;
}

inline void EXTCondition::destroy()
{
    m_initialised = false;
}

inline void EXTCondition::wait(EXTMutex* Mutex)
{
    std::unique_lock<std::recursive_mutex> lock(Mutex->m_mutex);
    m_cond.wait(lock);
}

inline void EXTCondition::signal()
{
    m_cond.notify_one();
}

#else // begin POSIX

inline void EXTCondition::init()
{
    auto result(pthread_cond_init(&m_cond, NULL));
    m_initialised = !result;
#ifdef _EXTCONDITION_DEBUG_
    if (result)
    {
        dprintf(2, "Error initialising condition: %d\n", result);
    }
#endif
}

inline void EXTCondition::destroy()
{
    if (m_initialised)
    {
        m_initialised = false;
        auto __attribute__((unused)) result(pthread_cond_destroy(&m_cond));
#ifdef _EXTCONDITION_DEBUG_
        if (result)
        {
            dprintf(2, "Error destroying condition: %d\n", result);
        }
#endif
    }
}

inline void EXTCondition::wait(EXTMutex* Mutex)
{
    auto __attribute__((unused)) result(pthread_cond_wait(&m_cond, &Mutex->m_mutex));
#ifdef _EXTCONDITION_DEBUG_
    if (result)
    {
        dprintf(2, "Error waiting on condition: %d\n", result);
    }
#endif
}

inline void EXTCondition::signal()
{
    auto __attribute__((unused)) result(pthread_cond_signal(&m_cond));
#ifdef _EXTCONDITION_DEBUG_
    if (result)
    {
        dprintf(2, "Error signalling condition: %d\n", result);
    }
#endif
}

#endif // end POSIX

} //End Namespace

#endif
