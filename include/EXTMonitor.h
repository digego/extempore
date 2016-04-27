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

#ifndef EXT_MONITOR_H
#define EXT_MONITOR_H

#include <string>

#include "EXTMutex.h"
#include "EXTCondition.h"

namespace extemp
{
class EXTMonitor
{
public:
    class ScopedLock
    {
    private:
        EXTMonitor& m_monitor;
        bool        m_signal;
    public:
        ScopedLock(EXTMonitor& Monitor, bool Signal = false): m_monitor(Monitor), m_signal(Signal) {
            m_monitor.lock();
        }
        ~ScopedLock() {
            if (m_signal) {
                m_monitor.signal();
            }
            m_monitor.unlock();
        }
    };
private:
    std::string  m_name;
    EXTMutex     m_mutex;
    EXTCondition m_condition;
    bool         m_initialised;
public:
    EXTMonitor(const std::string& Name): m_name(Name), m_mutex(Name), m_initialised(false) {
        init();
    }
    ~EXTMonitor() {
        destroy();
    }

    void init()
    {
        if (!m_initialised)
        {
            m_mutex.init();
            m_condition.init();
            m_initialised = true;
        }
    }
    void destroy() {
        if (m_initialised)
        {
            m_initialised = false;
            m_mutex.destroy();
            m_condition.destroy();
        }
    }
    void lock() {
        m_mutex.lock();
    }
    void unlock() {
        m_mutex.unlock();
    }
    void wait() {
        m_condition.wait(&m_mutex);
    }
    void signal() {
        m_condition.signal();
    }
};

} //End Namespace

#endif
