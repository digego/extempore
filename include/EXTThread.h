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

#include <string>

#include "UNIV.h"

namespace extemp
{

class EXTThread
{
public:
    typedef void* (*function_type)(void*);
private:
    function_type m_function;
    void*         m_arg;
    std::string   m_name;
    bool          m_initialised;
    bool          m_detached;
    bool          m_joined;
    bool          m_subsume; // subsume the current thread
#ifndef _WIN32
    pthread_t     m_thread;
#else
    std::thread   m_thread;
#endif

    static THREAD_LOCAL EXTThread* sm_current;
public:
    EXTThread(function_type EntryPoint, void* Arg, const std::string& Name = std::string()): m_function(EntryPoint),
      m_arg(Arg), m_name(Name), m_initialised(false), m_detached(false), m_joined(false), m_subsume(false) {
    }
    ~EXTThread();

    int start(function_type EntryPoint = nullptr, void* Arg = nullptr); // overrides - ugly, from OSC
    int kill();
    int detach();
    int join();
    void setSubsume() { m_subsume = true; }
    bool isRunning() const { return m_initialised; }
    bool isCurrentThread() { return sm_current == this; }
    int setPriority(int Priority, bool Realtime);
    int getPriority() const; //doesn't say if it's realtime or not
#ifdef _WIN32
    std::thread& getThread() { return m_thread; }
#else
    pthread_t getThread() { return m_thread; }
#endif

    static void* Trampoline(void* Arg) {
        auto thread(reinterpret_cast<EXTThread*>(Arg));
#ifdef __APPLE__ // unforunately apple requires pthread_setname_np in current thread
        if (!thread->m_name.empty()) {
            pthread_setname_np(thread->m_name.c_str());
        }
#endif
        sm_current = thread;
        return thread->m_function(thread->m_arg);
    }
    static EXTThread* activeThread() {
        return sm_current;
    }
};

} //End Namespace

#endif
