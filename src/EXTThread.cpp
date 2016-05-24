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

THREAD_LOCAL EXTThread* EXTThread::sm_current = 0;

EXTThread::~EXTThread()
{
#ifdef _EXTTHREAD_DEBUG_
    if (m_initialised && !m_detached && !m_joined) {
        printf("Resource leak destroying EXTThread: creator has not joined nor detached thread.\n");
    }
#endif
}

int EXTThread::start(function_type EntryPoint, void* Arg)
{
    if (EntryPoint) {
        m_function = EntryPoint;
    }
    if (Arg) {
        m_arg = Arg;
    }
    int result = 22; //EINVAL;
    if (!m_initialised) {
#ifdef _WIN32
        std::function<void*()> fn = [=]()->void* { return Trampoline(this); };
        m_thread = std::thread(fn);
        result = 0;
#elif __APPLE__
        result = pthread_create(&m_thread, NULL, Trampoline, this);
#else
        result = pthread_create(&m_thread, NULL, Trampoline, this);
        if (!result && !m_name.empty()) {
            pthread_setname_np(m_thread, m_name.c_str());
        }
#endif
        m_initialised = !result;
    }
#ifdef _EXTTHREAD_DEBUG_
    if (result) {
        printf("Error creating thread: %d\n", result);
    }
#endif
    return result;
}

int EXTThread::kill()
{
#ifdef _WIN32
    return 0;
#else
    return pthread_cancel(m_thread);
#endif
}

int EXTThread::detach()
{
    int result = 22; //EINVAL;
    if (m_initialised) {
#ifdef _WIN32
        m_thread.detach();
        result = 0;
#else
        result = pthread_detach(m_thread);
#endif
        m_detached = !result;
    }
#ifdef _EXTTHREAD_DEBUG_
    if (result) {
        printf("Error detaching thread: %d\n", result);
    }
#endif
    return result;
}

int EXTThread::join()
{
    int result = 22; //EINVAL;
    if (m_initialised) {
#ifdef _WIN32
        m_thread.join();
        result = 0;
#else
        result = pthread_join(m_thread, NULL);
#endif
        m_joined = ! result;
    }
#ifdef _EXTTHREAD_DEBUG_
    if (result) {
        printf("Error joining thread: %d\n", result);
    }
#endif
    return result;
}

int EXTThread::setPriority(int Priority, bool Realtime)
{
#ifdef _WIN32
    auto thread = m_thread.native_handle();
#else
    auto thread = m_thread;
#endif
#ifdef __linux__
    sched_param param;
    int policy;
    pthread_getschedparam(m_thread, &policy, &param);
    param.sched_priority = Priority;
    if (Realtime) { // for realtime threads, use SCHED_RR policy
      policy = SCHED_RR;
    }
    int result = pthread_setschedparam(thread, policy, &param);
    if (result) {
      printf("Error: failed to set thread priority: %s\n", strerror(result));
      return 0;
    }
    return 1;
#elif __APPLE__
    struct thread_time_constraint_policy ttcpolicy;
    int result;
    // OSX magic numbers
    ttcpolicy.period = uint32_t(UNIV::SAMPLE_RATE / 100); // HZ/160
    ttcpolicy.computation = uint32_t(UNIV::SAMPLE_RATE / 143); // HZ/3300;
    ttcpolicy.constraint = uint32_t(UNIV::SAMPLE_RATE / 143); // HZ/2200;
    ttcpolicy.preemptible = 1; // 1
    result = thread_policy_set(pthread_mach_thread_np(thread),
                               THREAD_TIME_CONSTRAINT_POLICY,
                               (thread_policy_t)&ttcpolicy,
                               THREAD_TIME_CONSTRAINT_POLICY_COUNT);
    if (result != KERN_SUCCESS) {
        printf("Error: failed to set thread priority: %s\n", strerror(result));
        return 0;
      }
    return 1;
#else
    printf("Error: cannot set thread priority on Windows\n");
    return 0;
#endif
}

int EXTThread::getPriority() const
{
#ifdef __linux__
    int policy;
    sched_param param;
    pthread_getschedparam(m_thread, &policy, &param);
    return param.sched_priority;
#endif
    // fprintf(stderr, "Error: thread priority only available Linux\n");
    return 0;
}

}
