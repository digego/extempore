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

#include <cerrno>
#include <cstdio>
#include <cstring>

#include "UNIV.h"
#include "EXTThread.h"

#ifdef _WIN32
#include <Windows.h>
#elif __APPLE__
#include <mach/thread_policy.h>
#include <mach/thread_act.h>
#include <pthread.h>
#else
#include <pthread.h>
#endif

namespace extemp {

thread_local EXTThread* EXTThread::sm_current = nullptr;

EXTThread::~EXTThread() {
    m_stopRequested.store(true, std::memory_order_release);
    if (m_thread.joinable()) {
        m_thread.join();
    }
}

// Body of the thread: names it, publishes it as the active thread, then runs
// the user entry point. Runs on m_thread, or on the caller when subsumed.
void* EXTThread::run() {
#ifdef _WIN32
    m_nativeHandle = GetCurrentThread();
#else
    m_nativeHandle = pthread_self();
#endif
#ifdef __APPLE__  // apple requires pthread_setname_np in current thread
    if (!m_name.empty()) {
        pthread_setname_np(m_name.c_str());
    }
#elif __linux__
    if (!m_name.empty()) {
        pthread_setname_np(pthread_self(), m_name.c_str());
    }
#endif
    sm_current = this;
    return m_function(m_arg);
}

int EXTThread::start(function_type EntryPoint, void* Arg) {
    if (EntryPoint) {
        m_function = EntryPoint;
    }
    if (Arg) {
        m_arg = Arg;
    }
    if (m_started) {
        return EINVAL;
    }
    m_started = true;
    if (m_subsume) {
        run();  // never returns for the process threads that subsume main
        return 0;
    }
    m_thread = std::thread([this] { run(); });
    return 0;
}

int EXTThread::kill() {
    m_stopRequested.store(true, std::memory_order_release);
    return 0;
}

int EXTThread::detach() {
    if (!m_thread.joinable()) {
        return EINVAL;
    }
    m_thread.detach();
    return 0;
}

int EXTThread::join() {
    if (!m_thread.joinable()) {
        return EINVAL;
    }
    m_thread.join();
    return 0;
}

int EXTThread::setPriority(int Priority, bool Realtime) {
#ifdef __linux__
    sched_param param;
    int policy;
    pthread_getschedparam(m_nativeHandle, &policy, &param);
    param.sched_priority = Priority;
    if (Realtime) {  // for realtime threads, use SCHED_RR policy
        policy = SCHED_RR;
    }
    int result = pthread_setschedparam(m_nativeHandle, policy, &param);
    if (result) {
        printf("Error: failed to set thread priority: %s\n", strerror(result));
        return 0;
    }
    return 1;
#elif __APPLE__
    struct thread_time_constraint_policy ttcpolicy;
    int result;
    // OSX magic numbers
    ttcpolicy.period = uint32_t(UNIV::SAMPLE_RATE / 100);       // HZ/160
    ttcpolicy.computation = uint32_t(UNIV::SAMPLE_RATE / 143);  // HZ/3300;
    ttcpolicy.constraint = uint32_t(UNIV::SAMPLE_RATE / 143);   // HZ/2200;
    ttcpolicy.preemptible = 1;                                  // 1
    result =
        thread_policy_set(pthread_mach_thread_np(m_nativeHandle), THREAD_TIME_CONSTRAINT_POLICY,
                          (thread_policy_t)&ttcpolicy, THREAD_TIME_CONSTRAINT_POLICY_COUNT);
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

int EXTThread::getPriority() {
#ifdef __linux__
    int policy;
    sched_param param;
    pthread_getschedparam(m_nativeHandle, &policy, &param);
    return param.sched_priority;
#endif
    return 0;
}

}  // namespace extemp
