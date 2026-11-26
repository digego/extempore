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
#include <optional>

#include "UNIV.h"
#include "EXTThread.h"

#ifdef _WIN32
#include <Windows.h>
#elif __APPLE__
#include <mach/mach_error.h>
#include <mach/mach_time.h>
#include <mach/thread_policy.h>
#include <mach/thread_act.h>
#include <pthread.h>
#else
#include <pthread.h>
#endif

namespace extemp {

namespace {

#ifdef __linux__
// Sets Thread's priority, and its scheduling policy when one is given.
bool setScheduling(pthread_t Thread, int Priority, std::optional<int> Policy) {
    sched_param param;
    int policy;
    pthread_getschedparam(Thread, &policy, &param);
    param.sched_priority = Priority;
    const int result = pthread_setschedparam(Thread, Policy.value_or(policy), &param);
    if (result) {
        log::warn("failed to set thread priority: {}", strerror(result));
        return false;
    }
    return true;
}
#elif __APPLE__
// Puts Thread under the time-constraint policy. The times are in host clock
// ticks (the units of mach_absolute_time).
bool setTimeConstraint(pthread_t Thread, uint32_t Period, uint32_t Computation,
                       uint32_t Constraint) {
    thread_time_constraint_policy policy;
    policy.period = Period;
    policy.computation = Computation;
    policy.constraint = Constraint;
    policy.preemptible = 1;
    const kern_return_t result =
        thread_policy_set(pthread_mach_thread_np(Thread), THREAD_TIME_CONSTRAINT_POLICY,
                          (thread_policy_t)&policy, THREAD_TIME_CONSTRAINT_POLICY_COUNT);
    if (result != KERN_SUCCESS) {
        log::warn("failed to set thread priority: {}", mach_error_string(result));
        return false;
    }
    return true;
}
#endif

}  // namespace

thread_local EXTThread* EXTThread::sm_current = nullptr;
std::atomic<uint64_t> EXTThread::sm_made{0};

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
    // for realtime threads, use SCHED_RR policy
    return setScheduling(m_nativeHandle, Priority,
                         Realtime ? std::optional<int>(SCHED_RR) : std::nullopt);
#elif __APPLE__
    // OSX magic numbers
    return setTimeConstraint(m_nativeHandle, uint32_t(UNIV::SAMPLE_RATE / 100),
                             uint32_t(UNIV::SAMPLE_RATE / 143), uint32_t(UNIV::SAMPLE_RATE / 143));
#else
    log::warn("cannot set thread priority on Windows");
    return 0;
#endif
}

bool EXTThread::makeCurrentRealtime() {
#ifdef __linux__
    return setScheduling(pthread_self(), 20, SCHED_RR);
#elif __APPLE__
    mach_timebase_info_data_t timebase;
    mach_timebase_info(&timebase);
    const double second = 1e9 * timebase.denom / timebase.numer;  // in host clock ticks
    return setTimeConstraint(pthread_self(), uint32_t(second * .01), uint32_t(second * .007),
                             uint32_t(second * .007));
#else
    return SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_TIME_CRITICAL) != 0;
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
