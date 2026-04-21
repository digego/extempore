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

#ifndef TASK_SCHEDULER_H
#define TASK_SCHEDULER_H

#include "PriorityQueue.h"
#include "EXTThread.h"
#include "UNIV.h"

#include <condition_variable>
#include <mutex>

namespace extemp {

// Paired mutex + condvar used by the scheduler and its clients.
struct Monitor {
    std::recursive_mutex mutex;
    std::condition_variable_any cond;
    void lock() {
        mutex.lock();
    }
    void unlock() {
        mutex.unlock();
    }
    void wait() {
        std::unique_lock<std::recursive_mutex> lk(mutex);
        cond.wait(lk);
    }
    void signal() {
        cond.notify_one();
    }
};

class TaskScheduler {
  private:
    unsigned m_numFrames;
    PriorityQueue<TaskI> m_queue;
    EXTThread m_queueThread;
    Monitor m_guard;
    std::recursive_mutex m_queueMutex;

    static TaskScheduler sm_instance;

  private:
    void* queueThreadImpl();
    void timeSlice();

    static void* queueThread(void* Arg) {
        return reinterpret_cast<TaskScheduler*>(Arg)->queueThreadImpl();
    }

  public:
    TaskScheduler();

    void start() {
        m_queueThread.start();
    }
    void setFrames(unsigned Frames) {
        m_numFrames = Frames;
    }
    Monitor& getGuard() {
        return m_guard;
    }

    void add(TaskI* Task) {
        std::lock_guard<std::recursive_mutex> lock(m_queueMutex);
        m_queue.add(Task);
    }
    template <typename T>
    static void addTask(uint64_t StartTime, uint64_t Duration, CM* ClassMember, const T& Arg,
                        int Tag, bool Callback) {
        sm_instance.add(new Task<T>(StartTime, Duration, ClassMember, Arg, Tag, Callback));
    };

    static TaskScheduler* I() {
        return &sm_instance;
    };
};

}  // namespace extemp
#endif
