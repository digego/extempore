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

#include "TaskScheduler.h"
#include "EXTMonitor.h"
#include "AudioDevice.h"
#include <math.h>

namespace extemp {

TaskScheduler TaskScheduler::sm_instance;

TaskScheduler::TaskScheduler(): m_numFrames(0), m_queueThread(TaskScheduler::queueThread, this, "scheduler"),
    m_guard("task_scheduler_guard"), m_queueMutex("taskQueue")
{
    m_guard.init();
    m_queueMutex.init();
}

static uint64_t AUDIO_DEVICE_START_OFFSET = 0;
static double LAST_REALTIME_STAMP = 0.0;

void TaskScheduler::timeSlice()
{
    uint32_t frames = m_numFrames / UNIV::TIME_DIVISION;
    uint64_t nanosecs = double(frames) / UNIV::SAMPLE_RATE * D_BILLION;
    timespec remain { 0, 0 };
    if (unlikely(UNIV::AUDIO_NONE)) { // i.e. if no audio device
        AudioDevice::CLOCKBASE = getRealTime();
        UNIV::AUDIO_CLOCK_BASE = AudioDevice::CLOCKBASE;
    }
    LAST_REALTIME_STAMP = getRealTime();
    do {
        m_queueMutex.lock();
        auto task(m_queue.peek());
        while (task && task->getStartTime() < UNIV::TIME + frames) {
            m_queue.pop();
            m_queueMutex.unlock();
            try {
                if (likely(!task->getTag())) {
                    task->execute();
                }
            } catch(std::exception& e) {
                std::cout << "Error executing scheduled task! " << e.what() << std::endl;
            }
            delete task;
            m_queueMutex.lock();
            task = m_queue.peek();
        }
        m_queueMutex.unlock();
        if (likely(UNIV::TIME_DIVISION == 1)) {
            return;
        }
        if (unlikely(UNIV::AUDIO_NONE)) {
            AudioDevice::REALTIME = getRealTime();
            UNIV::AUDIO_CLOCK_NOW = AudioDevice::REALTIME;
        } else if (!UNIV::DEVICE_TIME) {
            AUDIO_DEVICE_START_OFFSET = UNIV::TIME;
        }
        UNIV::TIME += frames;
#ifdef _WIN32
    // not on windows yet!
#else
    // if last error (b.tv_nsec) is small then keep sleeping
        double realtimeStamp = getRealTime();
        double timediff = realtimeStamp - (LAST_REALTIME_STAMP + double(frames) / UNIV::SAMPLE_RATE);
        LAST_REALTIME_STAMP = realtimeStamp;
        timespec delay { 0, long(nanosecs - remain.tv_nsec) }; // this better be all sub-second
        // subtract any timediff error!
        // then multiply by 0.5 to split the difference (i.e only move halfway towards the error).
        delay.tv_nsec -= timediff / 2 * BILLION;
        if (likely(!UNIV::AUDIO_NONE)) {
            delay.tv_nsec += (double(UNIV::TIME) - (UNIV::DEVICE_TIME + AUDIO_DEVICE_START_OFFSET)) /
                    UNIV::SAMPLE_RATE / 2 * BILLION;
        }
        nanosleep(&delay, &remain);
#endif
    } while (true);
}

void* TaskScheduler::queueThreadImpl()
{
    if (likely(UNIV::TIME_DIVISION == 1)) {
        while (true) {
            timeSlice();
            m_guard.lock();
            m_guard.wait();
            m_guard.unlock();
        }
        return this;
    }
    timeSlice(); // will never return
    return nullptr;
}

} // End Namespace
