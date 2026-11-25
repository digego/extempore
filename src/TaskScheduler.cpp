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
#include "Log.h"
#include "AudioDevice.h"
#include "HangReport.h"
#include <cmath>
#include <iostream>
#include <thread>
#include <chrono>
#include <vector>

namespace extemp {

TaskScheduler TaskScheduler::sm_instance;

TaskScheduler::TaskScheduler()
    : m_numFrames(0), m_queueThread(TaskScheduler::queueThread, this, "scheduler") {}

// Only reached on the exit(1) paths -- (quit) goes through _Exit and skips
// static destructors. Ask the scheduler thread to stop and wake it so the
// EXTThread member's joining destructor returns promptly.
TaskScheduler::~TaskScheduler() {
    m_queueThread.kill();
    m_tick.release();
}

static int64_t steadyNanoseconds() {
    return std::chrono::duration_cast<std::chrono::nanoseconds>(
               std::chrono::steady_clock::now().time_since_epoch())
        .count();
}

static uint64_t AUDIO_DEVICE_START_OFFSET = 0;
static double LAST_REALTIME_STAMP = 0.0;

void TaskScheduler::run(TaskI* Task) {
    try {
        if (!Task->getTag()) [[likely]] {
            Task->execute();
        }
    } catch (std::exception& e) {
        log::error("error executing scheduled task: {}", e.what());
    }
    delete Task;
}

TaskI* TaskScheduler::take(uint64_t Before) {
    TaskI* task = m_queue.peek();
    if (!task || task->getStartTime() >= Before) {
        return nullptr;
    }
    m_queue.pop();
    m_housekeeping -= task->isHousekeeping();
    m_dispatching.fetch_add(1);
    return task;
}

void TaskScheduler::runDue(uint64_t Until) {
    m_lastPass.store(steadyNanoseconds());
    std::vector<TaskI*> due;
    {
        const std::lock_guard<std::recursive_mutex> lock(m_queueMutex);
        while (TaskI* task = take(Until)) {
            due.push_back(task);
        }
    }
    for (auto task : due) {
        run(task);
        m_dispatching.fetch_sub(1);
    }
}

void TaskScheduler::timeSlice() {
    using dseconds = std::chrono::duration<double>;
    const uint32_t frames = m_numFrames / UNIV::TIME_DIVISION;
    const dseconds slice(double(frames) / UNIV::SAMPLE_RATE);
    if (UNIV::AUDIO_NONE) [[unlikely]] {  // i.e. if no audio device
        AudioDevice::CLOCKBASE = getRealTime();
        UNIV::AUDIO_CLOCK_BASE.store(AudioDevice::CLOCKBASE.load());
    }
    LAST_REALTIME_STAMP = getRealTime();
    do {
        m_lastPass.store(steadyNanoseconds());
        m_queueMutex.lock();
        while (TaskI* task = take(UNIV::TIME + frames)) {
            m_queueMutex.unlock();
            run(task);
            m_dispatching.fetch_sub(1);
            m_queueMutex.lock();
        }
        m_queueMutex.unlock();
        if (UNIV::TIME_DIVISION == 1) [[likely]] {
            return;
        }
        if (UNIV::AUDIO_NONE) [[unlikely]] {
            AudioDevice::REALTIME = getRealTime();
            UNIV::AUDIO_CLOCK_NOW.store(AudioDevice::REALTIME.load());
        } else if (!UNIV::DEVICE_TIME) {
            AUDIO_DEVICE_START_OFFSET = UNIV::TIME;
        }
        UNIV::TIME += frames;
        // Sleep for one slice, pulling in half of any drift against the wall
        // clock (and, with a device, against the audio clock).
        const dseconds now(getRealTime());
        const dseconds drift = now - (dseconds(LAST_REALTIME_STAMP) + slice);
        LAST_REALTIME_STAMP = now.count();
        dseconds delay = slice - drift / 2;
        if (!UNIV::AUDIO_NONE) [[likely]] {
            delay += dseconds(
                (double(UNIV::TIME) - double(UNIV::DEVICE_TIME + AUDIO_DEVICE_START_OFFSET)) /
                UNIV::SAMPLE_RATE / 2);
        }
        if (delay > dseconds::zero()) {
            std::this_thread::sleep_for(
                std::chrono::duration_cast<std::chrono::nanoseconds>(delay));
        }
    } while (!m_queueThread.stopRequested());
}

#ifndef _WIN32
void TaskScheduler::writeHangReport() {
    HangReport::Line line;
    line << "scheduler: TIME " << UNIV::TIME << ", last pass ";
    const int64_t last = m_lastPass.load();
    if (last == 0) {
        line << "never";
    } else {
        line << uint64_t((steadyNanoseconds() - last) / 1'000'000) << " ms ago";
    }
    if (m_queueMutex.try_lock()) {
        const TaskI* next = m_queue.peek();
        line << ", " << uint64_t(m_queue.size()) << " tasks queued";
        if (next) {
            line << ", the first due in ";
            line.seconds(int64_t(next->getStartTime()) - int64_t(UNIV::TIME));
        }
        m_queueMutex.unlock();
    } else {
        line << ", queue locked";
    }
    line.write();
}
#endif

void* TaskScheduler::queueThreadImpl() {
    if (UNIV::TIME_DIVISION == 1) [[likely]] {
        // One pass per audio buffer, paced by the callback's tick().
        while (!m_queueThread.stopRequested()) {
            timeSlice();
            m_tick.acquire();
        }
        return this;
    }
    timeSlice();  // self-paced; only returns once a stop is requested
    return nullptr;
}

}  // namespace extemp
