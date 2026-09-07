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

#ifndef IMP_TASK_H
#define IMP_TASK_H

#include <cstdint>
#include <functional>
#include <utility>

namespace extemp {

class TaskI;

// What a scheduled task runs when it fires. Held by pointer -- Scheme sees it
// as a cptr (*callback*, *io:osc:send-msg*) and TaskScheduler::addTask takes
// one -- so instances are heap objects that outlive the tasks using them.
class CM {
  private:
    std::function<void(TaskI*)> m_function;

  public:
    explicit CM(std::function<void(TaskI*)> Function) : m_function(std::move(Function)) {}

    void execute(TaskI* Task) {
        m_function(Task);
    }
};

// A CM calling `instance->func(task)`.
#define mk_cb(instance, class, func)                                                               \
    (new extemp::CM([instance](extemp::TaskI* task) { (instance)->func(task); }))

class TaskI {
  private:
    uint64_t m_startTime;
    uint64_t m_duration;
    CM* m_classMember;
    int m_tag;

  protected:
    // Callback is accepted for the callers that pass it and otherwise unused.
    TaskI(uint64_t StartTime, uint64_t Duration, CM* ClassMember, int Tag, bool /*Callback*/ = false)
        : m_startTime(StartTime), m_duration(Duration), m_classMember(ClassMember), m_tag(Tag) {}

  public:
    virtual ~TaskI() = default;

    uint64_t getStartTime() const {
        return m_startTime;
    }
    uint64_t getDuration() const {
        return m_duration;
    }
    int getTag() const {
        return m_tag;
    }
    void execute() {
        m_classMember->execute(this);
    }
};

template <typename T = int> class Task : public TaskI {
  private:
    T m_arg;

  public:
    Task(uint64_t StartTime, uint64_t Duration, CM* ClassMember, const T& Arg, int Tag = 0,
         bool Callback = false)
        : TaskI(StartTime, Duration, ClassMember, Tag, Callback), m_arg(Arg) {}

    const T& getArg() const {
        return m_arg;
    }
};

}  // namespace extemp
#endif
