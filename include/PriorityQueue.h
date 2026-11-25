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

#ifndef PRIORITY_QUEUE_H
#define PRIORITY_QUEUE_H

// #define _DEBUG_QUEUE_

#include <queue>

#include "Task.h"

namespace extemp {

// Tasks come out in order of start time, and those with the same start time
// in the order they were added (the counter in the key), so two callbacks
// scheduled for one instant run first-scheduled first, every time.
template <typename T> class PriorityQueue {
  private:
    struct Entry {
        uint64_t time;
        uint64_t added;
        T* value;
        bool operator>(const Entry& Other) const {
            return time != Other.time ? time > Other.time : added > Other.added;
        }
    };
    typedef std::priority_queue<Entry, std::vector<Entry>, std::greater<Entry>> queue_type;

  private:
    queue_type m_queue;
    uint64_t m_added = 0;

  public:
    ~PriorityQueue() {
        clear();
    }

    void add(T* Val) {
        m_queue.push(Entry{Val->getStartTime(), m_added++, Val});
    }
    void clear() {
        while (!m_queue.empty()) {
            delete m_queue.top().value;
            m_queue.pop();
        }
    }
    T* pop() {
        if (m_queue.empty()) [[unlikely]] {
            return nullptr;
        }
        auto element(m_queue.top().value);
        m_queue.pop();
        return element;
    }
    T* peek() {
        if (m_queue.empty()) [[unlikely]] {
            return nullptr;
        }
        return m_queue.top().value;
    }
    size_t size() {
        return m_queue.size();
    }
};

}  // namespace extemp
#endif
