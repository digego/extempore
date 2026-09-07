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

#include <stop_token>
#include <thread>
#include <string>

#include "UNIV.h"

namespace extemp {

// A named thread running a C-style `void* fn(void*)` entry point (the shape
// xtlang's thread_fork hands us), with cooperative cancellation.
//
// Lifetime: the destructor requests a stop and then joins, so an EXTThread
// must not be destroyed while its body ignores stopRequested() and never
// returns -- detach() it first (AudioDevice does this for its worker pool), or
// have the body poll stopRequested(). Deleting a thread that has already
// finished (the xthread.xtm join-then-destroy pattern) returns immediately.
class EXTThread {
  public:
    typedef void* (*function_type)(void*);

  private:
    function_type m_function;
    void* m_arg;
    std::string m_name;
    bool m_subsume = false;  // run the body on the calling thread instead
    std::stop_source m_stopSource;
    std::jthread m_thread;
    // The body's native handle, whether it runs on m_thread or on the thread
    // that subsumed it; setPriority/getPriority need a handle in both cases.
    std::thread::native_handle_type m_nativeHandle{};
    bool m_started = false;

    static thread_local EXTThread* sm_current;

    void* run();

  public:
    EXTThread(function_type EntryPoint, void* Arg, const std::string& Name = std::string())
        : m_function(EntryPoint), m_arg(Arg), m_name(Name) {}
    ~EXTThread();
    EXTThread(const EXTThread&) = delete;
    EXTThread& operator=(const EXTThread&) = delete;

    // Optional overrides let a caller supply the entry point at start time
    // (OSC reuses one EXTThread for either its UDP or TCP server body).
    int start(function_type EntryPoint = nullptr, void* Arg = nullptr);
    int kill();  // cooperative: requests stop; the body must poll stopRequested()
    bool stopRequested() const {
        return m_stopSource.stop_requested();
    }
    int detach();
    int join();
    void setSubsume() {
        m_subsume = true;
    }
    // True once started and until joined or detached.
    bool isRunning() const {
        return m_subsume ? m_started : m_thread.joinable();
    }
    bool isCurrentThread() const {
        return sm_current == this;
    }
    int setPriority(int Priority, bool Realtime);
    int getPriority();  // doesn't say if it's realtime or not

    static EXTThread* activeThread() {
        return sm_current;
    }
};

}  // namespace extemp

#endif
