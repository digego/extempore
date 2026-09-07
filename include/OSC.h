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

#ifndef OSC_H
#define OSC_H

#include "UNIV.h"
#include <cstdio>
#include <iostream>
#include <map>
#include <mutex>
#include <string>
#include "SchemeProcess.h"
#include "EXTThread.h"
#include "ext/OscWire.h"
#include "ext/UdpSocket.h"

extern "C" {
uint64_t swap64f(double d);
double unswap64f(uint64_t a);
uint32_t swap32f(float f);
float unswap32f(uint32_t a);
uint64_t swap64i(uint64_t d);
uint64_t unswap64i(uint64_t a);
uint32_t swap32i(uint32_t f);
uint32_t unswap32i(uint32_t a);
}
// #define _OSC_DEBUG_

namespace extemp {

// Connection types accepted by io:osc:start-server.
constexpr int OSC_UDP_TYPE = 1;
constexpr int OSC_TCP_TYPE = 2;

class OSC {

  public:
    OSC();

    // The OSC server for this interpreter, or null if none has been started.
    // Foreign functions run under the FFI trampoline, which turns a thrown
    // exception into a catchable Scheme error; the scheduler's send task does
    // not run under it, so it checks for null itself.
    static OSC* I(scheme* _sc);

    static void schemeInit(SchemeProcess* scm);

    // Encode a Scheme argument list onto Out, appending one type tag per value.
    static void processArgs(pointer Arg, osc::Writer& Out, std::string& TypeTags, scheme* _sc,
                            char RealType);

    static pointer registerScheme(scheme* _sc, pointer args);
    static pointer set_real_type(scheme* _sc, pointer args);
    static pointer set_msg_include_netaddr(scheme* _sc, pointer args);

    UdpSocket& getSocket() {
        return m_socket;
    }
    int getConnectionType() const {
        return conn_type;
    }
    void setConnectionType(int type) {
        conn_type = type;
    }
#ifndef _WIN32
    // The TCP-OSC listener is POSIX-only; the Windows server thread is a stub.
    int getTcpSocketFD() const {
        return m_tcpSocketFd;
    }
    void setTcpSocketFD(int fd) {
        m_tcpSocketFd = fd;
    }
#endif
    EXTThread& getThread() {
        return threadOSC;
    }
    bool getStarted() const {
        return started;
    }
    void setStarted(bool val) {
        started = val;
    }
    int (*getNativeOSC())(char*, char*, char*, int) {
        return nativeOSC;
    }
    void setNativeOSC(int (*val)(char*, char*, char*, int)) {
        nativeOSC = val;
    }
    int (*getNativeUDP())(char*, int) {
        return nativeUDP;
    }
    void setNativeUDP(int (*val)(char*, int)) {
        nativeUDP = val;
    }
    void sendOSC(TaskI* task);

    scheme* sc;
    std::string fname;
    char scheme_real_type;
    bool msg_include_netaddr;

    // One OSC server per interpreter. Registration happens on whichever thread
    // evaluates io:osc:start-server, while lookups happen from the scheduler
    // and from foreign functions, so the map needs a lock.
    static std::map<scheme*, OSC*> SCHEME_MAP;
    static std::mutex SCHEME_MAP_MUTEX;

  private:
    EXTThread threadOSC;
    UdpSocket m_socket;
#ifndef _WIN32
    int m_tcpSocketFd;
#endif
    int conn_type;  // OSC_UDP_TYPE or OSC_TCP_TYPE
    bool started;
    int (*nativeOSC)(char*, char*, char*,
                     int); /* if not null then use this compiled function for callbacks */
    int (*nativeUDP)(char*, int);
};

}  // namespace extemp

#endif
