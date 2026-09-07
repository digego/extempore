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

#include "OSC.h"
#include "SchemeProcess.h"
#include "ext/NetUtil.h"
#include "ext/OscText.h"
#include "ext/OscWire.h"
#include <stdexcept>
#include <string>
#include <string_view>
#include <sstream>
#include <cmath>
#include <algorithm>
#include <array>
#include <bit>
#include <cstddef>
#include <cstring>

#include <oscpp/server.hpp>

#include <chrono>
#include <thread>

#ifndef _WIN32
#include <unistd.h>
#endif
#include <cstdlib>

#ifndef _WIN32
#include <sys/errno.h>
#include <sys/stat.h>
#include <netinet/tcp.h>
#include <netdb.h> /* host to IP resolution       */
#endif

// constants for SLIP TCP-packetizing
// from http://tools.ietf.org/html/rfc1055
constexpr char SLIP_END = char(0300);      // indicates begin/end of packet
constexpr char SLIP_ESC = char(0333);      // indicates byte stuffing
constexpr char SLIP_ESC_END = char(0334);  // ESC ESC_END means END data byte
constexpr char SLIP_ESC_ESC = char(0335);  // ESC ESC_ESC means ESC data byte

// for thread args (will be passed as void*)
typedef struct scm_osc_pair {
    extemp::SchemeProcess* scm_p;
    extemp::OSC* osc_p;
} scm_osc_pair;

///////////////////////////////////////////////
//
// Byte-order conversion for xtlang: these eight functions keep their extern "C"
// signatures because they are registered into the JIT by name. The conversion
// itself lives in ext/OscWire.h alongside the message writer.
//
using extemp::osc::byteswap;

uint64_t swap64f(double d) {
    return byteswap(std::bit_cast<uint64_t>(d));
}

double unswap64f(uint64_t a) {
    return std::bit_cast<double>(byteswap(a));
}

uint32_t swap32f(float f) {
    return byteswap(std::bit_cast<uint32_t>(f));
}

float unswap32f(uint32_t a) {
    return std::bit_cast<float>(byteswap(a));
}

uint64_t swap64i(uint64_t d) {
    return byteswap(d);
}

uint64_t unswap64i(uint64_t a) {
    return byteswap(a);
}

uint32_t swap32i(uint32_t f) {
    return byteswap(f);
}

uint32_t unswap32i(uint32_t a) {
    return byteswap(a);
}

///////////////////////////////////////////////////////////////

// #define _OSC_DEBUG_

namespace extemp {

std::map<scheme*, OSC*> OSC::SCHEME_MAP;
std::mutex OSC::SCHEME_MAP_MUTEX;

OSC* OSC::I(scheme* _sc) {
    std::lock_guard<std::mutex> lock(SCHEME_MAP_MUTEX);
    auto entry = SCHEME_MAP.find(_sc);
    return entry == SCHEME_MAP.end() ? nullptr : entry->second;
}

// Foreign functions go through this: the FFI trampoline turns the exception
// into a Scheme error the user can catch, rather than unwinding out of s7.
static OSC* osc_for_scheme(scheme* _sc) {
    OSC* osc = OSC::I(_sc);
    if (!osc) {
        throw std::runtime_error("no OSC server for this interpreter "
                                 "-- call (io:osc:start-server ...) first");
    }
    return osc;
}

// ---------------------------------------------------------------------------
// OSC receive path (oscpp).
//
// Incoming packets are untrusted network data, so parsing goes through oscpp's
// bounds-checked reader: every read validates the remaining length, and a
// malformed packet throws OSCPP::UnderrunError / ParseError, which
// osc_dispatch_packet() catches and drops. This replaces the hand-rolled
// getOSC* parser, which over-read on malformed input.
// ---------------------------------------------------------------------------

// Convert a 64-bit NTP timetag to seconds; (0, 1) is the OSC "immediately" tag.
static double osc_ntp_to_seconds(uint64_t timetag) {
    int64_t seconds = static_cast<int64_t>(timetag >> 32);
    uint32_t fractional = static_cast<uint32_t>(timetag & 0xFFFFFFFFu);
    if (seconds == 0 && fractional == 1)
        return 0.0;
    seconds -= 3187296000ul;
    return static_cast<double>(seconds) + static_cast<double>(fractional) / 4294967296.0;
}

// Build the Scheme call string for one parsed message and queue it.
//
// Everything appended here becomes source text that the interpreter will
// evaluate, so no packet-derived byte may reach it unescaped: the address is
// checked against the OSC charset (and dropped if it fails), strings go out as
// escaped Scheme string literals, and numbers are formatted from their binary
// values. Argument reads go through oscpp's bounds-checked stream, so a
// truncated argument throws and the message is dropped by the caller.
static void osc_emit_scheme_message(SchemeProcess* proc, const char* fname, double t,
                                    const char* address, OSCPP::Server::ArgStream argv,
                                    bool include_netaddr, const std::string& netaddy, int netport) {
    if (!osc_text::address_is_valid(address)) {
        return;
    }
    std::string form("(");
    form += fname;
    form += " ";
    form += osc_text::scheme_real_literal(t);
    form += " ";
    form += osc_text::scheme_string_literal(address);
    if (include_netaddr) {
        form += " ";
        form += osc_text::scheme_string_literal(netaddy);
        form += " ";
        form += std::to_string(netport);
    }
    auto streams = argv.state();
    auto tags = std::get<0>(streams);
    auto args = std::get<1>(streams);
    int depth = 0;  // nesting of OSC array type tags, emitted as Scheme lists
    while (!tags.atEnd()) {
        switch (tags.getChar()) {
            case 'i': form += " " + std::to_string(args.getInt32()); break;
            case 'f': form += " " + osc_text::scheme_real_literal(args.getFloat32()); break;
            case 'd': form += " " + osc_text::scheme_real_literal(args.getFloat64()); break;
            case 'h':
                form += " " + std::to_string(static_cast<int64_t>(args.getUInt64()));
                break;
            case 't':
                form += " " + osc_text::scheme_real_literal(osc_ntp_to_seconds(args.getUInt64()));
                break;
            case 's':
                form += " " + osc_text::scheme_string_literal(std::string_view(args.getString()));
                break;
            case '[':
                form += " (list ";
                ++depth;
                break;
            case ']':
                if (--depth < 0) {
                    return;  // unbalanced array tags: drop the message
                }
                form += ")";
                break;
            default: return;  // unsupported type tag: drop the message
        }
    }
    if (depth != 0) {
        return;  // unterminated array: drop rather than emit a broken form
    }
    form += ")";
    if (proc != nullptr) {
        proc->createSchemeTask(new std::string(std::move(form)), "OSC TASK",
                               SchemeTask::Type::LOCAL_PROCESS_STRING);
    }
}

// Dispatch one parsed message to the native callback or the Scheme interpreter.
static void osc_handle_message(OSC* osc, SchemeProcess* proc, double t,
                               const OSCPP::Server::Message& msg, bool include_netaddr,
                               const std::string& netaddy, int netport) {
    auto nativeOSC = osc->getNativeOSC();
    if (nativeOSC != nullptr) {
        // oscpp has validated the address and type-tag string lie within the
        // packet; hand the native callback the address, the reconstructed
        // ",..." tag string and the (bounded) raw argument bytes.
        auto streams = msg.args().state();
        auto tags = std::get<0>(streams);
        auto args = std::get<1>(streams);
        std::string typetags(",");
        typetags.append(tags.begin(), tags.capacity());
        nativeOSC(const_cast<char*>(msg.address()), const_cast<char*>(typetags.c_str()),
                  const_cast<char*>(args.pos()), static_cast<int>(args.consumable()));
    } else {
        osc_emit_scheme_message(proc, osc->fname.c_str(), t, msg.address(), msg.args(), include_netaddr,
                                netaddy, netport);
    }
}

// Parse an incoming OSC packet (message or bundle) and dispatch each message.
// All reads are bounds-checked by oscpp, so a malformed/hostile packet throws
// and is dropped here rather than over-reading the receive buffer.
static void osc_dispatch_packet(OSC* osc, SchemeProcess* proc, char* buf, int len,
                                bool include_netaddr, const std::string& netaddy, int netport) {
    try {
        OSCPP::Server::Packet packet(buf, static_cast<size_t>(len));
        if (packet.isBundle()) {
            auto bundle = static_cast<OSCPP::Server::Bundle>(packet);
            double t = osc_ntp_to_seconds(bundle.time());
            auto stream = bundle.packets();
            while (!stream.atEnd()) {
                auto element = stream.next();  // bundle element size is bounds-checked
                if (element.isMessage())
                    osc_handle_message(osc, proc, t, static_cast<OSCPP::Server::Message>(element),
                                       include_netaddr, netaddy, netport);
                // nested bundles are unsupported, as in the previous parser
            }
        } else {
            osc_handle_message(osc, proc, 0.0, static_cast<OSCPP::Server::Message>(packet),
                               include_netaddr, netaddy, netport);
        }
    } catch (const std::exception&) {
        // malformed or hostile packet -- drop it (no over-read, no crash)
    }
}

void* osc_mesg_callback(void* obj_p) {
    OSC* osc = (OSC*)obj_p;
    // The receive buffer is this thread's alone; 70000 bytes covers the
    // largest datagram the socket will hand us.
    std::vector<char> buffer(70000);
    sockaddr_in sender;
    while (true) {
        long bytes_read = osc->getSocket().recvFrom(buffer.data(), buffer.size(), sender);
        if (bytes_read < 0) {
            // Nothing waiting on the non-blocking socket, or a receive error
            // (e.g. Windows' spurious WSAECONNRESET after a send to a closed
            // port). Neither is worth tearing down a live performance over.
            std::this_thread::sleep_for(std::chrono::microseconds(1000));
            continue;
        }
        if (osc->getNativeUDP() != nullptr) {
            osc->getNativeUDP()(buffer.data(), static_cast<int>(bytes_read));
            continue;
        }
        osc_dispatch_packet(osc, osc->sc != nullptr ? osc->sc->m_process : nullptr, buffer.data(),
                            static_cast<int>(bytes_read), osc->msg_include_netaddr,
                            UdpSocket::addressText(sender), int(ntohs(sender.sin_port)));
    }
    return nullptr;
}

#ifdef _WIN32
void* tcp_osc_server_thread(void* obj_p) {
    // seed rng for process
    // UNIV::initRand();
    return nullptr;
}
#else

// return codes:
// 2 = successfully completed loading slip packet
// 1 = still filling packet + active escape is ON
// 0 = still filling packet + active escape is OFF
// -1 = bad packet
int parse_osc_slip_data(std::vector<char>* data, char* buf, int res, bool active_escape) {
    // copy buf into data
    for (int i = 0; i < res; i++, buf++) {
        switch (*buf) {
        case SLIP_END:  // close slip packet
            // return successful slip packet completion
            return 2;
        case SLIP_ESC:
            active_escape = true;
            continue;
        default:
            if (active_escape) {
                active_escape = false;
                if (*buf == SLIP_ESC_ESC)
                    data->push_back(SLIP_ESC);
                else if (*buf == SLIP_ESC_END)
                    data->push_back(SLIP_END);
                else {
                    fprintf(stderr, "Error in SLIP packet: bad escape type.\n");
                    return -1;  // bad packet
                }
                continue;
            }
            data->push_back(*buf);
        }
    }
    return (active_escape) ? 1 : 0;
}

int process_osc_data(SchemeProcess* scm, OSC* osc, struct sockaddr_in client_address, char* args,
                     long length) {
    // printf("Processing osc data %lld:%p\n",length,args);
    if (length > 0 && args != nullptr) {
        // process the OSC data (should be its own method)
        osc_dispatch_packet(osc, scm, args, static_cast<int>(length), false, std::string(), 0);
    }
    return 0;
}

void* tcp_osc_server_thread(void* obj_p) {
    // seed rng for process
    // UNIV::initRand();

    scm_osc_pair* sop = (scm_osc_pair*)obj_p;
    SchemeProcess* scm = sop->scm_p;
    OSC* osc = sop->osc_p;

    int socket_fd = osc->getTcpSocketFD();

    if (socket_fd < 0) {
        ascii_error();
        printf("Bad TCP-OSC socket: %s\n", strerror(errno));
        ascii_normal();
        return obj_p;
    }

    struct sockaddr_in client_address;
    int client_address_size = sizeof(client_address);

    fd_set rfd;  // open read sockets (man select for more info)
    std::vector<int> client_sockets;
    std::map<int, std::vector<char>> data_map;
    std::map<int, bool> data_packet;
    std::map<int, bool> data_active_escape;
    FD_ZERO(&rfd);  // zero out open sockets
    // printf("SERVER SOCKET FD_SET: %d\n",socket_fd);
    FD_SET(socket_fd, &rfd);  // add server socket to open sockets list
    int highest_fd = socket_fd + 1;
    // printf("FD SIZE=%d  and %d\n",highest_fd,FD_SETSIZE);
    static constexpr int BUFLEN = 1024;
    char buf[BUFLEN];
    while (scm->getRunning()) {
        fd_set c_rfd;
        FD_ZERO(&c_rfd);
        c_rfd = rfd;
        timeval pause;
        pause.tv_sec = 1;
        pause.tv_usec = 0;
        int res = select(highest_fd, &c_rfd, nullptr, nullptr, &pause);
        if (res >= 0) {
        } else {
            struct stat st;
            std::vector<int>::iterator pos = client_sockets.begin();
            while (pos != client_sockets.end()) {
                int result = fstat(*pos, &st);
                if (result < 0) {
                    FD_CLR(*pos, &rfd);
                    client_sockets.erase(pos);
                    break;
                }
                pos++;
            }
            ascii_error();
            printf("%s SERVER ERROR: %s\n", scm->getName().c_str(), strerror(errno));
            ascii_normal();
            continue;
        }
        if (FD_ISSET(socket_fd, &c_rfd)) {  // check if we have any new accpets on our server socket
            res = accept(socket_fd, (struct sockaddr*)&client_address,
                         (socklen_t*)&client_address_size);
            if (res < 0) {
                std::cout << "Bad Accept in Server Socket Handling" << std::endl;
                continue;  // continue on error
            }
            if (res >= highest_fd)
                highest_fd = res + 1;
            FD_SET(res, &rfd);  // add new socket to the FD_SET
            client_sockets.push_back(res);
            data_map[res] = std::vector<char>();
            std::string outstr("OSC connected over TCP.");
            write(res, outstr.c_str(), outstr.length() + 1);
            continue;
        }
        std::vector<int>::iterator pos = client_sockets.begin();
        std::vector<char> oscpacket;

        while (pos !=
               client_sockets.end()) {     // check through all fd's for matches against FD_ISSET
            if (FD_ISSET(*pos, &c_rfd)) {  // see if any client sockets have data for us
                int sock = *pos;
                for (;;) {  // read from stream in BUFLEN blocks
                    res = read(sock, buf, BUFLEN);
                    if (res == 0) {  // close the socket
                        FD_CLR(sock, &rfd);
                        data_map.erase(sock);
                        ascii_warning();
                        std::cout << "Closed TCP-OSC Socket" << std::endl;
                        ascii_normal();
                        pos = client_sockets.erase(pos);
                        close(sock);
                        break;
                    } else if (res < 0) {
                        ascii_error();
                        printf("Error with socket read for TCP OSC socket: %s", strerror(errno));
                        ascii_normal();
                        pos++;
                        break;
                    }
                    bool fullbuf = (res == BUFLEN) ? true : false;
                    // first check to see if we are currently
                    // NOT *in* a valid osc SLIP packet
                    char* bufptr = &buf[0];
                    if (!data_packet[sock]) {
                        for (; res > 0; res--, bufptr++) {
                            if (*bufptr == SLIP_END) {
                                data_packet[sock] = true;
                                bufptr++;
                                res--;
                                break;
                            }
                        }
                        if (!data_packet[sock]) {  // if still not in packet
                            if (fullbuf)
                                continue;  // keep reading
                            else
                                break;
                        }
                    }

                    // OK from here we can assume that we are
                    // in a valid OSC SLIP packet and can start
                    // loading up data_map[sock]
                    int result =
                        parse_osc_slip_data(&data_map[sock], bufptr, res, data_active_escape[sock]);

                    if (result == 2) {  // complete osc packet
                        // printf("full osc packet\n");
                        process_osc_data(scm, osc, client_address, data_map[sock].data(),
                                         data_map[sock].size());
                        data_map[sock].clear();
                        data_active_escape[sock] = false;
                        data_packet[sock] = false;
                    } else if (result == -1) {  // bad osc packet
                        ascii_error();
                        printf("Bad SLIP OSC Packet!!!!!\n");
                        ascii_normal();
                        data_map[sock].clear();
                        data_active_escape[sock] = false;
                        data_packet[sock] = false;
                    } else if (result == 0 || result == 1) {  // more reading to do
                        if (result == 0)
                            data_active_escape[sock] = false;
                        else
                            data_active_escape[sock] = true;
                    } else {
                        ascii_error();
                        printf("Unknown return type from parse_osc_slip_data!!!!!\n");
                        ascii_normal();
                        data_map[sock].clear();
                        data_active_escape[sock] = false;
                        data_packet[sock] = false;
                    }

                    // let's leave out the 10M catchall for the moment

                    // if last read was a full res
                    // then try to keep reading from current connection
                    // otherwise break, and try a new connection
                    if (fullbuf)
                        continue;
                    else {
                        pos++;
                        break;
                    }
                }
            } else {
                pos++;
            }
        }
    }
    // std::cout << "Close any client sockets" << std::endl;
    std::vector<int>::iterator pos = client_sockets.begin();
    while (pos != client_sockets.end()) {  // check through all fd's for matches against FD_ISSET
        int sock = *pos;
        if (sock < 0) {
            std::cout << "BAD FILE DESCRIPTOR!" << std::endl;
            pos = client_sockets.erase(pos);  // erase returns next pos
            continue;
        }
        FD_CLR(sock, &rfd);
        data_map.erase(sock);
        std::cout << "CLOSE CLIENT-SOCKET" << std::endl;
        close(sock);
        std::cout << "DONE-CLOSING_CLIENT" << std::endl;
        pos = client_sockets.erase(pos);  // erase returns next pos
    }
    if (close(socket_fd)) {
        std::cerr << "SchemeProcess Error: Error closing server socket" << std::endl;
        perror(nullptr);
    }
    delete sop;
    std::cout << "Exiting server thread" << std::endl;
    return nullptr;
}
#endif

OSC::OSC()
    : sc(nullptr), scheme_real_type('f'), msg_include_netaddr(false),
      threadOSC(&osc_mesg_callback, this, "OSC"),
#ifndef _WIN32
      m_tcpSocketFd(-1),
#endif
      conn_type(OSC_UDP_TYPE), started(false), nativeOSC(nullptr), nativeUDP(nullptr) {
}

void OSC::schemeInit(SchemeProcess* scm) {
    // scm->addForeignFunc("osc-send-msg", &OSC::sendOSC);
    // scm->addGlobalCptr((char*)"*io:osc-send-msg*",mk_cb(this,OSC,sendOSC));
    scm->addForeignFunc("io:osc:start-server", &OSC::registerScheme);
    scm->addForeignFunc("io:osc:set-real-64bit?", &OSC::set_real_type);
    scm->addForeignFunc("io:osc:netaddress?", &OSC::set_msg_include_netaddr);

    // scm->addGlobal("*samplerate*",mk_integer(scm->getSchemeEnv(),AUHost::SAMPLERATE));
}

// Encode one Scheme argument list into `out`, appending a type tag per value.
// The writer grows as it goes, so the size of the arguments is bounded by what
// the datagram can carry, not by a fixed scratch buffer.
void OSC::processArgs(pointer arg, osc::Writer& out, std::string& typetags, scheme* _sc,
                      char realType) {
#ifdef _OSC_DEBUG_
    printf("PROCESS ARGS\n");
#endif
    int items = list_length(_sc, arg);
    for (int i = 0; i < items; ++i) {
        if (is_string(pair_car(arg))) {
            out.string(string_value(pair_car(arg)));
            typetags += "s";
        } else if (is_pair(pair_car(arg))) {
            typetags += "[";
            processArgs(pair_car(arg), out, typetags, _sc, realType);
            typetags += "]";
        } else if (is_vector(pair_car(arg)) || is_symbol(pair_car(arg))) {
            arg = pair_cdr(arg);
            continue;
        } else if (is_integer(pair_car(arg))) {
            out.int32(static_cast<int32_t>(ivalue(pair_car(arg))));
            typetags += "i";
        } else if (is_real(pair_car(arg))) {
            if (realType == 'f') {
                out.float32(static_cast<float>(rvalue(pair_car(arg))));
                typetags += "f";
            } else {
                out.float64(rvalue(pair_car(arg)));
                typetags += "d";
            }
        }
        arg = pair_cdr(arg);
    }
}

void OSC::sendOSC(TaskI* task) {
    Task<SchemeObj*>* t = static_cast<Task<SchemeObj*>*>(task);
    pointer args = t->getArg()->getValue();
    scheme* _sc = t->getArg()->getScheme();

    // This runs on the scheduler thread, not under the FFI trampoline, so a
    // missing server has to be reported rather than thrown.
    OSC* osc = OSC::I(_sc);
    if (!osc) {
        printf("OSC Error: no OSC server for this interpreter\n");
        delete t->getArg();
        return;
    }

    const char* host = string_value(pair_car(args));
    int port = ivalue(pair_cadr(args));
#ifdef _OSC_DEBUG_
    std::cout << "SENDTO: " << host << "  ON PORT: " << port << std::endl;
#endif

    std::string typetags(",");
    osc::Writer argbytes;
    processArgs(pair_cadddr(args), argbytes, typetags, _sc, osc->scheme_real_type);

    osc::Writer message;
    message.string(string_value(pair_caddr(args)));
    message.string(typetags);
    message.append(argbytes);

#ifdef _OSC_DEBUG_
    std::cout << "SENDING MSG of size: " << message.size() << std::endl;
#endif

    uint32_t resolved = extemp::net_util::resolve_ipv4(host);
    if (!resolved) {
        printf("OSC Error: could not resolve host name: %s\n", host);
        delete t->getArg();
        return;
    }
    sockaddr_in destination;
    std::memset(&destination, 0, sizeof(destination));
    destination.sin_family = AF_INET;
    destination.sin_port = htons(static_cast<uint16_t>(port));
    destination.sin_addr.s_addr = resolved;

    // Normally we send from the server socket, so replies come back to the
    // port the user started the server on. A TCP-OSC server has no UDP socket,
    // so fall back to a throwaway one.
    UdpSocket fallback;
    UdpSocket* socket = &osc->getSocket();
    if (!socket->isOpen()) {
        if (!fallback.open()) {
            printf("OSC Error: could not open a socket to send from\n");
            delete t->getArg();
            return;
        }
        socket = &fallback;
    }

    // sendto reports the byte count, never an errno -- the reason for a
    // failure is in errno / WSAGetLastError.
    if (socket->sendTo(message.data(), message.size(), destination) < 0) {
        const int error = UdpSocket::lastError();
#ifndef _WIN32
        if (error == EMSGSIZE) {
#else
        if (error == WSAEMSGSIZE) {
#endif
            printf("Error: OSC message of %zu bytes is too large for one UDP datagram\n",
                   message.size());
        } else {
            printf("Error: problem sending OSC message: %s\n",
                   UdpSocket::errorText(error).c_str());
        }
    }

    delete t->getArg();
}

pointer OSC::set_real_type(scheme* _sc, pointer args) {
    OSC* osc = osc_for_scheme(_sc);

    if (pair_car(args) == _sc->T) {
        osc->scheme_real_type = 'd';
    } else {
        osc->scheme_real_type = 'f';
    }
    return _sc->T;
}

pointer OSC::set_msg_include_netaddr(scheme* _sc, pointer args) {
    OSC* osc = osc_for_scheme(_sc);

    if (pair_car(args) == _sc->T) {
        osc->msg_include_netaddr = 1;
    } else {
        osc->msg_include_netaddr = 0;
    }
    return _sc->T;
}

// Every OSC server started for an interpreter, keyed by port. Servers live for
// the life of the process -- their receive threads run an unconditional loop
// with no shutdown path, so destroying one while its thread runs would be a
// use-after-free -- but keeping them in a registry means re-running
// io:osc:start-server on a port reuses that server instead of stacking up
// another socket and thread on every call.
static std::map<scheme*, std::map<int, OSC*>>& oscServers() {
    static auto* servers = new std::map<scheme*, std::map<int, OSC*>>();
    return *servers;
}

pointer OSC::registerScheme(scheme* _sc, pointer args) {
    const int port = ivalue(pair_car(args));
    const char* name = string_value(pair_cadr(args));

    OSC* osc = nullptr;
    {
        std::lock_guard<std::mutex> lock(SCHEME_MAP_MUTEX);
        auto& servers = oscServers()[_sc];
        auto entry = servers.find(port);
        if (entry == servers.end()) {
            entry = servers.emplace(port, new OSC()).first;
        }
        osc = entry->second;
        // I() answers with the most recently started server, which is the one
        // io:osc:send and the type-flag foreign functions act on.
        SCHEME_MAP[_sc] = osc;
    }
    osc->fname = name;

    // should we use native callback?
    if (pair_cddr(args) != _sc->NIL && is_cptr(pair_caddr(args))) {
        if (pair_cdddr(args) != _sc->NIL && pair_cadddr(args) == _sc->T) {
            osc->setNativeUDP((int (*)(char*, int))cptr_value(pair_caddr(args)));
            osc->setNativeOSC(nullptr);
        } else {
            osc->setNativeOSC((int (*)(char*, char*, char*, int))cptr_value(pair_caddr(args)));
            osc->setNativeUDP(nullptr);
        }
    } else {
        osc->setNativeOSC(nullptr);
        osc->setNativeUDP(nullptr);
    }

    // setup server port
    // check type of connection: UDP (default) or TCP
    if (list_length(_sc, args) == 3 && is_string(pair_caddr(args)) &&
        std::strcmp(string_value(pair_caddr(args)), "TCP-OSC") == 0) {
        osc->setConnectionType(OSC_TCP_TYPE);
    } else {
        osc->setConnectionType(OSC_UDP_TYPE);
    }

    if (osc->getConnectionType() == OSC_UDP_TYPE) {

        SchemeProcess* scm = _sc->m_process;
        scm->addGlobalCptr("*io:osc:send-msg*", mk_cb(osc, OSC, sendOSC));

        if (!osc->getSocket().isOpen()) {
            if (!osc->getSocket().bindAny(port)) {
                ascii_error();
                printf("Error opening OSC socket on port %d: %s\n", port,
                       UdpSocket::errorText(UdpSocket::lastError()).c_str());
                ascii_normal();
                return _sc->F;
            }
            printf("Starting OSC server on port: %d calling back to %s\n", port, name);
        } else {
            printf("OSC server on port %d now calling back to %s\n", port, name);
        }

        if (!osc->getStarted()) {
            osc->getThread().start();
            osc->setStarted(true);
        }
        osc->sc = _sc;
        return _sc->NIL;
    }
    // TCP setup
    if (osc->getConnectionType() == OSC_TCP_TYPE) {

        SchemeProcess* scm = _sc->m_process;
        scm->addGlobalCptr("*io:osc:send-msg*", mk_cb(osc, OSC, sendOSC));

        // SchemeProcess* scm = new extemp::SchemeProcess(UNIV::SHARE_DIR,
        // std::string("tcp-osc-server"), port, 0); scm->start();
        // scm->addGlobalCptr((char*)"*io:osc:send-msg*",mk_cb(osc,OSC,sendOSC));

#ifndef _WIN32
        int socket_fd = socket(AF_INET, SOCK_STREAM, 0);
        if (socket_fd == -1) {
            std::cout << "Error opening TCP-OSC socket" << std::endl;
            return _sc->F;
        }
        int t_reuse = 1;
        int result = setsockopt(socket_fd,        /* socket affected */
                                IPPROTO_TCP,      /* set option at TCP level */
                                TCP_NODELAY,      /* name of option */
                                (char*)&t_reuse,  /* the cast is historical cruft */
                                sizeof(t_reuse)); /* length of option value */
        result += setsockopt(socket_fd, SOL_SOCKET, SO_REUSEADDR, (char*)&t_reuse, sizeof(t_reuse));
        result += setsockopt(socket_fd, SOL_SOCKET, SO_BROADCAST, (char*)&t_reuse, sizeof(t_reuse));

        if (result < 0) {
            std::cout << "Error opening TCP-OSC socket" << std::endl;
            return _sc->F;
        }
        // Bind Server Socket
        struct sockaddr_in server_address;
        size_t server_address_size = sizeof(server_address);

        // start socket
        memset((char*)&server_address, 0, server_address_size);

        server_address.sin_family = AF_INET;
        server_address.sin_port = htons(port);
        server_address.sin_addr.s_addr = htonl(INADDR_ANY);  // set server's IP

        if (bind(socket_fd, (struct sockaddr*)&server_address, server_address_size) == -1) {
            std::cout << "Error binding TCP-OSC server address to socket" << std::endl;
            return _sc->F;
        }
        if (listen(socket_fd, 5) == -1) {
            std::cout << "Problem listening on TCP-OSC socket." << std::endl;
            return _sc->F;
        }

        osc->setTcpSocketFD(socket_fd);

        ascii_warning();
        printf("Started TCP-OSC server on port %d\n", port);
        ascii_normal();

#endif
        if (!osc->getStarted()) {
            scm_osc_pair* sop = new scm_osc_pair;
            sop->scm_p = scm;
            sop->osc_p = osc;
            osc->getThread().start(&tcp_osc_server_thread, sop);
            osc->setStarted(true);
        }
        osc->sc = _sc;
    }
    return _sc->NIL;
}
}  // namespace extemp
