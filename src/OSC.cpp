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
#include <string>
#include <iomanip>
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

#ifdef _WIN32
#else
#include <sys/errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netdb.h> /* host to IP resolution       */
#include <sys/fcntl.h>
#include <arpa/inet.h>
#endif

// FD_COPY IS BSD ONLY
#ifndef FD_COPY
#define FD_COPY(f, t) (void)(*(t) = *(f))
#endif

// constants for SLIP TCP-packetizing
// from http://tools.ietf.org/html/rfc1055

#define SLIP_END (char)0300     /* indicates begin/end of packet */
#define SLIP_ESC (char)0333     /* indicates byte stuffing */
#define SLIP_ESC_END (char)0334 /* ESC ESC_END means END data byte */
#define SLIP_ESC_ESC (char)0335 /* ESC ESC_ESC means ESC data byte */

#define OSC_UDP_TYPE 1
#define OSC_TCP_TYPE 2

// for thread args (will be passed as void*)
typedef struct scm_osc_pair {
    extemp::SchemeProcess* scm_p;
    extemp::OSC* osc_p;
} scm_osc_pair;

///////////////////////////////////////////////
//
// OSC encodes numbers big-endian. On a little-endian host -- every platform we
// target -- converting a value to/from the wire is a byte reversal; std::endian
// makes that assumption explicit and std::bit_cast does the type-puns without
// the old unsigned char* aliasing. The eight functions keep their extern "C"
// signatures: they are registered into the JIT by name.
//
namespace {
template <class T>
T osc_byteswap(T value) {
    if constexpr (std::endian::native == std::endian::little) {
        auto bytes = std::bit_cast<std::array<std::byte, sizeof(T)>>(value);
        std::reverse(bytes.begin(), bytes.end());
        return std::bit_cast<T>(bytes);
    }
    return value;  // big-endian host: already in OSC wire order
}
}  // namespace

uint64_t swap64f(double d) {
    return osc_byteswap(std::bit_cast<uint64_t>(d));
}

double unswap64f(uint64_t a) {
    return std::bit_cast<double>(osc_byteswap(a));
}

uint32_t swap32f(float f) {
    return osc_byteswap(std::bit_cast<uint32_t>(f));
}

float unswap32f(uint32_t a) {
    return std::bit_cast<float>(osc_byteswap(a));
}

uint64_t swap64i(uint64_t d) {
    return osc_byteswap(d);
}

uint64_t unswap64i(uint64_t a) {
    return osc_byteswap(a);
}

uint32_t swap32i(uint32_t f) {
    return osc_byteswap(f);
}

uint32_t unswap32i(uint32_t a) {
    return osc_byteswap(a);
}

///////////////////////////////////////////////////////////////

// #define _OSC_DEBUG_

namespace extemp {

std::map<scheme*, OSC*> OSC::SCHEME_MAP;

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

// Backslash-escape double quotes so a string can be embedded in the Scheme
// expression handed to the interpreter.
static void osc_escape_quotes(std::string& str) {
    for (unsigned i = 0; i < str.length(); i++) {
        if (str.at(i) == '"') {
            if (i == 0 || str.at(i - 1) != '\\') {
                str.insert(i, "\\");
                i++;
            }
        }
    }
}

// Build the Scheme call string for one parsed message and queue it. Argument
// reads go through oscpp's bounds-checked stream, so a truncated argument throws
// and the message is dropped by the caller.
static void osc_emit_scheme_message(SchemeProcess* proc, const char* fname, double t,
                                    const char* address, OSCPP::Server::ArgStream argv,
                                    bool include_netaddr, const std::string& netaddy, int netport) {
    std::stringstream ss;
    ss << "(" << fname << " " << std::fixed << std::showpoint << std::setprecision(23) << t << " \""
       << address << "\"";
    if (include_netaddr)
        ss << " \"" << netaddy << "\" " << netport;
    auto streams = argv.state();
    auto tags = std::get<0>(streams);
    auto args = std::get<1>(streams);
    while (!tags.atEnd()) {
        switch (tags.getChar()) {
            case 'i': ss << " " << args.getInt32(); break;
            case 'f': ss << " " << args.getFloat32(); break;
            case 'd': ss << " " << args.getFloat64(); break;
            case 'h': ss << " " << static_cast<int64_t>(args.getUInt64()); break;
            case 't': ss << " " << osc_ntp_to_seconds(args.getUInt64()); break;
            case 's': {
                std::string s(args.getString());
                osc_escape_quotes(s);
                ss << " \"" << s << "\"";
                break;
            }
            case '[': ss << " (list "; break;
            case ']': ss << ")"; break;
            default: return;  // unsupported type tag: drop the message
        }
    }
    ss << ")";
    if (proc != nullptr)
        proc->createSchemeTask(new std::string(ss.str()), "OSC TASK",
                               SchemeTask::Type::LOCAL_PROCESS_STRING);
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
        osc_emit_scheme_message(proc, osc->fname, t, msg.address(), msg.args(), include_netaddr,
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
    while (true) {
#ifdef _WIN32
        std::experimental::net::ip::udp::endpoint sender;
        long bytes_read = 0;
        try {
            bytes_read = osc->getSocketFD()->receive_from(
                std::experimental::net::buffer(osc->getMessageData(), 20000),
                sender);  //*osc->getClientAddress());
        } catch (std::exception& e) {
            // A receive error (e.g. Windows' spurious WSAECONNRESET after a prior
            // send to a closed port) must not tear down the whole process -- and
            // hence any live performance. Log, pause briefly, and retry, mirroring
            // the POSIX path's behaviour when recvfrom fails below.
            std::cout << "OSC Message Receive Exception: " << e.what() << std::endl;
            std::this_thread::sleep_for(std::chrono::microseconds(1000));
            continue;
        }
        std::string netaddy = osc->getClientAddress()->address().to_string();
        int netport = (int)osc->getClientAddress()->port();
#else
        long bytes_read = recvfrom(*osc->getSocketFD(), osc->getMessageData(), 70000, 0,
                                   (struct sockaddr*)osc->getClientAddress(),
                                   (socklen_t*)osc->getClientAddressSize());
        std::string netaddy(inet_ntoa(osc->getClientAddress()->sin_addr));
        int netport = (int)ntohs(osc->getClientAddress()->sin_port);
#endif
        if (osc->getNativeUDP() != nullptr) {
            char* args = osc->getMessageData();
            int (*nativeUDP)(char*, int) = osc->getNativeUDP();
            nativeUDP(args, bytes_read);
        }
        if (bytes_read > -1 && osc->getNativeUDP() == nullptr) {
            // printf("udp packet size(%lld)\n",bytes_read);
            // std::cout << "OSC from client port: " << osc->getClientAddress() << " " <<
            // osc->getAddress() <<  std::endl;
            osc_dispatch_packet(osc, osc->sc != nullptr ? osc->sc->m_process : nullptr,
                                osc->getMessageData(), static_cast<int>(bytes_read),
                                osc->msg_include_netaddr, netaddy, netport);
        } else {
            std::this_thread::sleep_for(std::chrono::microseconds(1000));
        }
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

    int socket_fd = *(osc->getSocketFD());

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
        FD_COPY(&rfd, &c_rfd);
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

OSC::OSC() : threadOSC(&osc_mesg_callback, this, "OSC"), message_length(0), started(false) {
#ifdef _WIN32
    io_service = new std::experimental::net::io_context;
    osc_address = new std::experimental::net::ip::udp::endpoint();
    osc_client_address = new std::experimental::net::ip::udp::endpoint();
#endif
    send_from_serverfd = 1;  // default to true!
    msg_include_netaddr = 0;
    scheme_real_type = 'f';
    scheme_integer_type = 'i';
}

void OSC::schemeInit(SchemeProcess* scm) {
    // scm->addForeignFunc("osc-send-msg", &OSC::sendOSC);
    // scm->addGlobalCptr((char*)"*io:osc-send-msg*",mk_cb(this,OSC,sendOSC));
    scm->addForeignFunc("io:osc:start-server", &OSC::registerScheme);
    scm->addForeignFunc("io:osc:set-real-64bit?", &OSC::set_real_type);
    scm->addForeignFunc("io:osc:set-integer-64bit?", &OSC::set_integer_type);
    scm->addForeignFunc("io:osc:send-from-server-socket?", &OSC::send_from_server_socket);
    scm->addForeignFunc("io:osc:netaddress?", &OSC::set_msg_include_netaddr);

    // scm->addGlobal("*samplerate*",mk_integer(scm->getSchemeEnv(),AUHost::SAMPLERATE));
}

int OSC::setOSCString(char* data, std::string* str) {
#ifdef _OSC_DEBUG_
    std::cout << "SET OSC STRING = " << *str << std::endl;
#endif
    int n = 4 - (int)fmod((double)str->length(), 4.0);
    for (int i = 0; i < n; ++i) {
        str->push_back('\0');
    }
    const char* str_d = str->data();
    for (unsigned i = 0; i < str->length(); ++i) {
        data[i] = str_d[i];
    }
    return str->length();
}

int OSC::setOSCfloat(char* data, float* f) {
#ifdef _OSC_DEBUG_
    std::cout << "SET OSC FLOAT 32 = " << *f << std::endl;
#endif
    uint32_t sf = swap32f(*f);
    char* byte_array = (char*)&sf;
    for (int i = 0; i < 4; ++i) {
        data[i] = byte_array[i];
    }
    return 4;
}

int OSC::setOSCdouble(char* data, double* f) {
#ifdef _OSC_DEBUG_
    std::cout << "SET OSC FLOAT 64 = " << *f << std::endl;
#endif
    uint64_t sf = swap64f(*f);
    char* byte_array = (char*)&sf;
    for (int i = 0; i < 8; ++i) {
        data[i] = byte_array[i];
    }
    return 8;
}

int OSC::setOSCTimestamp(char* data, double d) {
    uint32_t seconds = static_cast<uint32_t>(d);

    double fractional = d - (double)seconds;
    seconds += 3187296000ul;  // 1543503872;

    uint32_t fractionali = (uint32_t)(fractional * 4294967296.0);

    uint32_t out[2] = {swap32i(seconds), swap32i(fractionali)};
    std::memcpy(data, out, sizeof(out));

    // great! now we have both bits of the NTP puzzle, we just need to jam them into a datastream
    return 8;
}

int OSC::setOSCInt(char* data, int* i) {
#ifdef _OSC_DEBUG_
    std::cout << "SET OSC INT = " << *i << std::endl;
#endif
    *i = swap32i(*i);
    char* byte_array = (char*)i;
    for (int j = 0; j < 4; ++j) {
        data[j] = byte_array[j];
    }
    return 4;
}

int OSC::setOSCLong(char* data, int64_t* l) {
    *l = swap64i(*l);
    char* byte_array = (char*)l;
    for (int i = 0; i < 8; ++i) {
        data[i] = byte_array[i];
    }
    return 8;
}

void OSC::processArgs(pointer arg, char** tmp, char** ptr, int* lgth, std::string& typetags,
                      scheme* _sc) {
#ifdef _OSC_DEBUG_
    printf("PROCESS ARGS\n");
#endif
    OSC* osc = OSC::I(_sc);
    int ret = 0;
    int items = list_length(_sc, arg);
    for (int i = 0; i < items; ++i) {
        if (is_string(pair_car(arg))) {
            std::string str(string_value(pair_car(arg)));
            ret = OSC::setOSCString(*ptr, &str);
            typetags += "s";
        } else if (is_pair(pair_car(arg))) {
            typetags += "[";
            processArgs(pair_car(arg), tmp, ptr, lgth, typetags, _sc);
            typetags += "]";
            ret = 0;
        } else if (is_vector(pair_car(arg))) {
            arg = pair_cdr(arg);
            continue;
        } else if (is_symbol(pair_car(arg))) {
            arg = pair_cdr(arg);
            continue;
        } else if (is_integer(pair_car(arg))) {
            if (osc->scheme_integer_type == 'i') {
                int val = ivalue(pair_car(arg));
                ret = OSC::setOSCInt(*ptr, &val);
                typetags += "i";
            } else {
                int64_t val = ivalue(pair_car(arg));
                ret = OSC::setOSCLong(*ptr, &val);
                typetags += "h";
            }
        } else if (is_real(pair_car(arg))) {
            if (osc->scheme_real_type == 'f') {
                float val = (float)rvalue(pair_car(arg));
                ret = OSC::setOSCfloat(*ptr, &val);
                typetags += "f";
            } else {
                double val = (double)rvalue(pair_car(arg));
                ret = OSC::setOSCdouble(*ptr, &val);
                typetags += "d";
            }
        }
        *lgth += ret;
        *ptr += ret;
        arg = pair_cdr(arg);
    }
}

// pointer OSC::sendOSC(scheme* _sc, pointer args)
void OSC::sendOSC(TaskI* task) {
    Task<SchemeObj*>* t = static_cast<Task<SchemeObj*>*>(task);
    pointer args = t->getArg()->getValue();
    scheme* _sc = t->getArg()->getScheme();

    char* host = string_value(pair_car(args));
    int port = ivalue(pair_cadr(args));
#ifdef _OSC_DEBUG_
    std::cout << "SENDTO: " << host << "  ON PORT: " << port << std::endl;
#endif
    int length = 0;
    int ret = 0;
    char* ptr;

#ifdef _WIN32
    // std::experimental::net::ip::udp::resolver::iterator end;
    std::experimental::net::ip::udp::resolver resolver(*io_service);
    std::stringstream ss;
    ss << port;
    std::experimental::net::ip::udp::resolver::results_type res =
        resolver.resolve(std::experimental::net::ip::udp::v4(), host, ss.str());
    auto iter = res.begin();
    auto end = res.end();
    std::experimental::net::ip::udp::endpoint sa = *iter;
#else
    struct sockaddr_in sa;

    uint32_t resolved = extemp::net_util::resolve_ipv4(host);
    if (!resolved) {
        printf("OSC Error: Could not resolve host name\n");
        delete t->getArg();
        return;
    }

    memset(&sa, 0, sizeof(sa));
    sa.sin_family = AF_INET;
    sa.sin_port = htons(port);
    sa.sin_addr.s_addr = resolved;
#endif

#ifdef _WIN32
    std::experimental::net::ip::udp::socket* fd = 0;
    if (OSC::I(_sc)->send_from_serverfd) {
        fd = OSC::I(_sc)->getSocketFD();  //  getSendFD();
    }
#else
    int fd = 0;
    if (OSC::I(_sc)->send_from_serverfd) {
        fd = *(OSC::I(_sc)->getSocketFD());  //  getSendFD();
    } else {
        fd = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
    }
#endif

    std::string address(string_value(pair_caddr(args)));
    std::string typetags(",");
    std::string body;

    pointer arg = pair_cadddr(args);
    int tmpsize = 1024;
    std::vector<char> tmp(tmpsize);
    char* tmpPtr = tmp.data();
    ptr = tmpPtr;
    int lgth = 0;
    processArgs(arg, &tmpPtr, &ptr, &lgth, typetags, _sc);

    std::vector<char> message(1024 + tmpsize);
    ptr = message.data();
    ret = OSC::setOSCString(ptr, &address);
    length += ret;
    ptr += ret;
    ret = OSC::setOSCString(ptr, &typetags);
    length += ret;
    ptr += ret;
    memcpy(ptr, tmp.data(), lgth);
    length += lgth;
#ifdef _OSC_DEBUG_
    std::cout << "SENDING MSG: " << message.data() << "  of size: " << length << std::endl;
#endif

#ifdef _WIN32
    int err = 0;
    if (OSC::I(_sc)->send_from_serverfd) {
        err = fd->send_to(std::experimental::net::buffer(message.data(), length), sa);
    } else {
        std::experimental::net::io_context service;
        std::experimental::net::ip::udp::socket socket(service);
        socket.open(std::experimental::net::ip::udp::v4());
        socket.send_to(std::experimental::net::buffer(message.data(), length), sa);
    }
#else
    int err = sendto(fd, message.data(), length, 0, (struct sockaddr*)&sa, sizeof(sa));
    if (!OSC::I(_sc)->send_from_serverfd)
        close(fd);
#endif
    if (err < 0) {
#ifdef _OSC_DEBUG_
        std::cout << "OSC Send Error: " << err << std::endl;
#endif
        if (err == EMSGSIZE) {
            printf("Error: OSC message too large: UDP 8k message MAX\n");
        } else {
            printf("Error: Problem sending OSC message: %d\n", err);
        }
    }

    delete t->getArg();
    return;
    // return _sc->NIL;
}

pointer OSC::set_real_type(scheme* _sc, pointer args) {
    OSC* osc = OSC::I(_sc);

    if (pair_car(args) == _sc->T) {
        osc->scheme_real_type = 'd';
    } else {
        osc->scheme_real_type = 'f';
    }
    return _sc->T;
}

pointer OSC::set_integer_type(scheme* _sc, pointer args) {
    OSC* osc = OSC::I(_sc);

    if (pair_car(args) == _sc->T) {
        osc->scheme_integer_type = 'h';
    } else {
        osc->scheme_integer_type = 'i';
    }
    return _sc->T;
}

pointer OSC::send_from_server_socket(scheme* _sc, pointer args) {
    OSC* osc = OSC::I(_sc);

    if (pair_car(args) == _sc->T) {
        osc->send_from_serverfd = 1;
    } else {
        osc->send_from_serverfd = 0;
    }
    return _sc->T;
}

pointer OSC::set_msg_include_netaddr(scheme* _sc, pointer args) {
    OSC* osc = OSC::I(_sc);

    if (pair_car(args) == _sc->T) {
        osc->msg_include_netaddr = 1;
    } else {
        osc->msg_include_netaddr = 0;
    }
    return _sc->T;
}

pointer OSC::registerScheme(scheme* _sc, pointer args) {
    OSC* osc = new OSC();  // OSC::I();
    SCHEME_MAP[_sc] = osc;
    int port = ivalue(pair_car(args));
    char* name = string_value(pair_cadr(args));
    std::snprintf(osc->fname, sizeof(osc->fname), "%s", name);

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
        strncmp(string_value(pair_caddr(args)), "TCP-OSC", 4) == 0) {
        osc->setConnectionType(OSC_TCP_TYPE);
    } else {
        osc->setConnectionType(OSC_UDP_TYPE);
    }

    if (osc->getConnectionType() == OSC_UDP_TYPE) {

        SchemeProcess* scm = _sc->m_process;
        scm->addGlobalCptr("*io:osc:send-msg*", mk_cb(osc, OSC, sendOSC));

#ifdef _WIN32
        std::experimental::net::ip::udp::endpoint* osc_address = osc->getAddress();
        int port =
            ivalue(pair_car(args));  // [[[imp::NativeScheme::RESOURCES getPreferencesDictionary]
                                     // valueForKey:@"osc_port"] intValue];
        osc_address->port(port);

        try {
            std::experimental::net::ip::udp::socket* sock =
                new std::experimental::net::ip::udp::socket(
                    *osc->getIOService());  //(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
            sock->open(std::experimental::net::ip::udp::v4());
            // sock->io_control(command);
            sock->bind(*osc_address);

            osc->setSocket(sock);
            printf("Starting OSC server on port: %d calling back to %s\n", port, name);
        } catch (std::exception& e) {
            std::cout << "Error establishing OSC socket: is address allready used?" << std::endl;
            return _sc->NIL;
        }
#else
        // UDP setup
        struct sockaddr_in* osc_address = osc->getAddress();
        memset((char*)osc_address, 0, sizeof(*osc_address));
        printf("Starting OSC server on port: %d calling back to %s\n", port, name);

        osc_address->sin_family = AF_INET;
        osc_address->sin_port = htons(port);
        osc_address->sin_addr.s_addr = htonl(INADDR_ANY);  // set server's IP address

        int socket_fd = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
        if (socket_fd == -1) {
            printf("Error opening OSC socket\n");
            std::cout << "Error opening OSC socket" << std::endl;
        }
        int broadcastEnable = 1;
        setsockopt(socket_fd, SOL_SOCKET, SO_BROADCAST, &broadcastEnable,
                   sizeof(broadcastEnable));  // TODO: error check?

        fcntl(socket_fd, F_SETFL, O_NONBLOCK);  // set to non-blocking socket

        if (bind(socket_fd, (struct sockaddr*)osc_address, sizeof(*osc_address)) == -1) {
            printf("Error opening OSC socket\n");
            std::cout << "Error binding OSC address to socket" << std::endl;
        }

        osc->setSocketFD(socket_fd);

        // setup client struct.
        struct sockaddr_in* osc_client_address = osc->getClientAddress();
        osc->setClientAddressSize(sizeof(*osc_client_address));
#endif
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

        osc->setSocketFD(socket_fd);

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
