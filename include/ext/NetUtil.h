#pragma once

#include <cstdint>
#include <mutex>

#ifdef _WIN32
#include <winsock2.h>
#include <ws2tcpip.h>
#else
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>
#endif

namespace extemp {
namespace net_util {

// On Windows, getaddrinfo silently returns WSANOTINITIALISED until
// WSAStartup has been called. The extempore binary initialises Winsock
// elsewhere, but standalone consumers (e.g. the cpp-unit tests) don't.
// Do it here, once, on the first resolve_ipv4 call.
inline void ensure_winsock_initialised() {
#ifdef _WIN32
    static std::once_flag flag;
    std::call_once(flag, []() {
        WSADATA wsa;
        WSAStartup(MAKEWORD(2, 2), &wsa);
    });
#endif
}

// Resolves `host` (IPv4) to an address in network byte order.
// Returns 0 on failure. Thread-safe (uses getaddrinfo, not gethostbyname).
inline uint32_t resolve_ipv4(const char* host) {
    if (!host) return 0;
    ensure_winsock_initialised();
    struct addrinfo hints{};
    hints.ai_family = AF_INET;
    hints.ai_socktype = SOCK_STREAM;
    struct addrinfo* res = nullptr;
    if (getaddrinfo(host, nullptr, &hints, &res) != 0 || !res) return 0;
    uint32_t addr = reinterpret_cast<sockaddr_in*>(res->ai_addr)->sin_addr.s_addr;
    freeaddrinfo(res);
    return addr;
}

}  // namespace net_util
}  // namespace extemp
