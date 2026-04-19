#pragma once

#include <cstdint>

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

// Resolves `host` (IPv4) to an address in network byte order.
// Returns 0 on failure. Thread-safe (uses getaddrinfo, not gethostbyname).
inline uint32_t resolve_ipv4(const char* host) {
    if (!host) return 0;
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
