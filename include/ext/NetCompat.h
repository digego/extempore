#pragma once

// Berkeley-sockets compatibility shim: Winsock2 on Windows, the BSD socket
// headers everywhere else. Include this and write one code path in terms of
// SOCKET, INVALID_SOCKET, closesocket() and the extemp::net helpers below,
// instead of branching on _WIN32 at each call site.
//
// For a UDP socket with RAII lifetime, prefer ext/UdpSocket.h, which is built
// on the same primitives.

#ifdef _WIN32
// As in UNIV.h: the SDK version defaults must be in scope before any Windows
// header, or winsock2.h hides WSAPoll and struct pollfd behind _WIN32_WINNT.
#include <SDKDDKVer.h>
#endif

// NetUtil.h supplies the platform socket headers and the one-time WSAStartup.
#include "ext/NetUtil.h"

#include <cstddef>
#include <string>
#include <vector>

#ifdef _WIN32
// winsock2.h already supplies SOCKET, INVALID_SOCKET, closesocket() and the
// WSAPoll flavour of struct pollfd.

// Winsock spells the shutdown(2) constants SD_*; keep the POSIX names.
constexpr int SHUT_RD = SD_RECEIVE;
constexpr int SHUT_WR = SD_SEND;
constexpr int SHUT_RDWR = SD_BOTH;
#else
#include <netinet/tcp.h>
#include <poll.h>
#include <unistd.h>
#include <cerrno>
#include <cstring>

using SOCKET = int;
inline constexpr SOCKET INVALID_SOCKET = -1;

inline int closesocket(SOCKET Socket) {
    return ::close(Socket);
}
#endif

namespace extemp {
namespace net {

// Winsock refuses every socket call until WSAStartup has run; a no-op on
// POSIX.
inline void ensureInitialised() {
    net_util::ensure_winsock_initialised();
}

// The errno of the socket API. Winsock keeps its own per-thread code rather
// than setting errno.
inline int lastError() {
#ifdef _WIN32
    return WSAGetLastError();
#else
    return errno;
#endif
}

inline std::string errorText(int Code) {
#ifdef _WIN32
    return "winsock error " + std::to_string(Code);
#else
    return std::strerror(Code);
#endif
}

// True when the call failed only because it would have blocked.
inline bool wouldBlock(int Code) {
#ifdef _WIN32
    return Code == WSAEWOULDBLOCK;
#else
    return Code == EAGAIN || Code == EWOULDBLOCK;
#endif
}

// True when the call was cut short by a signal. Winsock has no such case.
inline bool interrupted([[maybe_unused]] int Code) {
#ifdef _WIN32
    return false;
#else
    return Code == EINTR;
#endif
}

// send(2)/recv(2) over a connected socket, returning the byte count or -1.
// Winsock takes char* buffers with int lengths and returns int; that is the
// whole of the difference.
inline long send(SOCKET Socket, const void* Buffer, std::size_t Length) {
#ifdef _WIN32
    return ::send(Socket, static_cast<const char*>(Buffer), static_cast<int>(Length), 0);
#else
    return ::send(Socket, Buffer, Length, 0);
#endif
}

inline long recv(SOCKET Socket, void* Buffer, std::size_t Length) {
#ifdef _WIN32
    return ::recv(Socket, static_cast<char*>(Buffer), static_cast<int>(Length), 0);
#else
    return ::recv(Socket, Buffer, Length, 0);
#endif
}

// poll(2) over a set of sockets. WSAPoll is Winsock's analogue and shares both
// the struct pollfd layout and the return convention.
inline int poll(std::vector<pollfd>& Fds, int TimeoutMs) {
#ifdef _WIN32
    return ::WSAPoll(Fds.data(), ULONG(Fds.size()), TimeoutMs);
#else
    return ::poll(Fds.data(), nfds_t(Fds.size()), TimeoutMs);
#endif
}

}  // namespace net
}  // namespace extemp
