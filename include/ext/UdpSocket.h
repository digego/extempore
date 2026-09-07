#pragma once

// A UDP socket with RAII lifetime. The platform split (int fd vs SOCKET,
// close vs closesocket, errno vs WSAGetLastError) lives in here so callers are
// written once, and closing on destruction means a socket cannot outlive the
// object that owns it.
//
// Winsock must already be initialised by the process (Extempore.cpp does this
// at startup); NetUtil.h's ensure_winsock_initialised() covers standalone
// consumers such as the unit tests.

#include <cstdint>
#include <cstring>
#include <string>

#ifdef _WIN32
#include <winsock2.h>
#include <ws2tcpip.h>
#else
#include <arpa/inet.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>
#include <cerrno>
#endif

namespace extemp {

class UdpSocket {
  public:
#ifdef _WIN32
    using Handle = SOCKET;
    static constexpr Handle INVALID = INVALID_SOCKET;
#else
    using Handle = int;
    static constexpr Handle INVALID = -1;
#endif

    UdpSocket() = default;
    ~UdpSocket() {
        close();
    }
    UdpSocket(const UdpSocket&) = delete;
    UdpSocket& operator=(const UdpSocket&) = delete;

    // Non-blocking, broadcast-capable UDP socket, unbound. Sending only.
    bool open() {
        close();
        Handle handle = ::socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
        if (handle == INVALID) {
            return false;
        }
        int broadcast = 1;
        ::setsockopt(handle, SOL_SOCKET, SO_BROADCAST,
                     reinterpret_cast<const char*>(&broadcast), sizeof(broadcast));
        setNonBlocking(handle);
        m_handle = handle;
        return true;
    }

    // As open(), then bound to Port on every interface. Leaves the socket
    // closed if either step fails.
    bool bindAny(int Port) {
        if (!open()) {
            return false;
        }
        sockaddr_in address;
        std::memset(&address, 0, sizeof(address));
        address.sin_family = AF_INET;
        address.sin_port = htons(static_cast<uint16_t>(Port));
        address.sin_addr.s_addr = htonl(INADDR_ANY);
        if (::bind(m_handle, reinterpret_cast<sockaddr*>(&address), sizeof(address)) != 0) {
            close();
            return false;
        }
        return true;
    }

    // Returns the number of bytes read, or -1 (nothing available, or an
    // error -- see lastError()). Sender is only meaningful on success.
    long recvFrom(void* Buffer, std::size_t Size, sockaddr_in& Sender) {
        std::memset(&Sender, 0, sizeof(Sender));
#ifdef _WIN32
        int senderSize = static_cast<int>(sizeof(Sender));
        return ::recvfrom(m_handle, static_cast<char*>(Buffer), static_cast<int>(Size), 0,
                          reinterpret_cast<sockaddr*>(&Sender), &senderSize);
#else
        socklen_t senderSize = sizeof(Sender);
        return ::recvfrom(m_handle, Buffer, Size, 0, reinterpret_cast<sockaddr*>(&Sender),
                          &senderSize);
#endif
    }

    // Returns the number of bytes sent, or -1 (see lastError()).
    long sendTo(const void* Buffer, std::size_t Size, const sockaddr_in& Dest) {
#ifdef _WIN32
        return ::sendto(m_handle, static_cast<const char*>(Buffer), static_cast<int>(Size), 0,
                        reinterpret_cast<const sockaddr*>(&Dest), sizeof(Dest));
#else
        return ::sendto(m_handle, Buffer, Size, 0, reinterpret_cast<const sockaddr*>(&Dest),
                        sizeof(Dest));
#endif
    }

    void close() {
        if (m_handle == INVALID) {
            return;
        }
#ifdef _WIN32
        ::closesocket(m_handle);
#else
        ::close(m_handle);
#endif
        m_handle = INVALID;
    }

    bool isOpen() const {
        return m_handle != INVALID;
    }
    Handle handle() const {
        return m_handle;
    }

    static int lastError() {
#ifdef _WIN32
        return WSAGetLastError();
#else
        return errno;
#endif
    }

    static std::string errorText(int Code) {
#ifdef _WIN32
        return "winsock error " + std::to_string(Code);
#else
        return std::strerror(Code);
#endif
    }

    // True when a failed recvFrom/sendTo simply had nothing to do.
    static bool wouldBlock(int Code) {
#ifdef _WIN32
        return Code == WSAEWOULDBLOCK;
#else
        return Code == EAGAIN || Code == EWOULDBLOCK;
#endif
    }

    // Dotted-quad form of an address, using the reentrant inet_ntop rather
    // than inet_ntoa's shared static buffer.
    static std::string addressText(const sockaddr_in& Address) {
        char text[INET_ADDRSTRLEN] = {0};
        if (!::inet_ntop(AF_INET, &Address.sin_addr, text, sizeof(text))) {
            return std::string();
        }
        return std::string(text);
    }

  private:
    static void setNonBlocking(Handle Socket) {
#ifdef _WIN32
        u_long mode = 1;
        ::ioctlsocket(Socket, FIONBIO, &mode);
#else
        ::fcntl(Socket, F_SETFL, O_NONBLOCK);
#endif
    }

    Handle m_handle{INVALID};
};

}  // namespace extemp
