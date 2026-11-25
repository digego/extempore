#pragma once

// The nREPL client built into the extempore binary: extempore send, extempore
// status and extempore repl, and the REPL a session runs with --repl. They
// talk to a running extempore's nREPL server (include/NreplServer.h) and start
// no runtime of their own.

#include "Bencode.h"
#include "ext/NetCompat.h"

#include <cstdint>
#include <optional>
#include <string>

namespace extemp::nrepl::client {

// One connection to a server. Requests get ids; read() returns the next
// message, whichever request it answers. One thread may send while another
// reads.
class Connection {
  public:
    Connection() = default;
    Connection(const Connection&) = delete;
    Connection& operator=(const Connection&) = delete;
    ~Connection();

    // Empty on success, else why not.
    std::string open(const std::string& Host, std::uint16_t Port);
    // Sends Msg under a fresh id, which it returns ("" if the send failed).
    std::string send(bencode::Dict Msg);

    enum class Read : std::uint8_t { Message, Timeout, Closed };
    // The next message. TimeoutMs < 0 waits for as long as it takes.
    Read read(bencode::Dict& Out, int TimeoutMs = -1);
    // The reply to a request which gets exactly one (describe, clone, close).
    bool request(bencode::Dict Msg, bencode::Dict& Reply);
    // Ends a read() in progress on another thread, which then returns Closed.
    void shutdown();

  private:
    SOCKET m_socket = INVALID_SOCKET;
    std::string m_buffer;
    int m_ids = 0;
};

// True if Msg's status list has Status in it.
bool hasStatus(const bencode::Dict& Msg, std::string_view Status);

// Which server, and which of its Scheme processes.
struct Target {
    std::string host = "127.0.0.1";
    std::optional<std::uint16_t> port;  // unset: findPort
    std::string process;                // empty: primary
};

// The port to connect to: Given, else $EXTEMPORE_NREPL_PORT, else a
// .nrepl-port file in NearFile's directory (the working directory if NearFile
// is empty) or one above it, else the default.
std::uint16_t findPort(std::optional<std::uint16_t> Given, const std::string& NearFile);

struct SendOptions {
    std::optional<std::string> code;  // unset: the file, or stdin
    std::string file;                 // alone: load the whole file
    std::size_t line = 0;             // with file: the top-level form at line and column
    std::size_t column = 1;
};

// Each returns the process's exit status: 0, 1 if the evaluation failed, 2 if
// there was nothing to send or no server to send it to.
int send(const Target& To, const SendOptions& Options);
int status(const Target& To);
// An interactive REPL on this terminal. WaitForServer retries the connection
// for a few seconds, for a session starting its own.
int repl(const Target& To, bool WaitForServer);

// For the SIGINT handler: if a REPL is waiting on an evaluation, asks it to
// interrupt that and returns true. Async-signal-safe.
bool interruptFromSignal();

}  // namespace extemp::nrepl::client
