#pragma once

// The nREPL server (backlog/docs/doc-002): bencode over TCP on loopback, one
// per extempore process. A server thread owns the sockets and the session
// table and answers the ops that need no Scheme (describe, clone, close,
// ls-sessions, interrupt). The rest become SchemeTask::Type::NREPL tasks on
// the session's process, where runRequest hands them to nrepl:handle
// (runtime/nrepl.xtm) and sends the request's final done.

#include <cstdint>
#include <string>

struct scheme;

namespace extemp::nrepl {

struct Request;

inline constexpr std::uint16_t DEFAULT_PORT = 7888;

// Bind Host:Port and start serving. Port 0 picks a free port and writes it to
// .nrepl-port in the working directory. Prints the banner line clients parse,
// or the reason it couldn't bind, and returns whether it is serving.
bool start(const std::string& Host, std::uint16_t Port);

// Delete .nrepl-port if start wrote one. Async-signal-safe, for the signal
// handlers and (quit), which end the process without running destructors.
void removePortFile();

// Run Request on the calling task thread (the process Scheme belongs to), then
// delete it. Every request gets exactly one done, sent from here.
void runRequest(scheme* Scheme, Request* Req, std::uint64_t MaxDuration);

}  // namespace extemp::nrepl
