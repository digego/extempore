#include "NreplServer.h"

#include "Bencode.h"
#include "ExtemporeVersion.h"
#include "SchemeFFIRegistry.h"
#include "SchemeProcess.h"
#include "UNIV.h"
#include "ext/NetCompat.h"

#include <atomic>
#include <chrono>
#include <cstdio>
#include <cstring>
#include <filesystem>
#include <iostream>
#include <memory>
#include <mutex>
#include <random>
#include <string>
#include <string_view>
#include <thread>
#include <unordered_map>
#include <utility>
#include <vector>

#ifdef _WIN32
#include <io.h>
#else
#include <arpa/inet.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <unistd.h>
#endif

namespace extemp::nrepl {

using bencode::Dict;
using bencode::List;
using bencode::Value;

namespace {

constexpr std::size_t MAX_MESSAGE = std::size_t{16} << 20;
// Output a client hasn't read yet. Past this it is disconnected, so a client
// that stops reading can never block a task thread.
constexpr std::size_t MAX_PENDING_OUTPUT = std::size_t{16} << 20;
// An eval's output goes out at each newline, or in chunks this size.
constexpr std::size_t OUT_CHUNK = 8192;
constexpr int POLL_TIMEOUT_MS = 100;

long sendNoSignal(SOCKET Fd, const char* Data, std::size_t Length) {
#if defined(_WIN32)
    return ::send(Fd, Data, int(Length), 0);
#elif defined(MSG_NOSIGNAL)
    return ::send(Fd, Data, Length, MSG_NOSIGNAL);
#else
    return ::send(Fd, Data, Length, 0);  // SO_NOSIGPIPE is set on the socket
#endif
}

void configureClientSocket(SOCKET Fd) {
    int on = 1;
#ifdef _WIN32
    u_long nonBlocking = 1;
    ioctlsocket(Fd, FIONBIO, &nonBlocking);
#else
    fcntl(Fd, F_SETFL, fcntl(Fd, F_GETFL, 0) | O_NONBLOCK);
#ifdef SO_NOSIGPIPE
    setsockopt(Fd, SOL_SOCKET, SO_NOSIGPIPE, &on, sizeof(on));
#endif
#endif
    setsockopt(Fd, IPPROTO_TCP, TCP_NODELAY, reinterpret_cast<char*>(&on), sizeof(on));
}

// A client connection. The server thread reads from it and owns the socket's
// lifetime; any thread may write to it.
class Connection {
  public:
    explicit Connection(SOCKET Fd) : m_fd(Fd) {}

    // Queue Bytes for the client, sending what the socket takes at once. The
    // server thread sends the rest when the socket becomes writable.
    void write(std::string_view Bytes) {
        const std::lock_guard<std::mutex> lock(m_mutex);
        if (m_closed) {
            return;
        }
        if (m_pending.empty()) {
            Bytes.remove_prefix(sendSome(Bytes));
        }
        if (m_closed || Bytes.empty()) {
            return;
        }
        m_pending.append(Bytes);
        if (m_pending.size() > MAX_PENDING_OUTPUT) {
            closeLocked();
        }
    }

    // The rest is for the server thread.
    SOCKET fd() const {
        return m_fd;
    }
    bool wantsWrite() {
        const std::lock_guard<std::mutex> lock(m_mutex);
        return !m_closed && !m_pending.empty();
    }
    bool closed() {
        const std::lock_guard<std::mutex> lock(m_mutex);
        return m_closed;
    }
    void drain() {
        const std::lock_guard<std::mutex> lock(m_mutex);
        if (!m_closed) {
            m_pending.erase(0, sendSome(m_pending));
        }
    }
    void close() {
        const std::lock_guard<std::mutex> lock(m_mutex);
        closeLocked();
    }
    // Close the socket for good. Writers check m_closed under the lock, so
    // none can be using the descriptor.
    void release() {
        const std::lock_guard<std::mutex> lock(m_mutex);
        closeLocked();
        if (m_fd != INVALID_SOCKET) {
            closesocket(m_fd);
            m_fd = INVALID_SOCKET;
        }
    }

    std::string input;  // received bytes not yet decoded

  private:
    std::mutex m_mutex;
    SOCKET m_fd;
    std::string m_pending;
    bool m_closed = false;

    std::size_t sendSome(std::string_view Bytes) {
        std::size_t sent = 0;
        while (sent < Bytes.size()) {
            const long n = sendNoSignal(m_fd, Bytes.data() + sent, Bytes.size() - sent);
            if (n > 0) {
                sent += std::size_t(n);
                continue;
            }
            const int err = net::lastError();
            if (n < 0 && net::interrupted(err)) {
                continue;
            }
            if (n < 0 && net::wouldBlock(err)) {
                break;
            }
            closeLocked();
            break;
        }
        return sent;
    }

    void closeLocked() {
        if (!m_closed) {
            m_closed = true;
            m_pending.clear();
            ::shutdown(m_fd, SHUT_RDWR);
        }
    }
};

struct Session {
    std::string process;
    std::weak_ptr<Connection> owner;
};

// The request a process is running, for interrupt.
struct Running {
    std::uint64_t serial;
    std::string session;
    std::string id;
    scheme* sc;
};

// A request which is waiting its turn on its process's task queue.
struct Queued {
    std::string session;
    std::string id;
    bool cancelled = false;  // interrupted before it began
};

std::string idString(const Dict& Msg) {
    const Value* id = bencode::find(Msg, "id");
    if (!id) {
        return {};
    }
    return id->isInteger() ? std::to_string(id->integer) : id->string;
}

// Every response carries the request's id and session.
void stamp(const Dict& Request, Dict& Response, std::string_view Session) {
    if (const Value* id = bencode::find(Request, "id")) {
        Response["id"] = *id;
    }
    if (!Session.empty()) {
        Response["session"] = Value(Session);
    }
}

List statuses(std::initializer_list<const char*> Names) {
    List list;
    for (const char* name : Names) {
        list.emplace_back(name);
    }
    return list;
}

std::string newSessionId() {
    static std::mutex mutex;
    static std::mt19937_64 rng{std::random_device{}()};
    const std::lock_guard<std::mutex> lock(mutex);
    const std::uint64_t hi = rng();
    const std::uint64_t lo = rng();
    char buf[40];
    std::snprintf(buf, sizeof(buf), "%08x-%04x-4%03x-%04x-%012llx", unsigned(hi >> 32),
                  unsigned((hi >> 16) & 0xffff), unsigned(hi & 0xfff),
                  unsigned(0x8000 | ((lo >> 48) & 0x3fff)),
                  static_cast<unsigned long long>(lo & 0xffffffffffffULL));
    return buf;
}

}  // namespace

// One request on its way to, or running on, a process's task thread.
struct Request {
    std::shared_ptr<Connection> conn;
    Dict msg;
    std::string session;
    std::string process;
    std::uint64_t serial = 0;

    std::string out;  // output not yet sent
    int escape = 0;   // 1 after ESC, 2 inside an ESC [ ... sequence
    bool done = false;

    void reply(Dict Response) {
        if (done) {
            return;
        }
        if (!Response.contains("out")) {
            flushOut();
        }
        if (const Value* status = bencode::find(Response, "status"); status && status->isList()) {
            for (const auto& s : status->list) {
                done = done || (s.isString() && s.string == "done");
            }
        }
        stamp(msg, Response, session);
        conn->write(bencode::encode(Value(std::move(Response))));
    }

    void flushOut() {
        if (!out.empty()) {
            Dict response{{"out", Value(std::move(out))}};
            out.clear();
            reply(std::move(response));
        }
    }

    // A character the eval printed, on its way to the client without the
    // terminal colour codes the console gets.
    void tee(char C) {
        if (escape == 1) {
            escape = (C == '[') ? 2 : 0;
            return;
        }
        if (escape == 2) {
            if (C >= 0x40 && C <= 0x7e) {
                escape = 0;
            }
            return;
        }
        if (C == '\x1b') {
            escape = 1;
            return;
        }
        out += C;
        if (C == '\n' || out.size() >= OUT_CHUNK) {
            flushOut();
        }
    }
};

namespace {

thread_local Request* t_request = nullptr;
std::atomic<std::uint64_t> s_nextSerial{1};
char s_portFile[4096] = {0};

class Server {
  public:
    bool listen(const std::string& Host, std::uint16_t Port) {
        net::ensureInitialised();
        m_listen = socket(AF_INET, SOCK_STREAM, 0);
        if (m_listen == INVALID_SOCKET) {
            m_error = "could not create a socket";
            return false;
        }
        int on = 1;
#ifdef _WIN32
        setsockopt(m_listen, SOL_SOCKET, SO_EXCLUSIVEADDRUSE, reinterpret_cast<char*>(&on),
                   sizeof(on));
#else
        setsockopt(m_listen, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on));
#endif
        sockaddr_in address{};
        address.sin_family = AF_INET;
        address.sin_port = htons(Port);
        const std::string host = (Host == "localhost") ? "127.0.0.1" : Host;
        if (inet_pton(AF_INET, host.c_str(), &address.sin_addr) != 1) {
            m_error = "not an IPv4 address: " + Host;
            return false;
        }
        if (bind(m_listen, reinterpret_cast<sockaddr*>(&address), sizeof(address)) != 0 ||
            ::listen(m_listen, 16) != 0) {
            m_error = net::errorText(net::lastError());
            closesocket(m_listen);
            m_listen = INVALID_SOCKET;
            return false;
        }
        socklen_t length = sizeof(address);
        getsockname(m_listen, reinterpret_cast<sockaddr*>(&address), &length);
        m_host = host;
        m_port = ntohs(address.sin_port);
        return true;
    }

    const std::string& error() const {
        return m_error;
    }
    const std::string& host() const {
        return m_host;
    }
    std::uint16_t port() const {
        return m_port;
    }

    void run() {
        // Hold requests in the listen backlog until the primary process has
        // queued its startup tasks (base library and so on), so an early eval
        // runs after them.
        for (;;) {
            SchemeProcess* primary = SchemeProcess::find("primary");
            if (primary && primary->libsLoaded()) {
                break;
            }
            std::this_thread::sleep_for(std::chrono::milliseconds(10));
        }
        std::vector<std::shared_ptr<Connection>> conns;
        std::vector<pollfd> fds;
        std::vector<char> buf(65536);
        for (;;) {
            fds.clear();
            fds.push_back({m_listen, POLLIN, 0});
            for (const auto& conn : conns) {
                const short events = short(POLLIN | (conn->wantsWrite() ? POLLOUT : 0));
                fds.push_back({conn->fd(), events, 0});
            }
            const int ready = net::poll(fds, POLL_TIMEOUT_MS);
            if (ready < 0) {
                if (!net::interrupted(net::lastError())) {
                    std::this_thread::sleep_for(std::chrono::milliseconds(10));
                }
                continue;
            }
            // fds[i + 1] is conns[i]; a connection accepted below is polled
            // from the next round
            const std::size_t polled = conns.size();
            if (fds[0].revents & POLLIN) {
                const SOCKET fd = accept(m_listen, nullptr, nullptr);
                if (fd != INVALID_SOCKET) {
                    configureClientSocket(fd);
                    conns.push_back(std::make_shared<Connection>(fd));
                }
            }
            for (std::size_t i = 0; i < polled; ++i) {
                const short revents = fds[i + 1].revents;
                auto& conn = conns[i];
                if (revents & POLLOUT) {
                    conn->drain();
                }
                if (revents & POLLNVAL) {
                    conn->close();
                } else if (revents & (POLLIN | POLLHUP | POLLERR)) {
                    receive(conn, buf);
                }
            }
            for (auto& conn : conns) {
                if (conn->closed()) {
                    drop(conn);
                    conn.reset();
                }
            }
            std::erase(conns, nullptr);
        }
    }

    // A request's turn has come. False if it was interrupted while it waited,
    // in which case it isn't to run.
    bool setRunning(const std::string& Process, Running R) {
        const std::lock_guard<std::mutex> lock(m_mutex);
        const auto queued = m_queued.find(R.serial);
        const bool cancelled = queued != m_queued.end() && queued->second.cancelled;
        if (queued != m_queued.end()) {
            m_queued.erase(queued);
        }
        if (cancelled) {
            return false;
        }
        m_running.insert_or_assign(Process, std::move(R));
        return true;
    }

    void clearRunning(const std::string& Process) {
        const std::lock_guard<std::mutex> lock(m_mutex);
        m_running.erase(Process);
    }

  private:
    SOCKET m_listen = INVALID_SOCKET;
    std::string m_host;
    std::uint16_t m_port = 0;
    std::string m_error;

    std::mutex m_mutex;  // guards the three tables below
    std::unordered_map<std::string, Session> m_sessions;
    std::unordered_map<std::string, Running> m_running;  // by process name
    std::unordered_map<std::uint64_t, Queued> m_queued;  // by request serial

    void receive(const std::shared_ptr<Connection>& Conn, std::vector<char>& Buf) {
        const long n = net::recv(Conn->fd(), Buf.data(), Buf.size());
        if (n == 0) {
            Conn->close();
            return;
        }
        if (n < 0) {
            const int err = net::lastError();
            if (!net::wouldBlock(err) && !net::interrupted(err)) {
                Conn->close();
            }
            return;
        }
        Conn->input.append(Buf.data(), std::size_t(n));
        for (;;) {
            bencode::Decoded decoded = bencode::decode(Conn->input, MAX_MESSAGE);
            if (decoded.status == bencode::Decoded::Status::Incomplete) {
                return;
            }
            if (decoded.status == bencode::Decoded::Status::Error) {
                // nothing after a framing error can be trusted
                Dict response{{"status", Value(statuses({"error", "decode-error", "done"}))},
                              {"err", Value("nREPL: " + decoded.error + "\n")}};
                Conn->write(bencode::encode(Value(std::move(response))));
                Conn->close();
                return;
            }
            Conn->input.erase(0, decoded.consumed);
            if (!decoded.value.isDict()) {
                Dict response{{"status", Value(statuses({"error", "unknown-op", "done"}))}};
                Conn->write(bencode::encode(Value(std::move(response))));
                continue;
            }
            handle(Conn, std::move(decoded.value.dict));
        }
    }

    // Forget a closed connection's sessions, then close its socket.
    void drop(const std::shared_ptr<Connection>& Conn) {
        {
            const std::lock_guard<std::mutex> lock(m_mutex);
            std::erase_if(m_sessions, [&](const auto& entry) {
                auto owner = entry.second.owner.lock();
                return !owner || owner == Conn;
            });
        }
        Conn->release();
    }

    static void reply(const std::shared_ptr<Connection>& Conn, const Dict& Request,
                      Dict Response, std::string_view Session) {
        stamp(Request, Response, Session);
        Conn->write(bencode::encode(Value(std::move(Response))));
    }

    static void replyStatus(const std::shared_ptr<Connection>& Conn, const Dict& Request,
                            std::initializer_list<const char*> Status, std::string_view Session) {
        reply(Conn, Request, Dict{{"status", Value(statuses(Status))}}, Session);
    }

    void handle(const std::shared_ptr<Connection>& Conn, Dict Msg) {
        const std::string op(bencode::findString(Msg, "op"));
        const std::string session(bencode::findString(Msg, "session"));
        if (op == "describe") {
            reply(Conn, Msg, describe(), session);
        } else if (op == "clone") {
            clone(Conn, Msg, session);
        } else if (op == "close") {
            const std::lock_guard<std::mutex> lock(m_mutex);
            if (m_sessions.erase(session) != 0) {
                replyStatus(Conn, Msg, {"session-closed", "done"}, session);
            } else {
                replyStatus(Conn, Msg, {"error", "unknown-session", "done"}, session);
            }
        } else if (op == "ls-sessions") {
            List ids;
            {
                const std::lock_guard<std::mutex> lock(m_mutex);
                for (const auto& entry : m_sessions) {
                    ids.emplace_back(entry.first);
                }
            }
            reply(Conn, Msg,
                  Dict{{"sessions", Value(std::move(ids))},
                       {"status", Value(statuses({"done"}))}},
                  session);
        } else if (op == "interrupt") {
            interrupt(Conn, Msg, session);
        } else if (op == "eval" || op == "load-file" || op == "completions" || op == "lookup") {
            dispatch(Conn, std::move(Msg), op, session);
        } else {
            Dict response{{"status", Value(statuses({"error", "unknown-op", "done"}))},
                          {"op", Value(op)}};
            reply(Conn, Msg, std::move(response), session);
        }
    }

    static Dict describe() {
        auto op = [](const char* Doc) { return Value(Dict{{"doc", Value(Doc)}}); };
        Dict ops{
            {"clone", op("Creates a session on a process: extempore/process names it (default "
                         "\"primary\"), or the cloned session's process.")},
            {"close", op("Closes a session.")},
            {"completions", op("Lists what is defined and starts with prefix, each candidate "
                               "with its type: Scheme's globals and the names the xtlang "
                               "compiler knows.")},
            {"describe", op("Describes this server.")},
            {"eval", op("Evaluates code on the session's process. Evals on one process run "
                        "one at a time, in arrival order, whichever session sent them.")},
            {"interrupt", op("Stops the session's running eval when control is next in "
                             "Scheme; compiled xtlang code runs on to completion.")},
            {"load-file", op("Evaluates file (the contents), reporting errors against "
                             "file-path.")},
            {"lookup", op("Describes sym in info: its arglists-str and doc, and its "
                          "extempore/type and extempore/kind. The status no-info means "
                          "nothing is known of it.")},
            {"ls-sessions", op("Lists sessions.")},
        };
        List processes;
        for (auto& name : SchemeProcess::liveNames()) {
            processes.emplace_back(std::move(name));
        }
        Dict versions{
            {"nrepl", Value(Dict{{"major", Value(1)},
                                 {"minor", Value(0)},
                                 {"incremental", Value(0)},
                                 {"version-string", Value("1.0.0")}})},
            {"extempore", Value(Dict{{"version-string", Value(EXTEMPORE_VERSION)}})},
        };
        return Dict{{"ops", Value(std::move(ops))},
                    {"versions", Value(std::move(versions))},
                    {"aux", Value(Dict{{"extempore/processes", Value(std::move(processes))}})},
                    {"status", Value(statuses({"done"}))}};
    }

    void clone(const std::shared_ptr<Connection>& Conn, const Dict& Msg,
               const std::string& Session) {
        std::string process(bencode::findString(Msg, "extempore/process"));
        const std::lock_guard<std::mutex> lock(m_mutex);
        if (process.empty()) {
            auto it = m_sessions.find(Session);
            process = (it != m_sessions.end()) ? it->second.process : "primary";
        }
        if (!SchemeProcess::find(process)) {
            Dict response{{"status", Value(statuses({"error", "unknown-process", "done"}))},
                          {"err", Value("nREPL: no process named " + process + "\n")}};
            reply(Conn, Msg, std::move(response), Session);
            return;
        }
        const std::string id = newSessionId();
        m_sessions.insert_or_assign(id, nrepl::Session{process, Conn});
        reply(Conn, Msg,
              Dict{{"new-session", Value(id)}, {"status", Value(statuses({"done"}))}},
              Session);
    }

    // Stops the session's eval: the one named by interrupt-id, or without an
    // id whatever of the session's is running or waiting. A request still
    // waiting its turn (behind another eval, or the libraries loading at
    // startup) is cancelled, and answers interrupted when its turn comes.
    void interrupt(const std::shared_ptr<Connection>& Conn, const Dict& Msg,
                   const std::string& Session) {
        const std::lock_guard<std::mutex> lock(m_mutex);
        auto session = m_sessions.find(Session);
        if (session == m_sessions.end()) {
            replyStatus(Conn, Msg, {"error", "unknown-session", "done"}, Session);
            return;
        }
        const std::string_view wanted = bencode::findString(Msg, "interrupt-id");
        bool stopped = false;
        for (auto& entry : m_queued) {
            if (entry.second.session == Session && (wanted.empty() || wanted == entry.second.id)) {
                entry.second.cancelled = true;
                stopped = true;
            }
        }
        auto running = m_running.find(session->second.process);
        const bool ours = running != m_running.end() && running->second.session == Session;
        if (ours && (wanted.empty() || wanted == running->second.id)) {
            running->second.sc->interrupt_serial.store(running->second.serial,
                                                       std::memory_order_relaxed);
            stopped = true;
        }
        if (stopped) {
            replyStatus(Conn, Msg, {"done"}, Session);
        } else if (ours) {
            replyStatus(Conn, Msg, {"error", "interrupt-id-mismatch", "done"}, Session);
        } else {
            replyStatus(Conn, Msg, {"session-idle", "done"}, Session);
        }
    }

    void dispatch(const std::shared_ptr<Connection>& Conn, Dict Msg, const std::string& Op,
                  std::string Session) {
        if ((Op == "eval" && !bencode::find(Msg, "code")) ||
            (Op == "load-file" && !bencode::find(Msg, "file"))) {
            replyStatus(Conn, Msg, {"error", "no-code", "done"}, Session);
            return;
        }
        if ((Op == "completions" && !bencode::find(Msg, "prefix")) ||
            (Op == "lookup" && !bencode::find(Msg, "sym"))) {
            replyStatus(Conn, Msg, {"error", "no-symbol", "done"}, Session);
            return;
        }
        std::string process = "primary";
        if (Session.empty()) {
            Session = newSessionId();  // ephemeral, never in the table
        } else {
            const std::lock_guard<std::mutex> lock(m_mutex);
            auto it = m_sessions.find(Session);
            if (it == m_sessions.end()) {
                replyStatus(Conn, Msg, {"error", "unknown-session", "done"}, Session);
                return;
            }
            process = it->second.process;
        }
        SchemeProcess* target = SchemeProcess::find(process);
        if (!target) {
            Dict response{{"status", Value(statuses({"error", "unknown-session", "done"}))},
                          {"err", Value("nREPL: the session's process " + process +
                                        " has exited\n")}};
            reply(Conn, Msg, std::move(response), Session);
            return;
        }
        auto* req = new Request{Conn, std::move(Msg), std::move(Session), process,
                                s_nextSerial.fetch_add(1)};
        {
            const std::lock_guard<std::mutex> lock(m_mutex);
            m_queued.emplace(req->serial, Queued{req->session, idString(req->msg)});
        }
        target->createSchemeTask(req, "nrepl", SchemeTask::Type::NREPL);
    }
};

Server* s_server = nullptr;  // lives as long as the process

// Scheme's view of a request: an association list keyed by strings.
s7_pointer toScheme(s7_scheme* S7, const Value& V) {
    switch (V.kind) {
    case Value::Kind::Integer:
        return s7_make_integer(S7, V.integer);
    case Value::Kind::String:
        return s7_make_string_with_length(S7, V.string.data(), s7_int(V.string.size()));
    case Value::Kind::ListValue: {
        s7_pointer result = s7_nil(S7);
        for (auto it = V.list.rbegin(); it != V.list.rend(); ++it) {
            result = s7_cons(S7, toScheme(S7, *it), result);
        }
        return result;
    }
    case Value::Kind::DictValue: {
        s7_pointer result = s7_nil(S7);
        for (auto it = V.dict.rbegin(); it != V.dict.rend(); ++it) {
            s7_pointer key =
                s7_make_string_with_length(S7, it->first.data(), s7_int(it->first.size()));
            result = s7_cons(S7, s7_cons(S7, key, toScheme(S7, it->second)), result);
        }
        return result;
    }
    }
    return s7_nil(S7);
}

// A response value from Scheme: strings, integers and symbols, lists of them,
// and hash tables (nrepl:dict) for dictionaries.
Value fromScheme(s7_scheme* S7, s7_pointer P) {
    if (s7_is_string(P)) {
        return Value(std::string(s7_string(P), std::size_t(s7_string_length(P))));
    }
    if (s7_is_integer(P)) {
        return Value(std::int64_t(s7_integer(P)));
    }
    if (s7_is_symbol(P)) {
        return Value(s7_symbol_name(P));
    }
    if (s7_is_hash_table(P)) {
        Dict dict;
        s7_pointer iter = s7_make_iterator(S7, P);
        const s7_int loc = s7_gc_protect(S7, iter);
        for (s7_pointer entry = s7_iterate(S7, iter); !s7_iterator_is_at_end(S7, iter);
             entry = s7_iterate(S7, iter)) {
            s7_pointer key = s7_car(entry);
            std::string name = s7_is_string(key)   ? std::string(s7_string(key))
                               : s7_is_symbol(key) ? std::string(s7_symbol_name(key))
                                                   : std::string();
            if (!name.empty()) {
                dict.insert_or_assign(std::move(name), fromScheme(S7, s7_cdr(entry)));
            }
        }
        s7_gc_unprotect_at(S7, loc);
        return Value(std::move(dict));
    }
    if (s7_is_pair(P) || s7_is_null(S7, P)) {
        List list;
        for (s7_pointer p = P; s7_is_pair(p); p = s7_cdr(p)) {
            list.push_back(fromScheme(S7, s7_car(p)));
        }
        return Value(std::move(list));
    }
    char* text = s7_object_to_c_string(S7, P);
    Value value{std::string(text ? text : "")};
    std::free(text);
    return value;
}

// A message logged during the request's eval: a warning or an error goes to
// the client as err, anything else as out.
void forwardLog(void* Context, log::Level L, std::string_view Line) {
    auto* req = static_cast<Request*>(Context);
    if (L == log::Level::Error || L == log::Level::Warning) {
        req->reply(Dict{{"err", Value(std::string(Line) + "\n")}});
        return;
    }
    for (const char c : Line) {
        req->tee(c);
    }
    req->tee('\n');
}

void teeChar(s7_scheme* /*S7*/, uint8_t C, s7_pointer /*Port*/) {
    std::fputc(C, stdout);
    if (t_request) {
        t_request->tee(char(C));
    }
}

s7_pointer teePort(scheme* Scheme) {
    if (!Scheme->nrepl_port) {
        Scheme->nrepl_port = s7_open_output_function(Scheme->sc, teeChar);
        s7_gc_protect(Scheme->sc, Scheme->nrepl_port);
    }
    return Scheme->nrepl_port;
}

// (nrepl:send key value ...) sends one response to the client whose request
// is running: keys are strings, values as fromScheme takes them. A response
// whose status includes done is the request's last (completions go out with
// their done, as proof expects).
pointer nreplSend(scheme* Scheme, pointer Args) {
    if (!t_request) {
        return Scheme->F;
    }
    s7_scheme* s7 = Scheme->sc;
    Dict response;
    for (pointer p = Args; s7_is_pair(p) && s7_is_pair(s7_cdr(p)); p = s7_cddr(p)) {
        s7_pointer key = s7_car(p);
        const char* name = s7_is_string(key)   ? s7_string(key)
                           : s7_is_symbol(key) ? s7_symbol_name(key)
                                               : nullptr;
        if (name) {
            response.insert_or_assign(name, fromScheme(s7, s7_cadr(p)));
        }
    }
    t_request->reply(std::move(response));
    return Scheme->T;
}

}  // namespace

bool start(const std::string& Host, std::uint16_t Port) {
    auto* server = new Server();
    if (!server->listen(Host, Port)) {
        ascii_error();
        std::printf("ERROR:");
        ascii_normal();
        std::cout << " nREPL server: could not listen on " << Host << ":" << Port << " ("
                  << server->error() << ")" << std::endl;
        delete server;
        return false;
    }
    s_server = server;
    if (Port == 0) {
        std::error_code ec;
        const auto path = std::filesystem::absolute(".nrepl-port", ec);
        std::FILE* file = ec ? nullptr : std::fopen(path.string().c_str(), "w");
        if (file) {
            std::fprintf(file, "%u", unsigned(server->port()));
            std::fclose(file);
            std::snprintf(s_portFile, sizeof(s_portFile), "%s", path.string().c_str());
        }
    }
    std::cout << "nREPL server started on port " << server->port() << " on host "
              << server->host() << " - nrepl://" << server->host() << ":" << server->port()
              << std::endl;
    // Runs for the life of the process, like the other server threads.
    std::thread([server] { server->run(); }).detach();
    return true;
}

std::uint16_t boundPort() {
    return s_server ? s_server->port() : std::uint16_t{0};
}

void removePortFile() {
    if (s_portFile[0] != '\0') {
#ifdef _WIN32
        _unlink(s_portFile);
#else
        ::unlink(s_portFile);
#endif
    }
}

void runRequest(scheme* Scheme, Request* Req, std::uint64_t MaxDuration) {
    std::unique_ptr<Request> req(Req);
    s7_scheme* s7 = Scheme->sc;
    if (!s_server->setRunning(req->process,
                              Running{req->serial, req->session, idString(req->msg), Scheme})) {
        // interrupted while it waited its turn
        req->reply(Dict{{"status", Value(statuses({"interrupted"}))}});
        req->reply(Dict{{"status", Value(statuses({"done"}))}});
        return;
    }
    t_request = req.get();
    const log::ScopedForward forward(forwardLog, req.get());
    Scheme->stop_reason = scheme::StopReason::None;
    Scheme->running_serial = req->serial;

    s7_pointer previous = s7_current_output_port(s7);
    s7_set_current_output_port(s7, teePort(Scheme));
    s7_pointer handler = s7_name_to_value(s7, "nrepl:handle");
    bool failed = !s7_is_procedure(handler);
    if (!failed) {
        s7_pointer args = s7_list(s7, 1, toScheme(s7, Value(req->msg)));
        const s7_int loc = s7_gc_protect(s7, args);
        scheme_call(Scheme, handler, args, UNIV::TIME, MaxDuration);
        s7_gc_unprotect_at(s7, loc);
        failed = Scheme->retcode != 0;
    }
    s7_set_current_output_port(s7, previous);
    std::fflush(stdout);

    Scheme->running_serial = 0;
    s_server->clearRunning(req->process);
    switch (Scheme->stop_reason) {
    case scheme::StopReason::Interrupted:
        req->reply(Dict{{"status", Value(statuses({"interrupted"}))}});
        break;
    case scheme::StopReason::TimeLimit:
        req->reply(Dict{{"ex", Value("time-limit")},
                        {"root-ex", Value("time-limit")},
                        {"status", Value(statuses({"eval-error"}))}});
        break;
    case scheme::StopReason::None:
        if (failed) {
            req->reply(Dict{{"err", Value("nREPL: the request failed inside extempore; "
                                          "see its console\n")},
                            {"status", Value(statuses({"error", "server-error"}))}});
        }
        break;
    }
    req->reply(Dict{{"status", Value(statuses({"done"}))}});
    t_request = nullptr;
}

}  // namespace extemp::nrepl

namespace extemp::SchemeFFI {

std::span<const FFIEntry> nreplDefs() {
    static const FFIEntry defs[] = {
        FFI_DEF("nrepl:send", nrepl::nreplSend, 0, 0, true),
    };
    return defs;
}

}  // namespace extemp::SchemeFFI
