#include "NreplClient.h"

#include "FormFinder.h"
#include "NreplServer.h"
#include "UNIV.h"

#include <atomic>
#include <charconv>
#include <chrono>
#include <cstdio>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <iterator>
#include <sstream>
#include <thread>

#ifndef _WIN32
#include "linenoise/linenoise.h"
#endif

namespace extemp::nrepl::client {

using bencode::Dict;
using bencode::Value;

Connection::~Connection() {
    if (m_socket != INVALID_SOCKET) {
        closesocket(m_socket);
    }
}

std::string Connection::open(const std::string& Host, std::uint16_t Port) {
    net::ensureInitialised();
    const uint32_t address = net_util::resolve_ipv4(Host.c_str());
    if (address == 0 && Host != "0.0.0.0") {
        return "unknown host";
    }
    m_socket = socket(AF_INET, SOCK_STREAM, 0);
    if (m_socket == INVALID_SOCKET) {
        return net::errorText(net::lastError());
    }
    sockaddr_in sa{};
    sa.sin_family = AF_INET;
    sa.sin_port = htons(Port);
    sa.sin_addr.s_addr = address;
    if (connect(m_socket, reinterpret_cast<sockaddr*>(&sa), sizeof(sa)) != 0) {
        std::string why = net::errorText(net::lastError());
        closesocket(m_socket);
        m_socket = INVALID_SOCKET;
        return why;
    }
    int flag = 1;
    setsockopt(m_socket, IPPROTO_TCP, TCP_NODELAY, reinterpret_cast<char*>(&flag), sizeof(flag));
    return {};
}

std::string Connection::send(Dict Msg) {
    std::string id = "c" + std::to_string(++m_ids);
    Msg["id"] = Value(id);
    const std::string bytes = bencode::encode(Value(std::move(Msg)));
    std::size_t sent = 0;
    while (sent < bytes.size()) {
        const long n = net::send(m_socket, bytes.data() + sent, bytes.size() - sent);
        if (n <= 0) {
            if (n < 0 && net::interrupted(net::lastError())) {
                continue;
            }
            return {};
        }
        sent += std::size_t(n);
    }
    return id;
}

Connection::Read Connection::read(Dict& Out, int TimeoutMs) {
    for (;;) {
        auto decoded = bencode::decode(m_buffer);
        if (decoded.status == bencode::Decoded::Status::Complete) {
            m_buffer.erase(0, decoded.consumed);
            if (!decoded.value.isDict()) {
                continue;
            }
            Out = std::move(decoded.value.dict);
            return Read::Message;
        }
        if (decoded.status == bencode::Decoded::Status::Error) {
            return Read::Closed;
        }
        if (TimeoutMs >= 0) {
            std::vector<pollfd> fds{{m_socket, POLLIN, 0}};
            const int ready = net::poll(fds, TimeoutMs);
            if (ready == 0 || (ready < 0 && net::interrupted(net::lastError()))) {
                return Read::Timeout;
            }
        }
        char chunk[65536];
        const long n = net::recv(m_socket, chunk, sizeof(chunk));
        if (n < 0 && net::interrupted(net::lastError())) {
            if (TimeoutMs >= 0) {
                return Read::Timeout;
            }
            continue;
        }
        if (n <= 0) {
            return Read::Closed;
        }
        m_buffer.append(chunk, std::size_t(n));
    }
}

bool Connection::request(Dict Msg, Dict& Reply) {
    const std::string id = send(std::move(Msg));
    if (id.empty()) {
        return false;
    }
    while (read(Reply) == Read::Message) {
        if (bencode::findString(Reply, "id") == id) {
            return true;
        }
    }
    return false;
}

void Connection::shutdown() {
    if (m_socket != INVALID_SOCKET) {
        ::shutdown(m_socket, SHUT_RDWR);
    }
}

bool hasStatus(const Dict& Msg, std::string_view Status) {
    const Value* statuses = bencode::find(Msg, "status");
    if (!statuses || !statuses->isList()) {
        return false;
    }
    for (const auto& status : statuses->list) {
        if (status.isString() && status.string == Status) {
            return true;
        }
    }
    return false;
}

namespace {

std::string address(const Target& To, std::uint16_t Port) {
    return To.host + ":" + std::to_string(Port);
}

// Connects, with a message on stderr if it can't.
bool connectTo(Connection& Conn, const Target& To, std::uint16_t Port) {
    const std::string why = Conn.open(To.host, Port);
    if (why.empty()) {
        return true;
    }
    std::cerr << "extempore: no extempore to talk to: can't reach an nREPL server at "
              << address(To, Port) << " (" << why << ")" << std::endl;
    return false;
}

// A session on To's process, or "" for the default (an eval without a session
// runs on primary). False if the server has no such process.
bool openSession(Connection& Conn, const Target& To, bool Always, std::string& Session) {
    Session.clear();
    if (To.process.empty() && !Always) {
        return true;
    }
    Dict msg{{"op", Value("clone")}};
    if (!To.process.empty()) {
        msg["extempore/process"] = Value(To.process);
    }
    Dict reply;
    if (!Conn.request(std::move(msg), reply)) {
        std::cerr << "extempore: the connection closed" << std::endl;
        return false;
    }
    Session = std::string(bencode::findString(reply, "new-session"));
    if (Session.empty()) {
        const auto err = bencode::findString(reply, "err");
        std::cerr << "extempore: "
                  << (err.empty() ? std::string("couldn't open a session\n") : std::string(err));
        return false;
    }
    return true;
}

void closeSession(Connection& Conn, const std::string& Session) {
    if (!Session.empty()) {
        Dict reply;
        Conn.request(Dict{{"op", Value("close")}, {"session", Value(Session)}}, reply);
    }
}

std::atomic<bool> s_waiting{false};    // a REPL is waiting on an evaluation
std::atomic<bool> s_interrupt{false};  // and SIGINT asked for it to stop

// Prints the responses to request Id as they arrive: output and values on
// stdout, errors on stderr. Returns the exit status. With Interruptible, a
// SIGINT meanwhile is passed on as an interrupt.
int printResponses(Connection& Conn, const std::string& Id, const std::string& Session,
                   bool Interruptible) {
    bool failed = false;
    bool sawError = false;
    for (;;) {
        Dict msg;
        const auto got = Conn.read(msg, Interruptible ? 100 : -1);
        if (got == Connection::Read::Timeout) {
            if (s_interrupt.exchange(false)) {
                Conn.send(Dict{{"op", Value("interrupt")},
                               {"session", Value(Session)},
                               {"interrupt-id", Value(Id)}});
            }
            continue;
        }
        if (got == Connection::Read::Closed) {
            std::cerr << "extempore: the connection closed before the evaluation finished"
                      << std::endl;
            return 1;
        }
        if (bencode::findString(msg, "id") != Id) {
            continue;
        }
        if (const auto out = bencode::findString(msg, "out"); !out.empty()) {
            std::fwrite(out.data(), 1, out.size(), stdout);
            std::fflush(stdout);
        }
        if (const Value* value = bencode::find(msg, "value"); value && value->isString()) {
            std::fwrite(value->string.data(), 1, value->string.size(), stdout);
            std::fputc('\n', stdout);
            std::fflush(stdout);
        }
        if (const auto err = bencode::findString(msg, "err"); !err.empty()) {
            sawError = true;
            std::cerr << err;
            if (err.back() != '\n') {
                std::cerr << '\n';
            }
            std::cerr.flush();
        }
        if (hasStatus(msg, "eval-error") || hasStatus(msg, "error")) {
            failed = true;
        }
        if (hasStatus(msg, "interrupted")) {
            failed = true;
            if (!sawError) {
                std::cerr << "interrupted" << std::endl;
            }
        }
        if (hasStatus(msg, "done")) {
            return (failed || sawError) ? 1 : 0;
        }
    }
}

std::optional<std::string> readFile(const std::string& Path) {
    std::ifstream in(Path, std::ios::binary);
    if (!in) {
        return std::nullopt;
    }
    std::ostringstream text;
    text << in.rdbuf();
    return text.str();
}

std::string absolutePath(const std::string& Path) {
    std::error_code ec;
    const auto absolute = std::filesystem::absolute(Path, ec);
    return ec ? Path : absolute.lexically_normal().string();
}

}  // namespace

std::uint16_t findPort(std::optional<std::uint16_t> Given, const std::string& NearFile) {
    if (Given) {
        return *Given;
    }
    const auto parse = [](std::string_view Text) -> std::optional<std::uint16_t> {
        while (!Text.empty() && (Text.back() == '\n' || Text.back() == '\r' || Text.back() == ' ')) {
            Text.remove_suffix(1);
        }
        std::uint16_t port = 0;
        const auto [end, ec] = std::from_chars(Text.data(), Text.data() + Text.size(), port);
        if (ec != std::errc() || end != Text.data() + Text.size() || Text.empty()) {
            return std::nullopt;
        }
        return port;
    };
    if (const char* env = std::getenv("EXTEMPORE_NREPL_PORT"); env && *env) {
        if (const auto port = parse(env)) {
            return *port;
        }
    }
    std::error_code ec;
    std::filesystem::path dir = NearFile.empty()
                                    ? std::filesystem::current_path(ec)
                                    : std::filesystem::absolute(NearFile, ec).parent_path();
    for (int depth = 0; !dir.empty() && depth < 64; ++depth) {
        if (const auto text = readFile((dir / ".nrepl-port").string())) {
            if (const auto port = parse(*text)) {
                return *port;
            }
        }
        const auto parent = dir.parent_path();
        if (parent == dir) {
            break;
        }
        dir = parent;
    }
    return DEFAULT_PORT;
}

int send(const Target& To, const SendOptions& Options) {
    Dict msg{{"op", Value("eval")}};
    if (!Options.file.empty()) {
        const auto text = readFile(Options.file);
        if (!text) {
            std::cerr << "extempore: can't read " << Options.file << std::endl;
            return 2;
        }
        if (Options.line > 0) {
            const auto form = forms::at(*text, forms::offsetOf(*text, Options.line, Options.column));
            if (!form) {
                std::cerr << "extempore: no form in " << Options.file << std::endl;
                return 2;
            }
            msg["code"] = Value(text->substr(form->begin, form->end - form->begin));
            msg["line"] = Value(std::int64_t(form->line));
            msg["file"] = Value(absolutePath(Options.file));
        } else {
            msg["op"] = Value("load-file");
            msg["file"] = Value(*text);
            msg["file-path"] = Value(absolutePath(Options.file));
        }
    } else if (Options.code) {
        msg["code"] = Value(*Options.code);
    } else {
        if (UNIV::is_terminal(stdin)) {
            std::cerr << "extempore: nothing to send: give the code as an argument, on stdin, "
                         "or with --file (see extempore send --help)"
                      << std::endl;
            return 2;
        }
        msg["code"] = Value(std::string(std::istreambuf_iterator<char>(std::cin), {}));
    }
    if (const Value* code = bencode::find(msg, "code");
        code && code->string.find_first_not_of(" \t\r\n") == std::string::npos) {
        std::cerr << "extempore: nothing to send" << std::endl;
        return 2;
    }
    Connection conn;
    if (!connectTo(conn, To, findPort(To.port, Options.file))) {
        return 2;
    }
    std::string session;
    if (!openSession(conn, To, false, session)) {
        return 2;
    }
    if (!session.empty()) {
        msg["session"] = Value(session);
    }
    const std::string id = conn.send(std::move(msg));
    const int result = id.empty() ? 1 : printResponses(conn, id, session, false);
    closeSession(conn, session);
    return result;
}

int status(const Target& To) {
    const std::uint16_t port = findPort(To.port, {});
    Connection conn;
    if (!connectTo(conn, To, port)) {
        return 2;
    }
    Dict reply;
    if (!conn.request(Dict{{"op", Value("describe")}}, reply)) {
        std::cerr << "extempore: the connection closed" << std::endl;
        return 2;
    }
    std::string version = "?";
    if (const Value* versions = bencode::find(reply, "versions"); versions && versions->isDict()) {
        if (const Value* xtm = bencode::find(versions->dict, "extempore"); xtm && xtm->isDict()) {
            version = std::string(bencode::findString(xtm->dict, "version-string"));
        }
    }
    std::cout << "extempore " << version << " at " << address(To, port) << "\nprocesses:";
    if (const Value* aux = bencode::find(reply, "aux"); aux && aux->isDict()) {
        if (const Value* names = bencode::find(aux->dict, "extempore/processes");
            names && names->isList()) {
            for (const auto& name : names->list) {
                std::cout << " " << name.string;
            }
        }
    }
    std::cout << "\nops:\n";
    if (const Value* ops = bencode::find(reply, "ops"); ops && ops->isDict()) {
        for (const auto& [name, info] : ops->dict) {
            std::cout << "  " << name << std::string(name.size() < 12 ? 12 - name.size() : 0, ' ')
                      << " " << (info.isDict() ? bencode::findString(info.dict, "doc") : "")
                      << "\n";
        }
    }
    std::cout.flush();
    return 0;
}

bool interruptFromSignal() {
    if (!s_waiting.load()) {
        return false;
    }
    s_interrupt.store(true);
    return true;
}

#ifdef _WIN32

int repl(const Target&, bool) {
    std::cerr << "extempore: the REPL is not supported on Windows" << std::endl;
    return 2;
}

#else

int repl(const Target& To, bool WaitForServer) {
    const std::uint16_t port = findPort(To.port, {});
    Connection conn;
    std::string why = conn.open(To.host, port);
    for (int tries = 0; !why.empty() && WaitForServer && tries < 60; ++tries) {
        std::this_thread::sleep_for(std::chrono::milliseconds(250));
        why = conn.open(To.host, port);
    }
    if (!why.empty()) {
        std::cerr << "extempore: no extempore to talk to: can't reach an nREPL server at "
                  << address(To, port) << " (" << why << ")" << std::endl;
        return 2;
    }
    std::string session;
    if (!openSession(conn, To, true, session)) {
        return 2;
    }

    const char* home = std::getenv("HOME");
    const std::string history = (home ? std::string(home) + "/" : std::string()) +
                                ".extempore_history";
    linenoiseSetMultiLine(1);
    linenoiseHistoryLoad(history.c_str());

    int result = 0;
    std::string input;
    for (;;) {
        char* line = linenoise(input.empty() ? "xtm> " : "...> ");
        if (!line) {
            // Ctrl-C drops what has been typed; Ctrl-D (or the end of a piped
            // stdin) leaves
            if (errno == EAGAIN && !input.empty()) {
                input.clear();
                continue;
            }
            break;
        }
        const std::string text(line);
        linenoiseFree(line);
        if (text.empty() && input.empty()) {
            continue;
        }
        if (!input.empty()) {
            input.push_back('\n');
        }
        input += text;
        if (!forms::complete(input)) {
            continue;
        }
        linenoiseHistoryAdd(input.c_str());
        linenoiseHistorySave(history.c_str());
        const std::string id = conn.send(
            Dict{{"op", Value("eval")}, {"code", Value(input)}, {"session", Value(session)}});
        input.clear();
        if (id.empty()) {
            std::cerr << "extempore: the connection closed" << std::endl;
            result = 1;
            break;
        }
        s_interrupt.store(false);
        s_waiting.store(true);
        const int status = printResponses(conn, id, session, true);
        s_waiting.store(false);
        if (status != 0 && !UNIV::is_terminal(stdin)) {
            result = 1;  // piped input: the exit status says whether it all worked
        }
    }
    closeSession(conn, session);
    return result;
}

#endif

}  // namespace extemp::nrepl::client
