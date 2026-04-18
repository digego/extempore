#ifndef _WIN32

#include "LinenoiseREPL.h"
#include "linenoise/linenoise.h"

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netdb.h>
#include <unistd.h>
#include <cstring>
#include <cstdio>
#include <cstdlib>
#include <string>
#include <chrono>
#include <thread>

static int connect_to_server(const std::string& host, int port)
{
    struct hostent* hen = gethostbyname(host.c_str());
    if (!hen) {
        fprintf(stderr, "repl: could not resolve host '%s'\n", host.c_str());
        return -1;
    }

    struct sockaddr_in sa;
    memset(&sa, 0, sizeof(sa));
    sa.sin_family = AF_INET;
    sa.sin_port = htons(port);
    memcpy(&sa.sin_addr.s_addr, hen->h_addr_list[0], hen->h_length);

    int retries = 30;
    for (int i = 0; i < retries; i++) {
        int sock = socket(AF_INET, SOCK_STREAM, 0);
        if (sock < 0) {
            fprintf(stderr, "repl: socket creation failed\n");
            return -1;
        }
        int flag = 1;
        setsockopt(sock, IPPROTO_TCP, TCP_NODELAY, &flag, sizeof(flag));

        if (connect(sock, (struct sockaddr*)&sa, sizeof(sa)) == 0) {
            return sock;
        }
        close(sock);
        std::this_thread::sleep_for(std::chrono::milliseconds(500));
    }
    fprintf(stderr, "repl: could not connect to %s:%d after 15s\n", host.c_str(), port);
    return -1;
}

static bool read_response(int sock, std::string& out)
{
    out.clear();
    char buf[4096];
    while (true) {
        ssize_t n = recv(sock, buf, sizeof(buf), 0);
        if (n <= 0) {
            return false;
        }
        for (ssize_t i = 0; i < n; i++) {
            if (buf[i] == '\0') {
                return true;
            }
            out.push_back(buf[i]);
        }
    }
}

static bool send_expression(int sock, const std::string& expr)
{
    std::string msg = expr + "\r\n";
    const char* p = msg.c_str();
    size_t remaining = msg.size();
    while (remaining > 0) {
        ssize_t n = write(sock, p, remaining);
        if (n <= 0) {
            return false;
        }
        p += n;
        remaining -= n;
    }
    return true;
}

static int paren_depth(const std::string& input)
{
    int depth = 0;
    bool in_string = false;
    bool in_line_comment = false;
    int block_comment_depth = 0;

    for (size_t i = 0; i < input.size(); i++) {
        char c = input[i];

        if (in_line_comment) {
            if (c == '\n') {
                in_line_comment = false;
            }
            continue;
        }

        if (block_comment_depth > 0) {
            if (c == '#' && i + 1 < input.size() && input[i + 1] == '|') {
                block_comment_depth++;
                i++;
            } else if (c == '|' && i + 1 < input.size() && input[i + 1] == '#') {
                block_comment_depth--;
                i++;
            }
            continue;
        }

        if (in_string) {
            if (c == '\\' && i + 1 < input.size()) {
                i++;
            } else if (c == '"') {
                in_string = false;
            }
            continue;
        }

        if (c == '"') {
            in_string = true;
        } else if (c == ';') {
            in_line_comment = true;
        } else if (c == '#' && i + 1 < input.size() && input[i + 1] == '|') {
            block_comment_depth++;
            i++;
        } else if (c == '(') {
            depth++;
        } else if (c == ')') {
            depth--;
        }
    }
    return depth;
}

static const char* history_path()
{
    static std::string path;
    if (path.empty()) {
        const char* home = getenv("HOME");
        if (home) {
            path = std::string(home) + "/.extempore_history";
        } else {
            path = ".extempore_history";
        }
    }
    return path.c_str();
}

void* linenoise_repl(void* dat)
{
    auto* args = static_cast<linenoise_repl_args*>(dat);
    std::string host = args->host;
    int port = args->port;
    delete args;

    int sock = connect_to_server(host, port);
    if (sock < 0) {
        return nullptr;
    }

    std::string welcome;
    if (!read_response(sock, welcome)) {
        fprintf(stderr, "repl: failed to read welcome banner\n");
        close(sock);
        return nullptr;
    }

    linenoiseSetMultiLine(1);
    linenoiseHistoryLoad(history_path());

    std::string accumulator;
    const char* prompt_primary = "xtm> ";
    const char* prompt_continuation = "...> ";

    while (true) {
        const char* prompt = accumulator.empty() ? prompt_primary : prompt_continuation;
        char* line = linenoise(prompt);

        if (!line) {
            if (!accumulator.empty()) {
                accumulator.clear();
                printf("\n");
                continue;
            }
            break;
        }

        std::string input(line);
        linenoiseFree(line);

        if (input.empty() && accumulator.empty()) {
            continue;
        }

        if (!accumulator.empty()) {
            accumulator.push_back('\n');
        }
        accumulator += input;

        if (paren_depth(accumulator) > 0) {
            continue;
        }

        linenoiseHistoryAdd(accumulator.c_str());
        linenoiseHistorySave(history_path());

        if (!send_expression(sock, accumulator)) {
            fprintf(stderr, "repl: send failed, connection lost\n");
            break;
        }

        std::string response;
        if (!read_response(sock, response)) {
            fprintf(stderr, "repl: connection closed by server\n");
            break;
        }

        if (!response.empty()) {
            printf("%s\n", response.c_str());
        }

        accumulator.clear();
    }

    printf("\nrepl: goodbye\n");
    close(sock);
    return nullptr;
}

#endif
