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

#include "TaskScheduler.h"
#include "SchemeProcess.h"
#include "SchemeFFI.h"
#include "OSC.h"

#include <iosfwd>
#include <iomanip>
#include <stdexcept>
#include <errno.h>

#include <sys/types.h>
#include <sys/stat.h>

#ifdef _WIN32
#include <ws2tcpip.h>

static void usleep(LONGLONG Us)
{
    auto timer(CreateWaitableTimer(NULL, TRUE, NULL));
    if (!timer) {
        return;
    }
    LARGE_INTEGER li;
    li.QuadPart = -Us * 10;
    if (!SetWaitableTimer(timer, &li, 0, NULL, NULL, FALSE)) {
        CloseHandle(timer);
        return;
    }
    WaitForSingleObject(timer, INFINITE);
    CloseHandle(timer);
}
#else
#include <sys/socket.h>
#include <sys/select.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netdb.h>         /* host to IP resolution       */
#include <unistd.h>
static int closesocket(int Socket) {
    return close(Socket);
}
#endif
#include <stdlib.h>
#include "UNIV.h"

#define EXT_INITEXPR_BUFLEN 1024
static const char TERMINATION_CHAR = 23;

// FD_COPY IS BSD ONLY
#ifndef FD_COPY
#define FD_COPY(f, t) static_cast<void>(*(t) = *(f))
#endif

namespace extemp {
namespace EXTLLVM {

llvm_zone_t* llvm_zone_create(uint64_t);

}
}

#include "EXTLLVM.h"

namespace extemp {

THREAD_LOCAL SchemeProcess* SchemeProcess::sm_current = 0;
const char* SchemeProcess::sm_banner = "\n"
        "##########################################\n"
        "##                                      ##\n"
        "##               EXTEMPORE              ##\n"
        "##                                      ##\n"
        "##           andrew@moso.com.au         ##\n"
        "##                                      ##\n"
        "##            (c) 2005-2015             ##\n"
        "##                                      ##\n"
        "##########################################\n"
        "     ################################\n"
        "          ######################\n"
        "               ############\n"
        "                    ##\n\n";

SchemeProcess::SchemeProcess(const std::string& LoadPath, const std::string& Name, int ServerPort, bool Banner,
        const std::string& InitExpr): m_loadPath(LoadPath), m_name(Name), m_serverPort(ServerPort),
        m_banner(Banner), m_initExpr(InitExpr), m_libsLoaded(false), m_guard("scheme_server_guard"),
        m_running(true), m_threadTask(&taskTrampoline, this, "SP_task"),
        m_threadServer(&serverTrampoline, this, "SP_server")
{
    if (m_loadPath[m_loadPath.length() - 1] != '/') {
        m_loadPath.push_back('/');
    }
    m_scheme = scheme_init_new();
    m_scheme->m_process = this;
    m_defaultZone = extemp::EXTLLVM::llvm_zone_create(50 * 1024 * 1024); // allocate default zone of 50M
    strcpy(m_scheme->name, m_name.c_str());
    m_maxDuration = m_scheme->call_default_time;
    memset(m_schemeOutportString, 0, SCHEME_OUTPORT_STRING_LENGTH);
    scheme_set_output_port_string(m_scheme, m_schemeOutportString, m_schemeOutportString +
            SCHEME_OUTPORT_STRING_LENGTH - 1);
    FILE* initscm = fopen((m_loadPath + "runtime/init.xtm").c_str(), "r");
    if (!initscm) {
        std::cout << "ERROR: Could not locate file: init.xtm" << std::endl << "Exiting system!!" << std::endl;
        exit(1);
    }
    scheme_load_file(m_scheme, initscm);
    m_serverSocket = socket(AF_INET, SOCK_STREAM, 0);
    if (m_serverSocket < 0) {
        std::cout << "Error opening extempore socket" << std::endl;
        return;
    }
    int flag = 1;
    setsockopt(m_serverSocket, IPPROTO_TCP, TCP_NODELAY, reinterpret_cast<char*>(&flag), sizeof(flag));
    setsockopt(m_serverSocket, SOL_SOCKET, SO_REUSEADDR, reinterpret_cast<char*>(&flag), sizeof(flag));
    scheme_define(m_scheme, m_scheme->global_env, mk_symbol(m_scheme, "*imp-envs*"), m_scheme->NIL);
    scheme_define(m_scheme, m_scheme->global_env, mk_symbol(m_scheme, "*callback*"),
            mk_cptr(m_scheme, mk_cb(this, SchemeProcess, schemeCallback)));
    m_extemporeCallback = mk_cb(this, SchemeProcess, extemporeCallback);
    SchemeFFI::initSchemeFFI(m_scheme);
}

bool SchemeProcess::start()
{
    //set socket options
    int t_reuse = 1;
    setsockopt(m_serverSocket, SOL_SOCKET, SO_REUSEADDR, reinterpret_cast<char*>(&t_reuse), sizeof(t_reuse));
    struct sockaddr_in address;
    memset(&address, 0, sizeof(address));
    address.sin_family = AF_INET;
    address.sin_port = htons(m_serverPort);
    address.sin_addr.s_addr = htonl(INADDR_ANY); //set server's IP
    if (bind(m_serverSocket, reinterpret_cast<sockaddr*>(&address), sizeof(address)) < 0) {
        std::cout << "Error binding extempore address to socket" << std::endl;
        //[NativeScheme::LOGVIEW error:[[NSString alloc] initWithString:@"Error binding to socket 7010. Is Impromptu already running? Close any open Impromptu instances and restart"]];
        m_running = false;
        return false;
    }
    if (listen(m_serverSocket, 5) < 0) {
        std::cout << "Problem listening to extempore socket" << std::endl;
        m_running = false;
        return false;
    }
    m_threadTask.start();
    m_threadServer.start();
    m_guard.init();
    return true;
}

void SchemeProcess::stop()
{
    std::cout << "Stop Scheme Interface" << std::endl;
    m_running = false;
    scheme_deinit(m_scheme);
    // TODO: what about sm_current?/name lookup
}

void SchemeProcess::addCallback(TaskI* TaskAdd, SchemeTask::Type Type)
{
    EXTMonitor::ScopedLock lock(m_guard, true);
    auto currentTime(TaskAdd->getStartTime());
    auto duration(TaskAdd->getDuration());
    auto task(static_cast<Task<SchemeObj*>*>(TaskAdd));
    m_taskQueue.push(SchemeTask(currentTime, duration, task->getArg(), "tmp_label", Type));
}

void SchemeProcess::createSchemeTask(void* Arg, const std::string& Label, SchemeTask::Type Type)
{
    EXTMonitor::ScopedLock lock(m_guard, true);
    m_taskQueue.push(SchemeTask(extemp::UNIV::TIME, m_maxDuration, Arg, Label, Type));
}

bool SchemeProcess::loadFile(const std::string& File, const std::string& Path)
{
    auto fullPath((Path.empty() ? std::string() : (Path + "/")) + File);
    auto impscm(fopen(fullPath.c_str(), "r"));
    if (!impscm) {
        std::cout << "ERROR: Unable to locate file: " << fullPath << std::endl;
        return false;
    }
    scheme_load_file(m_scheme, impscm);
    return true;
}

void* SchemeProcess::taskImpl()
{
    sm_current = this;
    OSC::schemeInit(this);
#ifdef _WIN32
    Sleep(1000);
#else
    sleep(1); // give time for NSApp etc. to init
#endif
    while(!m_running) {
    }
    loadFile("runtime/scheme.xtm", UNIV::SHARE_DIR);
    loadFile("runtime/llvmti.xtm", UNIV::SHARE_DIR);
    loadFile("runtime/llvmir.xtm", UNIV::SHARE_DIR);
    m_libsLoaded = true;
#ifdef _WIN32
    Sleep(1000);
#else
    sleep(1); // give time for NSApp etc. to init
#endif
    // only load extempore.xtm in primary process
    if (m_name == "primary") {
        EXTMonitor::ScopedLock lock(m_guard);
        m_taskQueue.push(SchemeTask(extemp::UNIV::TIME, m_maxDuration,
                new std::string("(sys:compile-init-ll)"), "file_init",
                    SchemeTask::Type::LOCAL_PROCESS_STRING));
        if (extemp::UNIV::EXT_LOADBASE) {
            m_taskQueue.push(SchemeTask(extemp::UNIV::TIME, m_maxDuration,
                    new std::string("(sys:load \"libs/base/base.xtm\" 'quiet)"), "file_init",
                        SchemeTask::Type::LOCAL_PROCESS_STRING));
        }
        if (!m_initExpr.empty()) {
            ascii_text_color(0, 5, 10);
            printf("\nEvaluating expression: ");
            ascii_normal();
            printf("%s\n\n", m_initExpr.c_str());
            m_taskQueue.push(SchemeTask(extemp::UNIV::TIME + 1000, 60 * 60 * UNIV::SECOND(),
                    new std::string(m_initExpr), "file_init", SchemeTask::Type::LOCAL_PROCESS_STRING));
        }
    }
    while (likely(m_running)) {
        if (unlikely(m_taskQueue.empty())) {
            usleep(1000); // 1 ms
            continue;
        }
        while (likely(!m_taskQueue.empty() && m_running)) {
            m_guard.lock();
            SchemeTask task = m_taskQueue.front();
            m_taskQueue.pop();
            m_guard.unlock();
            switch (task.getType()) {
            case SchemeTask::Type::DESTROY_ENV:
                m_scheme->imp_env.erase(reinterpret_cast<pointer>(task.getPtr()));
                break;
            case SchemeTask::Type::LOCAL_PROCESS_STRING: //string from local process (MIDI, OSC or similar)
                {
                    auto evalString(reinterpret_cast<std::string*>(task.getPtr()));
                    if (evalString->length() > 2) {
                        uint64_t now(UNIV::TIME);
                        scheme_load_string(m_scheme, evalString->c_str(), now, now + task.getMaxDuration());
                        if (unlikely(m_scheme->retcode)) { //scheme error
                            resetOutportString();
                        }
                    }
                    delete evalString;
                }
                break;
            case SchemeTask::Type::REPL:
                {
                    auto returnSocket(atoi(task.getLabel().c_str()));
                    auto evalString(reinterpret_cast<std::string*>(task.getPtr()));
                    if (evalString->length() > 1) {
                        std::stringstream ss;
                        bool write_reply(!!evalString->compare(0, 4, "(ipc"));
                        if ((*evalString)[evalString->length() - 1] == TERMINATION_CHAR) {
                            evalString->erase(--evalString->end());
                        }
                        uint64_t now(UNIV::TIME);
                        scheme_load_string(m_scheme, (const char*) evalString->c_str(), now,
                                now + task.getMaxDuration());
                        if (unlikely(m_scheme->retcode)) { //scheme error
                            resetOutportString();
                        } else {
                            if (m_banner) {
                                auto time(UNIV::TIME);
                                auto hours(time / UNIV::HOUR());
                                time -= hours * UNIV::HOUR();
                                auto minutes(time / UNIV::MINUTE());
                                time -= minutes * UNIV::MINUTE();
                                auto seconds(time / UNIV::SECOND());
                                char prompt[24];
                                sprintf(prompt, "\n[extempore %.2u:%.2u:%.2u]: ", unsigned(hours), unsigned(minutes), unsigned(seconds));
                                ss << prompt;
                            }
                            UNIV::printSchemeCell(m_scheme, ss, m_scheme->value);
                            if (write_reply) {
                                auto res(ss.str());
                                send(returnSocket, res.c_str(), int(res.length() + 1), 0);
                            }
                        }
                    }
                    delete evalString;
                }
                break;
            case SchemeTask::Type::SCHEME_CALLBACK:
                {
                    auto obj(reinterpret_cast<SchemeObj*>(task.getPtr()));
                    auto pair(reinterpret_cast<pointer>(obj->getValue()));
                    auto func(pair_car(pair));
                    auto args(pair_cdr(pair));
                    if (is_closure(func) || is_macro(func) || is_continuation(func) || is_proc(func) ||
                            is_foreign(func)) {
                        uint64_t now(UNIV::TIME);
                        scheme_call(m_scheme, func, args, now, now + task.getMaxDuration());
                        if (unlikely(m_scheme->retcode)) { //scheme error
                            resetOutportString();
                        }
                    } else {
                        std::stringstream ss;
                        UNIV::printSchemeCell(m_scheme, ss, pair);
                        std::cerr << "Bad Closure ... " << ss.str() << " Ignoring callback request " << std::endl;
                    }
                    delete obj;
                }
                break;
            case SchemeTask::Type::CALLBACK_SYMBOL: //callback with symbol as char*
                {
                    auto obj(reinterpret_cast<SchemeObj*>(task.getPtr()));
                    auto symbol(reinterpret_cast<char*>(obj->getValue()));
                    auto symbolsymbol(mk_symbol(m_scheme, symbol));
                    auto func(pair_cdr(find_slot_in_env(m_scheme, m_scheme->global_env, symbolsymbol, 1)));
                    pointer args = m_scheme->NIL;
                    if (is_closure(func) || is_continuation(func) || is_proc(func) || is_foreign(func)) {
                        uint64_t now(UNIV::TIME);
                        scheme_call(m_scheme, func, args, now, now + task.getMaxDuration());
                        if (m_scheme->retcode) { //scheme error
                            resetOutportString();
                        }
                    } else {
                        std::stringstream ss;
                        extemp::UNIV::printSchemeCell(m_scheme, ss, func);
                        std::cerr << "Bad Closure From Symbol ... " << ss.str() <<
                                " Ignoring callback request " << std::endl;
                    }
                    delete obj;
                }
                break;
            case SchemeTask::Type::EXTEMPORE_CALLBACK:
                {
                    auto s(reinterpret_cast<_llvm_callback_struct_*>(task.getPtr()));
                    s->fptr(s->dat);
                }
                break;
            default:
                std::cerr << "ERROR: BAD SchemeTask type!!" << std::endl;
            }
        }
    }
    std::cout << "Exiting task thread" << std::endl;
    return this;
}

void* SchemeProcess::serverImpl()
{
    while (!m_libsLoaded) {
        usleep(1000);
    }
    fd_set readFds;
    std::vector<SOCKET> clientSockets;
    std::map<SOCKET, std::string> inStrings;
    FD_ZERO(&readFds); //zero out open sockets
    FD_SET(m_serverSocket, &readFds); //add server socket to open sockets list
    int numFds = int(m_serverSocket) + 1;
    while (m_running) {
        fd_set curReadFds;
        FD_COPY(&readFds, &curReadFds);
        int res(select(numFds, &curReadFds, NULL, NULL, nullptr));
        if (unlikely(res < 0)) { // assumes only one failure
            auto iter(clientSockets.begin());
            for (; iter != clientSockets.end(); ++iter) {
                struct stat buf;
                if (fstat(int(*iter), &buf) < 0) {
                    FD_CLR(*iter, &readFds);
                    clientSockets.erase(iter);
                    break;
                }
            }
            ascii_error();
            printf("%s SERVER ERROR: %s\n", m_name.c_str(), strerror(errno));
            ascii_normal();
            continue;
        }
        if (unlikely(FD_ISSET(m_serverSocket, &curReadFds))) { //check if we have any new accepts on our server socket
            sockaddr_in client_address;
            socklen_t clientAddressSize(sizeof(client_address));
            auto res(accept(m_serverSocket, reinterpret_cast<sockaddr*>(&client_address), &clientAddressSize));
            if (unlikely(res < 0)) {
                std::cout << "Bad Accept in Server Socket Handling" << std::endl;
                continue; //continue on error
            }
            numFds = int(res) + 1;
            FD_SET(res, &readFds); //add new socket to the FD_SET
            ascii_warning();
            printf("New Client Connection\n");
            ascii_normal();
            fflush(stdout);
            clientSockets.push_back(res);
            inStrings[res].clear();
            std::string outString;
            if (m_banner) {
                outString += sm_banner;
                auto time(UNIV::TIME);
                auto hours(time / UNIV::HOUR());
                time -= hours * UNIV::HOUR();
                auto minutes(time / UNIV::MINUTE());
                time -= minutes * UNIV::MINUTE();
                auto seconds(time / UNIV::SECOND());
                char prompt[23];
                sprintf(prompt, "[extempore %.2u:%.2u:%.2u]: ", unsigned(hours), unsigned(minutes), unsigned(seconds));
                outString += prompt;
            } else {
                outString += "Welcome to extempore!";
            }
            send(res, outString.c_str(), int(outString.length() + 1), 0);
            continue;
        }
        for (unsigned index = 0; index < clientSockets.size(); ++index) {
            auto sock(clientSockets[index]);
            const int BUFLEN = 1024;
            char buf[BUFLEN + 1];
            if (FD_ISSET(sock, &curReadFds)) { //see if any client sockets have data for us
                std::string evalStr;
                for (int j = 0; true; j++) { //read from stream in BUFLEN blocks
                    res = recv(sock, buf, BUFLEN, 0);
                    if (unlikely(!res)) { //close the socket
                        FD_CLR(sock, &readFds);
                        inStrings.erase(sock);
                        ascii_warning();
                        std::cout << "Close Client Socket" << std::endl;
                        ascii_normal();
                        clientSockets.erase(clientSockets.begin() + index);
                        closesocket(sock);
                        --index;
                        break;
                    } else if (unlikely(res < 0)) {
                        ascii_error();
                        printf("Error with socket read from extempore process %s", strerror(errno));
                        ascii_normal();
                        break;
                    }
                    auto& string(inStrings[sock]);
                    buf[res] = '\0';
                    string += buf;
                    if (buf[res - 2] == 0x0d && buf[res - 1] == 0x0a) {
                        evalStr.swap(string);
                        break;
                    }
                    if (unlikely(j > 1024 *10)) {
                        ascii_error();
                        printf("Error reading eval string from server socket. No terminator received before 10MB limit.\n");
                        ascii_normal();
                        string.clear();
                        break;
                    }
                }
                if (likely(evalStr != "#break#")) {
                    std::string::size_type pos = 0;
                    std::string::size_type end = evalStr.find_first_of('\x0d', pos);
                    for (; end != std::string::npos; pos = end + 2, end = evalStr.find_first_of('\x0d', pos)) {
                        EXTMonitor::ScopedLock lock(m_guard, true);
                        char c[8];
                        sprintf(c, "%i", int(sock));
                        std::string* s = new std::string(evalStr.substr(pos, end - pos + 1));
                        // std::cout << extemp::UNIV::TIME << "> SCHEME TASK WITH SUBEXPR:" << *s << std::endl;
                        m_taskQueue.push(SchemeTask(extemp::UNIV::TIME, m_maxDuration, s, c, SchemeTask::Type::REPL));
                    }
                }
            }
        }
    }
    for (auto sock : clientSockets) {
        std::cout << "CLOSE CLIENT-SOCKET" << std::endl;
        closesocket(sock);
        std::cout << "DONE-CLOSING_CLIENT" << std::endl;
    }
    if (closesocket(m_serverSocket)) {
        std::cerr << "SchemeProcess Error: Error closing server socket" << std::endl;
        perror(NULL);
    }
    std::cout << "Exiting server thread" << std::endl;
    return this;
}

SchemeObj::SchemeObj(scheme* Scheme, pointer Values, pointer Env): m_scheme(Scheme), m_values(Values), m_env(Env)
{
    if (unlikely(!Env)) {
        std::cout << "BANG CRASH SHEBANG" << std::endl;
        fflush(stdout);
        abort();
    }
    m_scheme->imp_env.insert(Env);
}

SchemeObj::~SchemeObj()
{
    if (likely(m_env)) { // impossible to be null?
        m_scheme->m_process->createSchemeTask(m_env, "destroy SchemeObj", SchemeTask::Type::DESTROY_ENV);
    }
}

} // namespace imp
