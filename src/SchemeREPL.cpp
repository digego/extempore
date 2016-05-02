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

#include "SchemeREPL.h"
#include "EXTMutex.h"
#include "UNIV.h"

#include <stdio.h>         /* Basic I/O routines          */

#ifdef EXT_BOOST
// nothing
#else
#include <sys/types.h>     /* standard system types       */
#include <netinet/in.h>    /* Internet address structures */
#include <netinet/tcp.h>   /* for define of TCP_NODELAY  Nagles Algorithm*/
#include <sys/socket.h>    /* socket interface functions  */
#include <netdb.h>         /* host to IP resolution       */
#endif

#ifndef _WIN32
#include <unistd.h>
#endif
#include <iostream>
#include <sstream>
#include <string.h>
#include <errno.h>

namespace extemp {

std::unordered_map<std::string, SchemeREPL*> SchemeREPL::sm_repls;

SchemeREPL::SchemeREPL(const std::string& Title, SchemeProcess* Process): m_title(Title), m_process(Process),
        m_serverSocket(0), m_connected(false), m_active(true), m_writeLock("repl_lock")
{
    ascii_default();
    printf("\nStarting ");
    ascii_info();
    printf("%s", m_title.c_str());
    ascii_default();
    printf(" process\n");
    ascii_default();
    m_writeLock.init();
    sm_repls[m_title] = this;
}

void SchemeREPL::writeString(std::string&& String)
{
    if (!m_serverSocket) {
        return;
    }
    EXTMutex::ScopedLock lock(m_writeLock);
    String.push_back('\r');
    String.push_back('\n');
    int length = String.length();
    const char* b = String.c_str();
    while (true) {
        int lth = (length > 1024) ? 1024 : length;
#ifdef EXT_BOOST
        int chars_written = m_serverSocket->write_some(boost::asio::buffer(b, lth));
#else
        int chars_written = write(m_serverSocket, b, lth);
#endif
        if (chars_written != lth) {
            printf("There was an error sending this expression to the interpreter. Check for non-ascii characters in your code.\n");
        }
        length -= lth;
        if (length < 1) {
            return;
        }
        b += lth;
    }
}

bool SchemeREPL::connectToProcessAtHostname(const std::string& hostname, int port)
{
    if (m_connected) {
        return false;
    }
    int rc;
#ifdef EXT_BOOST
    // do nothing for boost
#else
    struct sockaddr_in sa;
    struct hostent* hen; /* host-to-IP translation */
#endif
    printf("Trying to connect to ");
    printf("'%s'",hostname.c_str());
    printf(" on port ");
    printf("%d\n",port);
    /* Address resolution stage */

#ifdef EXT_BOOST
    boost::asio::ip::tcp::resolver::iterator end;
    boost::asio::io_service service;
    boost::asio::ip::tcp::resolver resolver(service);
    std::stringstream ss;
    ss << port;
    boost::asio::ip::tcp::resolver::query newQuery(boost::asio::ip::tcp::v4(),hostname, ss.str());
    boost::asio::ip::tcp::resolver::iterator iter = resolver.resolve(newQuery);

    boost::asio::ip::tcp::endpoint ep = *iter;
    //std::cout << "resolved: " << ep << std::endl << std::flush;
    if(iter == end) {
#else
    hen = gethostbyname(hostname.c_str());
    if (!hen) {
#endif
        ascii_error();
        printf("Could not resolve host name\n");
        ascii_default();
        return false;
    }
    // wait for main server to start up first time out of the gates.
#ifdef _WIN32
    Sleep(1000);
#else
    sleep(1);
#endif

#ifdef EXT_BOOST
    m_serverIoService = new boost::asio::io_service;
    try {
        m_serverSocket = new boost::asio::ip::tcp::socket(*m_serverIoService);
        m_serverSocket->open(boost::asio::ip::tcp::v4());
        m_serverSocket->connect(ep);
    } catch(std::exception& e){
        ascii_error();
        std::cout << "Connection Error:" << e.what() << std::endl;
        ascii_default();
        return false;
    }
    rc = m_serverSocket->read_some(boost::asio::buffer(m_buf, sizeof(m_buf)));
#else
    memset(&sa, 0, sizeof(sa));
    sa.sin_family = AF_INET;
    sa.sin_port = htons(port);
    memcpy(&sa.sin_addr.s_addr, hen->h_addr_list[0], hen->h_length);
    m_serverSocket = socket(AF_INET, SOCK_STREAM, 0);

    if (m_serverSocket < 0) {
        ascii_error();
        printf("Socket Connection Failed\n");
        ascii_default();
        return false;
    }
    int flag = 1;
    int result = setsockopt(m_serverSocket,            /* socket affected */
                            IPPROTO_TCP,     /* set option at TCP level */
                            TCP_NODELAY,     /* name of option */
                            (char *) &flag,  /* the cast is historical cruft */
                            sizeof(int));    /* length of option value */
    if (result < 0) {
        printf("error turning off TCP Nagle ALGO\n");
    }
    rc = connect(m_serverSocket, (struct sockaddr *)&sa, sizeof(sa));
    if (rc) {
        ascii_error();
        printf("Connection error:%d\n",errno);
        ascii_default();
        return false;
    }

    //should now be connected
    rc = read(m_serverSocket, m_buf, sizeof(m_buf));
#endif
    if (!rc) {
        this->closeREPL();
        printf("Could not connect to port %d. Make sure don't have any other instances of impromptu running and that no other app is using this port!",port);
        return false;
    }
    ascii_text_color(1,2,10);
    printf("Successfully");
    ascii_default();
    printf(" connected to remote process\n");
    fflush(NULL);
    m_connected = true;
    return true;
}

void SchemeREPL::closeREPL()
{
    m_active = false;
#ifdef EXT_BOOST
    m_serverSocket->close();
    delete m_serverSocket;
    delete m_serverIoService;
    m_serverIoService = 0;
#else
    shutdown(m_serverSocket, SHUT_RDWR);
    close(m_serverSocket);
#endif
    m_serverSocket = 0;
    m_connected = false;
}

}
