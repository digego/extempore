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

#include "UNIV.h"
#include "EXTMutex.h"
#include <string>
#include <unordered_map>

#ifdef EXT_BOOST
#include <boost/asio.hpp>
#endif

namespace extemp {

class SchemeProcess;

class SchemeREPL {
private:
    typedef std::unordered_map<std::string, SchemeREPL*> repls_type;

    static const int BUFLENGTH = 1024;
private:
    std::string                   m_title;
    SchemeProcess*                m_process;
#ifdef EXT_BOOST
    boost::asio::ip::tcp::socket* m_serverSocket;
    boost::asio::io_service*      m_serverIoService;
#else
    int                           m_serverSocket;
#endif
    char                          m_buf[BUFLENGTH];
    bool                          m_connected;
    bool                          m_active;
    EXTMutex                      m_writeLock;

    static repls_type sm_repls;
public:
    SchemeREPL(const std::string& Title, SchemeProcess* Process);

    const std::string& getTitle() { return m_title; }
    void writeString(std::string&&); // ick (modifying)
    bool connectToProcessAtHostname(const std::string&, int);
    void closeREPL();
    SchemeProcess* getProcess() { return m_process; }

    static SchemeREPL* I(const std::string& name)
    {
        auto iter(sm_repls.find(name));
        if (unlikely(iter == sm_repls.end())) {
            return nullptr;
        }
        return iter->second;
    }
};

}
