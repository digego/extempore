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
#include "EXTThread.h"
#include <string>
#include <map>

#ifdef EXT_BOOST
#include <boost/asio.hpp>
#endif

namespace extemp {
    class EXTMutex;
   
    class SchemeREPL {
    public:
	SchemeREPL(std::string&);
	~SchemeREPL();
	static SchemeREPL* I(std::string&);	
	std::string& getTitle();
	// static void* readThread(void*);
	void writeString(std::string&);
	bool connectToProcessAtHostname(std::string&, int);
	void closeREPL();

    private:
	static std::map<std::string,SchemeREPL*> REPL_MAP;
	static int BUFLENGTH;
#ifdef EXT_BOOST
	boost::asio::ip::tcp::socket* server_socket;
	boost::asio::io_service* server_io_service;
#else
	int server_socket;
#endif
	char* buf;
	bool connected;
	bool active;
	std::string title;
	EXTMutex* write_lock;
	// EXTThread read_thread;
    };
}
