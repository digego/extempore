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
 * ARE DISCLEXTD. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE 
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

#include <stdio.h>         /* Basic I/O routines          */
#include <sys/types.h>     /* standard system types       */
#include <netinet/in.h>    /* Internet address structures */
#include <netinet/tcp.h>   /* for define of TCP_NODELAY  Nagles Algorithm*/
#include <sys/socket.h>    /* socket interface functions  */
#include <netdb.h>         /* host to IP resolution       */

#include <iostream>
#include <sstream>    
#include <string.h>
#include <errno.h>

namespace extemp {

    std::map<std::string,SchemeREPL*> SchemeREPL::REPL_MAP;
    int SchemeREPL::BUFLENGTH = 1024;

    SchemeREPL::SchemeREPL(std::string &_title)
    {
	active = true;
	title = _title;
	printf("Init REPL for %s\n",title.c_str());
	server_socket = 0;

	buf = new char[BUFLENGTH+1];
	connected = false;
	write_lock = new EXTMutex("repl_lock");
	write_lock->init();
	REPL_MAP[title] = this;
    }

    SchemeREPL::~SchemeREPL()
    {
	delete buf;
	write_lock->destroy();
    }

    SchemeREPL* SchemeREPL::I(std::string &name)
    {
	SchemeREPL* repl = REPL_MAP[name];
	if(repl) return REPL_MAP[name];
	else {
	    printf("Could not find REPL named '%s'\n",name.c_str());
	    return 0;
	}
    }

    std::string& SchemeREPL::getTitle()
    {
	return title;
    }

    void* SchemeREPL::readThread(void* obj)
    {
	SchemeREPL* repl = static_cast<SchemeREPL*>(obj);
	while(repl->active) {
	    //NSMutableString* str = [[NSMutableString alloc] init];
	    std::stringstream ss; 
	    if(repl->server_socket == 0) { 
		ss << "No REPL Connection Established";
		break;
	    }else{
		memset(repl->buf,0,BUFLENGTH+1);
		int lgth = 0;
		while((lgth = read(repl->server_socket, repl->buf, BUFLENGTH)) == BUFLENGTH) {		    
		    if(lgth < 0) {
			printf("PROBLEM WITH REPL SOCKET: %s\n",repl->buf);
			break;
		    }
		    ss << repl->buf;
		    memset(repl->buf,0,BUFLENGTH+1);
		}
		if(lgth == 0) { // end of stream
		    printf("CLOSE REPL READ STREAM %d",lgth);
		    return 0;
		}
	    }
	    //printf("repl-result: %s",ss.str().c_str());
	    if(repl->server_socket == 0) break;
	}
	printf("Exit REPL Thread\n");
    }

    void SchemeREPL::writeString(std::string& string)
    {
	if(server_socket == 0) return;
	write_lock->lock();
    
	char c1 = 13;
	char c2 = 10;
	string.push_back(c1);
	string.push_back(c2);

	int length = string.length();
	const char* b = string.c_str(); 
    
	for(;;) {
	    int lth = (length > 1024) ? 1024 : length;
	    int chars_written = write(server_socket, b, lth);
	    if(chars_written != lth) {
		printf("There was an error sending this expression to the interpreter. Check for non-ascii characters in your code.\n");
	    }
	    length -= lth;
	    if(length < 1) { write_lock->unlock(); return; }
	    b+=lth;
	}
	write_lock->unlock();
	return;
    }

    bool SchemeREPL::connectToProcessAtHostname(std::string& hostname, int port) 
    {
	if(connected) return false;
	int rc;
	struct sockaddr_in sa;
	struct hostent* hen; /* host-to-IP translation */
	printf("Trying to connect to hostname: '%s' on port %d\n",hostname.c_str(),port); 
	/* Address resolution stage */
	hen = gethostbyname(hostname.c_str());
	if (!hen) {
	    printf("Could not resolve host name\n");
	    return false;
	}
    
	memset(&sa, 0, sizeof(sa));

	sa.sin_family = AF_INET;
	sa.sin_port = htons(port);
	memcpy(&sa.sin_addr.s_addr, hen->h_addr_list[0], hen->h_length);
	server_socket = socket(AF_INET, SOCK_STREAM, 0);
    
	if (server_socket < 0) {
	    printf("Socket Connection Failed\n");
	    return false;
	}else{
	    int flag = 1;
	    int result = setsockopt(server_socket,            /* socket affected */
				    IPPROTO_TCP,     /* set option at TCP level */
				    TCP_NODELAY,     /* name of option */
				    (char *) &flag,  /* the cast is historical cruft */
				    sizeof(int));    /* length of option value */
	    if (result < 0) {
		printf("error turning off TCP Nagle ALGO\n");
	    }
	    printf("Create socket successful\n");
	}
	rc = connect(server_socket, (struct sockaddr *)&sa, sizeof(sa));
	if(rc) {
	    printf("Error connecting to socket:%d\n",errno);
	    return false;
	}else{
	    printf("Connect socket successful\n");
	}
    
	//should now be connected
	rc = read(server_socket,buf,BUFLENGTH);
	if(rc == 0) {
	    this->closeREPL();
	    printf("Could not connect to port %d. Make sure don't have any other instances of impromptu running and that no other app is using this port!",port);
	    return false;
	}
	printf("successfully connected to %d\n",server_socket);
	connected = true;
	read_thread.create(&readThread,this);
	return true;
    }

    void SchemeREPL::closeREPL()
    {
	active = false;
	shutdown(server_socket, SHUT_RDWR);
	close(server_socket);
	server_socket = 0;
	connected = false;
	// removeREPL will release us
    }

} //close namespace
