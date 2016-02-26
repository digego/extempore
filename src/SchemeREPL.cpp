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

    std::map<std::string,SchemeREPL*> SchemeREPL::REPL_MAP;
    int SchemeREPL::BUFLENGTH = 1024;

    SchemeREPL::SchemeREPL(std::string &_title)
    {
	active = true;
	title = _title;
	ascii_text_color(0,9,10);
	printf("\nStarting ");
	ascii_text_color(1,6,10);
	printf("%s",title.c_str());	
	ascii_text_color(0,9,10);
	printf(" process\n");
	ascii_text_color(0,9,10);
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
      if(repl) return repl;
      else {
        // ascii_text_color(1,1,10);
        // printf("Could not find REPL named '%s'\n",name.c_str());
        // ascii_text_color(0,9,10);
        return 0;
      }
    }

    std::string& SchemeREPL::getTitle()
    {
	return title;
    }

//     void* SchemeREPL::readThread(void* obj)
//     {
// 	SchemeREPL* repl = static_cast<SchemeREPL*>(obj);
// 	while(repl->active) {
// 	    //NSMutableString* str = [[NSMutableString alloc] init];
// 	    std::stringstream ss;
// 	    if(repl->server_socket == 0) {
// 		ss << "No REPL Connection Established";
// 		break;
// 	    }else{
// 		memset(repl->buf,0,BUFLENGTH+1);
// 		int lgth = 0;
// #ifdef EXT_BOOST
//                	while((lgth = repl->server_socket->read_some(boost::asio::buffer(repl->buf, BUFLENGTH))) == BUFLENGTH) {
// #else
// 		while((lgth = read(repl->server_socket, repl->buf, BUFLENGTH)) == BUFLENGTH) {
// #endif

// 		    if(lgth < 0) {
// 		      ascii_text_color(1,1,10);
// 			printf("PROBLEM WITH REPL SOCKET: %s\n",repl->buf);
// 		      ascii_text_color(0,9,10);
// 			break;
// 		    }
// 		    ss << repl->buf;
// 		    memset(repl->buf,0,BUFLENGTH+1);
// 		}
// 		if(lgth == 0) { // end of stream
// 		  ascii_text_color(1,3,10);
// 		    printf("CLOSE REPL READ STREAM %d",lgth);
// 		  ascii_text_color(0,9,10);
// 		    return 0;
// 		}
// 	    }
// 	    //printf("repl-result: %s",ss.str().c_str());
// 	    if(repl->server_socket == 0) break;
// 	}
// 	//printf("Exit REPL Thread\n");
//     }

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
#ifdef EXT_BOOST
	    int chars_written = server_socket->write_some(boost::asio::buffer(b, lth));
#else
	    int chars_written = write(server_socket, b, lth);
#endif
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
#ifdef EXT_BOOST
	// do nothing for boost
#else
	struct sockaddr_in sa;
	struct hostent* hen; /* host-to-IP translation */
#endif
	//ascii_text_color(1,9,10);
	printf("Trying to connect to ");
	//ascii_text_color(1,6,10);
	printf("'%s'",hostname.c_str());
	//ascii_text_color(1,9,10);
	printf(" on port ");
	//ascii_text_color(1,6,10);
	printf("%d\n",port);
	//ascii_text_color(0,9,10);
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
  	ascii_text_color(1,1,10);
	    printf("Could not resolve host name\n");
	ascii_text_color(0,9,10);
	    return false;
	}
        // wait for main server to start up first time out of the gates.
#ifdef _WIN32
        Sleep(1000);
#else
        sleep(1);
#endif

#ifdef EXT_BOOST        
        server_io_service = new boost::asio::io_service;
    try{
        server_socket = new boost::asio::ip::tcp::socket(*server_io_service);
        server_socket->open(boost::asio::ip::tcp::v4());
        server_socket->connect(ep);
	}catch(std::exception& e){
	    ascii_text_color(1,1,10);
		std::cout << "Connection Error:" << e.what() << std::endl;
	    //printf("Connection error:%d\n",errno);
	    ascii_text_color(0,9,10);
	    return false;
	}    
	rc = server_socket->read_some(boost::asio::buffer(buf,BUFLENGTH));
#else    
	memset(&sa, 0, sizeof(sa));

	sa.sin_family = AF_INET;
	sa.sin_port = htons(port);
	memcpy(&sa.sin_addr.s_addr, hen->h_addr_list[0], hen->h_length);
	server_socket = socket(AF_INET, SOCK_STREAM, 0);
    
	if (server_socket < 0) {
	  ascii_text_color(1,1,10);
	    printf("Socket Connection Failed\n");
	ascii_text_color(0,9,10);
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
	    //printf("Create socket successful\n");
	}

	rc = connect(server_socket, (struct sockaddr *)&sa, sizeof(sa));

	if(rc) {
	ascii_text_color(1,1,10);
	    printf("Connection error:%d\n",errno);
	ascii_text_color(0,9,10);
	    return false;
	}else{
	  //printf("Connect socket successful\n");
	}
    
	//should now be connected
	rc = read(server_socket,buf,BUFLENGTH);
#endif
	if(rc == 0) {
	    this->closeREPL();
	    printf("Could not connect to port %d. Make sure don't have any other instances of impromptu running and that no other app is using this port!",port);
	    return false;
	}

	ascii_text_color(1,2,10);
	printf("Successfully"); 	
	ascii_text_color(0,9,10);
	printf(" connected to remote process\n");
        fflush(NULL);
	connected = true;
	// read_thread.create(&readThread,this);
	return true;
    }

    void SchemeREPL::closeREPL()
    {

	active = false;
#ifdef EXT_BOOST
        server_socket->close();
        delete(server_socket);
        delete(server_io_service);
        server_io_service = 0;
#else
	shutdown(server_socket, SHUT_RDWR);
	close(server_socket);
#endif
	server_socket = 0;
	connected = false;
	// removeREPL will release us
    }

} //close namespace
