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

#ifndef OSC_H
#define OSC_H

#include <stdio.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <iostream>
#include <stdexcept>
#include <map>
#include "SchemeProcess.h"
#include "EXTThread.h"


namespace extemp {
    
    class OSC {
        
    public:
	OSC();
	static OSC* I(scheme* _sc) { 
	    if(SCHEME_MAP.count(_sc)<1) {
		throw std::runtime_error("Error: NO such OSC Server");
	    }				
	    return SCHEME_MAP[_sc];
	    //if(OSC::singleton == NULL) OSC::singleton = new OSC(); return OSC::singleton; 
	}
	static void schemeInit(SchemeProcess* scm);
	//void getMessage();
	static int setOSCTimestamp(char* data, double d);
	static int getOSCTimestamp(const char* data, double* d);		
	static int setOSCString(char* data, std::string* str);
	static int getOSCString(const char* data, std::string* str);
	static int setOSCfloat(char* data, float* f);
	static int getOSCfloat(const char* data, float* f);
	static int setOSCdouble(char* data, double* f);
	static int getOSCdouble(const char* data, double* f);		
	static int setOSCInt(char* data, int* i);
	static int getOSCInt(const char* data, int* i);
	static int setOSCLong(char* data, int64_t* l);
	static int getOSCLong(const char* data, int64_t* l);
	// static int setOSCData(char* data, NSData* data);
	// static int getOSCData(const char* data, NSData** data);
	static void processArgs(pointer arg, char** tmp, char** ptr, int* lgth, std::string& typetags, scheme* _sc);
		
	int clearMessageBuffer();
	static void getOSCStringSection(std::string* input, std::string* output, int section);
	//static pointer sendOSC(scheme* _sc, pointer args);

	static pointer registerScheme(scheme* _sc, pointer args);
	static pointer set_real_type(scheme* _sc, pointer args);		
	static pointer set_integer_type(scheme* _sc, pointer args);
	static pointer send_from_server_socket(scheme* _sc, pointer args);		
	//static void osc_callback(std::string* address, std::string* typetags, char* args, int length, char* reply, int* reply_length, std::string* caller);		
        
	struct sockaddr_in* getAddress() { return &osc_address; }
	struct sockaddr_in* getClientAddress() { return &osc_client_address; }
//        int sizeOfClientAddress() { return sizeof(osc_client_address); }
	int* getClientAddressSize() { return &osc_client_address_size; }        
	char* getMessageData() { return message_data; }
	int getMessageLength() { return message_length; }
	int getSendFD() { return send_socket_fd; }
	void setSendFD(int fd) { send_socket_fd = fd; }		
	//OSC_CALLBACK getCallback() { return callback; }
	int* getSocketFD() { return &socket_fd; }
	EXTThread& getThread() { return threadOSC; }
	bool getStarted() { return started; }
	void setStarted(bool val) { started = val; }
	void sendOSC(TaskI* task);
		
		
	scheme* sc;
	static std::map<scheme*, OSC*> SCHEME_MAP;		
	char scheme_real_type;
	char scheme_integer_type;
	bool send_from_serverfd;
		
    private:
	static OSC* singleton;
	EXTThread threadOSC;
	int socket_fd;
	int send_socket_fd;
	struct sockaddr_in osc_address;
	struct sockaddr_in osc_client_address;
	int osc_client_address_size;
	char message_data[256];
	int message_length;
	bool started;		
	//char scheme_real_type;
	//char scheme_integer_type;
	//OSC_CALLBACK callback;
    };

} //End Namespace

#endif
