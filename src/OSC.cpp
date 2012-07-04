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

#include "OSC.h"
#include <string>
#include <iomanip>
#include <sstream>
#include <math.h>

#include <stdlib.h>

#ifdef EXT_BOOST
// header boost/asio.h from OSC.h 
#else
#include <sys/errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>         /* host to IP resolution       */
#include <sys/fcntl.h>
#include <arpa/inet.h>
#endif




///////////////////////////////////////////////
//
// THIS IS UGLY AND INEFFICIENT CHANGE ME!
// 
// swap using char pointers
//
uint64_t swap64f(double d)
{
	uint64_t a;
	unsigned char *dst = (unsigned char *)&a;
	unsigned char *src = (unsigned char *)&d;

	dst[0] = src[7];
	dst[1] = src[6];
	dst[2] = src[5];
	dst[3] = src[4];
	dst[4] = src[3];
	dst[5] = src[2];
	dst[6] = src[1];
	dst[7] = src[0];

	return a;
}

// unswap using char pointers
double unswap64f(uint64_t a) 
{

	double d;
	unsigned char *src = (unsigned char *)&a;
	unsigned char *dst = (unsigned char *)&d;

	dst[0] = src[7];
	dst[1] = src[6];
	dst[2] = src[5];
	dst[3] = src[4];
	dst[4] = src[3];
	dst[5] = src[2];
	dst[6] = src[1];
	dst[7] = src[0];

	return d;
}

// swap using char pointers
uint32_t swap32f(float f)
{
	uint32_t a;
	unsigned char *dst = (unsigned char *)&a;
	unsigned char *src = (unsigned char *)&f;

	dst[0] = src[3];
	dst[1] = src[2];
	dst[2] = src[1];
	dst[3] = src[0];

	return a;
}

// unswap using char pointers
float unswap32f(uint32_t a) 
{

	float f;
	unsigned char *src = (unsigned char *)&a;
	unsigned char *dst = (unsigned char *)&f;

	dst[0] = src[3];
	dst[1] = src[2];
	dst[2] = src[1];
	dst[3] = src[0];

	return f;
}

// swap using char pointers
uint64_t  swap64i(uint64_t d)
{
	uint64_t a;
	unsigned char *dst = (unsigned char *)&a;
	unsigned char *src = (unsigned char *)&d;

	dst[0] = src[7];
	dst[1] = src[6];
	dst[2] = src[5];
	dst[3] = src[4];
	dst[4] = src[3];
	dst[5] = src[2];
	dst[6] = src[1];
	dst[7] = src[0];

	return a;
}

// unswap using char pointers
uint64_t unswap64i(uint64_t a) 
{

	uint64_t d;
	unsigned char *src = (unsigned char *)&a;
	unsigned char *dst = (unsigned char *)&d;

	dst[0] = src[7];
	dst[1] = src[6];
	dst[2] = src[5];
	dst[3] = src[4];
	dst[4] = src[3];
	dst[5] = src[2];
	dst[6] = src[1];
	dst[7] = src[0];

	return d;
}

// swap using char pointers
uint32_t swap32i(uint32_t f)
{
	uint32_t a;
	unsigned char *dst = (unsigned char *)&a;
	unsigned char *src = (unsigned char *)&f;

	dst[0] = src[3];
	dst[1] = src[2];
	dst[2] = src[1];
	dst[3] = src[0];

	return a;
}

// unswap using char pointers
uint32_t unswap32i(uint32_t a) 
{

	uint32_t f;
	unsigned char *src = (unsigned char *)&a;
	unsigned char *dst = (unsigned char *)&f;

	dst[0] = src[3];
	dst[1] = src[2];
	dst[2] = src[1];
	dst[3] = src[0];

	return f;
}

///////////////////////////////////////////////////////////////


//#define _OSC_DEBUG_

namespace extemp {

	std::map<scheme*, OSC*> OSC::SCHEME_MAP;	
	//OSC* OSC::singleton = NULL;
	//scheme* OSC::sc = NULL;

	int get_message_length(std::string& typetags, char* args)
	{
		int pos = 0;
		for(int i=1; i<typetags.size(); ++i) {
			if(typetags[i] == 'i') {
				pos += 4;
			}else if(typetags[i] == 'f'){
				pos += 4; 
			}else if(typetags[i] == 'd'){
				pos += 8;
			}else if(typetags[i] == 's'){
				std::string osc_str;
				pos += OSC::getOSCString(args+pos, &osc_str);
			}else if(typetags[i] == 'h'){
				pos += 8;
			}else if(typetags[i] == 't'){
				pos += 8;
			}else if(typetags[i] == '[') {
				pos += 0;
			}else if(typetags[i] == ']') {
				pos += 0;
			}else{
				return -1;
			}
		}
		return pos;
	}

	int send_scheme_call(scheme* _sc, char* fname, double t, std::string& address, std::string& typetags, char* args)
	{
#ifdef _OSC_DEBUG_
		std::cout << "[OSC]  ADDRESS: " << address << "  TAGS: " << typetags << std::endl;
#endif

		int pos = 0;
		std::stringstream ss;
		ss << "(" << fname << " " << std::fixed << std::showpoint << std::setprecision(23) << t << " \"" << address << "\"";
		//ss << "(io:osc:receive " << std::fixed << std::showpoint << std::setprecision(23) << t << " \"" << address << "\"";
		for(int i=1; i<typetags.size(); ++i) {
			if(typetags[i] == 'i') {
				int osc_int = 0;
				pos += OSC::getOSCInt(args+pos,&osc_int);
				ss << " " << osc_int;
			}else if(typetags[i] == 'f'){
				float osc_float = 0.0f;
				pos += OSC::getOSCfloat(args+pos,&osc_float);
				ss << " " << osc_float;
			}else if(typetags[i] == 'd'){
				double osc_double = 0.0;
				pos += OSC::getOSCdouble(args+pos,&osc_double);
				ss << " " << osc_double;
			}else if(typetags[i] == 's'){
				std::string osc_str;
				pos += OSC::getOSCString(args+pos, &osc_str);
				ss << " \"" << osc_str << "\"";
			}else if(typetags[i] == 'h'){
				int64_t osc_long = 0;
				pos += OSC::getOSCLong(args+pos,&osc_long);
				ss << " " << osc_long;
			}else if(typetags[i] == 't'){
				double timestamp = 0.0;
				pos += OSC::getOSCTimestamp(args+pos, &timestamp);
				ss << " " << timestamp;				
				// }else if(typetags[i] == 'b'){
				// 	NSData* osc_data;
				// 	pos += OSC::getOSCData(args+pos, &osc_data);
				// 	char str[64];
				// 	sprintf(str,"%p",osc_data);
				// 	ss << " \"" << str << "\"";

				//}else if(typetags[i] == ',') {
				//if it's a comma just skip over it
				//}else if (typetags[i] == ' ') {
				// if it's a space just skip over it
			}else if(typetags[i] == '[') {
				ss << " (list ";
			}else if(typetags[i] == ']') {
				ss << ")";
			}else{
				printf("Bad or unsuppored argument type (%c) - dropping message\n",typetags[i]);
				return -1;
			}
		}
		ss << ")";
		if(_sc != NULL) {
#ifdef _OSC_DEBUG_
			std::cout << "SEND SCHEME: " << ss.str() << std::endl;
#endif
			SchemeProcess::I(_sc)->createSchemeTask(new std::string(ss.str()),"OSC TASK",5);
		} else {
			printf("No OSC Registered\n");
		}
		return pos;
	}

	void* osc_mesg_callback(void* obj_p)
	{
		OSC* osc = (OSC*) obj_p;
		while(true) {
#ifdef EXT_BOOST
			boost::asio::ip::udp::endpoint sender;
			long bytes_read = 0;
			try{
				bytes_read = osc->getSocketFD()->receive_from(boost::asio::buffer(osc->getMessageData(),20000), sender); //*osc->getClientAddress());
			}catch(std::exception& e){
				std::cout << "OSC Message Receive Exception: " << e.what() << std::endl;
				exit(1);
			}
#else
			long bytes_read = recvfrom(*osc->getSocketFD(), osc->getMessageData(), 70000, 0, (struct sockaddr*)osc->getClientAddress(), (socklen_t *) osc->getClientAddressSize());

#endif			
			if(bytes_read > -1) {
			  //printf("udp packet size(%lld)\n",bytes_read);
				//std::cout << "OSC from client port: " << osc->getClientAddress() << " " << osc->getAddress() <<  std::endl;
				char* args = osc->getMessageData();
				long length = bytes_read; //osc->getMessageLength();
				double timestamp;
				long pos = 0;
                                long used = 0;
				std::string address;// = new std::string;
				std::string typetags;// = new std::string;
				pos += OSC::getOSCString(args+pos,&address);
                                used += pos;
				if(address.find("#bundle") != std::string::npos) {
#ifdef _OSC_DEBUG_
					std::cout << "OSC BUNDLE:: " << length <<  "  args: " << args << std::endl;
#endif									
					pos += OSC::getOSCTimestamp(args+pos, &timestamp); // skip time tag
					while(pos < length) {
                                                int size = 0;
                                                used = 0;
                                                int res = OSC::getOSCInt(args+pos,&size);
                                                //Used += res;
                                                //don't add res from getting size to used
                                                pos += res;
#ifdef _OSC_DEBUG_					
					        std::cout << "\t--> bundle msg   size(" << size << ") pos(" << pos-4 << ")" << std::endl;
#endif										
						//pos += 4; // skip element size
						address.clear();
						typetags.clear();
						res = OSC::getOSCString(args+pos,&address);
                                                if(address.find("#bundle") != std::string::npos) {
						  std::cout << "WARNING!!!!! Extempore OSC doesn't support recursive bundles!" << std::endl;
                                                  return 0;
						}
                                                used += res;
                                                pos += res; 
						res = OSC::getOSCString(args+pos,&typetags);
                                                used += res;
                                                pos += res; 
						if(osc->getNative() == NULL) {
							int ret_from_call = send_scheme_call(osc->sc,osc->fname,timestamp,address,typetags,args+pos);
							if(ret_from_call < 0) break;
							else pos += size-used; //ret_from_call;
						}else{
          						int (*native) (char*,char*,char*,int) = osc->getNative();
							native((char*)address.c_str(),(char*)typetags.c_str(),args+pos,size-used);
							pos += size-used; //get_message_length(typetags, args);
						}
					}
				}else{
					if(osc->getNative() == NULL) {
						pos += OSC::getOSCString(args+pos,&typetags);
						pos += send_scheme_call(osc->sc,osc->fname,0.0,address,typetags,args+pos);
					}else{
						int res = OSC::getOSCString(args+pos,&typetags);
                                                pos+=res;
                                                used+=res;
						int (*native) (char*,char*,char*,int) = osc->getNative();
						native((char*)address.c_str(),(char*)typetags.c_str(),args+pos,length-used);
					}
				}				
				char reply[256];
				memset(reply,0,256);
#ifdef EXT_BOOST
				std::string caller = osc->getClientAddress()->address().to_string();
#else
				std::string caller(inet_ntoa((*osc->getClientAddress()).sin_addr));
#endif
				//osc->getCallback()(address,typetags,args,(bytes_read - (typetags_length + address_length)),reply,&reply_length,&caller);
				//if(reply_length > 0) sendto(osc->getSocketFD(), reply, reply_length, 0, (struct sockaddr*)osc->getClientAddress(), osc->sizeOfClientAddress());
			}else{
#ifdef EXT_BOOST
				boost::this_thread::sleep(boost::posix_time::microseconds(1000));
#else
				usleep(1000);
#endif
			}
		}
		return NULL;
	}

	OSC::OSC() : threadOSC(), message_length(0), started(false) 
	{
#ifdef EXT_BOOST
		io_service = new boost::asio::io_service;
		osc_address = new boost::asio::ip::udp::endpoint();
		osc_client_address = new boost::asio::ip::udp::endpoint();
#endif
		send_from_serverfd = 0;
		scheme_real_type = 'f';
		scheme_integer_type = 'i';
	}

	void OSC::schemeInit(SchemeProcess* scm)
	{
		//scm->addForeignFunc("osc-send-msg", &OSC::sendOSC);
		//scm->addGlobalCptr((char*)"*io:osc-send-msg*",mk_cb(this,OSC,sendOSC));
		scm->addForeignFunc((char*)"io:osc:start-server", &OSC::registerScheme);
		scm->addForeignFunc((char*)"io:osc:set-real-64bit?", &OSC::set_real_type);
		scm->addForeignFunc((char*)"io:osc:set-integer-64bit?", &OSC::set_integer_type);			
		scm->addForeignFunc((char*)"io:osc:send-from-server-socket?", &OSC::send_from_server_socket);

		//scm->addGlobal("*samplerate*",mk_integer(scm->getSchemeEnv(),AUHost::SAMPLERATE));
	}

	int OSC::setOSCString(char* data, std::string* str) {
#ifdef _OSC_DEBUG_        
		std::cout << "SET OSC STRING = " << *str << std::endl;
#endif	
		int n = 4 - (int)fmod((double)str->length(),4.0);
		for(int i=0;i<n;++i) {
			str->push_back('\0');
		}
		const char* str_d = str->data();
		for(int i=0;i<str->length();++i) {
			data[i] = str_d[i];
		}
		return str->length();
	}

	int OSC::getOSCString(const char* data, std::string* str) {
		int str_cnt = 0;
		for( ;str_cnt<256; ++str_cnt) {
			if (data[str_cnt] == '\0') break;
			str->push_back(data[str_cnt]);
		}
		str_cnt += (4 - (int)fmod((double)str_cnt,4.0));		

		//added because we need to quote quotes to add to scheme expressions
		for(int i=0;i<str->length();i++)
		{
			if(str->at(i)=='"')
			{
				if(str->at(i-1) != '\\')
				{
					str->insert(i,"\\");
					i++;
				}
			}
		}

#ifdef _OSC_DEBUG_        
		std::cout << "GET OSC STRING = " << *str << std::endl;
#endif				
		return str_cnt;        
	}

	int OSC::setOSCfloat(char* data, float* f) {
#ifdef _OSC_DEBUG_        
		std::cout << "SET OSC FLOAT 32 = " << *f << std::endl;
#endif        	
		uint32_t sf = swap32f(*f);
		char* byte_array = (char*) &sf;
		for(int i=0;i<4;++i) {
			data[i] = byte_array[i];
		}
		return 4;
	}

	int OSC::getOSCfloat(const char* data, float* f) {
		*f = unswap32f(*((uint32_t*)data));
#ifdef _OSC_DEBUG_        
		std::cout << "OSC FLOAT 32 = " << *f << std::endl;
#endif        
		return 4;
	}


	int OSC::setOSCdouble(char* data, double* f) {
#ifdef _OSC_DEBUG_        
		std::cout << "SET OSC FLOAT 64 = " << *f << std::endl;
#endif        	
		uint64_t sf = swap64f(*f);
		char* byte_array = (char*) &sf;
		for(int i=0;i<8;++i) {
			data[i] = byte_array[i];
		}
		return 8;
	}

	int OSC::getOSCdouble(const char* data, double* f) {
		*f = unswap64f(*((uint64_t*)data));
#ifdef _OSC_DEBUG_        
		std::cout << "OSC FLOAT 64 = " << *f << std::endl;
#endif        
		return 8;
	}

	int OSC::getOSCTimestamp(const char* data, double* d) {
		uint32_t* dat = (uint32_t*) data;
		int64_t seconds = unswap32i(dat[0]);
		uint32_t fractional = unswap32i(dat[1]);

		if((seconds == 0) && (fractional == 1)) {
			*d = 0.0; 
			return 8;
		}
		//std::cout << "seconds:" << seconds << " fraction:" << fractional << std::endl;
		seconds -= 3187296000ul;
		double dfraction = fractional/4294967296.0; //32 bit unsigned
		*d = ((double)seconds)+dfraction;
		return 8;
	}

	int OSC::setOSCTimestamp(char* data, double d)
	{
#ifdef TARGET_OS_WINDOWS
		uint32_t seconds = (uint32_t) d;
#else
		uint32_t seconds = trunc(d); 
#endif

		double fractional = d - (double) seconds;
		seconds += 3187296000ul; //1543503872;

		uint32_t fractionali = (uint32_t)(fractional * 4294967296.0);

		uint32_t* si = (uint32_t*)data;
		uint32_t* sf = (uint32_t*)(data+4);

		*si = swap32i(seconds);
		*sf = swap32i(fractionali);

		// great! now we have both bits of the NTP puzzle, we just need to jam them into a datastream
		return 8;		
	}


	int OSC::setOSCInt(char* data, int* i) {
#ifdef _OSC_DEBUG_        
		std::cout << "SET OSC INT = " << *i << std::endl;
#endif        		
		*i = swap32i(*i);
		char* byte_array = (char*) i;
		for(int i=0;i<4;++i) {
			data[i] = byte_array[i];
		}
		return 4;
	}


	int OSC::getOSCInt(const char* data, int* i) {
		*i = unswap32i(*((int*) data));
#ifdef _OSC_DEBUG_        
		std::cout << "OSC INT = " << *i << std::endl;
#endif        
		return 4;
	}

	int OSC::setOSCLong(char* data, int64_t* l) {
		*l = swap64i(*l);
		char* byte_array = (char*) l;
		for(int i=0;i<8;++i) {
			data[i] = byte_array[i];
		}
		return 8;
	}

	int OSC::getOSCLong(const char* data, int64_t* l) {
		*l = unswap64i(*((int64_t*)data));
#ifdef _OSC_DEBUG_        
		std::cout << "OSC LONG = " << *l << std::endl;
#endif        
		return 8;
	}


	void OSC::getOSCStringSection(std::string* input, std::string* output, int num) {
		int start = 0;
		int end = 0;
		for(int i=0;i<=num;++i) {
			end = input->find('/',start+1);
			if(i < num) start = end; 
		}
		start++; //skip over the "/"
		output->append(input->substr(start,end-start));
	}

	int OSC::clearMessageBuffer()
	{
#ifdef _OSC_DEBUG_
		printf("CLEAR MESSAGE BUFFER\n");
#endif				
		message_length = 0;
		int cnt = -1;
		do {
			cnt++;
#ifdef EXT_BOOST
			message_length = socket->receive_from(boost::asio::buffer(message_data, 256), *osc_client_address);
#else
			message_length = recvfrom(socket_fd, message_data, 256, 0, (struct sockaddr*)&osc_client_address, (socklen_t *) &osc_client_address_size);
#endif
		}while(message_length > -1);
		return cnt;
	}


	void OSC::processArgs(pointer arg, char** tmp, char** ptr, int* lgth, std::string& typetags, scheme* _sc)
	{
#ifdef _OSC_DEBUG_
		printf("PROCESS ARGS\n");
#endif					
		OSC* osc = OSC::I(_sc);
		int ret = 0;
		int tmpsize = 1024;
		int items = list_length(_sc,arg);
		for(int i=0;i<items;++i) {
			if(is_string(pair_car(arg))) {
				std::string str(string_value(pair_car(arg)));
				ret = OSC::setOSCString(*ptr,&str);
				typetags += "s";
			}else if(is_pair(pair_car(arg))) {
				typetags += "[";  
				processArgs(pair_car(arg),tmp,ptr,lgth,typetags,_sc);
				typetags += "]";
				ret = 0;
			}else if(is_vector(pair_car(arg))) {
				arg = pair_cdr(arg);
				continue;
			}else if(is_symbol(pair_car(arg))) {
				arg = pair_cdr(arg);
				continue;
			}else if(is_integer(pair_car(arg))) {
				if(osc->scheme_integer_type == 'i')
				{
					int val = ivalue(pair_car(arg));
					ret = OSC::setOSCInt(*ptr, &val);
					typetags += "i";									
				}
				else
				{
					int64_t val = ivalue(pair_car(arg));
					ret = OSC::setOSCLong(*ptr, &val);
					typetags += "h";				
				}
			}else if(is_real(pair_car(arg))){
				if(osc->scheme_real_type == 'f') {
					float val = (float) rvalue(pair_car(arg));
					ret = OSC::setOSCfloat(*ptr, &val);					
					typetags += "f";
				}else{
					double val = (double) rvalue(pair_car(arg));
					ret = OSC::setOSCdouble(*ptr, &val);
					typetags += "d";					
				}
			}
			*lgth += ret; *ptr += ret;			
			arg = pair_cdr(arg);
		}	
	}

	//pointer OSC::sendOSC(scheme* _sc, pointer args)
	void OSC::sendOSC(TaskI* task)
	{
		Task<SchemeObj*>* t = static_cast<Task<SchemeObj*>*>(task);
		pointer args = t->getArg()->getValue();
		scheme* _sc = t->getArg()->getScheme();

		char* host = string_value(pair_car(args));
		int port = ivalue(pair_cadr(args));
#ifdef _OSC_DEBUG_
		std::cout << "SENDTO: " << host << "  ON PORT: " << port << std::endl;
#endif
		int length = 0;
		int ret = 0;
		char* ptr;

#ifdef EXT_BOOST
		boost::asio::ip::udp::resolver::iterator end;
		boost::asio::ip::udp::resolver resolver(*io_service);
		std::stringstream ss;
		ss << port;
		boost::asio::ip::udp::resolver::query newQuery(boost::asio::ip::udp::v4(),host, ss.str());
		boost::asio::ip::udp::resolver::iterator iter = resolver.resolve(newQuery);

		boost::asio::ip::udp::endpoint sa = *iter;
#else
		struct sockaddr_in sa;
		struct hostent* hen; /* host-to-IP translation */

		/* Address resolution stage */
		hen = gethostbyname(host);
		if (!hen) {
			printf("OSC Error: Could no resolve host name\n");
			delete t->getArg();
			return;			
		}

		memset(&sa, 0, sizeof(sa));

		sa.sin_family = AF_INET;
		sa.sin_port = htons(port);
		memcpy(&sa.sin_addr.s_addr, hen->h_addr_list[0], hen->h_length);
#endif		


#ifdef EXT_BOOST
		boost::asio::ip::udp::socket* fd = 0;
		if(OSC::I(_sc)->send_from_serverfd) {
			fd = OSC::I(_sc)->getSocketFD(); //  getSendFD();
		}
#else
		int fd = 0;
		if(OSC::I(_sc)->send_from_serverfd) {
			fd = *(OSC::I(_sc)->getSocketFD()); //  getSendFD();
		}else{
			fd = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);			
		}
#endif

		std::string address(string_value(pair_caddr(args)));
		std::string typetags(",");
		std::string body;

		pointer arg = pair_cadddr(args);
		int tmpsize = 1024;
		char* tmp = (char*) malloc(tmpsize);
		//char tmp[1024];
		ptr = tmp;
		int lgth = 0;
		processArgs(arg,&tmp,&ptr,&lgth,typetags,_sc);

		char* message = (char*) malloc(1024+tmpsize);		
		ptr = message;
		ret = OSC::setOSCString(ptr, &address);
		length += ret; ptr += ret;
		ret = OSC::setOSCString(ptr, &typetags);
		length += ret; ptr += ret;
		memcpy(ptr, tmp, lgth);
		length += lgth;
#ifdef _OSC_DEBUG_
		std::cout << "SENDING MSG: " << message << "  of size: " << length << std::endl;
#endif

#ifdef EXT_BOOST
		int err = 0;
		if(OSC::I(_sc)->send_from_serverfd) {
			err = fd->send_to(boost::asio::buffer(message, length), sa);
		}else{
			boost::asio::io_service service;
			boost::asio::ip::udp::socket socket(service);
			socket.open(boost::asio::ip::udp::v4());
			socket.send_to(boost::asio::buffer(message, length), sa);
		}
#else
		int err = sendto(fd, message, length, 0, (struct sockaddr*)&sa, sizeof(sa));
		if(!OSC::I(_sc)->send_from_serverfd) close(fd);
#endif
		if(err < 0)
		{
#ifdef _OSC_DEBUG_
			std::cout << "OSC Send Error: " << err << std::endl;
#endif		
			if(err == EMSGSIZE) {
				printf("Error: OSC message too large: UDP 8k message MAX\n");
			}else{
    			        printf("Error: Problem sending OSC message: %d\n",err);
			}			

		}

		free(tmp);
		free(message);

		delete t->getArg();		
		return;
		//return _sc->NIL;
	}

	pointer OSC::set_real_type(scheme* _sc, pointer args) {
		OSC* osc = OSC::I(_sc);

		if(pair_car(args)==_sc->T)
		{
			osc->scheme_real_type = 'd';
		}else{
			osc->scheme_real_type = 'f';
		}
		return _sc->T;
	}

	pointer OSC::set_integer_type(scheme* _sc, pointer args) {
		OSC* osc = OSC::I(_sc);

		if(pair_car(args)==_sc->T)
		{
			osc->scheme_integer_type = 'h';
		}else{
			osc->scheme_integer_type = 'i';
		}
		return _sc->T;
	}

	pointer OSC::send_from_server_socket(scheme* _sc, pointer args) {
		OSC* osc = OSC::I(_sc);

		if(pair_car(args)==_sc->T)
		{
			osc->send_from_serverfd = 1;
		}else{
			osc->send_from_serverfd = 0;
		}
		return _sc->T;
	}


	pointer OSC::registerScheme(scheme* _sc, pointer args) {
		OSC* osc = new OSC(); //OSC::I();
		SCHEME_MAP[_sc] = osc;
		memset(osc->fname,0,256);
		char* name = string_value(pair_cadr(args));
		strcpy(osc->fname,name);

		// should we use native callback?
		if(pair_cddr(args) != _sc->NIL && is_cptr(pair_caddr(args))) {
  		        osc->setNative( (int(*)(char*,char*,char*,int)) cptr_value(pair_caddr(args)));
		}else{
			osc->setNative(NULL);
		}

		SchemeProcess* scm = extemp::SchemeProcess::I(_sc);
		scm->addGlobalCptr((char*)"*io:osc:send-msg*",mk_cb(osc,OSC,sendOSC));

		// setup server port
#ifdef EXT_BOOST
		boost::asio::ip::udp::endpoint* osc_address = osc->getAddress();
		int port = ivalue(pair_car(args)); // [[[imp::NativeScheme::RESOURCES getPreferencesDictionary] valueForKey:@"osc_port"] intValue];
		osc_address->port(port);

		try{ 
			boost::asio::ip::udp::socket* sock = new boost::asio::ip::udp::socket(*osc->getIOService()); //(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
			sock->open(boost::asio::ip::udp::v4());
			//boost::asio::socket_base::non_blocking_io command(true);
			//sock->io_control(command);
			sock->bind(*osc_address); 

			osc->setSocket(sock);
			printf("Starting OSC server on port: %d calling back to %s\n",port,name);
		}catch(std::exception& e){
			std::cout << "Error establishing OSC socket: is address allready used?" << std::endl;
			return _sc->NIL;
		}
#else
		struct sockaddr_in* osc_address = osc->getAddress();

		memset((char*) osc_address, 0, sizeof(*osc_address));
		int port = ivalue(pair_car(args)); // [[[imp::NativeScheme::RESOURCES getPreferencesDictionary] valueForKey:@"osc_port"] intValue];
		printf("Starting OSC server on port: %d calling back to %s\n",port,name);

		osc_address->sin_family = AF_INET;
		osc_address->sin_port = htons(port);
		osc_address->sin_addr.s_addr = htonl(INADDR_ANY); //set server's IP address

		int socket_fd = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
		if(socket_fd == -1) {
			printf("Error opening OSC socket\n");
			std::cout << "Error opening OSC socket"<< std::endl;
		}
		fcntl(socket_fd, F_SETFL, O_NONBLOCK); //set to non-blocking socket

		if(bind(socket_fd, (struct sockaddr*) osc_address, sizeof(*osc_address)) == -1) {
			printf("Error opening OSC socket\n");
			std::cout << "Error binding OSC address to socket" << std::endl;
		}		

		*(osc->getSocketFD())=socket_fd;

		// setup client struct.
		struct sockaddr_in* osc_client_address = osc->getClientAddress();
		*(osc->getClientAddressSize())=sizeof(*osc_client_address);
#endif

		if(!osc->getStarted()) {
			osc->getThread().create(&osc_mesg_callback, osc);
			osc->setStarted(true);
		}
		osc->sc = _sc;
		return _sc->NIL;
	}
} //End Namespace

