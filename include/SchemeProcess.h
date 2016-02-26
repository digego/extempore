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

#ifndef SCHEME_PROCESS_H
#define SCHEME_PROCESS_H

#include "Scheme.h"
#include "SchemePrivate.h"	
#include <string>
#include "Task.h"
#include <queue>
#include <map>
#include <sstream>
#include "EXTLLVM.h"

#ifdef EXT_BOOST
#include <boost/asio.hpp>
#include <boost/bind.hpp>
#endif

#define pair_caar(p) pair_car(pair_car(p))
#define pair_cadr(p) pair_car(pair_cdr(p))
#define pair_cdar(p) pair_cdr(pair_car(p))
#define pair_cddr(p) pair_cdr(pair_cdr(p))
#define pair_cadar(p) pair_car(pair_cdr(pair_car(p)))
#define pair_caadr(p) pair_car(pair_car(pair_cdr(p)))
#define pair_cdaar(p) pair_cdr(pair_car(pair_car(p)))
#define pair_caddr(p) pair_car(pair_cdr(pair_cdr(p)))
#define pair_cddar(p) pair_cdr(pair_cdr(pair_car(p)))
#define pair_cdddr(p) pair_cdr(pair_cdr(pair_cdr(p)))
#define pair_cadddr(p) pair_car(pair_cdr(pair_cdr(pair_cdr(p))))
#define pair_cddddr(p) pair_cdr(pair_cdr(pair_cdr(pair_cdr(p))))
#define pair_caddddr(p) pair_car(pair_cdr(pair_cdr(pair_cdr(pair_cdr(p)))))
#define pair_cdddddr(p) pair_cdr(pair_cdr(pair_cdr(pair_cdr(pair_cdr(p)))))
#define pair_cadddddr(p) pair_car(pair_cdr(pair_cdr(pair_cdr(pair_cdr(pair_cdr(p))))))
#define pair_cddddddr(p) pair_cdr(pair_cdr(pair_cdr(pair_cdr(pair_cdr(pair_cdr(p))))))
#define pair_caddddddr(p) pair_car(pair_cdr(pair_cdr(pair_cdr(pair_cdr(pair_cdr(pair_cdr(p)))))))

#define TERMINATION_CHAR 23

namespace extemp {
	
    class SchemeTask {	
    public:
    SchemeTask(uint64_t _time, uint64_t _max_duration, void* _ptr, std::string _label, int _type, void* _ptr2 = 0) : time(_time), max_duration(_max_duration), ptr(_ptr), label(_label), type(_type), ptr2(_ptr2) /*, cnt_access_ptr(0) */ {}
	uint64_t getTime() { return time; }
	uint64_t getMaxDuration() { return max_duration; }
	void* getPtr() { return ptr; }
	void* getPtr2() { return ptr2; }
	std::string getLabel() { return label; }
	int getType() { return type; }
				
    private:
	uint64_t time;
	uint64_t max_duration;
	void* ptr;
	std::string label;
	int type; // 0 = repl task,  1 = callback task,  2 = destroy env task
	void* ptr2;
    };

    class SchemeProcess {
    public:
        SchemeProcess(std::string _load_path, std::string _name, int server_port=7010, bool banner=false, std::string load_file="");
	~SchemeProcess();
        static SchemeProcess* I();
	static SchemeProcess* I(int index);
	static SchemeProcess* I(std::string name);
	static SchemeProcess* I(scheme* sc);

/* #ifdef EXT_BOOST */
/* 	//	static SchemeProcess* I(int); */
/* #else */
/* 	static SchemeProcess* I(pthread_t); */
/* #endif */
	//Thread functions
	static void* impromptu_server_thread(void* obj_p);
	static void* impromptu_task_executer(void* obj_p);		
		
	long long int getMaxDuration();
	void setMaxDuration(long long int);		
	bool loadFile(const std::string file, const std::string path);
	bool loadFile(const std::string file);
	std::string getInitExpr() { return init_expr; }
	void addGlobal(char* symbol_name, pointer arg);		
	void addForeignFunc(char* symbol_name, foreign_func func);
	void addGlobalCptr(char* symbol_name, void* ptr);
	void schemeCallback(TaskI* task);
	void extemporeCallback(TaskI* task);
	void createSchemeTask(void* arg, std::string label, int taskType);
	bool isServerThreadRunning() { return threadServer.isRunning(); }
	bool isSchemeThreadRunning() { return threadScheme.isRunning(); }
		
	void stop();
	bool start();
	bool withBanner() { return with_banner; }
	int setPriority(int); 
	int getPriority(); 
		
	std::string getLoadPath() { return load_path; };	
	//pointer schemeApply(pointer func, pointer args);
	pointer deleteMemberFromList(pointer member, pointer list);	
	bool findMemberFromList(pointer member, pointer list);
	void addSchemeGlobal(char* symbol_name, void* c_ptr);			
	std::string eval(char* evalString);
	scheme* getSchemeEnv() { return sc; }
	void testCall(TaskI* task);
	void repl();
	std::string getEvalString();
	//CAGuard& getGuard() { return guard; }
	EXTMonitor& getGuard() { return guard; }
	//std::map<int, std::string>& getResultStrings() { return result_string; }
	bool getRunning() { return running; }
	//static void printSchemeCons(scheme* sc, std::stringstream& ss, pointer cons, bool full = false, bool stringquotes = true);
	static void banner(std::ostream* ss);
#ifdef EXT_BOOST
	boost::asio::ip::tcp::acceptor* getServerSocket() { return server_socket; }
	boost::asio::io_service* getIOService() { return io_service; }
        std::vector<boost::asio::ip::tcp::socket*>& getClientSockets() { return client_sockets;}
        std::map<boost::asio::ip::tcp::socket*,std::stringstream*>& getInStreams() { return in_streams; }        
#else
	int getServerSocket() { return server_socket; }
#endif
	int getServerPort() { return server_port; }
	std::queue<SchemeTask>& getQueue() { return taskq; }
	llvm_zone_t* getDefaultZone() { return default_zone; }
	/* // this added for dodgy contuations support */
        /* ucontext_t* getContext() { return _context; }         */
		
	std::string& getName() { return name; }
	void setLoadedLibs(bool v) { libs_loaded = v; }
	bool loadedLibs() {return libs_loaded; }
	//std::vector<int>* getClientSockets() { return &client_sockets; }

        #define SCHEME_OUTPORT_STRING_LENGTH 256
	char scheme_outport_string[SCHEME_OUTPORT_STRING_LENGTH];

	static bool CAPS_THROUGH; //send caps lock through to editor window or block?
		
	static std::vector<SchemeProcess*> INSTANCES;		
	static bool EXTENDED_ERROR_LOGGING;
	static bool SCHEME_OPS_LOGGING;				
	static bool TASK_QUEUE_LOGGING;		
	static bool SCHEME_EVAL_TIMING;				
	static std::map<scheme*, SchemeProcess*> SCHEME_MAP;
	static std::map<std::string, SchemeProcess*> SCHEME_NAME_MAP;

	extemp::CM* extempore_lang_cb;
		
    private:
	bool libs_loaded;
	std::string load_path;			
	std::string name;
	scheme* sc;
	EXTThread threadScheme;
	EXTThread threadServer;
#ifdef EXT_BOOST
	EXTThread threadBoost;
#endif
	EXTMonitor guard;
	bool running;
	int server_port;
	bool with_banner;
	uint64_t max_duration;				
#ifdef EXT_BOOST
	boost::asio::ip::tcp::acceptor* server_socket;
	boost::asio::io_service* io_service;
        std::vector<boost::asio::ip::tcp::socket*> client_sockets;
        std::map<boost::asio::ip::tcp::socket*,std::stringstream*> in_streams;
#else
	int server_socket;				
#endif
	std::queue<SchemeTask> taskq;
	llvm_zone_t* default_zone;
	/* // this added for dodgy continuations support */
        /* ucontext_t _context;		 */
	std::string init_expr;
    };
	
    class SchemeObj{
    public:
	SchemeObj(scheme* _sc, pointer _val, pointer _env = NULL);
	~SchemeObj();
	pointer getEnvironment();
	pointer getValue();
	scheme* getScheme();	
		
    private:
	scheme* sc;
	pointer env;
	pointer values;
    };
	

} //End Namespace

#endif
