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
#include <assert.h>

#ifdef _WIN32
#include <winsock2.h>
#else
typedef int SOCKET;
#endif

struct llvm_zone_t;

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

namespace extemp {

class SchemeTask {
public:
    enum class Type { REPL, SCHEME_CALLBACK, DESTROY_ENV, CALLBACK_SYMBOL,
            LOCAL_PROCESS_STRING = 5, EXTEMPORE_CALLBACK };
private:
    uint64_t    m_time;
    uint64_t    m_maxDuration;
    void*       m_ptr;
    std::string m_label;
    Type        m_type; // 0 = repl task,  1 = callback task,  2 = destroy env task
    void*       m_ptr2;
public:
    SchemeTask(uint64_t Time, uint64_t MaxDuration, void* Ptr, const std::string& Label, Type Type, void* Ptr2 = 0):
            m_time(Time), m_maxDuration(MaxDuration), m_ptr(Ptr), m_label(Label), m_type(Type), m_ptr2(Ptr2) {
    }

    uint64_t getTime() const { return m_time; }
    uint64_t getMaxDuration() const { return m_maxDuration; }
    void* getPtr() const { return m_ptr; }
    void* getPtr2() const { return m_ptr2; }
    const std::string& getLabel() const { return m_label; }
    Type getType() const { return m_type; }
};

class SchemeProcess {
private:
    typedef std::queue<SchemeTask> task_queue_type;
    static const unsigned SCHEME_OUTPORT_STRING_LENGTH = 256;
private:
    std::string     m_loadPath;
    std::string     m_name;
    int16_t         m_serverPort;
    bool            m_banner;
    std::string     m_initExpr;
    bool            m_libsLoaded;
    EXTMonitor      m_guard;
    bool            m_running;
    EXTThread       m_threadTask;
    EXTThread       m_threadServer;
    scheme*         m_scheme;
    uint64_t        m_maxDuration;
    SOCKET          m_serverSocket;
    task_queue_type m_taskQueue;
    llvm_zone_t*    m_defaultZone;
    extemp::CM*     m_extemporeCallback;
    char            m_schemeOutportString[SCHEME_OUTPORT_STRING_LENGTH];

    static const char*                           sm_banner;
    static THREAD_LOCAL SchemeProcess*           sm_current;
private:
    void schemeCallback(TaskI* Task) {
        addCallback(Task, SchemeTask::Type::SCHEME_CALLBACK);
    }
    void extemporeCallback(TaskI* Task) {
        addCallback(Task, SchemeTask::Type::EXTEMPORE_CALLBACK);
    }
    void addCallback(TaskI* Task, SchemeTask::Type Type);
    void* serverImpl();
    void* taskImpl();
    void resetOutportString() {
        m_scheme->outport->_object._port->rep.string.curr = m_schemeOutportString;
        memset(m_schemeOutportString, 0, sizeof(m_schemeOutportString));
    }
    bool loadFile(const std::string& File, const std::string& Path = std::string());

    static void* serverTrampoline(void* Arg) {
        return reinterpret_cast<SchemeProcess*>(Arg)->serverImpl();
    }
    static void* taskTrampoline(void* Arg) {
        return reinterpret_cast<SchemeProcess*>(Arg)->taskImpl();
    }
public:
    SchemeProcess(const std::string& LoadPath, const std::string& Name, int ServerPort = 7010, bool Banner = false,
            const std::string& InitExpr = std::string());

    uint64_t getMaxDuration() const { return m_maxDuration; }
    void setMaxDuration(uint64_t MaxDuration) { m_maxDuration = MaxDuration; }
    bool getRunning() const { return m_running; }
    llvm_zone_t* getDefaultZone() { return m_defaultZone; }
    const std::string& getName() { return m_name; }
    extemp::CM* getExtemporeCallback() const { return m_extemporeCallback; }
    void setPriority(int Priority) {
        m_threadTask.setPriority(Priority, false);
        m_threadServer.setPriority(Priority, false);
    }
    int getPriority() const {
        assert(m_threadTask.getPriority() == m_threadServer.getPriority());
        return m_threadTask.getPriority();
    }

    void addGlobal(char* Symbol, pointer Arg) {
        scheme_define(m_scheme, m_scheme->global_env, mk_symbol(m_scheme, Symbol), Arg);
    }
    void addForeignFunc(char* Symbol, foreign_func Func) {
        scheme_define(m_scheme, m_scheme->global_env, mk_symbol(m_scheme, Symbol), mk_foreign_func(m_scheme, Func));
    }
    void addGlobalCptr(char* Symbol, void* Ptr) {
        scheme_define(m_scheme, m_scheme->global_env, mk_symbol(m_scheme, Symbol), mk_cptr(m_scheme, Ptr));
    }
    void addSchemeGlobal(char* Symbol, void* Cptr)
    {
        scheme_define(m_scheme, m_scheme->global_env, mk_symbol(m_scheme, Symbol), mk_cptr(m_scheme, Cptr));
    }
    void createSchemeTask(void* Arg, const std::string& label, SchemeTask::Type TaskType);
    void stop();
    bool start(bool subsume=false);

    static SchemeProcess* I() { return sm_current; }
};

class SchemeObj
{
private:
    scheme* m_scheme;
    pointer m_values;
    pointer m_env;
public:
    SchemeObj(scheme* Sheme, pointer Values, pointer Env);
    ~SchemeObj();

    pointer getEnvironment() const { return m_env; }
    pointer getValue() const { return m_values; }
    scheme* getScheme() const { return m_scheme; }
};


} //End Namespace

#endif
