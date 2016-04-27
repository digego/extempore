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

#ifndef CM_H
#define CM_H

#include <string>
#include "BranchPrediction.h"

namespace extemp
{

#define mk_cb(instance, class, func) (new extemp::CMI<class>(instance, &class::func))

class TaskI;

class CM
{
public:
    virtual ~CM() = default;

    virtual void execute(TaskI* Task) = 0;
    virtual void print() {
    }
};

class CMG: public CM
{
private:
    typedef void (*function_type)(TaskI* Task);
private:
    function_type m_function;
public:
    CMG(function_type Function): m_function(Function) {
    }

    virtual void execute(TaskI* Task) {
        m_function(Task);
    }
};

template <typename T>
class CMI: public CM
{
private:
    typedef void (T::*function_type)(TaskI* Task);
private:
    T*            m_object;
    function_type m_function;
public:
    CMI(T* Object, function_type Function): m_object(Object), m_function(Function) {
    }

    virtual void execute(TaskI* Task) {
        //std::cout << "TIME: " << current_time << "  TAG: " << tag << "  ARG: " << arg << "   OBJ: " << object << "   MEMBER: " << member << std::endl;
        if (unlikely(!m_object)) {
            std::cerr << "AIME::Object has been removed before task could execute!" << std::endl;
            return;
        }
        (m_object->*m_function)(Task);
    }
    virtual void print() {
        std::cout << "OBJ: " << m_object << "  MEMBER: " << m_function << std::endl;
    }
};

}

#endif
