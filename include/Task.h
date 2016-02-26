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

#ifndef IMP_TASK_H
#define IMP_TASK_H

#include <iostream>

#include "CM.h"


namespace extemp {
	
    class TaskI {
    public:
	virtual ~TaskI() {} //responsibility for cleanup of classMember is outside of task
	long long int getStartTime() const { return this->startTime; }
	long long int getDuration() const { return this->duration; }		
	void setStartTime(long long int _startTime) { this->startTime = _startTime; }
	bool isActive() { return this->active; }
	bool isCallback() { return this->is_callback; }
	void isCallback(bool _callback) { this->is_callback = _callback; } 
	virtual void execute() { classMember->execute(this); }
	bool equal(TaskI* t)
	{
	    if(t->getStartTime() != startTime) { std::cout << "Failed Time Equality Test" << std::endl; return false; }
	    if(t->getTag() != tag) { std::cout << "Failed Tag Equality Test" << std::endl; return false; }
	    return true;
	}
	virtual int getTag() { return this->tag; }
				
    protected:
	TaskI(long long int _startTime, long long int _duration, CM* _classMember, int _tag/*std::string _label*/) : startTime(_startTime), duration(_duration), active(true), is_callback(false), is_aumidi(false), classMember(_classMember), tag(_tag) {} //label(_label) {}
		
    private:
	long long int startTime;
	long long int duration;
	bool active;
	bool is_callback;
	bool is_aumidi;
	int tag;
	CM* classMember; 
    };

    template<typename T = int>
    class Task : public TaskI {
    public:
	Task(long long int _startTime, long long int _duration, CM* _classMember, T _arg, int _tag=0/*std::string _label = "default"*/) : 
	    TaskI(_startTime, _duration, _classMember, _tag), //_label),
	    arg(_arg)            		
	{}

    T getArg() { return this->arg; }

    private:
	T arg; //responsibility for cleanup of arg is outside task
    };

} //End Namespace
#endif
