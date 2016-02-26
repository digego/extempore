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

#ifndef TASK_SCHEDULER_H
#define TASK_SCHEDULER_H

#include "PriorityQueue.h"
//#include "CAPThread.h"
//#include "CAGuard.h"
#include "EXTThread.h"
//#include "EXTMonitor.h"
#include "UNIV.h"

namespace extemp {

    class EXTMonitor;

    class TaskScheduler {
    public:
	TaskScheduler();
	~TaskScheduler() {};
	
	static TaskScheduler* I() { return &SINGLETON; };
	
	// add task to task queue
	void add(TaskI* t) { queue.add(t); }

  void start() { queueThread->create(TaskScheduler::queue_thread_callback, this); }
	
	// static addTask	
	template<typename T>
	static void addTask(uint64_t _startTime, uint64_t _duration, CM* _classMember, T _arg, int _tag, bool callback)
	{
	    Task<T>* task = new Task<T>(_startTime,_duration,_classMember,_arg,_tag);
	    task->isCallback(callback);
	    I()->add(task);
	};	
	
	// for pulling ready tasks from the task queue
	void timeSlice();
	//CAGuard* getGuard() {return guard;};
	EXTMonitor* getGuard() { return guard; }
	static void* queue_thread_callback(void* p_obj);

    private:
	PriorityQueue<uint64_t, TaskI*> queue;
	//CAPThread* queueThread;
	//CAGuard* guard;
	EXTThread* queueThread;
	EXTMonitor* guard;
	static TaskScheduler SINGLETON;
    };

} //End Namespace
#endif
