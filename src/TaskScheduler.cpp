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

#include "TaskScheduler.h"

namespace extemp {
	
    TaskScheduler TaskScheduler::SINGLETON;
	
    TaskScheduler::TaskScheduler()
    {
	queueThread = new EXTThread();
	guard = new EXTMonitor("task_scheduler_guard");		
	queueThread->create(TaskScheduler::queue_thread_callback, this);
	guard->init();
	//std::cout << "QUEUE THREAD " << queueThread->getPthread() << std::endl;
    }
		
    void TaskScheduler::timeSlice() //(const long timestamp, const int frames)
    {
	//lock the queue for this thread only
	queue.lock();

	// if(clearFlag) {
	// 	queue.clear();
	// 	clearFlag = false;
	// }
	TaskI* t = queue.peek();

	// this is a task we need to do something with
	while(t != NULL && (t->getStartTime() < (UNIV::TIME + UNIV::FRAMES))) {
	    t = queue.get();
	    try{
		   if(t->getTag() == 0) t->execute();
	    }catch(std::exception& e){ //...){
		   std::cout << "Error executing scheduled task! " << e.what() << std::endl;
	    }
	    delete t;
	    t = queue.peek();
	}

	//unlock queue for this thread
	queue.unlock();
    }	

    //realtime thread for handling all scheduled tasks
    void* TaskScheduler::queue_thread_callback(void* obj_p)
    {
	TaskScheduler* sched = static_cast<TaskScheduler*>(obj_p);					
	EXTMonitor* guard = sched->getGuard();
	while(true) {
#ifdef EXT_BOOST
	     sched->timeSlice();		
	     guard->wait();
#else
	     guard->lock();			
	     sched->timeSlice();		
	     guard->wait();
	     guard->unlock();
#endif
	}
	return obj_p;
    }

} // End Namespace
