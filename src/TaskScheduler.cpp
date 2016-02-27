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

#include "TaskScheduler.h"
#include "AudioDevice.h"
#include <math.h>

namespace extemp {
	
  TaskScheduler TaskScheduler::SINGLETON;
	
  TaskScheduler::TaskScheduler()
  {
    queueThread = new EXTThread();
    guard = new EXTMonitor("task_scheduler_guard");		
    guard->init();
    //std::cout << "QUEUE THREAD " << queueThread->getPthread() << std::endl;
  }

  // void TaskScheduler::timeSlice() 
  // {
  //   //lock the queue for this thread only
  //   queue.lock();

  //   // if(clearFlag) {
  //   // 	queue.clear();
  //   // 	clearFlag = false;
  //   // }
  //   TaskI* t = queue.peek();

  //   // this is a task we need to do something with
  //   while(t != NULL && (t->getStartTime() < (UNIV::TIME + UNIV::FRAMES))) {
	//     t = queue.get();
	//     try{
  //       if(t->getTag() == 0) t->execute();
	//     }catch(std::exception& e){ //...){
  //       std::cout << "Error executing scheduled task! " << e.what() << std::endl;
	//     }
	//     delete t;
	//     t = queue.peek();
  //   }

  //   //unlock queue for this thread
  //   queue.unlock();
  // }
  
  static uint64_t AUDIO_DEVICE_START_OFFSET = 0;
  static double LAST_REALTIME_STAMP = 0.0;

  void TaskScheduler::timeSlice() 
  {
    uint32_t frames = UNIV::FRAMES / UNIV::TIME_DIVISION;
    uint64_t nanosecs = ((double)frames / (double)UNIV::SAMPLERATE) * 1000000000.0;
    uint32_t division_num = 0;

#ifdef _WIN32
    // not on windows yet!
#else
    struct timespec a, b;
    b.tv_sec = 0;
    b.tv_nsec = 0;
#endif

    if(UNIV::AUDIO_NONE > 0) { // i.e. if no audio device
      AudioDevice::CLOCKBASE = getRealTime();
      UNIV::AUDIO_CLOCK_BASE = AudioDevice::CLOCKBASE;
    }

    LAST_REALTIME_STAMP=getRealTime();
    do{
      queue.lock();
      TaskI* t = queue.peek();      
      // this is a task we need to do something with
      while(t != NULL && (t->getStartTime() < (UNIV::TIME + frames))) {
        t = queue.get();
        try{
          if(t->getTag() == 0) t->execute();
        }catch(std::exception& e){ //...){
          std::cout << "Error executing scheduled task! " << e.what() << std::endl;
        }
        delete t;
        t = queue.peek();
      }
      queue.unlock();
      // increment time and run nanosleep if running separate to audiodevice (i.e. TIME_DIVISION > 1)
      if (UNIV::TIME_DIVISION > 1) {
        if(UNIV::AUDIO_NONE > 0) {
          AudioDevice::REALTIME = getRealTime();
          UNIV::AUDIO_CLOCK_NOW = AudioDevice::REALTIME;
        }else if(UNIV::DEVICE_TIME == 0) {
          AUDIO_DEVICE_START_OFFSET = UNIV::TIME;
        }        
        //std::cout << "TIME: " << UNIV::TIME << " diff: " << ((double)UNIV::TIME - ((double)UNIV::DEVICE_TIME+(double)AUDIO_DEVICE_START_OFFSET)) << " frames: " << frames << std::endl;
        UNIV::TIME += frames;        
#ifdef _WIN32
        // not on windows yet!
#else
        // if last error (b.tv_nsec) is small then keep sleeping
        double realtime_stamp = getRealTime();
        double timediff = realtime_stamp - (LAST_REALTIME_STAMP + ((double) frames / (double)UNIV::SAMPLERATE));
        LAST_REALTIME_STAMP = realtime_stamp;                  
        a.tv_sec = 0;
        a.tv_nsec = nanosecs - b.tv_nsec; // subtract error from last nanosleep
        // subtract any timediff error!
        // then multiply by 0.5 to split the difference (i.e only move halfway towards the error).
        a.tv_nsec -= ((uint64_t) (0.5*timediff*1000000000.0));
        // if we are running an audio device move slowly towards the device time.
        if(UNIV::AUDIO_NONE < 1) {
          a.tv_nsec += (uint64_t)(((double)UNIV::TIME - ((double)UNIV::DEVICE_TIME+AUDIO_DEVICE_START_OFFSET)) / (double)UNIV::SAMPLERATE * 1000000000.0 * 0.5);
        }
        nanosleep(&a,&b);
#endif
	  }
	}while(UNIV::TIME_DIVISION > 1);
    // return will never be called if UNIV::TIME_DIVISION > 1
    return;
  }

  //realtime thread for handling all scheduled tasks
  void* TaskScheduler::queue_thread_callback(void* obj_p)
  {
    // if TIME_DIVISION == 1 then lock to audiodevice.
    if(UNIV::TIME_DIVISION == 1) {      
      TaskScheduler* sched = static_cast<TaskScheduler*>(obj_p);					
      EXTMonitor* guard = sched->getGuard();
      while(true) {
#ifdef EXT_BOOST
        sched->timeSlice();		
        guard->wait();
#else
        sched->timeSlice();
        guard->lock();
        guard->wait();
        guard->unlock();
#endif
      }
      return obj_p;
    }else{ // otherwise if TIME_DIVISION > 1 then timeSlice never returns!
      TaskScheduler* sched = static_cast<TaskScheduler*>(obj_p);					      
      sched->timeSlice();
      // should never return from timeSlice
      return NULL;
    }
  }
} // End Namespace
