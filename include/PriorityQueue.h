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

#ifndef PRIORITY_QUEUE_H
#define PRIORITY_QUEUE_H

//#define _DEBUG_QUEUE_

#include <map>

#include <iterator>
#include <iostream>

#include "EXTMutex.h"
//#include "CAMutex.h"
#include "Task.h"

namespace extemp {
			
    template<typename KEY>
    class SortCriteria {
    public:
	bool operator() ( const KEY& elem1, const KEY& elem2) const {
	    return elem1 < elem2;
	} 
    };
	
    template<typename KEY, typename T>
    class PriorityQueue {
	typedef std::multimap< KEY, T, SortCriteria<KEY> > CONTAINER;
	typedef typename std::multimap< KEY, T, SortCriteria<KEY> >::iterator CIter;    
			
    public:
				
	PriorityQueue() : mutex("priority_queue")
	{
	    mutex.init();
	}
			
	~PriorityQueue()
	{
	    mutex.destroy();
	}
			
	CONTAINER& getQueue() { return this->queue; }
	CIter begin() { return queue.begin(); }
	CIter end() { return queue.end(); }
	CIter lower_bound(const KEY time) { return queue.lower_bound(time); }
	CIter upper_bound(const KEY time) { return queue.upper_bound(time); }
			
	void lock()
	{
	    mutex.lock();
	}
			
	void unlock()
	{
	    mutex.unlock();
	}
			
	bool add(T val)
	{
	    mutex.lock();
				
	    CIter pos  = queue.insert(std::make_pair(val->getStartTime(),val));
				
#ifdef _DEBUG_QUEUE_
	    std::cout << "QUEUE[" << this << "] " << "Successfully Added Task. Size: " << queue.size() << " at startime " << val->getStartTime() << std::endl;
	    std::cout << "QUEUE[" << this << "] " << "Inserted as element " << (std::distance(queue.begin(),pos) + 1) << " in queue of size " << queue.size() << std::endl;
				
	    /*
	      std::cout << "-----------START----------" << std::endl;
	      pos = queue.begin(); 
	      CIter end = queue.end();
	      for( ;pos != end; ++pos) {
	      std::cout << pos->second->getStartTime() << std::endl;
	      }
	      std::cout << "------------END-----------" << std::endl;
	    */
#endif
	    mutex.unlock();
				
	    return true;
	}
			
	int erase(std::string label)
	{
	    int res = 0;
				
	    mutex.lock();
				
	    CIter pos = queue.begin();
	    CIter end = queue.end();
	    for( ;pos != end; ++pos) {
		if(label == pos->second->getLabel()) {
#ifdef _DEBUG_QUEUE_
		    std::cout << "Removing " << pos->second << " from the queue.  LABEL(" << label << ")" << std::endl;
#endif
		    queue.erase(pos);
		    res = 1;
		    break;
		}
	    }
				
	    mutex.unlock();
				
	    return res;
	}
			
	int erase(T val)
	{
	    int res = 0;
				
	    mutex.lock();
				
	    CIter pos = queue.begin(); 
	    CIter end = queue.end();
	    for( ;pos != end; ++pos) {
		if(val == (pos->second)) {
#ifdef _DEBUG_QUEUE_
		    //std::cout << "A:  time(" << val->getStartTime() << ")   label(" << val->getLabel() << ")" << std::endl;
		    //std::cout << "B:  time(" << pos->second->getStartTime() << ")   label(" << pos->second->getLabel() << ")" << std::endl;
#endif
		    //A second equality test is required here to double check that the val==*pos test above
		    //is still accurate.  "Still accurate" in this context means that there is a possibility
		    //that the original Task has been removed and a new task has been placed at the same
		    //memory location.
		    if(val->equal(pos->second)) {
#ifdef _DEBUG_QUEUE_
			std::cout << "Removing " << val << " from the queue.  LABEL(" << val->getLabel() << ")" << std::endl;
#endif
			queue.erase(pos);
			res = 1;
			break;
		    }
		}
	    }
				
	    mutex.unlock();
				
	    return res;
	}
			
#ifdef _AIME_WIN32_
	CIter erase(CIter iter)
	{
	    return queue.erase(iter);
	}
#else
	void erase(CIter iter)
	{
	    queue.erase(iter);
	}	
#endif
 
	int reaper(const KEY time, bool callbacks)
	{
	    int res = 0;
				
	    mutex.lock();
				
	    CIter pos;
	    for(pos = queue.begin(); pos != queue.end(); ) {
		if(pos->first < time) {
		    if(!pos->second->isCallback() || callbacks) {
#ifdef _DEBUG_QUEUE_
			std::cout << "Removing " << pos->second << " from the queue. REAPED" << std::endl;
#endif						
			delete pos->second;
			//////////////////////////////////////
			queue.erase(pos++);
			res = 1;							
		    }else{
			++pos;
		    }							
		} else {
		    break;
		}
	    }
				
	    mutex.unlock();
				
	    return res;
	}
			
	void clear()
	{
	    CIter i = queue.begin();
	    while(i != queue.end()) {
		delete i->second;
		++i;
	    }
	    queue.clear();
	}
			
	bool empty()
	{
	    return queue.empty();
	}
			
	int size()
	{
	    return queue.size();
	}
			
	// if you want the locks you'll need mutex recursion!
	// you can set this in EXTMutex if you want it!
	T get()
	{
	    //mutex.lock();
				
	    if(queue.size() == 0) {
		//mutex.unlock();
		return NULL;
	    }
#ifdef _DEBUG_QUEUE_
	    std::cout << "Getting Task From Queue.   Queue Size: " << queue.size() << std::endl;
#endif
	    CIter i = queue.begin();
	    T element = i->second;
	    queue.erase(i);
				
	    //mutex.unlock();
				
	    return element;
	}
			
	// if you want the locks you'll need mutex recursion!
	// you can set this in EXTMutex if you want it!
	T peek()
	{
	    //mutex.lock();
				
	    if(queue.size() == 0) {
		//mutex.unlock();
		return NULL; //check for empty container
	    }
				
	    CIter i = queue.begin();
	    T element = i->second;
				
	    //mutex.unlock();
				
	    return element;
	}
			
    private:
	CONTAINER queue;
	EXTMutex mutex;
    };
	
} //End Namespace
#endif
