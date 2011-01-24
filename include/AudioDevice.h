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

#ifndef _AUDIO_DEVICE_H
#define _AUDIO_DEVICE_H

#ifndef TARGET_OS_LINUX
#include <CoreAudio/AudioHardware.h>
#else
#include <portaudio.h>
#endif

#include <stdint.h>

#include <vector>
#include "UNIV.h"

#define SAMPLE float

typedef double(*dsp_f_ptr)(void*,void*,double,double,double,double*);

namespace extemp {

class AudioDevice {
		
public:
	AudioDevice();
	~AudioDevice();
	static AudioDevice* I() { return &SINGLETON; }
	
	// start and stop audio processing (which also stops time!!)	
	void start();
	void stop();
	
	void* setDSPClosure(void* _dsp_func) { dsp_closure = _dsp_func; }
	void* getDSPClosure() { return dsp_closure; }
	void setDSPWrapper( double(*_wrapper)(void*,void*,double,double,double,double*) ) { dsp_wrapper = _wrapper; }
	dsp_f_ptr getDSPWrapper() { return dsp_wrapper; }

private:
	bool started;
#ifdef TARGET_OS_LINUX
	PaStream* stream;
#else 
    AudioDeviceID device;
#endif
    float* buffer;
	void* dsp_closure;
	dsp_f_ptr dsp_wrapper;
	
	static AudioDevice SINGLETON;
};

} //End Namespace
#endif
