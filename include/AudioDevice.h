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

#ifndef _AUDIO_DEVICE_H
#define _AUDIO_DEVICE_H

#if defined (__APPLE__)
#include <CoreAudio/AudioHardware.h>
#endif

#if defined (COREAUDIO) //__APPLE__)
#include <CoreAudio/AudioHardware.h>
#elif defined (ALSA_AUDIO)
#include <alsa/asoundlib.h>
#else
#include <portaudio.h>
#endif

#include <stdint.h>

#include <vector>
#include "UNIV.h"

#define SAMPLE float

typedef void(*dsp_f_ptr_array)(void*,void*,float*,float*,uint64_t,void*);
typedef void(*dsp_f_ptr_sum_array)(void*,void*,float**,float*,uint64_t,void*);
typedef SAMPLE(*dsp_f_ptr)(void*,void*,SAMPLE,uint64_t,uint64_t,SAMPLE*);
typedef SAMPLE(*dsp_f_ptr_sum)(void*,void*,SAMPLE*,uint64_t,uint64_t,SAMPLE*);

namespace extemp {

  class AudioDevice {
		
  public:
    AudioDevice();
    ~AudioDevice();
    static AudioDevice* I() { return &SINGLETON; }

    // start and stop audio processing (which also stops time!!)
    void start();
    void stop();
    static void startNoAudioThread(); // the --noaudio flag
  
    void setDSPClosure(void* _dsp_func)
    {
	    if(dsp_closure != NULL) { printf("You can only set the DSP callback once, but you\ncan re-define that function as often as you like\n"); return; }
	    dsp_closure = _dsp_func;
    }
    void* getDSPClosure() { return dsp_closure; }
    bool getZeroLatency() { return zerolatency; }
    void setZeroLatency(bool z) { zerolatency = z; }
    bool getToggle () { toggle = toggle ? false : true; return toggle; }

    void setDSPMTClosure(void* _dsp_func, int idx)
    {
	    if(dsp_mt_closure[idx] != NULL) { printf("You can only set the DSP callback once, but you\ncan re-define that function as often as you like\n"); return; }
	    dsp_mt_closure[idx] = _dsp_func;
    }
    void* getDSPMTClosure(int idx) { return dsp_mt_closure[idx]; }
	
    void setDSPWrapperArray( void(*_wrapper)(void*,void*,float*,float*,uint64_t,void*) )
    {
	    if(dsp_wrapper != NULL || dsp_wrapper_sum != NULL || dsp_wrapper_array != NULL || dsp_wrapper_sum_array != NULL) return;
	    dsp_wrapper_array = _wrapper; 
    }
    void setDSPWrapper( SAMPLE(*_wrapper)(void*,void*,SAMPLE,uint64_t,uint64_t,SAMPLE*) )
    {
	    if(dsp_wrapper_array != NULL || dsp_wrapper_sum != NULL || dsp_wrapper != NULL || dsp_wrapper_sum_array != NULL) return;
	    dsp_wrapper = _wrapper;
    }
    void setDSPMTWrapper( SAMPLE(*_wrapper)(void*,void*,SAMPLE*,uint64_t,uint64_t,SAMPLE*),
                          SAMPLE(*_wrappera)(void*,void*,SAMPLE,uint64_t,uint64_t,SAMPLE*))
    {
	    if(dsp_wrapper_array != NULL || dsp_wrapper_sum != NULL || dsp_wrapper != NULL || dsp_wrapper_sum_array != NULL) return;
	    dsp_wrapper_sum = _wrapper;
      dsp_wrapper = _wrappera;
    }
    void setDSPMTWrapperArray( void(*_wrapper)(void*,void*,float**,float*,uint64_t,void*),
                               void(*_wrappera)(void*,void*,float*,float*,uint64_t,void*))
    {
	    if(dsp_wrapper_array != NULL || dsp_wrapper_sum != NULL || dsp_wrapper != NULL || dsp_wrapper_sum_array != NULL) return;
	    dsp_wrapper_sum_array = _wrapper;
      dsp_wrapper_array = _wrappera;
    }

    void initMTAudio(int,bool);
    void initMTAudioBuf(int,bool);

    EXTThread** getMTThreads() { return threads; }
    int getNumThreads() { return numthreads; }
    dsp_f_ptr getDSPWrapper() { return dsp_wrapper; }
    dsp_f_ptr_array getDSPWrapperArray() { return dsp_wrapper_array; }
    dsp_f_ptr_sum getDSPSUMWrapper() { return dsp_wrapper_sum; }
    dsp_f_ptr_sum_array getDSPSUMWrapperArray() { return dsp_wrapper_sum_array; }

    SAMPLE* getDSPMTInBuffer() { return inbuf; }
    SAMPLE* getDSPMTOutBuffer() { return outbuf; }
    float* getDSPMTInBufferArray() { return inbuf_f; }
    float* getDSPMTOutBufferArray() { return outbuf_f; }
    //void setDSPMTOutBuffer(double* ob) { outbuf = ob; }
    //void setDSPMTInBuffer(double* ib) { inbuf = ib; }

    PaStream* getPaStream() { return stream; }
    static double getCPULoad();
    static void printDevices();

    static double CLOCKBASE;
    static double REALTIME;
    static double CLOCKOFFSET;
    int thread_idx[128];

  private:
    bool started;
    PaStream* stream;
    float* buffer;
    void* dsp_closure;
    void* dsp_mt_closure[128];
    dsp_f_ptr dsp_wrapper;
    dsp_f_ptr_sum dsp_wrapper_sum;
    dsp_f_ptr_array dsp_wrapper_array;
    dsp_f_ptr_sum_array dsp_wrapper_sum_array;
    SAMPLE* outbuf;
    SAMPLE* inbuf;
    float* outbuf_f;
    float* inbuf_f;
    static AudioDevice SINGLETON;
    EXTThread** threads;
    int numthreads;
    bool zerolatency;
    bool toggle;
  };

} //End Namespace
#endif
