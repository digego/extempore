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
#include "EXTThread.h"

typedef float SAMPLE;

typedef void (*dsp_f_ptr_array)(void*, void*, float*, float*, uint64_t, void*);
typedef void (*dsp_f_ptr_sum_array)(void* ,void* ,float** ,float*, uint64_t, void*);
typedef SAMPLE (*dsp_f_ptr)(void*, void*, SAMPLE, uint64_t, uint64_t, const SAMPLE*);
typedef SAMPLE (*dsp_f_ptr_sum)(void*, void*, SAMPLE*, uint64_t, uint64_t, const SAMPLE*);

typedef SAMPLE (*closure_fn_type)(SAMPLE, uint64_t, uint64_t, const SAMPLE*);

namespace extemp
{

class AudioDevice
{
private:
    typedef void* (*closure_getter_fn_type)();
public:
    static const unsigned MAX_RT_AUDIO_THREADS = 16;
private:
    bool m_started;
    PaStream* stream;
    float* buffer;
    closure_getter_fn_type m_dsp_closure;
    closure_getter_fn_type m_dsp_mt_closure[128];
    dsp_f_ptr dsp_wrapper;
    dsp_f_ptr_sum dsp_wrapper_sum;
    dsp_f_ptr_array dsp_wrapper_array;
    dsp_f_ptr_sum_array dsp_wrapper_sum_array;
    SAMPLE* outbuf;
    SAMPLE* inbuf;
    float* outbuf_f;
    float* inbuf_f;
    EXTThread* m_threads[MAX_RT_AUDIO_THREADS];
    unsigned   m_numThreads;
    bool       m_zeroLatency;
    bool       m_toggle;

    static AudioDevice SINGLETON;
private:
    bool WrapperSet() const {
        return dsp_wrapper || dsp_wrapper_array || dsp_wrapper_sum || dsp_wrapper_sum_array;
    }
public:
    AudioDevice();
    ~AudioDevice();

    // start and stop audio processing (which also stops time!!)
    void start();
    void stop();

    bool getZeroLatency() { return m_zeroLatency; }
    void setZeroLatency(bool Val) { m_zeroLatency = Val; }
    bool getToggle() {
        m_toggle = !m_toggle;
        return m_toggle;
    }

    void setDSPClosure(void* Function) {
        if (m_dsp_closure) {
            printf("You can only set the DSP callback once, but you\ncan re-define that function as often as you like\n");
            return;
        }
        m_dsp_closure = reinterpret_cast<closure_getter_fn_type>(Function);
    }
    closure_getter_fn_type getDSPClosure() { return m_dsp_closure; }
    void setDSPMTClosure(void* Function, int Index) {
        if (m_dsp_mt_closure[Index]) {
            printf("You can only set the DSP callback once, but you\ncan re-define that function as often as you like\n");
            return;
        }
        m_dsp_mt_closure[Index] = reinterpret_cast<closure_getter_fn_type>(Function);
    }
    closure_getter_fn_type getDSPMTClosure(int Index) { return m_dsp_mt_closure[Index]; }

    void setDSPWrapper(dsp_f_ptr Wrapper) {
        if (WrapperSet()) {
            return;
        }
        dsp_wrapper = Wrapper;
    }
    void setDSPWrapperArray(dsp_f_ptr_array Wrapper) {
        if (WrapperSet()) {
            return;
        }
        dsp_wrapper_array = Wrapper;
    }
    void setDSPMTWrapper(dsp_f_ptr_sum WrapperSum, dsp_f_ptr Wrapper) {
        if (WrapperSet()) {
            return;
        }
        dsp_wrapper_sum = WrapperSum;
        dsp_wrapper = Wrapper;
    }
    void setDSPMTWrapperArray(dsp_f_ptr_sum_array WrapperSumArray, dsp_f_ptr_array WrapperArray) {
        if (WrapperSet()) {
            return;
        }
        dsp_wrapper_sum_array = WrapperSumArray;
        dsp_wrapper_array = WrapperArray;
    }

    void initMTAudio(int NumThreads, bool ZeroLatency);
    void initMTAudioBuf(int,bool);

    EXTThread** getMTThreads() { return m_threads; }
    int getNumThreads() { return m_numThreads; }
    dsp_f_ptr getDSPWrapper() { return dsp_wrapper; }
    dsp_f_ptr_array getDSPWrapperArray() { return dsp_wrapper_array; }
    dsp_f_ptr_sum getDSPSUMWrapper() { return dsp_wrapper_sum; }
    dsp_f_ptr_sum_array getDSPSUMWrapperArray() { return dsp_wrapper_sum_array; }

    SAMPLE* getDSPMTInBuffer() { return inbuf; }
    SAMPLE* getDSPMTOutBuffer() { return outbuf; }
    float* getDSPMTInBufferArray() { return inbuf_f; }
    float* getDSPMTOutBufferArray() { return outbuf_f; }

    PaStream* getPaStream() { return stream; }

    static AudioDevice* I() { return &SINGLETON; }

    static double getCPULoad();
    static void printDevices();

    static double CLOCKBASE;
    static double REALTIME;
    static double CLOCKOFFSET;
};

} //End Namespace
#endif
