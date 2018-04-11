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

#include <time.h>
#include <iostream>
#include <string.h>
#include <inttypes.h>
#include <xmmintrin.h>

#include "AudioDevice.h"
#include "TaskScheduler.h"
#include "EXTMonitor.h"
#include "EXTLLVM.h"
#include "SchemeFFI.h"
#include "BranchPrediction.h"

#ifdef _WIN32
#include <Windows.h>
#endif

#ifdef __APPLE__
#include <CoreAudio/HostTime.h>
#include <mach/mach_init.h>
#include <mach/task_policy.h>
#include <mach/thread_act.h>
#include <mach/thread_policy.h>
#include <sys/sysctl.h>
#include <time.h>
#include <libkern/OSAtomic.h>
#endif

#include <stdlib.h>
#include <math.h>
#include <atomic>

// this is an aribrary maximum

#ifdef EXT_BOOST
#include <thread>
#endif

// this functionality is duplicated in EXTThread::setPriority(), but
// kep here to not mess with the MT audio stuff
#ifdef __APPLE__
int set_thread_realtime(thread_port_t threadport, float period, float computation, float constraint) {
  struct thread_time_constraint_policy ttcpolicy;
  int ret;

  ttcpolicy.period=period; // HZ/160
  ttcpolicy.computation=computation; // HZ/3300;
  ttcpolicy.constraint=constraint; // HZ/2200;
  ttcpolicy.preemptible=1; // 1

  if ((ret=thread_policy_set(threadport,
                             THREAD_TIME_CONSTRAINT_POLICY, (thread_policy_t)&ttcpolicy,
                             THREAD_TIME_CONSTRAINT_POLICY_COUNT)) != KERN_SUCCESS) {
    fprintf(stderr, "set_thread_realtime() failed.\n");
    return 0;
  }
  return 1;
}
#elif __linux__
// the arguments here on linux are a bit different to OSX, since the
// semantics of a thread's "priority" are different
int set_thread_realtime(pthread_t thread, int policy, int priority) {
    int current_policy; // currently we ignore this result
    sched_param param;
    pthread_getschedparam(thread, &current_policy, &param);
    param.sched_priority = priority;
    // policy should be SCHED_RR or SCHED_FIFO
    int res = pthread_setschedparam(thread, policy, &param);
    if (res) {
        printf("Failed to set realtime priority for Audio thread: %s\n", strerror(res));
    }
    return 1;
}
#endif

#ifdef _WIN32
#define isnan(x) ((x) != (x))
#define isinf(x) (isnan(x-x))
#endif

#if !defined(__clang__) && !defined(_WIN32)
#undef isinf
#undef isfinite
#undef isnan
#define isinf(x) __builtin_isinf(x)
#define isnan(x) __builtin_isnan(x)
#define isfinite(x) __builtin_finite(x)
#endif

static inline SAMPLE audio_sanity(SAMPLE x)
{
    if (likely(isfinite(x))) {
        if (unlikely(x < -0.99f)) return -0.99f;
        if (unlikely(x > 0.99f)) return 0.99f;
        return x;
    }
    return 0.0;
}

static inline float audio_sanity_f(float x)
{
    if (likely(isfinite(x))) {
        _mm_store_ss(&x, _mm_min_ss(_mm_max_ss(_mm_set_ss(x), _mm_set_ss(-0.99f)), _mm_set_ss(0.99f)));
        return x;
    }
    return 0.0;
}

// double audio_sanity_d(double x)
// {
//   if(isinf(x)) return 0.0f;
//   else if(isnan(x)) return 0.0f;
//   else if(x < -0.99f) return -0.99f;
//   else if(x > 0.99f) return 0.99f;
//   else return x;
// }

namespace extemp
{

AudioDevice AudioDevice::SINGLETON;

double AudioDevice::REALTIME = 0.0;
double AudioDevice::CLOCKBASE = 0.0;
double AudioDevice::CLOCKOFFSET = 0.0;
bool first_callback = true;
uint64_t start_time = 0;

  //-----------------------------------
  //  PORT AUDIO
  //-----------------------------------
#define NANO_SLEEP_DURATION 100000

static std::atomic_int sThreadDoneCount;
static std::atomic_int_fast64_t sSignalCount;

#ifndef _WIN32
static struct timespec MT_SLEEP_DURATION = { 0, NANO_SLEEP_DURATION };
#else
static LONGLONG MT_SLEEP_DURATION = NANO_SLEEP_DURATION;

static void nanosleep(LONGLONG* Ns, void*)
{
    auto timer(CreateWaitableTimer(NULL, TRUE, NULL));
    if (!timer) {
        return;
    }
    LARGE_INTEGER li;
    li.QuadPart = -*Ns / 100;
    if (!SetWaitableTimer(timer, &li, 0, NULL, NULL, FALSE)) {
        CloseHandle(timer);
        return;
    }
    WaitForSingleObject(timer, INFINITE);
    CloseHandle(timer);
}
#endif

void* audioCallbackMT(void* Args)
{
#ifdef __APPLE__
    Float64 clockFrequency = AudioGetHostClockFrequency();
    //set_thread_realtime(pthread_mach_thread_np(pthread_self()), sclockFrequency*.01,clockFrequency*.005,clockFrequency*.005);
    set_thread_realtime(pthread_mach_thread_np(pthread_self()), clockFrequency*.01,clockFrequency*.007,clockFrequency*.007);
#elif __linux__
    set_thread_realtime(pthread_self(), SCHED_RR, 20);
#elif _WIN32 
    SetThreadPriority(GetCurrentThread(), 15); // 15 = THREAD_PRIORITY_TIME_CRITICAL
#endif
    //printf("Starting RT Audio Process\n");
    unsigned idx = uintptr_t(Args);
    auto cache_wrapper(AudioDevice::I()->getDSPWrapper());
    auto zone(extemp::EXTLLVM::llvm_peek_zone_stack());
    SAMPLE* outbuf = AudioDevice::I()->getDSPMTOutBuffer();
    SAMPLE* outbufs[2];
    outbufs[0] = outbuf + UNIV::CHANNELS * UNIV::NUM_FRAMES * idx * 2;
    outbufs[1] = outbufs[0] + UNIV::CHANNELS * UNIV::NUM_FRAMES;
    SAMPLE* inbuf = AudioDevice::I()->getDSPMTInBuffer();
    SAMPLE* indata = (SAMPLE*) malloc(UNIV::IN_CHANNELS * 8);
    bool zerolatency = AudioDevice::I()->getZeroLatency();
    bool toggle = false;
    int64_t lcount = 0; // local count
    printf("Starting RT Audio process with SIG CNT: %" PRId64 "\n",int64_t(sSignalCount));
    while (true) {
        outbuf = outbufs[toggle];
        if (unlikely(!zerolatency)) {
            toggle = !toggle;
        }
        auto cache_closure(AudioDevice::I()->getDSPMTClosure(idx)());
        auto closure = *reinterpret_cast<closure_fn_type*>(cache_closure);
        int cnt = 0;
        while(sSignalCount <= lcount) { // wait);
            nanosleep(&MT_SLEEP_DURATION, nullptr);
            cnt++;
            if (!(cnt%100000)) {
                printf("Still locked in %d cnt(%" PRId64 ":%" PRId64 ")\n!",idx,lcount,int64_t(sSignalCount));
            }
        } // spin
        ++lcount;
        uint64_t LTIME = UNIV::DEVICE_TIME;
        for (uint32_t i=0;i<UNIV::NUM_FRAMES;i++) {
            uint32_t iout = i*UNIV::CHANNELS;
            uint32_t iin = i*UNIV::IN_CHANNELS;
            for (unsigned k=0;k<UNIV::IN_CHANNELS;k++) {
                indata[k] = (SAMPLE)inbuf[iin+k];
            }
            if (UNIV::IN_CHANNELS==UNIV::CHANNELS) {
                for(uint64_t k=0; k<UNIV::CHANNELS; k++) {
                    outbuf[iout+k] = audio_sanity(cache_wrapper(zone, (void*)closure, (SAMPLE)inbuf[iin+k], (i+LTIME),k,&(indata[0])));
                    extemp::EXTLLVM::llvm_zone_reset(zone);
                }
            } else if (UNIV::IN_CHANNELS==1) {
                for(uint64_t k=0; k<UNIV::CHANNELS; k++) {
                    outbuf[iout+k] = audio_sanity(cache_wrapper(zone, (void*)closure, (SAMPLE)inbuf[iin], (i+LTIME),k,&(indata[0])));
                    extemp::EXTLLVM::llvm_zone_reset(zone);
                }
            } else {
                for(uint64_t k=0; k<UNIV::CHANNELS; k++) {
                    outbuf[iout+k] = audio_sanity(cache_wrapper(zone, (void*)closure, 0.0, (i+LTIME),k,&(indata[0])));
                    extemp::EXTLLVM::llvm_zone_reset(zone);
                }
            }
        }
        ++sThreadDoneCount;
    }
    return 0;
}

  // buffered version of MT audio callback
void* audioCallbackMTBuf(void* dat) {
#ifdef __APPLE__
    Float64 clockFrequency = AudioGetHostClockFrequency();
    //set_thread_realtime(pthread_mach_thread_np(pthread_self()), clockFrequency*.01,clockFrequency*.005,clockFrequency*.005);
    set_thread_realtime(pthread_mach_thread_np(pthread_self()), clockFrequency*.01,clockFrequency*.007,clockFrequency*.007);
#elif __linux__
    set_thread_realtime(pthread_self(), SCHED_RR, 20);
#elif _WIN32 
    SetThreadPriority(GetCurrentThread(),15); // 15 = THREAD_PRIORITY_TIME_CRITICAL
#endif
    unsigned idx = uintptr_t(dat);
    int64_t lcount = 0; // local count

    dsp_f_ptr_array dsp_wrapper_array = AudioDevice::I()->getDSPWrapperArray();
    dsp_f_ptr_array cache_wrapper = dsp_wrapper_array;
    llvm_zone_t* zone = extemp::EXTLLVM::llvm_peek_zone_stack();
    float* outbuf = AudioDevice::I()->getDSPMTOutBufferArray();
    outbuf = outbuf+(UNIV::CHANNELS*UNIV::NUM_FRAMES*idx);
    float* inbuf = AudioDevice::I()->getDSPMTInBufferArray();

    printf("Starting RT Audio process with SIG CNT: %" PRId64 "\n",int64_t(sSignalCount));

// TODO: USED FOR????    float* indata = (float*) malloc(UNIV::IN_CHANNELS*4);
    while (true) {
      auto cache_closure(AudioDevice::I()->getDSPMTClosure(idx)());
      auto closure = *((void(**)(float*,float*,uint64_t,void*)) cache_closure);
      int cnt = 0;
      while (sSignalCount <= lcount) { // wait
        nanosleep(&MT_SLEEP_DURATION, NULL);
        cnt++;
        if (!(cnt%100000)) {
            printf("Still locked in %d cnt(%" PRId64 ":%" PRId64 ")\n!",idx,lcount, int64_t(sSignalCount));
        }
      } // spin
      lcount++;
      cache_wrapper(zone, reinterpret_cast<void*>(closure), inbuf, outbuf, UNIV::DEVICE_TIME, NULL);
      extemp::EXTLLVM::llvm_zone_reset(zone);
      ++sThreadDoneCount;
    }
    return 0;
}

int audioCallback(const void* InputBuffer, void* OutputBuffer, unsigned long FramesPerBuffer,
        const PaStreamCallbackTimeInfo* TimeInfo, PaStreamCallbackFlags StatusFlags, void* UserData)
{
    auto sched(reinterpret_cast<TaskScheduler*>(UserData));
    UNIV::DEVICE_TIME += FramesPerBuffer;
    if (likely(UNIV::TIME_DIVISION == 1)) {
        UNIV::TIME = UNIV::DEVICE_TIME;
    }
    if (unlikely(AudioDevice::CLOCKBASE < 1.0)) {
        AudioDevice::CLOCKBASE = getRealTime();
        UNIV::AUDIO_CLOCK_BASE = AudioDevice::CLOCKBASE;
    }
    AudioDevice::REALTIME = getRealTime();
    UNIV::AUDIO_CLOCK_NOW = AudioDevice::REALTIME;
    sched->setFrames(FramesPerBuffer);
    sched->getGuard().signal();
    auto dsp_closure(AudioDevice::I()->getDSPClosure());
    if (unlikely(!dsp_closure)) {
        memset(OutputBuffer, 0, UNIV::CHANNELS * FramesPerBuffer * sizeof(float));
        return paContinue;
    }
    auto cache_closure(dsp_closure());
    if (unlikely(StatusFlags & (paOutputUnderflow | paOutputOverflow))) {
        if (StatusFlags & paOutputUnderflow) {
            printf("Audio underflow: are you pushing extempore too hard?\n");
        }
        if (StatusFlags & paOutputOverflow) {
            printf("Audio output overflow\n");
        }
    }
    if (likely(AudioDevice::I()->getDSPWrapper() && !AudioDevice::I()->getDSPSUMWrapper())) { // sample by sample
        auto dsp_wrapper(AudioDevice::I()->getDSPWrapper());
        auto cache_wrapper(dsp_wrapper);
        auto closure = *((SAMPLE(**)(SAMPLE, uint64_t, uint64_t,SAMPLE*)) cache_closure);
        llvm_zone_t* zone = extemp::EXTLLVM::llvm_peek_zone_stack();
        auto dat(reinterpret_cast<float*>(OutputBuffer));
        auto in(reinterpret_cast<const float*>(InputBuffer));
        auto time(UNIV::DEVICE_TIME);
        if (likely(!UNIV::IN_CHANNELS)) {
            float dummy(0.0);
            for (uint64_t i = 0; i < FramesPerBuffer; ++i, ++time) {
                for (uint64_t k = 0; k < UNIV::CHANNELS; ++k) {
                    *(dat++) = audio_sanity_f(float(cache_wrapper(zone, reinterpret_cast<void*>(closure), 0.0, time, k,
                            &dummy)));
                    extemp::EXTLLVM::llvm_zone_reset(zone);
                }
            }
        } else if (UNIV::IN_CHANNELS == UNIV::CHANNELS) {
            for (uint64_t i = 0; i < FramesPerBuffer; ++i, ++time) {
                auto indata(in);
                for (uint64_t k = 0; k < UNIV::CHANNELS; ++k) {
                    *(dat++) = audio_sanity_f(float(cache_wrapper(zone, reinterpret_cast<void*>(closure), *(in++),
                            time, k, indata)));
                    extemp::EXTLLVM::llvm_zone_reset(zone);
                }
            }
        } else if (UNIV::IN_CHANNELS == 1) {
            for (uint64_t i = 0; i < FramesPerBuffer; ++i, ++time) {
                for (uint64_t k = 0; k < UNIV::CHANNELS; k++) {
                    *(dat++) = audio_sanity_f(float(cache_wrapper(zone, reinterpret_cast<void*>(closure), *in,
                            time, k, in)));
                    extemp::EXTLLVM::llvm_zone_reset(zone);
                }
                ++in;
            } 
        } else { // for when in channels & out channels don't match
          //SAMPLE* indata = alloc(UNIV::IN_CHANNELS); // auto
          //indata(in);
          auto indata(in);
          for (uint64_t i = 0; i < FramesPerBuffer; ++i, ++time) {
                for (uint64_t k = 0; k < UNIV::CHANNELS; ++k) {
                    *(dat++) = audio_sanity_f(float(cache_wrapper(zone, reinterpret_cast<void*>(closure), 0.0,
                                                                  time, k, &indata[i*UNIV::IN_CHANNELS])));
                    extemp::EXTLLVM::llvm_zone_reset(zone);
                }
            }
        }
        return 0;
    }
    if (AudioDevice::I()->getDSPWrapperArray() && !AudioDevice::I()->getDSPSUMWrapperArray()) { // if true then we must be buffer by buffer
        dsp_f_ptr_array cache_wrapper = AudioDevice::I()->getDSPWrapperArray();
        auto closure = *((void(**)(float*,float*,uint64_t,void*)) cache_closure);
        llvm_zone_t* zone = extemp::EXTLLVM::llvm_peek_zone_stack();
        float* indat = (float*) InputBuffer;
        float* outdat = (float*) OutputBuffer;
        cache_wrapper(zone, (void*)closure, indat, outdat, UNIV::DEVICE_TIME, UserData);
        extemp::EXTLLVM::llvm_zone_reset(zone);
    } else if (AudioDevice::I()->getDSPSUMWrapper()) { // if true then multi threaded sample-by-sample
        int numthreads = AudioDevice::I()->getNumThreads();
        bool zerolatency = AudioDevice::I()->getZeroLatency();
        SAMPLE in[32];
        SAMPLE* inb = AudioDevice::I()->getDSPMTInBuffer();
        float* input = (float*) InputBuffer;
        for(unsigned i=0;i<UNIV::IN_CHANNELS*UNIV::NUM_FRAMES;i++) inb[i] = (SAMPLE) input[i];
        sThreadDoneCount = 0;
        if (zerolatency) {
            ++sSignalCount;
            int cnt = 0;
            while (sThreadDoneCount != numthreads) {
                nanosleep(&MT_SLEEP_DURATION ,NULL);
                ++cnt;
                if (!(cnt % 100000)) {
                    printf("Locked with threads:%d of %d cnt(%" PRId64 ")!\n", sThreadDoneCount.load(), numthreads,
                            sSignalCount.load());
                    if(sThreadDoneCount > numthreads) printf("in MT Audio sThreadDoneCount should never be greater than numthreads! - this is a race :(");
                }
                numthreads = AudioDevice::I()->getNumThreads();
            }
            sThreadDoneCount = 0;
        }
        //printf("process audio sum ...\n");
        dsp_f_ptr_sum dsp_wrapper = AudioDevice::I()->getDSPSUMWrapper();
        dsp_f_ptr_sum cache_wrapper = dsp_wrapper;
        auto closure = * ((SAMPLE(**)(SAMPLE*,uint64_t,uint64_t,SAMPLE*)) cache_closure);
        llvm_zone_t* zone = extemp::EXTLLVM::llvm_peek_zone_stack();
        bool toggle = AudioDevice::I()->getToggle();
        SAMPLE* indats[AudioDevice::MAX_RT_AUDIO_THREADS]; // can't be variable on wi
        indats[0] = AudioDevice::I()->getDSPMTOutBuffer();
      // if we are NOT running zerolatency
      // and toggle is FALSE then use alternate buffers
        if (!zerolatency && !toggle) {
            indats[0] = indats[0] + UNIV::NUM_FRAMES*UNIV::CHANNELS;
        }
        for (int jj=1;jj<numthreads;jj++) {
            indats[jj] = indats[0] + (UNIV::NUM_FRAMES*UNIV::CHANNELS*jj*2);
        }
        for(uint64_t i=0;i<UNIV::NUM_FRAMES;i++) {
            uint32_t iout = i*UNIV::CHANNELS;
            float* dat = (float*) OutputBuffer;

        for(uint64_t k=0; k<UNIV::CHANNELS; k++)
          {
            for(int jj=0;jj<numthreads;jj++) {
              in[jj] = indats[jj][iout+k];
            }
            dat[iout+k] = audio_sanity_f((float)cache_wrapper(zone, (void*)closure, in, (i+UNIV::DEVICE_TIME),k,nullptr));
            extemp::EXTLLVM::llvm_zone_reset(zone);
          }
        }
        if (!zerolatency) {
            ++sSignalCount;
            int cnt = 0;
            while (sThreadDoneCount != numthreads) {
                nanosleep(&MT_SLEEP_DURATION ,NULL);
                ++cnt;
                if (!(cnt % 100000)) {
                    printf("Locked with threads:%d of %d cnt(%" PRId64 ")!\n", sThreadDoneCount.load(), numthreads,
                            sSignalCount.load());
                    if(sThreadDoneCount > numthreads) printf("in MT Audio sThreadDoneCount should never be greater than numthreads! - this is a race :(");
                }
                numthreads = AudioDevice::I()->getNumThreads();
            }
            sThreadDoneCount = 0;
        }
    }else if(AudioDevice::I()->getDSPSUMWrapperArray()) { // if true then both MT and buffer based
      int numthreads = AudioDevice::I()->getNumThreads();

      // TODO: UNUSED???      double in[AudioDevice::MAX_RT_AUDIO_THREADS];
      float* inb = AudioDevice::I()->getDSPMTInBufferArray();
      float* input = (float*) InputBuffer;
      for (unsigned i=0;i<UNIV::IN_CHANNELS*UNIV::NUM_FRAMES;i++) inb[i] = input[i];
      // start computing in all audio threads
        sThreadDoneCount = 0;
        ++sSignalCount;
        int cnt = 0;
        while (sThreadDoneCount != numthreads) {
            nanosleep(&MT_SLEEP_DURATION ,NULL);
            ++cnt;
            if (!(cnt % 100000)) {
                printf("Locked with threads:%d of %d cnt(%" PRId64 ")!\n", sThreadDoneCount.load(), numthreads,
                        sSignalCount.load());
                if(sThreadDoneCount > numthreads) printf("in MT Audio sThreadDoneCount should never be greater than numthreads! - this is a race :(");
            }
            numthreads = AudioDevice::I()->getNumThreads();
        }
        sThreadDoneCount = 0;
      dsp_f_ptr_sum_array dsp_wrapper = AudioDevice::I()->getDSPSUMWrapperArray();
      dsp_f_ptr_sum_array cache_wrapper = dsp_wrapper;
      auto closure  = *((void(**)(float**,float*,uint64_t,void*)) cache_closure);
      llvm_zone_t* zone = extemp::EXTLLVM::llvm_peek_zone_stack();
      //float** indat = (float**)
      float* indats[AudioDevice::MAX_RT_AUDIO_THREADS];
      float* outdat = (float*) OutputBuffer;
      indats[0] = AudioDevice::I()->getDSPMTOutBufferArray();
      for(int jj=1;jj<numthreads;jj++) {
        indats[jj] = indats[0]+(UNIV::NUM_FRAMES*UNIV::CHANNELS*jj);
      }
      cache_wrapper(zone, (void*)closure, indats, outdat, UNIV::DEVICE_TIME, UserData);
      extemp::EXTLLVM::llvm_zone_reset(zone);
      //printf("main out\n");
    } else {
        //zero out audiobuffer
        memset(OutputBuffer,0,(UNIV::CHANNELS*UNIV::NUM_FRAMES*sizeof(float)));
        //nothin to do
    }
    return 0;
  }

AudioDevice::AudioDevice(): m_started(false), buffer(0), m_dsp_closure(nullptr), dsp_wrapper(0), dsp_wrapper_array(0), m_numThreads(50) /* NOT 0! */, m_zeroLatency(true)
{
}

AudioDevice::~AudioDevice()
{
    if (!UNIV::AUDIO_NONE) {
        PaError err;
        err = Pa_StopStream(stream);
        if (err != paNoError) {
            std::cout << Pa_GetErrorText(err) << std::endl;
        }
        err = Pa_CloseStream(stream);
        if (err != paNoError) {
            std::cout << Pa_GetErrorText(err) << std::endl;
        }
        err = Pa_Terminate();
        if (err != paNoError) {
            std::cout << Pa_GetErrorText(err) << std::endl;
      }
  }
}

#undef max

static int findDevice(const std::string& Name)
{
    int numDevices(Pa_GetDeviceCount());
    for (unsigned i = 0; i < numDevices; ++i) {
        if (Name == Pa_GetDeviceInfo(i)->name) {
            return i;
        }
    }
    ascii_error();
    printf("\n*** Can't find named device: %s\n", Name.c_str());
    ascii_normal();
    fflush(stdout);
    std::_Exit(1);
}

void AudioDevice::start()
{
    if (m_started) {
        return;
    }
    if (UNIV::AUDIO_NONE) {
        ascii_error();
        fprintf(stderr, "Error: cannot set the audio device in --noaudio mode\n");
        ascii_normal();
        return;
    }
    Pa_Initialize();
    PaError err;
    int numDevices = Pa_GetDeviceCount();
    if (!UNIV::AUDIO_DEVICE_NAME.empty()) {
        UNIV::AUDIO_DEVICE = findDevice(UNIV::AUDIO_DEVICE_NAME);
    }
    if (!UNIV::AUDIO_IN_DEVICE_NAME.empty()) {
        UNIV::AUDIO_IN_DEVICE = findDevice(UNIV::AUDIO_IN_DEVICE_NAME);
    }
    if (numDevices < 0) {
        printf("No audio devices found!\n");
        printf( "ERROR: Pa_CountDevices returned 0x%x\n", numDevices);
        exit(1);
    }
    if (int(UNIV::AUDIO_DEVICE) < -1 || int(UNIV::AUDIO_DEVICE) >= numDevices) {
        ascii_error();
        printf("Output device not valid! %d\n", int(UNIV::AUDIO_DEVICE));
        ascii_normal();
        printf("\n");
        exit(1);
    }
    if (int(UNIV::AUDIO_IN_DEVICE) < -1 || int(UNIV::AUDIO_IN_DEVICE) >= numDevices) {
        ascii_error();
        printf("Input device not valid! %d\n",(int)UNIV::AUDIO_IN_DEVICE);
        ascii_normal();
        printf("\n");
        exit(1);
    }
    if (UNIV::IN_CHANNELS != UNIV::CHANNELS && UNIV::IN_CHANNELS != 1 && UNIV::IN_CHANNELS > 0) {
        ascii_warning();
        printf("Warning: dsp input will be 0.0, use data* for channel data\n");
        ascii_normal();
        printf("\n");
    }
    int inputDevice = Pa_GetDefaultInputDevice();
    const PaDeviceInfo* deviceInfo;
    if (UNIV::AUDIO_DEVICE == unsigned(-1)) {
        UNIV::AUDIO_DEVICE = Pa_GetDefaultOutputDevice();
    }
    PaStreamParameters pain;
    PaStreamParameters paout;
    deviceInfo = Pa_GetDeviceInfo(UNIV::AUDIO_DEVICE);
    pain.device = UNIV::AUDIO_DEVICE;
    if (UNIV::AUDIO_IN_DEVICE != unsigned(-1)) {
        deviceInfo = Pa_GetDeviceInfo(UNIV::AUDIO_IN_DEVICE);
        inputDevice = UNIV::AUDIO_IN_DEVICE;
        pain.device = UNIV::AUDIO_IN_DEVICE;
    }
    pain.channelCount = UNIV::IN_CHANNELS;
    pain.hostApiSpecificStreamInfo = nullptr;
    pain.sampleFormat = paFloat32; //|((UNIV::INTERLEAVED==0) ? 0 : paNonInterleaved);
    pain.suggestedLatency = deviceInfo->defaultLowInputLatency;
    PaStreamParameters* painptr = (UNIV::IN_CHANNELS < 1) ? nullptr : &pain;
    deviceInfo = Pa_GetDeviceInfo(UNIV::AUDIO_DEVICE);
    paout.channelCount= UNIV::CHANNELS;
    paout.device= UNIV::AUDIO_DEVICE;
    paout.sampleFormat = paFloat32; //|((UNIV::INTERLEAVED==0) ? 0 : paNonInterleaved);
    paout.suggestedLatency = std::max(UNIV::AUDIO_OUTPUT_LATENCY, deviceInfo->defaultLowOutputLatency);
    paout.hostApiSpecificStreamInfo = nullptr;
    PaStreamParameters* paoutptr = (UNIV::CHANNELS < 1) ? nullptr : &paout;
    err = Pa_OpenStream(&stream, painptr, paoutptr, UNIV::SAMPLE_RATE, UNIV::NUM_FRAMES, paNoFlag, audioCallback,
            TaskScheduler::I());
    if (err != paNoError) {
        ascii_error();
        std::cerr << "Initialization Error: " << Pa_GetErrorText(err) << std::endl;
        std::cerr << "AudioDevice: " << (Pa_GetDeviceInfo(UNIV::AUDIO_DEVICE))->name << std::endl;
        ascii_normal();
        exit(1);
    }

    err = Pa_StartStream(stream);

    if(err != paNoError) {
        ascii_error();
        std::cout << "ERROR: " << Pa_GetErrorText(err) << std::endl;
        std::cerr << "AudioDevice: " << (Pa_GetDeviceInfo(UNIV::AUDIO_DEVICE))->name << std::endl;
        ascii_normal();
      exit(1);
    }

    m_started = true;

    auto info(Pa_GetStreamInfo(stream));
    ascii_normal();
    std::cout << "Output Device  : " << std::flush;
    ascii_info();
    std::cout << Pa_GetDeviceInfo(UNIV::AUDIO_DEVICE)->name << std::endl;
    ascii_normal();
    std::cout << "Input Device   : " << std::flush;
    ascii_info();
    if (UNIV::AUDIO_IN_DEVICE != unsigned(-1)) {
        std::cout << Pa_GetDeviceInfo(inputDevice)->name << std::endl;
    } else {
        std::cout << std::endl;
    }
    ascii_normal();
    std::cout << "SampleRate     : " << std::flush;
    ascii_info();
    std::cout << UNIV::SAMPLE_RATE << std::endl << std::flush;
    ascii_normal();
    std::cout << "Channels Out   : " << std::flush;
    ascii_info();
    std::cout << UNIV::CHANNELS << std::endl << std::flush;
    ascii_normal();
    std::cout << "Channels In    : " << std::flush;
    ascii_info();
    std::cout << UNIV::IN_CHANNELS << std::endl << std::flush;
    ascii_normal();
    std::cout << "Frames         : " << std::flush;
    ascii_info();
    std::cout << UNIV::NUM_FRAMES << std::endl << std::flush;
    ascii_normal();
    std::cout << "Latency        : " << std::flush;
    ascii_info();
    std::cout << info->outputLatency << std::flush;
    std::cout << " sec" << std::endl << std::flush;
}

void AudioDevice::stop()
{
    if (!m_started) {
        return;
    }
    PaError err = Pa_StopStream(stream);
    if (err != paNoError) {
        std::cout << "PA Error: " << Pa_GetErrorText(err) << std::endl;
    }
    m_started = false;
}

void AudioDevice::initMTAudio(int Num, bool ZeroLatency)
{
    if (unsigned(Num) > MAX_RT_AUDIO_THREADS) {
        printf("HARD CEILING of %d RT AUDIO THREADS .. aborting!\n", MAX_RT_AUDIO_THREADS);
        exit(1);
    }
    //printf("ssignal %" PRId64 "\n!",int64_t(sSignalCount));
    m_numThreads = Num;
    m_zeroLatency = ZeroLatency;
    m_toggle = true;
    inbuf = (SAMPLE*) malloc(UNIV::IN_CHANNELS*UNIV::NUM_FRAMES*sizeof(SAMPLE));
    // outbuf * 2 for double buffering
    outbuf = (SAMPLE*) malloc(UNIV::CHANNELS*UNIV::NUM_FRAMES*sizeof(SAMPLE)*m_numThreads*2);
    memset(outbuf, 0, UNIV::CHANNELS*UNIV::NUM_FRAMES*sizeof(SAMPLE)*m_numThreads*2);
    for (unsigned i = 0; i < m_numThreads; ++i) {
        m_threads[i] = new EXTThread(audioCallbackMT, reinterpret_cast<void*>(uintptr_t(i)),
                std::string("MT_AUD_") + char('A' + i));
        m_threads[i]->start();
    }
}

void AudioDevice::initMTAudioBuf(int Num, bool ZeroLatency)
{
    if (unsigned(Num) > MAX_RT_AUDIO_THREADS) {
        printf("HARD CEILING of %d RT AUDIO THREADS .. aborting!\n", MAX_RT_AUDIO_THREADS);
        exit(1);
    }
    //printf("ssignal %" PRId64 "\n!",int64_t(sSignalCount));
    m_numThreads = Num;
    m_zeroLatency = ZeroLatency;
    inbuf_f = (float*) malloc(UNIV::IN_CHANNELS*UNIV::NUM_FRAMES*4);
    outbuf_f = (float*) malloc(UNIV::CHANNELS*UNIV::NUM_FRAMES*4*m_numThreads);
    for (unsigned i = 0; i < m_numThreads; ++i) {
        m_threads[i] = new EXTThread(audioCallbackMTBuf, reinterpret_cast<void*>(uintptr_t(i)),
            std::string("MT_AUDB_") + char('A' + i));
        m_threads[i]->start();
    }
}

double AudioDevice::getCPULoad()
{
    return Pa_GetStreamCpuLoad(AudioDevice::I()->getPaStream());
}

  void AudioDevice::printDevices() {
    Pa_Initialize();

    int numDevices = Pa_GetDeviceCount();
    if( numDevices <= 0 ) {
          printf("Error: no audio devices found! Exiting...\n");
      // printf("ERROR: Pa_CountDevices returned 0x%x\n", numDevices );
      exit(1);
    }
    ascii_normal();
    printf("\n-----Available Audio Devices-----------------------------\n");
    ascii_default();

    const   PaDeviceInfo *deviceInfo;
    const   PaHostApiInfo* apiInfo;

    for( int i=0; i<numDevices; i++ ) {
      deviceInfo = Pa_GetDeviceInfo( i );
      apiInfo = Pa_GetHostApiInfo(deviceInfo->hostApi);
      printf("audio device[%d]:%s api[%d]:%s inchan[%d] outchan[%d]\n",i,deviceInfo->name,deviceInfo->hostApi,apiInfo->name,deviceInfo->maxInputChannels,deviceInfo->maxOutputChannels);
    }
    ascii_normal();
    printf("----------------------------------------------------------\n\n");
    ascii_default();
#ifdef _WIN32
    Pa_Terminate();
#else
    fflush(stdout);
    freopen("/dev/null","w",stdout); // throttle termination messages
    Pa_Terminate();
    fflush(stdout);
#endif
    return;
  }

} //End Namespace
