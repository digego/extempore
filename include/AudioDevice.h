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

#if defined(__APPLE__)
#include <CoreAudio/AudioHardware.h>
#endif

#include <portaudio.h>

#include <cstdint>

#include <array>
#include <atomic>
#include <memory>
#include <mutex>
#include <span>
#include <utility>
#include <string>
#include <vector>
#include "UNIV.h"
#include "EXTThread.h"

typedef float SAMPLE;

typedef SAMPLE (*dsp_f_ptr)(void*, void*, SAMPLE, uint64_t, uint64_t, const SAMPLE*);
typedef SAMPLE (*dsp_f_ptr_sum)(void*, void*, SAMPLE*, uint64_t, uint64_t, const SAMPLE*);

typedef SAMPLE (*closure_fn_type)(SAMPLE, uint64_t, uint64_t, const SAMPLE*);

namespace extemp {

class AudioDevice {
  public:
    typedef void* (*closure_getter_fn_type)();

    static const unsigned MAX_RT_AUDIO_THREADS = 16;

    // Everything the audio callback needs to know about the currently
    // installed DSP, as one immutable value. dsp:set! builds these up in
    // stages (wrapper, then closure, then the per-thread closures, then the
    // worker threads), and the audio thread must never observe a half-applied
    // change, so each setter publishes a fresh copy and the callback reads the
    // pointer exactly once per block.
    struct DspState {
        closure_getter_fn_type closure = nullptr;
        dsp_f_ptr wrapper = nullptr;
        dsp_f_ptr_sum sumWrapper = nullptr;
        bool zeroLatency = true;
        // True only once the MT buffers, thread count and worker threads are
        // all in place; gates the multi-threaded branch of processFrames().
        bool mtReady = false;
        unsigned numThreads = 0;
        std::array<closure_getter_fn_type, MAX_RT_AUDIO_THREADS> mtClosures = {};
    };

    AudioDevice();
    ~AudioDevice();

    // start and stop audio processing (which also stops time!!)
    void start();
    void stop();

    // Core DSP dispatch: called by the PortAudio callback in realtime mode,
    // and by the offline file driver when --audio-outfile is active. Advances
    // UNIV::DEVICE_TIME/TIME, signals the task scheduler, and invokes the
    // registered DSP wrapper (sample-by-sample or MT-sum).
    void processFrames(const float* InputBuffer, float* OutputBuffer, uint64_t FramesPerBuffer,
                       void* UserData);

    // The published DSP state. Never null.
    const DspState* getDspState() const {
        return m_dspState.load(std::memory_order_acquire);
    }

    void setDSPClosure(void* Function) {
        publishDspState([&](DspState& State) {
            State.closure = reinterpret_cast<closure_getter_fn_type>(Function);
        });
    }
    // Index must be below MAX_RT_AUDIO_THREADS; out-of-range indices are
    // rejected here and at the foreign-function boundary, which raises a
    // Scheme error rather than writing past the table.
    bool setDSPMTClosure(void* Function, int Index) {
        if (Index < 0 || unsigned(Index) >= MAX_RT_AUDIO_THREADS) {
            return false;
        }
        publishDspState([&](DspState& State) {
            State.mtClosures[unsigned(Index)] =
                reinterpret_cast<closure_getter_fn_type>(Function);
        });
        return true;
    }
    void setDSPWrapper(dsp_f_ptr Wrapper) {
        publishDspState([&](DspState& State) {
            if (wrapperSet(State)) {
                return;
            }
            State.wrapper = Wrapper;
        });
    }
    void setDSPMTWrapper(dsp_f_ptr_sum WrapperSum, dsp_f_ptr Wrapper) {
        publishDspState([&](DspState& State) {
            if (wrapperSet(State)) {
                return;
            }
            State.sumWrapper = WrapperSum;
            State.wrapper = Wrapper;
        });
    }

    // Bring up the multi-threaded audio workers. Idempotent: calling it again
    // with the same shape is a no-op (a second dsp:set! must not replace live
    // threads or reallocate the buffers they are reading), and a different
    // thread count or latency mode is refused. Returns false with Reason set
    // when it refuses.
    bool initMTAudio(int NumThreads, bool ZeroLatency, std::string& Reason);

    // The double-buffered output region belonging to worker Index, and the
    // shared input buffer. Empty until initMTAudio() has run.
    std::span<SAMPLE> getMTOutSlice(unsigned Index);
    std::span<SAMPLE> getMTInBuffer() {
        return std::span<SAMPLE>(m_inbuf);
    }

    // Flip and return the buffer-selection toggle. Called from the audio
    // thread only, but published state is read elsewhere, so keep it atomic.
    bool getToggle() {
        return (m_toggle.fetch_xor(1, std::memory_order_relaxed) ^ 1) != 0;
    }

    PaStream* getPaStream() {
        return stream;
    }

    static AudioDevice* I() {
        return &SINGLETON;
    }

    static double getCPULoad();
    static void printDevices();

    // Under/overflow counts reported by PortAudio. Incremented in the audio
    // callback (which must not do I/O), read from the Scheme thread via
    // sys:audio-xruns. Returns {underflows, overflows}.
    std::pair<uint64_t, uint64_t> getXrunCounts() const {
        return {m_underflows.load(std::memory_order_relaxed),
                m_overflows.load(std::memory_order_relaxed)};
    }
    void countUnderflow() {
        m_underflows.fetch_add(1, std::memory_order_relaxed);
    }
    void countOverflow() {
        m_overflows.fetch_add(1, std::memory_order_relaxed);
    }

    // FileAudioDriver hooks (offline --audio-outfile mode). These are no-ops
    // unless the driver has been started via start() with UNIV::AUDIO_OUTFILE_PATH
    // set. stopFileDriver() finalizes the WAV header; must be called before
    // any std::_Exit() that would otherwise skip destructors.
    static bool fileDriverRunning();
    static void stopFileDriver();

    static std::atomic<double> CLOCKBASE;
    static std::atomic<double> REALTIME;
    static double CLOCKOFFSET;

  private:
    static bool wrapperSet(const DspState& State) {
        return State.wrapper || State.sumWrapper;
    }

    // Publish a modified copy of the current state. Retired states are never
    // freed: the audio thread may still be reading one, and there is no point
    // at which reclaiming it is provably safe. A dsp:set! is a human-scale
    // event, so the handful of small structs a session retires costs nothing.
    template <class Mutation>
    void publishDspState(Mutation&& Mutate) {
        std::lock_guard<std::mutex> lock(m_dspPublishMutex);
        auto* next = new DspState(*m_dspState.load(std::memory_order_relaxed));
        Mutate(*next);
        m_dspState.store(next, std::memory_order_release);
    }

    bool m_started;
    PaStream* stream;
    std::atomic<const DspState*> m_dspState;
    std::mutex m_dspPublishMutex;
    std::vector<SAMPLE> m_outbuf;
    std::vector<SAMPLE> m_inbuf;
    std::array<std::unique_ptr<EXTThread>, MAX_RT_AUDIO_THREADS> m_threads;
    std::atomic<unsigned char> m_toggle;
    std::atomic<uint64_t> m_underflows;
    std::atomic<uint64_t> m_overflows;

    static AudioDevice SINGLETON;
};

}  // namespace extemp
#endif
