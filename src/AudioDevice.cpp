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
#include <cstring>
#include <cinttypes>
#include <regex>

#include "AudioDevice.h"
#include "TaskScheduler.h"
#include "EXTLLVM.h"
#include "SchemeFFI.h"
#include "BranchPrediction.h"

#include <chrono>
#include <condition_variable>
#include <mutex>
#include <thread>

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

#include <cstdlib>
#include <cmath>
#include <algorithm>
#include <cstdio>
#include <atomic>
#include <vector>

// this is an aribrary maximum

// this functionality is duplicated in EXTThread::setPriority(), but
// kep here to not mess with the MT audio stuff
#ifdef __APPLE__
int set_thread_realtime(thread_port_t threadport, float period, float computation,
                        float constraint) {
    struct thread_time_constraint_policy ttcpolicy;
    int ret;

    ttcpolicy.period = period;            // HZ/160
    ttcpolicy.computation = computation;  // HZ/3300;
    ttcpolicy.constraint = constraint;    // HZ/2200;
    ttcpolicy.preemptible = 1;            // 1

    if ((ret = thread_policy_set(threadport, THREAD_TIME_CONSTRAINT_POLICY,
                                 (thread_policy_t)&ttcpolicy,
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
    int current_policy;  // currently we ignore this result
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

// Reject non-finite samples and clamp the rest to a safe range. The finite
// check runs first so NaN/inf never reach std::clamp; on x86 and ARM, compilers
// lower the clamp to min/max instructions, so no hand-written intrinsics needed.
static inline float audio_sanity(float x) {
    return likely(std::isfinite(x)) ? std::clamp(x, -0.99f, 0.99f) : 0.0f;
}

// double audio_sanity_d(double x)
// {
//   if(isinf(x)) return 0.0f;
//   else if(isnan(x)) return 0.0f;
//   else if(x < -0.99f) return -0.99f;
//   else if(x > 0.99f) return 0.99f;
//   else return x;
// }

namespace extemp {

AudioDevice AudioDevice::SINGLETON;
// AudioDevice* AudioDevice::SINGLETON = nullptr;

std::atomic<double> AudioDevice::REALTIME = 0.0;
std::atomic<double> AudioDevice::CLOCKBASE = 0.0;
double AudioDevice::CLOCKOFFSET = 0.0;
bool first_callback = true;
uint64_t start_time = 0;

//-----------------------------------
//  PORT AUDIO
//-----------------------------------

namespace {

// C++17 stand-in for std::counting_semaphore (which is C++20). Used to
// synchronise the multi-threaded audio dispatcher with its worker threads
// without spinning on atomics. Replaces the old sSignalCount /
// sThreadDoneCount pair that had a TOCTOU race on m_numThreads and could
// leave sThreadDoneCount > numThreads during a thread-count change.
class MtSemaphore {
  public:
    void release(unsigned n) {
        if (n == 0)
            return;
        {
            std::lock_guard<std::mutex> lk(m_mutex);
            m_count += n;
        }
        if (n == 1) {
            m_cv.notify_one();
        } else {
            m_cv.notify_all();
        }
    }
    void acquire() {
        std::unique_lock<std::mutex> lk(m_mutex);
        m_cv.wait(lk, [this] { return m_count > 0; });
        --m_count;
    }

  private:
    std::mutex m_mutex;
    std::condition_variable m_cv;
    unsigned m_count = 0;
};

// dispatcher -> workers
MtSemaphore sMtWorkAvailable;
// workers -> dispatcher
MtSemaphore sMtWorkComplete;

}  // anonymous namespace

void* audioCallbackMT(void* Args) {
#ifdef __APPLE__
    Float64 clockFrequency = AudioGetHostClockFrequency();
    // set_thread_realtime(pthread_mach_thread_np(pthread_self()),
    // sclockFrequency*.01,clockFrequency*.005,clockFrequency*.005);
    set_thread_realtime(pthread_mach_thread_np(pthread_self()), clockFrequency * .01,
                        clockFrequency * .007, clockFrequency * .007);
#elif __linux__
    set_thread_realtime(pthread_self(), SCHED_RR, 20);
#elif _WIN32
    SetThreadPriority(GetCurrentThread(), 15);  // 15 = THREAD_PRIORITY_TIME_CRITICAL
#endif
    // printf("Starting RT Audio Process\n");
    unsigned idx = uintptr_t(Args);
    auto cache_wrapper(AudioDevice::I()->getDSPWrapper());
    auto zone(extemp::EXTZones::llvm_peek_zone_stack());
    SAMPLE* outbuf = AudioDevice::I()->getDSPMTOutBuffer();
    SAMPLE* outbufs[2];
    outbufs[0] = outbuf + UNIV::CHANNELS * UNIV::NUM_FRAMES * idx * 2;
    outbufs[1] = outbufs[0] + UNIV::CHANNELS * UNIV::NUM_FRAMES;
    SAMPLE* inbuf = AudioDevice::I()->getDSPMTInBuffer();
    // per-worker scratch buffer for one frame of input, sized once before the
    // realtime loop so the loop stays allocation-free
    std::vector<SAMPLE> indata(UNIV::IN_CHANNELS);
    bool zerolatency = AudioDevice::I()->getZeroLatency();
    bool toggle = false;
    printf("Starting RT Audio MT worker %u\n", idx);
    while (true) {
        sMtWorkAvailable.acquire();
        outbuf = outbufs[toggle];
        if (unlikely(!zerolatency)) {
            toggle = !toggle;
        }
        auto cache_closure(AudioDevice::I()->getDSPMTClosure(idx)());
        auto closure = *reinterpret_cast<closure_fn_type*>(cache_closure);
        uint64_t LTIME = UNIV::DEVICE_TIME;
        for (uint32_t i = 0; i < UNIV::NUM_FRAMES; i++) {
            uint32_t iout = i * UNIV::CHANNELS;
            uint32_t iin = i * UNIV::IN_CHANNELS;
            for (unsigned k = 0; k < UNIV::IN_CHANNELS; k++) {
                indata[k] = (SAMPLE)inbuf[iin + k];
            }
            if (UNIV::IN_CHANNELS == UNIV::CHANNELS) {
                for (uint64_t k = 0; k < UNIV::CHANNELS; k++) {
                    outbuf[iout + k] =
                        audio_sanity(cache_wrapper(zone, (void*)closure, (SAMPLE)inbuf[iin + k],
                                                   (i + LTIME), k, &(indata[0])));
                    extemp::EXTZones::llvm_zone_reset(zone);
                }
            } else if (UNIV::IN_CHANNELS == 1) {
                for (uint64_t k = 0; k < UNIV::CHANNELS; k++) {
                    outbuf[iout + k] = audio_sanity(cache_wrapper(
                        zone, (void*)closure, (SAMPLE)inbuf[iin], (i + LTIME), k, &(indata[0])));
                    extemp::EXTZones::llvm_zone_reset(zone);
                }
            } else {
                for (uint64_t k = 0; k < UNIV::CHANNELS; k++) {
                    outbuf[iout + k] = audio_sanity(
                        cache_wrapper(zone, (void*)closure, 0.0, (i + LTIME), k, &(indata[0])));
                    extemp::EXTZones::llvm_zone_reset(zone);
                }
            }
        }
        sMtWorkComplete.release(1);
    }
    return 0;
}

void AudioDevice::processFrames(const float* InputBuffer, float* OutputBuffer,
                                uint64_t FramesPerBuffer, void* UserData) {
    auto sched(reinterpret_cast<TaskScheduler*>(UserData));
    UNIV::DEVICE_TIME += FramesPerBuffer;
    if (likely(UNIV::TIME_DIVISION == 1)) {
        UNIV::TIME.store(UNIV::DEVICE_TIME.load());
    }
    if (unlikely(AudioDevice::CLOCKBASE < 1.0)) {
        AudioDevice::CLOCKBASE = getRealTime();
        UNIV::AUDIO_CLOCK_BASE.store(AudioDevice::CLOCKBASE.load());
    }
    AudioDevice::REALTIME = getRealTime();
    UNIV::AUDIO_CLOCK_NOW.store(AudioDevice::REALTIME.load());
    sched->setFrames(FramesPerBuffer);
    sched->getGuard().signal();
    auto dsp_closure(AudioDevice::I()->getDSPClosure());
    if (unlikely(!dsp_closure)) {
        memset(OutputBuffer, 0, UNIV::CHANNELS * FramesPerBuffer * sizeof(float));
        return;
    }
    auto cache_closure(dsp_closure());
    if (likely(AudioDevice::I()->getDSPWrapper() &&
               !AudioDevice::I()->getDSPSUMWrapper())) {  // sample by sample
        auto cache_wrapper(AudioDevice::I()->getDSPWrapper());
        auto closure = *((SAMPLE(**)(SAMPLE, uint64_t, uint64_t, SAMPLE*))cache_closure);
        llvm_zone_t* zone = extemp::EXTZones::llvm_peek_zone_stack();
        auto dat(OutputBuffer);
        auto in(InputBuffer);
        uint64_t time(UNIV::DEVICE_TIME);
        if (likely(!UNIV::IN_CHANNELS)) {
            float dummy(0.0);
            for (uint64_t i = 0; i < FramesPerBuffer; ++i, ++time) {
                for (uint64_t k = 0; k < UNIV::CHANNELS; ++k) {
                    *(dat++) = audio_sanity(float(cache_wrapper(
                        zone, reinterpret_cast<void*>(closure), 0.0, time, k, &dummy)));
                    extemp::EXTZones::llvm_zone_reset(zone);
                }
            }
        } else if (UNIV::IN_CHANNELS == UNIV::CHANNELS) {
            for (uint64_t i = 0; i < FramesPerBuffer; ++i, ++time) {
                auto indata(in);
                for (uint64_t k = 0; k < UNIV::CHANNELS; ++k) {
                    *(dat++) = audio_sanity(float(cache_wrapper(
                        zone, reinterpret_cast<void*>(closure), *(in++), time, k, indata)));
                    extemp::EXTZones::llvm_zone_reset(zone);
                }
            }
        } else if (UNIV::IN_CHANNELS == 1) {
            for (uint64_t i = 0; i < FramesPerBuffer; ++i, ++time) {
                for (uint64_t k = 0; k < UNIV::CHANNELS; k++) {
                    *(dat++) = audio_sanity(float(
                        cache_wrapper(zone, reinterpret_cast<void*>(closure), *in, time, k, in)));
                    extemp::EXTZones::llvm_zone_reset(zone);
                }
                ++in;
            }
        } else {  // for when in channels & out channels don't match
            // SAMPLE* indata = alloc(UNIV::IN_CHANNELS); // auto
            // indata(in);
            auto indata(in);
            for (uint64_t i = 0; i < FramesPerBuffer; ++i, ++time) {
                for (uint64_t k = 0; k < UNIV::CHANNELS; ++k) {
                    *(dat++) = audio_sanity(
                        float(cache_wrapper(zone, reinterpret_cast<void*>(closure), 0.0, time, k,
                                            &indata[i * UNIV::IN_CHANNELS])));
                    extemp::EXTZones::llvm_zone_reset(zone);
                }
            }
        }
        return;
    }
    if (AudioDevice::I()->getDSPSUMWrapper()) {  // multi-threaded sample-by-sample
        const unsigned numthreads = unsigned(AudioDevice::I()->getNumThreads());
        const bool zerolatency = AudioDevice::I()->getZeroLatency();
        SAMPLE in[32];
        SAMPLE* inb = AudioDevice::I()->getDSPMTInBuffer();
        const float* input = InputBuffer;
        for (unsigned i = 0; i < UNIV::IN_CHANNELS * UNIV::NUM_FRAMES; i++)
            inb[i] = (SAMPLE)input[i];
        if (zerolatency) {
            sMtWorkAvailable.release(numthreads);
            for (unsigned i = 0; i < numthreads; ++i) {
                sMtWorkComplete.acquire();
            }
        }
        dsp_f_ptr_sum cache_wrapper = AudioDevice::I()->getDSPSUMWrapper();
        auto closure = *((SAMPLE(**)(SAMPLE*, uint64_t, uint64_t, SAMPLE*))cache_closure);
        llvm_zone_t* zone = extemp::EXTZones::llvm_peek_zone_stack();
        bool toggle = AudioDevice::I()->getToggle();
        SAMPLE* indats[AudioDevice::MAX_RT_AUDIO_THREADS];
        indats[0] = AudioDevice::I()->getDSPMTOutBuffer();
        // if we are NOT running zerolatency
        // and toggle is FALSE then use alternate buffers
        if (!zerolatency && !toggle) {
            indats[0] = indats[0] + UNIV::NUM_FRAMES * UNIV::CHANNELS;
        }
        for (unsigned jj = 1; jj < numthreads; jj++) {
            indats[jj] = indats[0] + (UNIV::NUM_FRAMES * UNIV::CHANNELS * jj * 2);
        }
        for (uint64_t i = 0; i < UNIV::NUM_FRAMES; i++) {
            uint32_t iout = i * UNIV::CHANNELS;
            float* dat = OutputBuffer;

            for (uint64_t k = 0; k < UNIV::CHANNELS; k++) {
                for (unsigned jj = 0; jj < numthreads; jj++) {
                    in[jj] = indats[jj][iout + k];
                }
                dat[iout + k] = audio_sanity((float)cache_wrapper(
                    zone, (void*)closure, in, (i + UNIV::DEVICE_TIME), k, nullptr));
                extemp::EXTZones::llvm_zone_reset(zone);
            }
        }
        if (!zerolatency) {
            sMtWorkAvailable.release(numthreads);
            for (unsigned i = 0; i < numthreads; ++i) {
                sMtWorkComplete.acquire();
            }
        }
    } else {
        // no wrapper registered — emit silence
        memset(OutputBuffer, 0, (UNIV::CHANNELS * UNIV::NUM_FRAMES * sizeof(float)));
    }
}

//-----------------------------------
//  FILE AUDIO DRIVER
//-----------------------------------
//
// Alternative backend to PortAudio: renders DSP output to a float32 WAV file,
// driven by a dedicated thread that free-runs as fast as processFrames() can
// execute. Used when --audio-outfile is specified. Intended for offline tests
// and best-effort headless rendering, NOT realtime playback.
//
// The WAV header is rewritten on stop() with the final sample count, so quit()
// (which calls std::_Exit and skips destructors) must invoke stop() first.
namespace {

class FileAudioDriver {
  public:
    bool start(const std::string& path, double maxSeconds);
    void stop();
    bool isRunning() const {
        return m_running.load(std::memory_order_acquire);
    }

  private:
    void run();
    bool openWav(const std::string& path);
    void finalizeWav();

    FILE* m_file = nullptr;
    std::string m_path;
    std::thread m_thread;
    std::atomic<bool> m_stop{false};
    std::atomic<bool> m_running{false};
    uint64_t m_framesWritten = 0;
    uint64_t m_maxFrames = 0;  // 0 = unlimited
    std::vector<float> m_inBuf;
    std::vector<float> m_outBuf;
};

FileAudioDriver sFileDriver;

bool FileAudioDriver::openWav(const std::string& path) {
    m_file = std::fopen(path.c_str(), "wb");
    if (!m_file) {
        return false;
    }
    // Reserve 44 bytes for the header; filled in by finalizeWav() once we know
    // the data size.
    char placeholder[44] = {0};
    if (std::fwrite(placeholder, 1, sizeof(placeholder), m_file) != sizeof(placeholder)) {
        std::fclose(m_file);
        m_file = nullptr;
        return false;
    }
    return true;
}

void FileAudioDriver::finalizeWav() {
    if (!m_file)
        return;
    const uint32_t channels = UNIV::CHANNELS;
    const uint32_t sampleRate = UNIV::SAMPLE_RATE;
    const uint32_t bitsPerSample = 32;
    const uint32_t blockAlign = channels * (bitsPerSample / 8);
    const uint32_t byteRate = sampleRate * blockAlign;
    const uint32_t dataBytes = static_cast<uint32_t>(m_framesWritten * blockAlign);
    const uint32_t riffSize = 36 + dataBytes;

    unsigned char hdr[44];
    std::memcpy(hdr + 0, "RIFF", 4);
    std::memcpy(hdr + 4, &riffSize, 4);
    std::memcpy(hdr + 8, "WAVE", 4);
    std::memcpy(hdr + 12, "fmt ", 4);
    const uint32_t fmtSize = 16;
    std::memcpy(hdr + 16, &fmtSize, 4);
    const uint16_t audioFormat = 3;  // WAVE_FORMAT_IEEE_FLOAT
    std::memcpy(hdr + 20, &audioFormat, 2);
    const uint16_t numChannels = static_cast<uint16_t>(channels);
    std::memcpy(hdr + 22, &numChannels, 2);
    std::memcpy(hdr + 24, &sampleRate, 4);
    std::memcpy(hdr + 28, &byteRate, 4);
    const uint16_t blockAlign16 = static_cast<uint16_t>(blockAlign);
    std::memcpy(hdr + 32, &blockAlign16, 2);
    const uint16_t bitsPerSample16 = static_cast<uint16_t>(bitsPerSample);
    std::memcpy(hdr + 34, &bitsPerSample16, 2);
    std::memcpy(hdr + 36, "data", 4);
    std::memcpy(hdr + 40, &dataBytes, 4);

    std::fflush(m_file);
    std::fseek(m_file, 0, SEEK_SET);
    std::fwrite(hdr, 1, sizeof(hdr), m_file);
    std::fflush(m_file);
    std::fclose(m_file);
    m_file = nullptr;
}

bool FileAudioDriver::start(const std::string& path, double maxSeconds) {
    if (m_running.load())
        return false;
    if (!openWav(path)) {
        ascii_error();
        printf("Error: could not open --audio-outfile path for writing: %s\n", path.c_str());
        ascii_normal();
        return false;
    }
    m_path = path;
    m_framesWritten = 0;
    m_maxFrames = (maxSeconds > 0.0) ? static_cast<uint64_t>(maxSeconds * UNIV::SAMPLE_RATE) : 0;
    m_inBuf.assign(UNIV::IN_CHANNELS * UNIV::NUM_FRAMES, 0.0f);
    m_outBuf.assign(UNIV::CHANNELS * UNIV::NUM_FRAMES, 0.0f);
    m_stop.store(false);
    m_running.store(true, std::memory_order_release);
    m_thread = std::thread([this] { this->run(); });
    return true;
}

void FileAudioDriver::run() {
    // Offline free-run: pump processFrames as fast as we can, writing each
    // buffer to disk. UNIV::DEVICE_TIME / UNIV::TIME advance inside
    // processFrames, so scheduled xtlang callbacks still fire at the correct
    // sample times (just faster than wall-clock).
    //
    // Free-run will typically outrun the Scheme compile/setup path, so we
    // don't start counting toward --duration until a DSP closure is registered.
    // Before that, we still tick processFrames (to advance the scheduler and
    // let the main thread progress) but we do NOT write output and we do NOT
    // advance m_framesWritten. This keeps the recorded output aligned with the
    // first sample after dsp:set! takes effect.
    const uint32_t frames = UNIV::NUM_FRAMES;
    bool dspSeen = false;
    bool reachedDuration = false;
    while (!m_stop.load(std::memory_order_acquire)) {
        if (dspSeen && m_maxFrames && m_framesWritten >= m_maxFrames) {
            reachedDuration = true;
            break;
        }

        const bool haveDSP = (AudioDevice::I()->getDSPClosure() != nullptr);
        std::fill(m_outBuf.begin(), m_outBuf.end(), 0.0f);
        AudioDevice::I()->processFrames(m_inBuf.empty() ? nullptr : m_inBuf.data(), m_outBuf.data(),
                                        frames, TaskScheduler::I());
        if (!dspSeen) {
            if (haveDSP) {
                dspSeen = true;
            } else {
                // Yield so the main thread can make progress compiling DSP.
                std::this_thread::sleep_for(std::chrono::milliseconds(1));
                continue;
            }
        }
        const size_t samples = m_outBuf.size();
        std::fwrite(m_outBuf.data(), sizeof(float), samples, m_file);
        m_framesWritten += frames;
        // Sleep briefly so the task scheduler thread actually gets CPU between
        // our per-buffer signal() calls. The scheduler uses a plain condvar
        // wait with no predicate loop, so signals that arrive before it
        // re-enters wait() are lost; without this throttle the driver runs
        // so tight that scheduled Scheme callbacks can miss the entire render
        // window. 100us * ~86 buffers/sec = <10ms overhead per render second,
        // still >>100x realtime.
        std::this_thread::sleep_for(std::chrono::microseconds(100));
    }

    // If we stopped because --duration was reached (rather than an external
    // stop() call), finalize and terminate the process here. The main thread
    // may still be in a loop waiting for us; _Exit bypasses destructors
    // (matching how Scheme's (quit) behaves).
    if (reachedDuration) {
        bool expected = true;
        if (m_running.compare_exchange_strong(expected, false)) {
            finalizeWav();
            printf("[--audio-outfile: %llu frames written to %s]\n",
                   static_cast<unsigned long long>(m_framesWritten), m_path.c_str());
            fflush(stdout);
            std::_Exit(0);
        }
    }
}

void FileAudioDriver::stop() {
    if (!m_running.exchange(false))
        return;
    m_stop.store(true, std::memory_order_release);
    if (m_thread.joinable())
        m_thread.join();
    finalizeWav();
}

}  // anonymous namespace

bool AudioDevice::fileDriverRunning() {
    return sFileDriver.isRunning();
}

void AudioDevice::stopFileDriver() {
    sFileDriver.stop();
}

static int audioCallback(const void* InputBuffer, void* OutputBuffer, unsigned long FramesPerBuffer,
                         const PaStreamCallbackTimeInfo* /*TimeInfo*/,
                         PaStreamCallbackFlags StatusFlags, void* UserData) {
    if (unlikely(StatusFlags & (paOutputUnderflow | paOutputOverflow))) {
        if (StatusFlags & paOutputUnderflow) {
            printf("Audio underflow: are you pushing extempore too hard?\n");
        }
        if (StatusFlags & paOutputOverflow) {
            printf("Audio output overflow\n");
        }
    }
    AudioDevice::I()->processFrames(reinterpret_cast<const float*>(InputBuffer),
                                    reinterpret_cast<float*>(OutputBuffer), FramesPerBuffer,
                                    UserData);
    return paContinue;
}

AudioDevice::AudioDevice()
    : m_started(false), buffer(0), m_dsp_closure(nullptr), dsp_wrapper(0), dsp_wrapper_sum(0),
      outbuf(nullptr), inbuf(nullptr), m_numThreads(50) /* NOT 0! */, m_zeroLatency(true) {}

AudioDevice::~AudioDevice() {
    // The RT audio threads run `while(true)` loops and are intentionally
    // leaked at process exit.  std::thread would call std::terminate if
    // destroyed while joinable, so detach each EXTThread before the
    // unique_ptr in m_threads destroys the underlying std::thread.
    for (auto& t : m_threads) {
        if (t && t->isRunning()) {
            t->detach();
        }
    }
    if (stream && !UNIV::AUDIO_NONE) {
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

static int findDevice(const std::string& Name) {
    std::regex rgx(Name);
    std::cmatch m;
    int numDevices(Pa_GetDeviceCount());
    for (int i = 0; i < numDevices; ++i) {
        if (std::regex_search(Pa_GetDeviceInfo(i)->name, m, rgx)) {
            return i;
        }
    }
    ascii_error();
    printf("\n*** Can't find device matching regex: %s\n", Name.c_str());
    ascii_normal();
    fflush(stdout);
    std::_Exit(1);
}

void AudioDevice::start() {
    if (m_started) {
        return;
    }
    if (UNIV::AUDIO_NONE) {
        ascii_error();
        fprintf(stderr, "Error: cannot set the audio device in --noaudio mode\n");
        ascii_normal();
        return;
    }
    if (!UNIV::AUDIO_OUTFILE_PATH.empty()) {
        if (!sFileDriver.start(UNIV::AUDIO_OUTFILE_PATH, UNIV::AUDIO_OUTFILE_DURATION)) {
            std::_Exit(1);
        }
        m_started = true;
        ascii_normal();
        std::cout << "Output Device  : " << std::flush;
        ascii_info();
        std::cout << "file://" << UNIV::AUDIO_OUTFILE_PATH << " (offline, " << UNIV::CHANNELS
                  << "ch float32 @ " << UNIV::SAMPLE_RATE << " Hz)" << std::endl;
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
        printf("ERROR: Pa_CountDevices returned 0x%x\n", numDevices);
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
        printf("Input device not valid! %d\n", (int)UNIV::AUDIO_IN_DEVICE);
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
    pain.sampleFormat = paFloat32;  //|((UNIV::INTERLEAVED==0) ? 0 : paNonInterleaved);
    pain.suggestedLatency = deviceInfo->defaultLowInputLatency;
    PaStreamParameters* painptr = (UNIV::IN_CHANNELS < 1) ? nullptr : &pain;
    deviceInfo = Pa_GetDeviceInfo(UNIV::AUDIO_DEVICE);
    paout.channelCount = UNIV::CHANNELS;
    paout.device = UNIV::AUDIO_DEVICE;
    paout.sampleFormat = paFloat32;  //|((UNIV::INTERLEAVED==0) ? 0 : paNonInterleaved);
    paout.suggestedLatency =
        std::max(UNIV::AUDIO_OUTPUT_LATENCY, deviceInfo->defaultLowOutputLatency);
    paout.hostApiSpecificStreamInfo = nullptr;
    PaStreamParameters* paoutptr = (UNIV::CHANNELS < 1) ? nullptr : &paout;
    err = Pa_OpenStream(&stream, painptr, paoutptr, UNIV::SAMPLE_RATE, UNIV::NUM_FRAMES, paNoFlag,
                        audioCallback, TaskScheduler::I());
    if (err != paNoError) {
        ascii_error();
        std::cout << "Initialization Error: " << Pa_GetErrorText(err) << std::endl;
        std::cout << "AudioDevice: " << (Pa_GetDeviceInfo(UNIV::AUDIO_DEVICE))->name << std::endl;
        ascii_normal();
        exit(1);
    }

    err = Pa_StartStream(stream);

    if (err != paNoError) {
        ascii_error();
        std::cout << "ERROR: " << Pa_GetErrorText(err) << std::endl;
        std::cout << "AudioDevice: " << (Pa_GetDeviceInfo(UNIV::AUDIO_DEVICE))->name << std::endl;
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

void AudioDevice::stop() {
    if (!m_started) {
        return;
    }
    if (sFileDriver.isRunning()) {
        sFileDriver.stop();
        m_started = false;
        return;
    }
    PaError err = Pa_StopStream(stream);
    if (err != paNoError) {
        std::cout << "PA Error: " << Pa_GetErrorText(err) << std::endl;
    }
    m_started = false;
}

void AudioDevice::initMTAudio(int Num, bool ZeroLatency) {
    if (unsigned(Num) > MAX_RT_AUDIO_THREADS) {
        printf("HARD CEILING of %d RT AUDIO THREADS .. aborting!\n", MAX_RT_AUDIO_THREADS);
        exit(1);
    }
    const unsigned n = unsigned(Num);
    m_numThreads.store(n, std::memory_order_release);
    m_zeroLatency = ZeroLatency;
    m_toggle = true;
    inbuf = (SAMPLE*)malloc(UNIV::IN_CHANNELS * UNIV::NUM_FRAMES * sizeof(SAMPLE));
    // outbuf * 2 for double buffering
    outbuf = (SAMPLE*)malloc(UNIV::CHANNELS * UNIV::NUM_FRAMES * sizeof(SAMPLE) * n * 2);
    memset(outbuf, 0, UNIV::CHANNELS * UNIV::NUM_FRAMES * sizeof(SAMPLE) * n * 2);
    for (unsigned i = 0; i < n; ++i) {
        m_threads[i] =
            std::make_unique<EXTThread>(audioCallbackMT, reinterpret_cast<void*>(uintptr_t(i)),
                                        std::string("MT_AUD_") + char('A' + i));
        m_threads[i]->start();
    }
}

double AudioDevice::getCPULoad() {
    return Pa_GetStreamCpuLoad(AudioDevice::I()->getPaStream());
}

void AudioDevice::printDevices() {
    Pa_Initialize();

    int numDevices = Pa_GetDeviceCount();
    if (numDevices <= 0) {
        printf("Error: no audio devices found! Exiting...\n");
        // printf("ERROR: Pa_CountDevices returned 0x%x\n", numDevices );
        exit(1);
    }
    ascii_normal();
    printf("\n-----Available Audio Devices-----------------------------\n");
    ascii_default();

    const PaDeviceInfo* deviceInfo;
    const PaHostApiInfo* apiInfo;

    for (int i = 0; i < numDevices; i++) {
        deviceInfo = Pa_GetDeviceInfo(i);
        apiInfo = Pa_GetHostApiInfo(deviceInfo->hostApi);
        printf("audio device[%d]:%s api[%d]:%s inchan[%d] outchan[%d]\n", i, deviceInfo->name,
               deviceInfo->hostApi, apiInfo->name, deviceInfo->maxInputChannels,
               deviceInfo->maxOutputChannels);
    }
    ascii_normal();
    printf("----------------------------------------------------------\n\n");
    ascii_default();
#ifdef _WIN32
    Pa_Terminate();
#else
    fflush(stdout);
    freopen("/dev/null", "w", stdout);  // throttle termination messages
    Pa_Terminate();
    fflush(stdout);
#endif
    return;
}

}  // namespace extemp
