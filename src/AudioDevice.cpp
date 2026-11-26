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

#include <algorithm>
#include <iostream>
#include <cstring>
#include <cerrno>
#include <cinttypes>

#include "AudioDevice.h"
#include "Log.h"
#include "Regex.h"
#include "TaskScheduler.h"
#include "EXTLLVM.h"
#include "SchemeFFI.h"
#include "SchemeProcess.h"

#include <chrono>
#include <condition_variable>
#include <mutex>
#include <array>
#include <semaphore>
#include <thread>

#ifdef _WIN32
#include <Windows.h>
#elif !defined(__APPLE__)
#include <alsa/asoundlib.h>
#include <cstdarg>
#endif

#include <cstdlib>
#include <cmath>
#include <algorithm>
#include <cstdio>
#include <atomic>
#include <vector>

// Tell the CPU to treat denormal floats as zero on the calling thread. A
// filter or delay line decaying towards silence passes through denormal
// numbers, which are tens of times slower to compute with and would show up
// as CPU spikes when the music stops. The setting is per-thread, and cheap
// enough to make at the top of every audio callback, which also covers audio
// APIs that call us from more than one thread.
#if defined(__SSE__) || defined(_M_X64) || defined(_M_IX86)
#include <xmmintrin.h>
#include <pmmintrin.h>
static inline void flush_denormals_to_zero() {
    _MM_SET_FLUSH_ZERO_MODE(_MM_FLUSH_ZERO_ON);
    _MM_SET_DENORMALS_ZERO_MODE(_MM_DENORMALS_ZERO_ON);
}
#elif defined(__aarch64__) && (defined(__GNUC__) || defined(__clang__))
static inline void flush_denormals_to_zero() {
    uint64_t fpcr;
    __asm__ __volatile__("mrs %0, fpcr" : "=r"(fpcr));
    fpcr |= (uint64_t(1) << 24);  // FZ: flush-to-zero
    __asm__ __volatile__("msr fpcr, %0" : : "r"(fpcr));
}
#else
static inline void flush_denormals_to_zero() {}
#endif

// Reject non-finite samples and clamp the rest to a safe range. The finite
// check runs first so NaN/inf never reach std::clamp; on x86 and ARM, compilers
// lower the clamp to min/max instructions, so no hand-written intrinsics needed.
static inline float audio_sanity(float x) {
    if (std::isfinite(x)) [[likely]] {
        return std::clamp(x, -0.99f, 0.99f);
    }
    return 0.0f;
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

std::atomic<double> AudioDevice::REALTIME = 0.0;
std::atomic<double> AudioDevice::CLOCKBASE = 0.0;
double AudioDevice::CLOCKOFFSET = 0.0;

//-----------------------------------
//  PORT AUDIO
//-----------------------------------

namespace {

// Semaphores synchronise the multi-threaded audio dispatcher with its worker
// threads without spinning on atomics. Each worker has its own work semaphore
// so that one block hands exactly one wake to every worker: with a single
// counting semaphore a fast worker could take two permits in one block and
// leave another worker's slice stale, which showed up as a missing voice.
// Completion is counted on one semaphore, since every worker releases it
// exactly once per block.

// dispatcher -> worker i
struct MtWorkerSignal {
    std::binary_semaphore work{0};
};
std::array<MtWorkerSignal, AudioDevice::MAX_RT_AUDIO_THREADS> sMtWorkers;
// workers -> dispatcher
std::counting_semaphore<> sMtWorkComplete{0};

}  // anonymous namespace

#if !defined(_WIN32) && !defined(__APPLE__)
// ALSA writes a line to stderr for every configuration PortAudio probes and it
// can't open: dozens on a machine without a card 0. Those are debug messages.
static void alsaError(const char* /*File*/, int /*Line*/, const char* /*Function*/, int Err,
                      const char* Fmt, ...) {
    if (!log::enabled(log::Level::Debug, log::audio.name)) {
        return;
    }
    char text[512];
    va_list args;
    va_start(args, Fmt);
    std::vsnprintf(text, sizeof(text), Fmt, args);
    va_end(args);
    log::debug(log::audio, "ALSA: {}{}{}", text, Err != 0 ? ": " : "",
               Err != 0 ? snd_strerror(Err) : "");
}
#endif

// Initialise PortAudio, with what its host APIs print going to the logger.
static PaError initialisePortAudio() {
#if !defined(_WIN32) && !defined(__APPLE__)
    snd_lib_error_set_handler(alsaError);
#endif
    return Pa_Initialize();
}

void* audioCallbackMT(void* Args) {
    flush_denormals_to_zero();
    log::mark_realtime_thread();
    EXTThread::makeCurrentRealtime();
    const unsigned idx = uintptr_t(Args);
    auto device(AudioDevice::I());
    // The wrapper, latency mode and buffers are fixed for the life of the
    // worker: initMTAudio() publishes them before starting us and refuses to
    // change them afterwards.
    auto cache_wrapper(device->getDspState()->wrapper);
    auto zone(extemp::EXTZones::llvm_peek_zone_stack());
    const std::span<SAMPLE> slice(device->getMTOutSlice(idx));
    const size_t blockSamples = size_t(UNIV::CHANNELS) * UNIV::NUM_FRAMES;
    SAMPLE* outbufs[2] = {slice.data(), slice.data() + blockSamples};
    const std::span<SAMPLE> inbuf(device->getMTInBuffer());
    // per-worker scratch buffer for one frame of input, sized once before the
    // realtime loop so the loop stays allocation-free
    std::vector<SAMPLE> indata(UNIV::IN_CHANNELS);
    const bool zerolatency = device->getDspState()->zeroLatency;
    bool toggle = false;
    log::debug(log::audio, "starting multi-threaded audio worker {}", idx);
    while (true) {
        sMtWorkers[idx].work.acquire();
        SAMPLE* outbuf = outbufs[toggle];
        if (!zerolatency) [[unlikely]] {
            toggle = !toggle;
        }
        auto getter(device->getDspState()->mtClosures[idx]);
        if (!getter) [[unlikely]] {
            // no closure installed for this worker yet: emit silence
            std::fill_n(outbuf, blockSamples, 0.0f);
            sMtWorkComplete.release(1);
            continue;
        }
        auto cache_closure(getter());
        auto closure = *reinterpret_cast<closure_fn_type*>(cache_closure);
        const uint64_t LTIME = UNIV::DEVICE_TIME;
        for (uint32_t i = 0; i < UNIV::NUM_FRAMES; i++) {
            const uint32_t iout = i * UNIV::CHANNELS;
            const uint32_t iin = i * UNIV::IN_CHANNELS;
            for (unsigned k = 0; k < UNIV::IN_CHANNELS; k++) {
                indata[k] = inbuf[iin + k];
            }
            if (UNIV::IN_CHANNELS == UNIV::CHANNELS) {
                for (uint64_t k = 0; k < UNIV::CHANNELS; k++) {
                    outbuf[iout + k] = audio_sanity(cache_wrapper(
                        zone, (void*)closure, inbuf[iin + k], (i + LTIME), k, indata.data()));
                    extemp::EXTZones::llvm_zone_reset(zone);
                }
            } else if (UNIV::IN_CHANNELS == 1) {
                for (uint64_t k = 0; k < UNIV::CHANNELS; k++) {
                    outbuf[iout + k] = audio_sanity(cache_wrapper(zone, (void*)closure, inbuf[iin],
                                                                  (i + LTIME), k, indata.data()));
                    extemp::EXTZones::llvm_zone_reset(zone);
                }
            } else {
                for (uint64_t k = 0; k < UNIV::CHANNELS; k++) {
                    outbuf[iout + k] = audio_sanity(
                        cache_wrapper(zone, (void*)closure, 0.0, (i + LTIME), k, indata.data()));
                    extemp::EXTZones::llvm_zone_reset(zone);
                }
            }
        }
        sMtWorkComplete.release(1);
    }
    return nullptr;
}

void AudioDevice::processFrames(const float* InputBuffer, float* OutputBuffer,
                                uint64_t FramesPerBuffer, void* UserData) {
    auto sched(reinterpret_cast<TaskScheduler*>(UserData));
    UNIV::DEVICE_TIME += FramesPerBuffer;
    if (UNIV::TIME_DIVISION == 1) [[likely]] {
        UNIV::TIME.store(UNIV::DEVICE_TIME.load());
    }
    if (AudioDevice::CLOCKBASE < 1.0) [[unlikely]] {
        AudioDevice::CLOCKBASE = getRealTime();
        UNIV::AUDIO_CLOCK_BASE.store(AudioDevice::CLOCKBASE.load());
    }
    AudioDevice::REALTIME = getRealTime();
    UNIV::AUDIO_CLOCK_NOW.store(AudioDevice::REALTIME.load());
    sched->setFrames(FramesPerBuffer);
    sched->tick();
    renderFrames(InputBuffer, OutputBuffer, FramesPerBuffer);
}

void AudioDevice::renderFrames(const float* InputBuffer, float* OutputBuffer,
                               uint64_t FramesPerBuffer) {
    flush_denormals_to_zero();
    // One acquire load per block. Every field below comes from that snapshot,
    // so a dsp:set! landing mid-callback is either wholly visible or not at
    // all -- there is no ordering contract between the setters to get wrong.
    const DspState* state = getDspState();
    if (!state->closure) [[unlikely]] {
        memset(OutputBuffer, 0, UNIV::CHANNELS * FramesPerBuffer * sizeof(float));
        return;
    }
    auto cache_closure(state->closure());
    if (state->wrapper && !state->sumWrapper) [[likely]] {  // sample by sample
        auto cache_wrapper(state->wrapper);
        auto closure = *((SAMPLE(**)(SAMPLE, uint64_t, uint64_t, SAMPLE*))cache_closure);
        llvm_zone_t* zone = extemp::EXTZones::llvm_peek_zone_stack();
        auto dat(OutputBuffer);
        auto in(InputBuffer);
        uint64_t time(UNIV::DEVICE_TIME);
        if (!UNIV::IN_CHANNELS) [[likely]] {
            const float dummy(0.0);
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
    if (state->sumWrapper && state->mtReady) {  // multi-threaded sample-by-sample
        // The worker slices are sized from UNIV::NUM_FRAMES, so a host buffer
        // of a different size would read past them.
        if (FramesPerBuffer != UNIV::NUM_FRAMES) [[unlikely]] {
            memset(OutputBuffer, 0, UNIV::CHANNELS * FramesPerBuffer * sizeof(float));
            return;
        }
        const unsigned numthreads = state->numThreads;
        const bool zerolatency = state->zeroLatency;
        SAMPLE in[AudioDevice::MAX_RT_AUDIO_THREADS];
        const std::span<SAMPLE> inb(getMTInBuffer());
        if (InputBuffer) {
            for (size_t i = 0; i < inb.size(); i++) {
                inb[i] = SAMPLE(InputBuffer[i]);
            }
        }
        if (zerolatency) {
            for (unsigned i = 0; i < numthreads; ++i) {
                sMtWorkers[i].work.release();
            }
            for (unsigned i = 0; i < numthreads; ++i) {
                sMtWorkComplete.acquire();
            }
        }
        dsp_f_ptr_sum cache_wrapper = state->sumWrapper;
        auto closure = *((SAMPLE(**)(SAMPLE*, uint64_t, uint64_t, SAMPLE*))cache_closure);
        llvm_zone_t* zone = extemp::EXTZones::llvm_peek_zone_stack();
        const bool toggle = getToggle();
        // if we are NOT running zerolatency and toggle is FALSE then the
        // workers wrote into the second half of their slice
        const size_t half =
            (!zerolatency && !toggle) ? size_t(UNIV::NUM_FRAMES) * UNIV::CHANNELS : 0;
        SAMPLE* indats[AudioDevice::MAX_RT_AUDIO_THREADS];
        for (unsigned jj = 0; jj < numthreads; jj++) {
            indats[jj] = getMTOutSlice(jj).data() + half;
        }
        for (uint64_t i = 0; i < UNIV::NUM_FRAMES; i++) {
            const uint32_t iout = i * UNIV::CHANNELS;
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
            for (unsigned i = 0; i < numthreads; ++i) {
                sMtWorkers[i].work.release();
            }
            for (unsigned i = 0; i < numthreads; ++i) {
                sMtWorkComplete.acquire();
            }
        }
    } else {
        // no wrapper registered -- emit silence
        memset(OutputBuffer, 0, UNIV::CHANNELS * FramesPerBuffer * sizeof(float));
    }
}

//-----------------------------------
//  FILE AUDIO DRIVER
//-----------------------------------
//
// Alternative backend to PortAudio: renders DSP output to a float32 WAV file
// on its own thread, as fast as the work allows. Used by extempore render,
// where stdout is for what the code prints, so its own messages go to stderr.
//
// The render thread owns the audio clock and keeps it in lockstep with the
// work. No scheduler thread runs; before each block the render thread runs the
// scheduler itself for the tasks due in that block, then waits for every
// Scheme process to finish them. So a callback always lands in the block it
// was scheduled for, however long it takes, and a render is repeatable.
//
// The WAV header is rewritten on stop() with the final sample count, so quit()
// (which calls std::_Exit and skips destructors) must invoke stop() first.
namespace {

class FileAudioDriver {
  public:
    bool open(const std::string& path, double maxSeconds);
    void start();
    void stop();
    bool isRunning() const {
        return m_running.load(std::memory_order_acquire);
    }

  private:
    void run();
    void showProgress();
    void clearProgress();
    bool openWav(const std::string& path);
    void finalizeWav();

    FILE* m_file = nullptr;
    std::string m_path;
    std::thread m_thread;
    std::atomic<bool> m_stop{false};
    std::atomic<bool> m_running{false};
    uint64_t m_framesWritten = 0;
    bool m_dspSeen = false;    // a block was rendered with a DSP closure set
    uint64_t m_maxFrames = 0;  // --duration, in frames
    // with stderr on a terminal: a progress line, redrawn at
    // most ten times a second
    bool m_progress = false;
    std::chrono::steady_clock::time_point m_progressDrawn{};
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
    // A canonical WAV header cannot describe more than 4 GB of samples. A
    // render that long is out of scope (RF64 would be the answer), so cap the
    // advertised size and say so rather than writing a header that wraps.
    const uint64_t dataBytes64 = m_framesWritten * blockAlign;
    uint32_t dataBytes = static_cast<uint32_t>(dataBytes64);
    if (dataBytes64 > 0xFFFFFFF0ull - 36) {
        dataBytes = 0xFFFFFFF0u - 36;
        log::warn(log::audio,
                  "the render exceeds the 4 GB WAV limit; the header describes the first {} "
                  "bytes only",
                  dataBytes);
    }
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
    if (std::fseek(m_file, 0, SEEK_SET) != 0 ||
        std::fwrite(hdr, 1, sizeof(hdr), m_file) != sizeof(hdr)) {
        log::error(log::audio, "could not finalise the WAV header for {}: {}", m_path,
                   std::strerror(errno));
    }
    std::fflush(m_file);
    std::fclose(m_file);
    m_file = nullptr;
}

bool FileAudioDriver::open(const std::string& path, double maxSeconds) {
    if (m_file)
        return false;
    if (!openWav(path)) {
        log::error(log::audio, "can't write {}: {}", path, std::strerror(errno));
        return false;
    }
    m_path = path;
    m_framesWritten = 0;
    m_maxFrames = static_cast<uint64_t>(maxSeconds * UNIV::SAMPLE_RATE);
    m_inBuf.assign(size_t{UNIV::IN_CHANNELS} * UNIV::NUM_FRAMES, 0.0f);
    m_outBuf.assign(size_t{UNIV::CHANNELS} * UNIV::NUM_FRAMES, 0.0f);
    return true;
}

void FileAudioDriver::showProgress() {
    if (!m_progress) {
        return;
    }
    const auto now = std::chrono::steady_clock::now();
    if (now - m_progressDrawn < std::chrono::milliseconds(100)) {
        return;
    }
    m_progressDrawn = now;
    fprintf(stderr, "\rrendering %.1f of %.1f s", double(m_framesWritten) / UNIV::SAMPLE_RATE,
            double(m_maxFrames) / UNIV::SAMPLE_RATE);
    std::fflush(stderr);
}

void FileAudioDriver::clearProgress() {
    if (m_progress) {
        fputs("\r\033[K", stderr);
    }
}

void FileAudioDriver::start() {
    if (!m_file || m_running.load())
        return;
    m_progress = log::is_terminal(stderr);
    m_stop.store(false);
    m_running.store(true, std::memory_order_release);
    m_thread = std::thread([this] { this->run(); });
}

void FileAudioDriver::run() {
    const uint32_t frames = UNIV::NUM_FRAMES;
    TaskScheduler::I()->setFrames(frames);
    AudioDevice::CLOCKBASE = getRealTime();
    UNIV::AUDIO_CLOCK_BASE.store(AudioDevice::CLOCKBASE.load());
    bool reachedDuration = false;
    while (!m_stop.load(std::memory_order_acquire)) {
        if (m_framesWritten >= m_maxFrames) {
            reachedDuration = true;
            break;
        }
        // The block [TIME, TIME + frames). Whatever is already running comes
        // first: before the first block that is the startup file, which so
        // loads at time 0 however long it takes to compile.
        if (!SchemeProcess::waitAllIdle(m_stop)) {
            break;
        }
        TaskScheduler::I()->runDue(UNIV::TIME + frames);
        if (!SchemeProcess::waitAllIdle(m_stop)) {
            break;
        }
        AudioDevice::REALTIME = getRealTime();
        UNIV::AUDIO_CLOCK_NOW.store(AudioDevice::REALTIME.load());
        std::ranges::fill(m_outBuf, 0.0f);
        m_dspSeen = m_dspSeen || AudioDevice::I()->getDspState()->closure != nullptr;
        AudioDevice::I()->renderFrames(m_inBuf.empty() ? nullptr : m_inBuf.data(), m_outBuf.data(),
                                       frames);
        // the last block is cut at --duration, so the file is exactly that long
        const uint64_t keep = std::min<uint64_t>(frames, m_maxFrames - m_framesWritten);
        const size_t samples = size_t(keep) * UNIV::CHANNELS;
        if (std::fwrite(m_outBuf.data(), sizeof(float), samples, m_file) != samples) {
            log::error(log::audio, "can't write {}: {}; stopping the render", m_path,
                       std::strerror(errno));
            break;
        }
        m_framesWritten += keep;
        showProgress();
        UNIV::DEVICE_TIME += frames;
        UNIV::TIME.store(UNIV::DEVICE_TIME.load());
    }

    // If we stopped because --duration was reached (rather than an external
    // stop() call), finalize and terminate the process here. Every process is
    // idle at this point; _Exit bypasses destructors (matching how Scheme's
    // (quit) behaves).
    if (reachedDuration) {
        bool expected = true;
        if (m_running.compare_exchange_strong(expected, false)) {
            finalizeWav();
            clearProgress();
            if (!m_dspSeen) {
                log::warn(log::audio, "no DSP was set (dsp:set!), so the render is silent");
            }
            log::flush();
            // what extempore render made: the command's own summary, always shown
            std::fprintf(stderr, "extempore: wrote %s (%.2f s, %u ch, %u Hz)\n", m_path.c_str(),
                         double(m_framesWritten) / UNIV::SAMPLE_RATE, unsigned(UNIV::CHANNELS),
                         unsigned(UNIV::SAMPLE_RATE));
            std::fflush(stdout);
            std::_Exit(UNIV::failed() ? 1 : 0);
        }
    }
}

// Called by (quit), from a task the render thread may be waiting on.
void FileAudioDriver::stop() {
    if (!m_running.exchange(false))
        return;
    m_stop.store(true, std::memory_order_release);
    SchemeProcess::wakeIdleWaiters();
    if (m_thread.joinable())
        m_thread.join();
    clearProgress();
    finalizeWav();
}

}  // anonymous namespace

bool AudioDevice::fileDriverRunning() {
    return sFileDriver.isRunning();
}

void AudioDevice::startFileDriver() {
    sFileDriver.start();
}

void AudioDevice::stopFileDriver() {
    sFileDriver.stop();
}

static int audioCallback(const void* InputBuffer, void* OutputBuffer, unsigned long FramesPerBuffer,
                         const PaStreamCallbackTimeInfo* /*TimeInfo*/,
                         PaStreamCallbackFlags StatusFlags, void* UserData) {
    // every call: PortAudio may call from a new thread after a restart
    log::mark_realtime_thread();
    if (StatusFlags & (paOutputUnderflow | paOutputOverflow)) [[unlikely]] {
        // Nothing may block or allocate on the audio thread: count the xruns
        // and let the Scheme side read them back with (sys:audio-xruns).
        if (StatusFlags & paOutputUnderflow) {
            AudioDevice::I()->countUnderflow();
        }
        if (StatusFlags & paOutputOverflow) {
            AudioDevice::I()->countOverflow();
        }
    }
    AudioDevice::I()->processFrames(reinterpret_cast<const float*>(InputBuffer),
                                    reinterpret_cast<float*>(OutputBuffer), FramesPerBuffer,
                                    UserData);
    return paContinue;
}

AudioDevice::AudioDevice()
    : m_started(false), stream(nullptr), m_dspState(new DspState()), m_toggle(1), m_underflows(0),
      m_overflows(0) {}

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
            log::warn(log::audio, "could not stop the audio stream: {}", Pa_GetErrorText(err));
        }
        err = Pa_CloseStream(stream);
        if (err != paNoError) {
            log::warn(log::audio, "could not close the audio stream: {}", Pa_GetErrorText(err));
        }
        err = Pa_Terminate();
        if (err != paNoError) {
            log::warn(log::audio, "could not terminate PortAudio: {}", Pa_GetErrorText(err));
        }
    }
}

// --device / --indevice take a regex, but the user types it on a command
// line: a pattern which isn't a valid one is matched as plain text instead.
static int findDevice(const std::string& Name) {
    const auto pattern = regex::compile(Name.c_str());
    if (!pattern) {
        log::warn(log::audio, "'{}' is not a valid regex ({}); matching it as plain text", Name,
                  pattern.error().message);
    }
    const int numDevices(Pa_GetDeviceCount());
    for (int i = 0; i < numDevices; ++i) {
        const std::string_view deviceName = Pa_GetDeviceInfo(i)->name;
        if (pattern) {
            if (regex::search(*pattern, regex::matchDataFor(*pattern), deviceName) > 0) {
                return i;
            }
        } else if (deviceName.contains(Name)) {
            return i;
        }
    }
    log::error(log::audio, "can't find an audio device matching '{}'; see `extempore devices`",
               Name);
    log::flush();
    std::_Exit(1);
}

void AudioDevice::start() {
    if (m_started) {
        return;
    }
    if (UNIV::AUDIO_NONE) {
        log::error(log::audio, "cannot set the audio device in --noaudio mode");
        return;
    }
    if (UNIV::MODE == UNIV::Mode::Render) {
        if (!sFileDriver.open(UNIV::AUDIO_OUTFILE_PATH, UNIV::AUDIO_OUTFILE_DURATION)) {
            log::flush();
            std::_Exit(1);
        }
        m_started = true;
        return;
    }
    PaError err = initialisePortAudio();
    if (err != paNoError) {
        log::error(log::audio, "could not initialise PortAudio: {}", Pa_GetErrorText(err));
        log::flush();
        std::_Exit(1);
    }
    const int numDevices = Pa_GetDeviceCount();
    if (!UNIV::AUDIO_DEVICE_NAME.empty()) {
        UNIV::AUDIO_DEVICE = findDevice(UNIV::AUDIO_DEVICE_NAME);
    }
    if (!UNIV::AUDIO_IN_DEVICE_NAME.empty()) {
        UNIV::AUDIO_IN_DEVICE = findDevice(UNIV::AUDIO_IN_DEVICE_NAME);
    }
    if (numDevices < 0) {
        log::error(log::audio, "no audio devices found (Pa_GetDeviceCount returned {})",
                   numDevices);
        log::flush();
        std::_Exit(1);
    }
    if (int(UNIV::AUDIO_DEVICE) < -1 || int(UNIV::AUDIO_DEVICE) >= numDevices) {
        log::error(log::audio, "output device {} is not valid; see `extempore devices`",
                   int(UNIV::AUDIO_DEVICE));
        log::flush();
        std::_Exit(1);
    }
    if (int(UNIV::AUDIO_IN_DEVICE) < -1 || int(UNIV::AUDIO_IN_DEVICE) >= numDevices) {
        log::error(log::audio, "input device {} is not valid; see `extempore devices`",
                   int(UNIV::AUDIO_IN_DEVICE));
        log::flush();
        std::_Exit(1);
    }
    if (UNIV::IN_CHANNELS != UNIV::CHANNELS && UNIV::IN_CHANNELS != 1 && UNIV::IN_CHANNELS > 0) {
        log::warn(log::audio, "dsp input will be 0.0, use data* for channel data");
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
        log::error(log::audio, "could not open the audio stream on '{}': {}",
                   Pa_GetDeviceInfo(UNIV::AUDIO_DEVICE)->name, Pa_GetErrorText(err));
        log::flush();
        std::_Exit(1);
    }

    err = Pa_StartStream(stream);

    if (err != paNoError) {
        log::error(log::audio, "could not start the audio stream on '{}': {}",
                   Pa_GetDeviceInfo(UNIV::AUDIO_DEVICE)->name, Pa_GetErrorText(err));
        log::flush();
        std::_Exit(1);
    }

    m_started = true;

    const PaStreamInfo* info = Pa_GetStreamInfo(stream);
    std::string input;
    if (UNIV::IN_CHANNELS > 0) {
        input = std::format(", {} in", UNIV::IN_CHANNELS);
        if (UNIV::AUDIO_IN_DEVICE != unsigned(-1)) {
            input += std::format(" from {}", Pa_GetDeviceInfo(inputDevice)->name);
        }
    }
    log::info(log::audio, "Audio: {}, {} out{}, {} Hz, {} frames, {} ms latency",
              Pa_GetDeviceInfo(UNIV::AUDIO_DEVICE)->name, UNIV::CHANNELS, input,
              UNIV::SAMPLE_RATE, UNIV::NUM_FRAMES, std::lround(info->outputLatency * 1000.0));
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
    const PaError err = Pa_StopStream(stream);
    if (err != paNoError) {
        log::warn(log::audio, "could not stop the audio stream: {}", Pa_GetErrorText(err));
    }
    m_started = false;
}

bool AudioDevice::initMTAudio(int Num, bool ZeroLatency, std::string& Reason) {
    if (Num < 1 || unsigned(Num) > MAX_RT_AUDIO_THREADS) {
        Reason = "requested " + std::to_string(Num) + " realtime audio threads; the limit is " +
                 std::to_string(MAX_RT_AUDIO_THREADS);
        return false;
    }
    const unsigned n = unsigned(Num);
    const DspState* state = getDspState();
    if (state->numThreads != 0) {
        // Already up. Restarting would replace EXTThreads whose std::threads
        // are still joinable and free the buffers the live workers are reading
        // from, so the shape is fixed for the session; the per-worker closures
        // set just before this call are picked up on the next block.
        if (state->numThreads != n || state->zeroLatency != ZeroLatency) {
            Reason = "multi-threaded audio is already running with " +
                     std::to_string(state->numThreads) + " threads (zero-latency " +
                     (state->zeroLatency ? "on" : "off") + "); restart Extempore to change that";
            return false;
        }
        return true;
    }

    publishDspState([&](DspState& next) {
        next.mtReady = false;
        next.numThreads = n;
        next.zeroLatency = ZeroLatency;
    });
    m_toggle.store(1, std::memory_order_relaxed);
    m_inbuf.assign(size_t(UNIV::IN_CHANNELS) * UNIV::NUM_FRAMES, 0.0f);
    // two blocks per worker, for double buffering
    m_outbuf.assign(size_t(UNIV::CHANNELS) * UNIV::NUM_FRAMES * n * 2, 0.0f);
    for (unsigned i = 0; i < n; ++i) {
        m_threads[i] =
            std::make_unique<EXTThread>(audioCallbackMT, reinterpret_cast<void*>(uintptr_t(i)),
                                        std::string("MT_AUD_") + char('A' + i));
        m_threads[i]->start();
    }
    // Buffers, thread count and workers are all up -- publish them as a unit
    // to processFrames(), which gates the MT branch on mtReady.
    publishDspState([](DspState& next) { next.mtReady = true; });
    return true;
}

std::span<SAMPLE> AudioDevice::getMTOutSlice(unsigned Index) {
    const size_t block = size_t(UNIV::CHANNELS) * UNIV::NUM_FRAMES * 2;
    const size_t offset = block * Index;
    if (offset + block > m_outbuf.size()) {
        return std::span<SAMPLE>();
    }
    return std::span<SAMPLE>(m_outbuf.data() + offset, block);
}

double AudioDevice::getCPULoad() {
    // No stream in --noaudio or offline-render mode.
    PaStream* stream = AudioDevice::I()->getPaStream();
    return stream ? Pa_GetStreamCpuLoad(stream) : 0.0;
}

void AudioDevice::printDevices() {
    const PaError err = initialisePortAudio();
    if (err != paNoError) {
        log::error(log::audio, "could not initialise PortAudio: {}", Pa_GetErrorText(err));
        log::flush();
        std::_Exit(1);
    }

    const int numDevices = Pa_GetDeviceCount();
    if (numDevices <= 0) {
        log::error(log::audio, "no audio devices found");
        log::flush();
        std::_Exit(1);
    }
    // one per line: the index (or a regex on the name) is what --device takes
    const PaDeviceIndex defaultOutput = Pa_GetDefaultOutputDevice();
    const PaDeviceIndex defaultInput = Pa_GetDefaultInputDevice();
    for (int i = 0; i < numDevices; i++) {
        const PaDeviceInfo* deviceInfo = Pa_GetDeviceInfo(i);
        const PaHostApiInfo* apiInfo = Pa_GetHostApiInfo(deviceInfo->hostApi);
        printf("%d: %s (%s, %d in, %d out)%s%s\n", i, deviceInfo->name, apiInfo->name,
               deviceInfo->maxInputChannels, deviceInfo->maxOutputChannels,
               i == defaultOutput ? " [default output]" : "",
               i == defaultInput ? " [default input]" : "");
    }
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
