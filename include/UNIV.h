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

#ifndef UNIV_H
#define UNIV_H

#include <atomic>
#include <cstdint>
#include <BranchPrediction.h>

#include <string>
#include <vector>
#include <unordered_map>

#ifdef _WIN32
#include <SDKDDKVer.h>
#define WIN32_LEAN_AND_MEAN
#define EXPORT extern "C" __declspec(dllexport)
#undef min
#undef max
#else
#define EXPORT extern "C"
#endif

#if __APPLE__
#include <CoreAudio/HostTime.h>
#include <CoreFoundation/CFDate.h>
#endif

#define BILLION 1000000000L
#define D_BILLION 1000000000.0
#define D_MILLION 1000000.0

struct scheme;
struct s7_cell;
typedef struct s7_cell* pointer;

extern "C" {

EXPORT bool rmatch(char* regex, const char* str);
EXPORT int64_t rmatches(char* regex, char* str, char** results,
                        int64_t maxnum);  // struct regex_matched_buffer* result);
EXPORT bool rsplit(const char* regex, const char* str, char* a, char* b);
EXPORT char* rreplace(char* regex, char* str, char* replacement, char* result);
EXPORT char* base64_encode(const unsigned char* data, size_t input_length, size_t* output_length);
EXPORT unsigned char* base64_decode(const char* data, size_t input_length, size_t* output_length);
EXPORT char* cname_encode(char* data, size_t input_length, size_t* output_length);
EXPORT char* cname_decode(char* data, size_t input_length, size_t* output_length);
EXPORT const char* sys_sharedir();
EXPORT char* sys_slurp_file(const char* fname);
EXPORT int register_for_window_events();
}

namespace extemp {

namespace UNIV {

extern std::string SHARE_DIR;
EXPORT uint32_t CHANNELS;
EXPORT uint32_t IN_CHANNELS;
EXPORT uint32_t SAMPLE_RATE;
// TIME is written by the scheduler thread (TaskScheduler::timeSlice) and
// read from every other thread; atomic<uint64_t> gives a lock-free load/
// store on all tier-1 platforms.  DEVICE_TIME is written by the audio
// callback and read from other threads, same story.  The xtlang JIT
// accesses TIME via @TIME in runtime/bitcode.ll as a plain i64 load — OK
// because the layout of std::atomic<uint64_t> is a single uint64_t on
// all supported compilers.
EXPORT std::atomic<uint64_t> TIME;
extern std::atomic<uint64_t> DEVICE_TIME;
extern std::atomic<double> AUDIO_CLOCK_BASE;
extern std::atomic<double> AUDIO_CLOCK_NOW;
extern uint64_t TIME_DIVISION;
inline uint32_t SECOND() {
    return SAMPLE_RATE;
}
inline uint32_t MINUTE() {
    return SAMPLE_RATE * 60;
}
inline uint32_t HOUR() {
    return MINUTE() * 60;
}
EXPORT uint32_t NUM_FRAMES;
enum class TerminalMode : uint32_t {
    Ansi = 0,
    Cmd = 1,
    Basic = 2,
    NoColor = 3,
};
extern TerminalMode EXT_TERM;
extern bool EXT_LOADBASE;
extern bool AUDIO_NONE;
extern bool BATCH_MODE;
extern uint32_t AUDIO_DEVICE;
extern uint32_t AUDIO_IN_DEVICE;
extern std::string AUDIO_DEVICE_NAME;
extern std::string AUDIO_IN_DEVICE_NAME;
extern double AUDIO_OUTPUT_LATENCY;
// --audio-outfile <path>: if non-empty, DSP output is written to a WAV file
// via the offline FileAudioDriver instead of opening a PortAudio stream.
extern std::string AUDIO_OUTFILE_PATH;
// --duration <seconds>: optional hard cap on offline render length. 0 means
// render until (quit) is called.
extern double AUDIO_OUTFILE_DURATION;
extern double CLOCK_OFFSET;
extern std::unordered_map<std::string, std::string> CMDPARAMS;
extern std::string ARCH;
extern std::string CPU;
extern std::vector<std::string> ATTRS;
extern double midi2frq(double pitch);
extern double frqRatio(double semitones);
extern void initRand();
extern void printSchemeCell(scheme* sc, std::stringstream& ss, pointer cell, bool = false,
                            bool = true);

}  // namespace UNIV

}  // namespace extemp

#ifdef _WIN32
#include <chrono>
#include <Windows.h>
#endif

// clock/time
#ifdef _WIN32

extern "C" inline double getRealTime() {
    return double(std::chrono::duration_cast<std::chrono::nanoseconds>(
                      std::chrono::high_resolution_clock::now().time_since_epoch())
                      .count()) /
           D_BILLION;
}

#elif __linux__
#include <time.h>

extern "C" inline double getRealTime() {
    struct timespec t;
    clock_gettime(CLOCK_REALTIME, &t);
    return t.tv_sec + t.tv_nsec / D_BILLION;
}

#elif __APPLE__

#include <CoreAudio/HostTime.h>

extern "C" inline double getRealTime() {
    return CFAbsoluteTimeGetCurrent() + kCFAbsoluteTimeIntervalSince1970;
}

#endif

inline void ascii_text_color(bool Bold, unsigned Foreground, unsigned Background) {
    if (unlikely(extemp::UNIV::EXT_TERM == extemp::UNIV::TerminalMode::NoColor)) {
        return;
    }
#ifdef _WIN32
    extern int WINDOWS_COLORS[];
    extern int WINDOWS_BGCOLORS[];
    if (unlikely(extemp::UNIV::EXT_TERM == extemp::UNIV::TerminalMode::Cmd)) {
        Foreground = (Foreground > 7) ? 7 : Foreground;
        Background = (Background > 7) ? 0 : Background;
        HANDLE console = GetStdHandle(STD_OUTPUT_HANDLE);
        if (Background > 0) {
            SetConsoleTextAttribute(console,
                                    WINDOWS_COLORS[Foreground] | WINDOWS_BGCOLORS[Background]);
        } else {
            SetConsoleTextAttribute(console, WINDOWS_COLORS[Foreground]);
        }
        return;
    }
#endif  // #else
    // if simple term (that doesn't support defaults)
    // then default to black background and white text
    Foreground = (Foreground > 9 || Foreground == 8) ? 9 : Foreground;
    Background = (Background > 9 || Background == 8) ? 9 : Background;
    if (unlikely(extemp::UNIV::EXT_TERM == extemp::UNIV::TerminalMode::Basic)) {
        if (unlikely(Background == 9)) {
            Background = 0;
        }
        if (unlikely(Foreground == 9)) {
            Foreground = 7;
        }
    }
    printf("\x1b[%u;%u;%um", Bold, Foreground + 30, Background + 40);
    // #endif
}

inline void ascii_default() {
    ascii_text_color(false, 9, 9);
}
inline void ascii_normal() {
    ascii_text_color(false, 7, 9);
}
inline void ascii_error() {
    ascii_text_color(true, 1, 9);
}
inline void ascii_warning() {
    ascii_text_color(true, 3, 9);
}
inline void ascii_info() {
    ascii_text_color(true, 6, 9);
}

#endif
