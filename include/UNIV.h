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
#include <cstdio>

#include <string>
#include <string_view>
#include <vector>
#include <unordered_map>

#include "Log.h"

#ifdef _WIN32
#include <SDKDDKVer.h>
#define WIN32_LEAN_AND_MEAN
#define EXPORT extern "C" __declspec(dllexport)
#undef min
#undef max
#else
#define EXPORT extern "C"
#endif

struct scheme;
struct s7_cell;
typedef struct s7_cell* pointer;

extern "C" {

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

// Where the runtime files (runtime/, libs/, examples/) live.  Resolved once at
// startup by resolve_share_dir(); pass the value of --sharedir, or an empty
// view when it was not given.
extern std::string SHARE_DIR;
std::string resolve_share_dir(std::string_view explicit_dir);
// Where the generated files (libs/aot-cache/, libs/platform-shlibs/) live; see
// share_dir::pick_build_dir.  Resolved once at startup, after SHARE_DIR.
extern std::string BUILD_DIR;
std::string resolve_build_dir(std::string_view share_dir);
// A relative path names a file in the working directory, the build dir or the
// share dir, tried in that order; an absolute one is returned as it is.
std::string find_file(const std::string& path);
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
extern bool EXT_LOADBASE;
extern bool AUDIO_NONE;
// What this extempore was started to do: a live session, or one of the
// subcommands which start a runtime (src/CommandLine.cpp). Only a session has
// a banner, a utility process and servers. In a render the render thread owns
// the clock and advances it only between blocks, so audio time stands still
// while a task runs.
enum class Mode : uint8_t { Session, Exec, Test, Render, Compile };
extern Mode MODE;
extern uint32_t AUDIO_DEVICE;
extern uint32_t AUDIO_IN_DEVICE;
extern std::string AUDIO_DEVICE_NAME;
extern std::string AUDIO_IN_DEVICE_NAME;
extern double AUDIO_OUTPUT_LATENCY;
// extempore render: the WAV file the offline FileAudioDriver writes, and the
// length of audio to render, in seconds.
extern std::string AUDIO_OUTFILE_PATH;
extern double AUDIO_OUTFILE_DURATION;
// False under --no-legacy-server: the processes open no eval-server sockets
// (7099 and below); nREPL is then the only way in.
extern bool LEGACY_SERVER;
// The code said it failed (sys:note-failure); a run also fails when an error
// was logged. exec and render then exit with status 1.
extern std::atomic<bool> FAILED;
inline bool failed() {
    return FAILED.load() || log::errors() > 0;
}
// extempore render --seed: every random number generator starts from SEED
// rather than from the system's entropy.
extern bool SEEDED;
extern uint64_t SEED;
// The running extempore's own path, or "" if the platform won't say.
std::string executable_file();
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
#include <Windows.h>
#endif

#include <chrono>

// clock/time
//
// Wall-clock seconds since the Unix epoch, uniform across platforms via
// std::chrono::system_clock (Unix-epoch wall time everywhere). This replaced
// three per-platform clocks: the Windows high_resolution_clock had a
// boot-relative epoch (wrong for the cross-machine clock sync that consumes
// it), while Linux's clock_gettime(CLOCK_REALTIME) and the macOS CoreFoundation
// clock both returned exactly the Unix wall time system_clock already provides.
extern "C" inline double getRealTime() {
    return std::chrono::duration<double>(std::chrono::system_clock::now().time_since_epoch())
        .count();
}

#endif
