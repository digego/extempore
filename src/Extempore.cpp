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

#include "UNIV.h"
#include "SchemeProcess.h"
#include "AudioDevice.h"
#include "TaskScheduler.h"
#include "SchemeREPL.h"
#include "EXTLLVM.h"

#include "ExtemporeVersion.h"

#include <algorithm>
#include <array>
#include <cerrno>
#include <charconv>
#include <chrono>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <thread>

#ifndef _WIN32
#include "LinenoiseREPL.h"
#include <signal.h>
#include <unistd.h>
#else
#undef min
#undef max
#include "llvm/TargetParser/Host.h"
#endif

#ifdef __APPLE__
#include <Cocoa/Cocoa.h>
#include <AppKit/AppKit.h>
#endif

// WARNING EVIL WINDOWS TERMINATION CODE!
#ifdef _WIN32

BOOL CtrlHandler(DWORD fdwCtrlType) {
    switch (fdwCtrlType) {
    case CTRL_C_EVENT:
        TerminateProcess(GetCurrentProcess(), 1);
        return (TRUE);
    default:
        return FALSE;
    }
}

#else  // !_WIN32

// Only async-signal-safe calls here: write(2) and _Exit are on the POSIX safe
// list, printf/exit are not. AudioDevice::stop() (which finalises a WAV
// written by --audio-outfile) is not safe to call from a handler either, so a
// render interrupted by a signal is left with a provisional header; (quit)
// is the clean way to end one.
void sig_handler(int Signo) {
    if (Signo == SIGINT) {
        static const char msg[] = "\nextempore: SIGINT, exiting\n";
        ssize_t r = ::write(STDERR_FILENO, msg, sizeof(msg) - 1);
        (void)r;
        _Exit(128 + SIGINT);
    } else if (Signo == SIGTERM) {
        static const char msg[] = "\nextempore: SIGTERM, exiting\n";
        ssize_t r = ::write(STDERR_FILENO, msg, sizeof(msg) - 1);
        (void)r;
        _Exit(128 + SIGTERM);
    }
}

static void install_signal_handlers() {
    struct sigaction sa;
    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = sig_handler;
    sigemptyset(&sa.sa_mask);
    if (sigaction(SIGINT, &sa, nullptr) != 0) {
        printf("\nWarning: can't catch SIGINT.\n");
    }
    if (sigaction(SIGTERM, &sa, nullptr) != 0) {
        printf("\nWarning: can't catch SIGTERM.\n");
    }
}

#endif

namespace {

enum class Opt {
    Help,
    Version,
    Run,
    Eval,
    Batch,
    Port,
    Term,
    ShareDir,
    Runtime,
    NoBase,
    OptLevel,
    SampleRate,
    Frames,
    Channels,
    InChannels,
    NoAudio,
    TimeDiv,
    Device,
    InDevice,
    DeviceName,
    InDeviceName,
    Latency,
    Realtime,
    Arch,
    Cpu,
    Attr,
    Compile,
    Repl,
    PrintDevices,
    AudioOutfile,
    Duration,
};

enum class Arg { None, Required };

struct OptionSpec {
    Opt id;
    std::string_view name;  // including the leading "--"
    Arg arg;
    std::string_view help;
};

// The one table behind both parsing and --help; --help lists it in this order.
constexpr std::array<OptionSpec, 31> kOptions{{
    {Opt::Help, "--help", Arg::None, "prints this menu"},
    {Opt::Version, "--version", Arg::None, "prints the Extempore version and exits"},
    {Opt::Run, "--run", Arg::Required, "path to a scheme file to load at startup"},
    {Opt::Eval, "--eval", Arg::Required, "scheme expression to evaluate at startup"},
    {Opt::Batch, "--batch", Arg::Required,
     "run in batch mode (no server, single process, no audio) with given expression"},
    {Opt::Port, "--port", Arg::Required, "port for primary process [7099]"},
    {Opt::Term, "--term", Arg::Required,
     "either ansi, cmd (windows), basic (for simpler ansi terms), or nocolor"},
    {Opt::ShareDir, "--sharedir", Arg::Required,
     "location of the Extempore share dir (which contains runtime/, libs/, examples/, etc.)"},
    {Opt::Runtime, "--runtime", Arg::Required, "[deprecated] use --sharedir instead"},
    {Opt::NoBase, "--nobase", Arg::None, "don't load base lib on startup"},
    {Opt::OptLevel, "--opt-level", Arg::Required, "LLVM optimization level 0-3"},
    {Opt::SampleRate, "--samplerate", Arg::Required, "audio samplerate"},
    {Opt::Frames, "--frames", Arg::Required, "attempts to force frames [1024]"},
    {Opt::Channels, "--channels", Arg::Required,
     "attempts to force num of output audio channels"},
    {Opt::InChannels, "--inchannels", Arg::Required,
     "attempts to force num of input audio channels"},
    {Opt::NoAudio, "--noaudio", Arg::None,
     "no audio output: use a \"dummy\" device (overrides --device option)"},
    {Opt::TimeDiv, "--timediv", Arg::Required,
     "timed sub divisions of FRAMES for scheduling engine (1 = no division which is the "
     "default)"},
    {Opt::Device, "--device", Arg::Required,
     "the index of the audio device to use (output or duplex)"},
    {Opt::InDevice, "--indevice", Arg::Required, "the index of the audio input device to use"},
    {Opt::DeviceName, "--device-name", Arg::Required,
     "a regex to match the name of the audio device to use (output or duplex) (overrides "
     "index)"},
    {Opt::InDeviceName, "--indevice-name", Arg::Required,
     "a regex to match the name of the audio input device to use (overrides index)"},
    {Opt::Latency, "--latency", Arg::Required, "attempts to force audio output latency"},
    {Opt::Realtime, "--realtime", Arg::None, "use realtime process priority (Windows only)"},
    {Opt::Arch, "--arch", Arg::Required, "the target architecture [current host]"},
    {Opt::Cpu, "--cpu", Arg::Required, "the target cpu [current host]"},
    {Opt::Attr, "--attr", Arg::Required, "additional target attributes (allows multiple)"},
    {Opt::Compile, "--compile", Arg::Required, "compiles xtm file to native executable"},
    {Opt::Repl, "--repl", Arg::None, "start an interactive REPL (Linux/macOS only)"},
    {Opt::PrintDevices, "--print-devices", Arg::None,
     "print the available audio devices to console"},
    {Opt::AudioOutfile, "--audio-outfile", Arg::Required,
     "render DSP output to the given WAV file (float32) instead of an audio device"},
    {Opt::Duration, "--duration", Arg::Required,
     "hard cap on --audio-outfile render length (seconds); 0 = render until (quit)"},
}};

const OptionSpec* find_option(std::string_view name) {
    for (const auto& spec : kOptions) {
        if (spec.name == name) {
            return &spec;
        }
    }
    return nullptr;
}

void print_help() {
    size_t width = 0;
    for (const auto& spec : kOptions) {
        width = std::max(width, spec.name.size());
    }
    std::cout << "Extempore's command line options: " << std::endl;
    for (const auto& spec : kOptions) {
        std::cout << std::string(width + 4 - spec.name.size(), ' ') << spec.name << ": "
                  << spec.help << std::endl;
    }
}

// Whole-string integer parse; false (with a message) on anything else, so
// "--samplerate abc" is an error rather than a sample rate of zero.
template <typename T> bool parse_integer(std::string_view option, std::string_view text, T& out) {
    T value{};
    auto [end, ec] = std::from_chars(text.data(), text.data() + text.size(), value);
    if (ec != std::errc() || end != text.data() + text.size()) {
        std::cout << "Error: " << option << " expects a whole number, got \"" << text << "\""
                  << std::endl;
        return false;
    }
    out = value;
    return true;
}

// strtod rather than from_chars: libc++ only gained floating-point from_chars
// recently and macOS ships older versions.
bool parse_real(std::string_view option, std::string_view text, double& out) {
    std::string copy(text);
    char* end = nullptr;
    errno = 0;
    double value = strtod(copy.c_str(), &end);
    if (copy.empty() || end != copy.c_str() + copy.size() || errno == ERANGE) {
        std::cout << "Error: " << option << " expects a number, got \"" << text << "\""
                  << std::endl;
        return false;
    }
    out = value;
    return true;
}

// Double every backslash in a string, so Windows paths survive embedding in a
// Scheme string literal.
std::string escape_backslashes(std::string str) {
    size_t start_pos = 0;
    const std::string from("\\");
    const std::string to("\\\\");
    while ((start_pos = str.find(from, start_pos)) != std::string::npos) {
        str.replace(start_pos, from.length(), to);
        start_pos += to.length();
    }
    return str;
}

struct Settings {
    std::string initexpr;
    int primary_port = 7099;
    bool repl_mode = false;
    std::string sharedir;  // --sharedir, empty when not given
};

enum class ParseResult { Continue, Exit };

// Apply one parsed option. `value` is set for Arg::Required options.
// Returns Exit (with status) when the option ends the run (--help, --version,
// --print-devices, or an invalid value).
ParseResult apply_option(const OptionSpec& spec, std::string_view value, Settings& settings,
                         int& status) {
    using namespace extemp;
    status = 0;
    switch (spec.id) {
    case Opt::Compile:
        UNIV::EXT_LOADBASE = false;  // never load base when compiling
        settings.initexpr =
            "(xtc:aot:compile-xtm-exe \"" + escape_backslashes(std::string(value)) + "\")";
        break;
    case Opt::ShareDir:
    case Opt::Runtime:
        settings.sharedir = std::string(value);
        break;
    case Opt::SampleRate:
        if (!parse_integer(spec.name, value, UNIV::SAMPLE_RATE)) {
            status = 1;
            return ParseResult::Exit;
        }
        if (UNIV::SAMPLE_RATE == 0) {
            std::cout << "Error: --samplerate must be a positive number" << std::endl;
            status = 1;
            return ParseResult::Exit;
        }
        break;
    case Opt::Frames:
        if (!parse_integer(spec.name, value, UNIV::NUM_FRAMES)) {
            status = 1;
            return ParseResult::Exit;
        }
        break;
    case Opt::Channels:
        if (!parse_integer(spec.name, value, UNIV::CHANNELS)) {
            status = 1;
            return ParseResult::Exit;
        }
        break;
    case Opt::InChannels:
        if (!parse_integer(spec.name, value, UNIV::IN_CHANNELS)) {
            status = 1;
            return ParseResult::Exit;
        }
        break;
    case Opt::Eval:
        settings.initexpr = std::string(value);
        break;
    case Opt::Batch:
        settings.initexpr = std::string(value);
        UNIV::BATCH_MODE = true;
        // AUDIO_NONE is set after parsing, only if --audio-outfile wasn't passed
        break;
    case Opt::Run:
        settings.initexpr = "(sys:load \"" + escape_backslashes(std::string(value)) + "\")";
        break;
    case Opt::NoBase:
        UNIV::EXT_LOADBASE = false;
        break;
    case Opt::Port:
        if (!parse_integer(spec.name, value, settings.primary_port)) {
            status = 1;
            return ParseResult::Exit;
        }
        break;
    case Opt::Term:
        if (value == "cmd") {
            UNIV::EXT_TERM = UNIV::TerminalMode::Cmd;
        } else if (value == "basic") {
            UNIV::EXT_TERM = UNIV::TerminalMode::Basic;
        } else if (value == "nocolor") {
            UNIV::EXT_TERM = UNIV::TerminalMode::NoColor;
        } else if (value == "ansi") {
            UNIV::EXT_TERM = UNIV::TerminalMode::Ansi;
        } else {
#ifdef _WIN32
            UNIV::EXT_TERM = UNIV::TerminalMode::Cmd;
#else
            UNIV::EXT_TERM = UNIV::TerminalMode::Ansi;
#endif
        }
        break;
    case Opt::NoAudio:
        UNIV::AUDIO_NONE = true;
        break;
    case Opt::TimeDiv:
        if (!parse_integer(spec.name, value, UNIV::TIME_DIVISION)) {
            status = 1;
            return ParseResult::Exit;
        }
        if (UNIV::TIME_DIVISION == 0) {
            std::cout << "Error: --timediv must be a positive number" << std::endl;
            status = 1;
            return ParseResult::Exit;
        }
        break;
    case Opt::Device:
        if (!parse_integer(spec.name, value, UNIV::AUDIO_DEVICE)) {
            status = 1;
            return ParseResult::Exit;
        }
        break;
    case Opt::InDevice:
        if (!parse_integer(spec.name, value, UNIV::AUDIO_IN_DEVICE)) {
            status = 1;
            return ParseResult::Exit;
        }
        break;
    case Opt::DeviceName:
        UNIV::AUDIO_DEVICE_NAME = std::string(value);
        break;
    case Opt::InDeviceName:
        UNIV::AUDIO_IN_DEVICE_NAME = std::string(value);
        break;
    case Opt::Latency: {
        int ms = 0;
        if (!parse_integer(spec.name, value, ms)) {
            status = 1;
            return ParseResult::Exit;
        }
        UNIV::AUDIO_OUTPUT_LATENCY = ms / 1000.0;
    } break;
    case Opt::PrintDevices:
        AudioDevice::printDevices();
        return ParseResult::Exit;
    case Opt::Realtime:
#ifdef _WIN32
        SetPriorityClass(GetCurrentProcess(), REALTIME_PRIORITY_CLASS);
#else
        std::cout << "Realtime priority setting not available on your platform" << std::endl;
#endif
        break;
    case Opt::Arch:
        UNIV::ARCH = std::string(value);
        break;
    case Opt::Cpu:
        UNIV::CPU = std::string(value);
        break;
    case Opt::Attr:
        UNIV::ATTRS.push_back(std::string(value));
        break;
    case Opt::OptLevel:
        if (!parse_integer(spec.name, value, EXTLLVM::OPTIMIZATION_LEVEL)) {
            status = 1;
            return ParseResult::Exit;
        }
        break;
    case Opt::AudioOutfile:
        UNIV::AUDIO_OUTFILE_PATH = std::string(value);
        break;
    case Opt::Duration:
        if (!parse_real(spec.name, value, UNIV::AUDIO_OUTFILE_DURATION)) {
            status = 1;
            return ParseResult::Exit;
        }
        break;
    case Opt::Repl:
#ifndef _WIN32
        settings.repl_mode = true;
#else
        std::cout << "--repl is not supported on Windows" << std::endl;
        status = 1;
        return ParseResult::Exit;
#endif
        break;
    case Opt::Version:
        std::cout << "Extempore " << EXTEMPORE_VERSION << std::endl;
        return ParseResult::Exit;
    case Opt::Help:
        print_help();
        return ParseResult::Exit;
    }
    return ParseResult::Continue;
}

// Parse the command line: known options take a value as "--x v" or "--x=v";
// an unknown "--name[=value]" is recorded in UNIV::CMDPARAMS for Scheme code
// to read (with a warning); a single-dash argument is an error.
ParseResult parse_args(std::span<char*> args, Settings& settings, int& status) {
    status = 0;
    for (size_t i = 0; i < args.size(); ++i) {
        std::string_view arg(args[i]);
        if (!arg.starts_with("--")) {
            if (arg.starts_with("-")) {
                std::cout << "Poorly formed argument: " << arg << std::endl;
                status = 1;
                return ParseResult::Exit;
            }
            std::cout << "Warning: ignoring stray argument: " << arg << std::endl;
            continue;
        }
        std::string_view name = arg;
        std::optional<std::string_view> inline_value;
        if (auto eq = arg.find('='); eq != std::string_view::npos) {
            name = arg.substr(0, eq);
            inline_value = arg.substr(eq + 1);
        }
        const auto* spec = find_option(name);
        if (!spec) {
            extemp::UNIV::CMDPARAMS[std::string(name.substr(2))] =
                std::string(inline_value.value_or(""));
            ascii_warning();
            std::cout << "**** WARNING: Setting non-standard option: " << name.substr(2);
            if (inline_value) {
                std::cout << " to " << *inline_value;
            }
            std::cout << std::endl << std::flush;
            ascii_default();
            continue;
        }
        std::string_view value;
        if (spec->arg == Arg::Required) {
            if (inline_value) {
                value = *inline_value;
            } else if (i + 1 < args.size()) {
                value = args[++i];
            } else {
                std::cout << "Error: option " << spec->name << " requires a value" << std::endl;
                status = 1;
                return ParseResult::Exit;
            }
        } else if (inline_value) {
            std::cout << "Error: option " << spec->name << " does not take a value" << std::endl;
            status = 1;
            return ParseResult::Exit;
        }
        if (apply_option(*spec, value, settings, status) == ParseResult::Exit) {
            return ParseResult::Exit;
        }
    }
    return ParseResult::Continue;
}

}  // namespace

EXPORT int extempore_init(int argc, char** argv) {
    const std::string host("localhost");
    const std::string primary_name("primary");
    const std::string utility_name("utility");
#ifndef _WIN32
    install_signal_handlers();
#else
    WSADATA wsadata;
    WSAStartup(0x0202, &wsadata);  // I didn't seem to need to call this... but (?)
    SetConsoleCtrlHandler(PHANDLER_ROUTINE(CtrlHandler), TRUE);
#endif

    Settings settings;
    int status = 0;
    if (parse_args(std::span<char*>(argv + 1, size_t(argc > 0 ? argc - 1 : 0)), settings,
                   status) == ParseResult::Exit) {
        return status;
    }
    extemp::UNIV::SHARE_DIR = extemp::UNIV::resolve_share_dir(settings.sharedir);
    const int primary_port = settings.primary_port;
    const int utility_port = primary_port - 1;
    // --batch implies --noaudio only when --audio-outfile was not specified.
    // --audio-outfile lets batch runs render DSP to a file via the offline driver.
    if (extemp::UNIV::BATCH_MODE && extemp::UNIV::AUDIO_OUTFILE_PATH.empty()) {
        extemp::UNIV::AUDIO_NONE = true;
    }
    if (!extemp::UNIV::AUDIO_OUTFILE_PATH.empty() && extemp::UNIV::AUDIO_NONE) {
        ascii_error();
        std::cout << "Error: --audio-outfile cannot be combined with --noaudio" << std::endl;
        ascii_normal();
        return 1;
    }
    ascii_normal();
    std::cout << std::endl;
    std::cout << "------------- Extempore -------------- " << std::endl;
    ascii_default();
    std::cout << EXTEMPORE_VERSION << std::endl;
    std::cout << "(c) Andrew Sorensen, Ben Swift" << std::endl;
    std::cout << "ben@benswift.me" << std::endl;
    std::cout << std::endl;
    ascii_default();
#ifdef _WIN32
    // on Windows with MCJIT we need to add "-elf" to the target triple, see
    // http://lists.cs.uiuc.edu/pipermail/llvmdev/2013-December/068407.html
    if (extemp::UNIV::ARCH.empty()) {
        extemp::UNIV::ARCH = llvm::sys::getProcessTriple() + "-elf";
    }
#endif

    if (extemp::UNIV::AUDIO_NONE) {
        if (extemp::UNIV::TIME_DIVISION == 1) {
            extemp::UNIV::TIME_DIVISION = 4;
        }
        extemp::TaskScheduler::I()->setFrames(extemp::UNIV::NUM_FRAMES);
    }
    extemp::TaskScheduler::I()->start();
    extemp::EXTLLVM::initLLVM();
#ifdef __APPLE__
    // we need to instantiate NSApp before potentially
    // calling something OSXy (like a window) inside
    // an initexpr.
    // We DONT want to start the run loop though as it
    // never exits - do that below
    [NSApplication sharedApplication];
#endif
    if (!extemp::UNIV::AUDIO_NONE) {
        extemp::AudioDevice* dev = extemp::AudioDevice::I();
        dev->start();
    }
    ascii_normal();
    std::cout << "Primary        : ";
    ascii_info();
    std::cout << "thread 0" << std::endl;
    ascii_default();
    std::cout << "---------------------------------------" << std::endl;
    ascii_default();

    // The processes and REPLs below live for the whole run and are never
    // destroyed: primary->start(true) subsumes this thread and (quit) ends the
    // process with _Exit, so they are deliberately leaked rather than owned.
    if (extemp::UNIV::BATCH_MODE) {
        // Batch mode: single process, no server, no utility process
        auto* primary = new extemp::SchemeProcess(extemp::UNIV::SHARE_DIR, primary_name,
                                                  primary_port, 0, settings.initexpr);
        primary->start(true);  // this will not return
        return 0;
    }

    // Normal mode: utility + primary processes with server threads
    auto* utility =
        new extemp::SchemeProcess(extemp::UNIV::SHARE_DIR, utility_name, utility_port, 0);
    bool startup_ok = utility->start();
    auto* utility_repl = new extemp::SchemeREPL(utility_name, utility);
    utility_repl->connectToProcessAtHostname(host, utility_port);

    if (!startup_ok) {
        ascii_error();
        printf("ERROR:");
        ascii_default();
        std::cout << " one or more processes failed to start, exiting." << std::endl;
        exit(1);
    }
    auto* primary = new extemp::SchemeProcess(extemp::UNIV::SHARE_DIR, primary_name,
                                              primary_port, 0, settings.initexpr);

    // Fire-and-forget helpers: they run for the life of the process and need
    // none of EXTThread's scheduling features, so a detached std::thread does.
    std::thread([primary, host, primary_name, primary_port] {
        std::this_thread::sleep_for(std::chrono::seconds(1));
        auto* primary_repl = new extemp::SchemeREPL(primary_name, primary);
        primary_repl->connectToProcessAtHostname(host, primary_port);
    }).detach();

#ifndef _WIN32
    if (settings.repl_mode) {
        std::thread([args = linenoise_repl_args{host, primary_port}]() mutable {
            linenoise_repl(&args);
        }).detach();
    }
#endif

    ascii_info();
    std::cout << "Listening on TCP port " << primary_port
              << " --- connect your editor to send code." << std::endl;
    ascii_default();

    // the primary process runs on this thread (process thread 0)
    primary->start(true);  // this will not return
    return 0;
}

int main(int argc, char** argv) {
    return extempore_init(argc, argv);
}
