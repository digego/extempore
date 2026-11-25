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
#include "HangReport.h"
#include "NreplServer.h"
#include "NreplClient.h"
#include "LanguageServer.h"
#include "EXTLLVM.h"
#include "CommandLine.h"
#include "SchemeFFI.h"

#include "ExtemporeVersion.h"

#include "llvm/Support/Program.h"

#include <algorithm>
#include <array>
#include <cerrno>
#include <charconv>
#include <cstdint>
#include <chrono>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <iostream>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <thread>
#include <vector>

#ifndef _WIN32
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
        extemp::nrepl::removePortFile();
        TerminateProcess(GetCurrentProcess(), 1);
        return (TRUE);
    default:
        return FALSE;
    }
}

#else  // !_WIN32

// Only async-signal-safe calls here: write(2) and _Exit are on the POSIX safe
// list, printf/exit are not. AudioDevice::stop() (which finalises the WAV
// extempore render writes) is not safe to call from a handler either, so a
// render interrupted by a signal is left with a provisional header; (quit)
// is the clean way to end one.
void sig_handler(int Signo) {
    // a REPL which is waiting on an evaluation takes Ctrl-C as "stop that"
    if (Signo == SIGINT && extemp::nrepl::client::interruptFromSignal()) {
        return;
    }
    if (Signo == SIGINT) {
        static const char msg[] = "\nextempore: SIGINT, exiting\n";
        const ssize_t r = ::write(STDERR_FILENO, msg, sizeof(msg) - 1);
        (void)r;
        extemp::nrepl::removePortFile();
        _Exit(128 + SIGINT);
    } else if (Signo == SIGTERM) {
        static const char msg[] = "\nextempore: SIGTERM, exiting\n";
        const ssize_t r = ::write(STDERR_FILENO, msg, sizeof(msg) - 1);
        (void)r;
        extemp::nrepl::removePortFile();
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
    extemp::HangReport::install();
}

#endif

namespace {

struct Settings {
    std::string initexpr;
    int primary_port = 7099;
    bool nrepl = true;
    std::uint16_t nrepl_port = extemp::nrepl::DEFAULT_PORT;
    bool nrepl_port_given = false;
    std::string nrepl_bind = "127.0.0.1";
    bool repl_mode = false;
    std::string sharedir;  // --share-dir, empty when not given
    // exec (without --wait) and compile: exit once the code has finished,
    // nothing is scheduled and every process is idle
    bool exit_when_done = false;
    // the file a subcommand was given, looked for before anything starts
    std::string file;
};

enum class ParseResult : std::uint8_t { Continue, Exit };

// A Scheme string literal for Text.
std::string scheme_string(std::string_view Text) {
    std::string out = "\"";
    for (const char c : Text) {
        if (c == '\\' || c == '"') {
            out += '\\';
        }
        out += c;
    }
    return out + '"';
}

// Set Out from a whole-number option the parser has already checked; false if
// the number doesn't fit.
template <typename T>
bool set_whole(const extemp::cli::Invocation& Inv, std::string_view Name, T& Out) {
    const auto text = Inv.get(Name);
    if (!text) {
        return true;
    }
    T value{};
    const auto [end, ec] = std::from_chars(text->data(), text->data() + text->size(), value);
    if (ec != std::errc()) {
        std::cerr << "extempore: --" << Name << " " << *text << " is too large" << std::endl;
        return false;
    }
    Out = value;
    return true;
}

// extempore test with several files: each runs in a process of its own, so no
// file's result depends on the ones before it.
int run_test_files(const extemp::cli::Invocation& Inv) {
    const std::string self = extemp::UNIV::executable_file();
    if (self.empty()) {
        std::cerr << "extempore: can't find the extempore executable to run each test file with"
                  << std::endl;
        return 2;
    }
    std::vector<std::string> failed;
    for (const auto& file : Inv.files) {
        std::vector<std::string> args{self, "test", file};
        const auto options = extemp::cli::optionArguments(Inv);
        args.insert(args.end(), options.begin(), options.end());
        const std::vector<llvm::StringRef> argv(args.begin(), args.end());
        std::cout << "\n=== " << file << std::endl;
        std::string error;
        const int rc = llvm::sys::ExecuteAndWait(self, argv, std::nullopt, {}, 0, 0, &error);
        if (rc != 0) {
            failed.push_back(file);
            if (!error.empty()) {
                std::cerr << "extempore: " << error << std::endl;
            }
        }
    }
    std::cout << "\n" << Inv.files.size() << " test files, " << failed.size() << " failed";
    for (const auto& file : failed) {
        std::cout << "\n  " << file;
    }
    std::cout << std::endl;
    return failed.empty() ? 0 : 1;
}

// Act on a parsed command line (src/CommandLine.cpp). Returns Exit, with the
// process's exit status, when there is no runtime to start.
ParseResult apply_invocation(const extemp::cli::Invocation& Inv, Settings& settings, int& status) {
    using namespace extemp;
    using cli::Command;
    status = 0;
    for (const auto& notice : Inv.notices) {
        std::cerr << "extempore: " << notice << std::endl;
    }
    if (!Inv.error.empty()) {
        std::cerr << "extempore: " << Inv.error << " (see extempore --help)" << std::endl;
        status = 2;
        return ParseResult::Exit;
    }
    if (Inv.help) {
        std::cout << cli::help(Inv.command);
        return ParseResult::Exit;
    }
    switch (Inv.command) {
    case Command::Version:
        std::cout << "Extempore " << EXTEMPORE_VERSION << std::endl;
        return ParseResult::Exit;
    case Command::Devices:
        AudioDevice::printDevices();
        return ParseResult::Exit;
    case Command::Send:
    case Command::Repl:
    case Command::Lsp:
    case Command::Status: {
        // clients of a running extempore: nothing of the runtime starts here
        nrepl::client::Target target;
        if (const auto host = Inv.get("host")) {
            target.host = std::string(*host);
        }
        std::uint16_t port = 0;
        if (Inv.has("port")) {
            if (!set_whole(Inv, "port", port)) {
                status = 2;
                return ParseResult::Exit;
            }
            target.port = port;
        }
        target.process = std::string(Inv.get("process").value_or(""));
        if (Inv.command == Command::Status) {
            status = nrepl::client::status(target);
        } else if (Inv.command == Command::Lsp) {
            status = lsp::run(target);
        } else if (Inv.command == Command::Repl) {
            status = nrepl::client::repl(target, false);
        } else {
            nrepl::client::SendOptions options;
            if (!Inv.files.empty()) {
                options.code = Inv.files.front();
            }
            options.file = std::string(Inv.get("file").value_or(""));
            if (!set_whole(Inv, "line", options.line) ||
                !set_whole(Inv, "column", options.column)) {
                status = 2;
                return ParseResult::Exit;
            }
            status = nrepl::client::send(target, options);
        }
        return ParseResult::Exit;
    }
    case Command::Test:
        if (Inv.files.size() > 1) {
            status = run_test_files(Inv);
            return ParseResult::Exit;
        }
        break;
    default:
        break;
    }

    // options of every subcommand which starts a runtime
    if (const auto dir = Inv.get("share-dir")) {
        settings.sharedir = std::string(*dir);
    }
    if (Inv.has("no-base")) {
        UNIV::EXT_LOADBASE = false;
    }
    bool ok = set_whole(Inv, "opt-level", EXTLLVM::OPTIMIZATION_LEVEL) &&
              set_whole(Inv, "sample-rate", UNIV::SAMPLE_RATE) &&
              set_whole(Inv, "channels", UNIV::CHANNELS) &&
              set_whole(Inv, "frames", UNIV::NUM_FRAMES) &&
              set_whole(Inv, "in-channels", UNIV::IN_CHANNELS);
    if (ok && (UNIV::SAMPLE_RATE == 0 || UNIV::NUM_FRAMES == 0 || UNIV::CHANNELS == 0)) {
        std::cerr << "extempore: --sample-rate, --frames and --channels must be more than 0"
                  << std::endl;
        ok = false;
    }
    if (!ok) {
        status = 2;
        return ParseResult::Exit;
    }
    for (const auto& [name, value] : Inv.scriptArgs) {
        UNIV::CMDPARAMS[name] = value;
    }
    const std::string file = Inv.files.empty() ? std::string() : Inv.files.front();
    settings.file = file;
    // loads File, and has the run exit with status 1 if a form in it failed
    const auto load = [](const std::string& File) {
        return "(if (not (sys:load " + scheme_string(File) + ")) (sys:note-failure))";
    };

    switch (Inv.command) {
    case Command::Session: {
        const std::string expr(Inv.get("e").value_or(""));
        if (!file.empty() && !expr.empty()) {
            settings.initexpr = "(begin (sys:load " + scheme_string(file) + ") " + expr + ")";
        } else if (!file.empty()) {
            settings.initexpr = "(sys:load " + scheme_string(file) + ")";
        } else {
            settings.initexpr = expr;
        }
        // a device is an index, or a regex on its name
        const auto device = [&](std::string_view Name, uint32_t& Index, std::string& Pattern) {
            if (const auto value = Inv.get(Name)) {
                const auto [end, ec] =
                    std::from_chars(value->data(), value->data() + value->size(), Index);
                if (ec != std::errc() || end != value->data() + value->size()) {
                    Pattern = std::string(*value);
                }
            }
        };
        device("device", UNIV::AUDIO_DEVICE, UNIV::AUDIO_DEVICE_NAME);
        device("in-device", UNIV::AUDIO_IN_DEVICE, UNIV::AUDIO_IN_DEVICE_NAME);
        int latency = 0;
        if (!set_whole(Inv, "latency", latency)) {
            status = 2;
            return ParseResult::Exit;
        }
        UNIV::AUDIO_OUTPUT_LATENCY = latency / 1000.0;
        UNIV::AUDIO_NONE = Inv.has("no-audio");
        if (Inv.has("realtime")) {
#ifdef _WIN32
            SetPriorityClass(GetCurrentProcess(), REALTIME_PRIORITY_CLASS);
#else
            std::cerr << "extempore: --realtime is for Windows only" << std::endl;
#endif
        }
        if (Inv.has("repl")) {
#ifndef _WIN32
            settings.repl_mode = true;
#else
            std::cerr << "extempore: --repl is not supported on Windows" << std::endl;
            status = 2;
            return ParseResult::Exit;
#endif
        }
        if (Inv.has("nrepl-port")) {
            settings.nrepl_port_given = true;
        }
        if (!set_whole(Inv, "nrepl-port", settings.nrepl_port) ||
            !set_whole(Inv, "legacy-port", settings.primary_port)) {
            status = 2;
            return ParseResult::Exit;
        }
        if (const auto bind = Inv.get("nrepl-bind")) {
            settings.nrepl_bind = std::string(*bind);
        }
        if (Inv.has("no-nrepl")) {
            settings.nrepl = false;
        }
        if (Inv.has("no-legacy-server")) {
            UNIV::LEGACY_SERVER = false;
        }
    } break;
    case Command::Exec:
        UNIV::MODE = UNIV::Mode::Exec;
        settings.exit_when_done = !Inv.has("wait");
        settings.initexpr = file.empty() ? std::string(Inv.get("e").value_or("")) : load(file);
        break;
    case Command::Test: {
        UNIV::MODE = UNIV::Mode::Test;
        std::string report;
        if (const auto dir = Inv.get("junit")) {
            std::error_code ec;
            std::filesystem::create_directories(std::filesystem::path(*dir), ec);
            // named as ctest names them: the file's path with its slashes
            // flattened
            std::string safe = file;
            std::ranges::replace(safe, '/', '_');
            std::ranges::replace(safe, '\\', '_');
            std::ranges::replace(safe, ':', '_');
            report = " " + scheme_string((std::filesystem::path(*dir) / (safe + ".xml")).string());
        }
        settings.initexpr = "(xtmtest-run-tests " + scheme_string(file) + " #t #t" + report + ")";
    } break;
    case Command::Render: {
        UNIV::MODE = UNIV::Mode::Render;
        UNIV::AUDIO_OUTFILE_PATH = std::string(Inv.get("o").value_or(""));
        UNIV::AUDIO_OUTFILE_DURATION =
            std::strtod(std::string(Inv.get("duration").value_or("0")).c_str(), nullptr);
        UNIV::SEEDED = true;
        if (!set_whole(Inv, "seed", UNIV::SEED)) {
            status = 2;
            return ParseResult::Exit;
        }
        settings.initexpr = load(file);
    } break;
    case Command::Compile:
        UNIV::MODE = UNIV::Mode::Compile;
        UNIV::EXT_LOADBASE = false;  // a library loads what it needs; an executable never has base
        settings.exit_when_done = true;
        if (const auto arch = Inv.get("arch")) {
            UNIV::ARCH = std::string(*arch);
        }
        if (const auto cpu = Inv.get("cpu")) {
            UNIV::CPU = std::string(*cpu);
        }
        for (const auto attr : Inv.all("attr")) {
            UNIV::ATTRS.emplace_back(attr);
        }
        if (Inv.has("exe")) {
            // xtc:aot:compile-xtm-exe reads these two with sys:cmdarg
            if (const auto link = Inv.get("link")) {
                UNIV::CMDPARAMS["link"] = std::string(*link);
            }
            if (Inv.has("dll")) {
                UNIV::CMDPARAMS["dll"] = "";
            }
            settings.initexpr = "(xtc:aot:compile-xtm-exe " + scheme_string(file) + ")";
        } else if (Inv.has("dll")) {
            settings.initexpr = "(xtc:aot:compile-xtm-file " + scheme_string(file) + " #t)";
        } else if (const auto name = Inv.get("name")) {
            settings.initexpr = "(xtc:aot:compile-xtm-file " + scheme_string(file) + " #f " +
                                scheme_string(*name) + ")";
        } else {
            settings.initexpr = "(xtc:aot:compile-xtm-file " + scheme_string(file) + ")";
        }
        break;
    case Command::Send:
    case Command::Repl:
    case Command::Lsp:
    case Command::Status:
    case Command::Devices:
    case Command::Version:
        break;
    }

    // only a session and a render have an audio device (the render's is a file)
    if (UNIV::MODE != UNIV::Mode::Session && UNIV::MODE != UNIV::Mode::Render) {
        UNIV::AUDIO_NONE = true;
    }
    const std::string_view color = Inv.get("color").value_or("auto");
    if (color == "never" || (color == "auto" && (!UNIV::is_terminal(UNIV::message_stream()) ||
                                                 std::getenv("NO_COLOR") != nullptr))) {
        UNIV::EXT_TERM = UNIV::TerminalMode::NoColor;
    }
    return ParseResult::Continue;
}

}  // namespace

EXPORT int extempore_init(int argc, char** argv) {
    const std::string primary_name("primary");
    const std::string utility_name("utility");
    // Under ctest or a pipe stdout would be fully buffered, and a process
    // killed for hanging would lose the lines which say how far it got.
    std::setvbuf(stdout, nullptr, _IOLBF, BUFSIZ);
#ifndef _WIN32
    install_signal_handlers();
#else
    WSADATA wsadata;
    WSAStartup(0x0202, &wsadata);  // I didn't seem to need to call this... but (?)
    SetConsoleCtrlHandler(PHANDLER_ROUTINE(CtrlHandler), TRUE);
#endif

    Settings settings;
    int status = 0;
    const std::vector<std::string_view> args(argv + (argc > 0 ? 1 : 0), argv + argc);
    if (apply_invocation(extemp::cli::parse(args), settings, status) == ParseResult::Exit) {
        return status;
    }
    extemp::UNIV::SHARE_DIR = extemp::UNIV::resolve_share_dir(settings.sharedir);
    extemp::UNIV::BUILD_DIR = extemp::UNIV::resolve_build_dir(extemp::UNIV::SHARE_DIR);
    if (!settings.file.empty()) {
        // sys:load looks in the working directory, then in the share dir
        std::error_code ec;
        const std::filesystem::path file(settings.file);
        if (!std::filesystem::exists(file, ec) &&
            !std::filesystem::exists(std::filesystem::path(extemp::UNIV::SHARE_DIR) / file, ec)) {
            std::cerr << "extempore: can't find " << settings.file << std::endl;
            return 2;
        }
    }
    const int primary_port = settings.primary_port;
    const int utility_port = primary_port - 1;
    const bool session = extemp::UNIV::MODE == extemp::UNIV::Mode::Session;
    if (session) {
        ascii_normal();
        std::cout << std::endl;
        std::cout << "------------- Extempore -------------- " << std::endl;
        ascii_default();
        std::cout << EXTEMPORE_VERSION << std::endl;
        std::cout << "(c) Andrew Sorensen, Ben Swift" << std::endl;
        std::cout << "ben@benswift.me" << std::endl;
        std::cout << std::endl;
        ascii_default();
    }
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
    // an offline render runs the scheduler itself, from its render thread
    if (extemp::UNIV::MODE != extemp::UNIV::Mode::Render) {
        extemp::TaskScheduler::I()->start();
    }
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
    if (session) {
        ascii_normal();
        std::cout << "Primary        : ";
        ascii_info();
        std::cout << "thread 0" << std::endl;
        ascii_default();
        std::cout << "---------------------------------------" << std::endl;
        ascii_default();
    }

    // The processes below live for the whole run and are never
    // destroyed: primary->start(true) subsumes this thread and (quit) ends the
    // process with _Exit, so they are deliberately leaked rather than owned.
    if (!session) {
        // every subcommand: a single process, no servers, no utility process
        auto* primary = new extemp::SchemeProcess(extemp::UNIV::SHARE_DIR, primary_name,
                                                  primary_port, 0, settings.initexpr);
        extemp::AudioDevice::startFileDriver();
        if (settings.exit_when_done) {
            std::thread([] {
                const std::atomic<bool> never{false};
                extemp::SchemeProcess::waitAllDone(never);
                extemp::SchemeFFI::exitExtempore(extemp::UNIV::FAILED.load() ? 1 : 0);
            }).detach();
        }
        primary->start(true);  // this will not return
        return 0;
    }

    // a session: utility + primary processes with server threads
    auto* utility =
        new extemp::SchemeProcess(extemp::UNIV::SHARE_DIR, utility_name, utility_port, 0);
    if (!utility->start()) {
        ascii_error();
        printf("ERROR:");
        ascii_default();
        std::cout << " one or more processes failed to start, exiting." << std::endl;
        exit(1);
    }
    auto* primary = new extemp::SchemeProcess(extemp::UNIV::SHARE_DIR, primary_name, primary_port,
                                              0, settings.initexpr);

    // The default port being taken (usually by another extempore) costs only
    // the nREPL server; a port asked for by name is worth stopping for, and so
    // is a --repl with no server to talk to. The utility process's threads are
    // already running, so end the process as (quit) does: exit() would run
    // static destructors under them.
    const bool serving =
        settings.nrepl && extemp::nrepl::start(settings.nrepl_bind, settings.nrepl_port);
    if (settings.nrepl && !serving && (settings.nrepl_port_given || settings.repl_mode)) {
        if (settings.repl_mode && !settings.nrepl_port_given) {
            std::cout << "--repl needs the nREPL server: give it a free port with --nrepl-port"
                      << std::endl;
        }
        std::fflush(stdout);
        std::_Exit(1);
    }

    if (settings.repl_mode) {
        // Runs for the life of the process and needs none of EXTThread's
        // scheduling features, so a detached std::thread does. It talks to
        // this session's own server, whichever port that got.
        extemp::nrepl::client::Target target;
        target.host = settings.nrepl_bind == "0.0.0.0" ? "127.0.0.1" : settings.nrepl_bind;
        target.port = extemp::nrepl::boundPort();
        std::thread([target] { extemp::nrepl::client::repl(target, true); }).detach();
    }

    extemp::AudioDevice::startFileDriver();
    // the primary process runs on this thread (process thread 0)
    primary->start(true);  // this will not return
    return 0;
}

int main(int argc, char** argv) {
    return extempore_init(argc, argv);
}
