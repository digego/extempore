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

#include "SimpleOpt.h"

#include "UNIV.h"
#include "SchemeProcess.h"
#include "AudioDevice.h"
#include "TaskScheduler.h"
#include "SchemeREPL.h"
#include <string>
#include "EXTLLVM.h"

#ifndef _WIN32
#include <unistd.h>
#include <signal.h>
#else
#undef min
#undef max
#include "llvm/Support/Host.h"
#endif

#ifdef __APPLE__
#include <Cocoa/Cocoa.h>
#include <AppKit/AppKit.h>
#endif

#define SUBSUME_PRIMARY

// main callback for use by XTLang code
void (*XTMMainCallback)();

extern "C" {
  void xtm_set_main_callback( void(*f)() )
  {
    XTMMainCallback = f;
    return;
  }
}

int pass_primary_port = 7099; // lazy :(

void* extempore_primary_repl_delayed_connect(void* dat)
{
    extemp::SchemeProcess* primary = (extemp::SchemeProcess*) dat;
    std::string host("localhost");
    std::string primary_name("primary");
    int primary_port = pass_primary_port;
#ifdef _WIN32
	Sleep(1000);
#else
    sleep(1);
#endif
    extemp::SchemeREPL* primary_repl = new extemp::SchemeREPL(primary_name, primary);
    primary_repl->connectToProcessAtHostname(host, primary_port);
    return NULL;
}

// WARNING EVIL WINDOWS TERMINATION CODE!
#ifdef _WIN32

BOOL CtrlHandler(DWORD fdwCtrlType)
{
    switch(fdwCtrlType)
  {
    case CTRL_C_EVENT:
      //printf( "Ctrl-C event\n\n" );
      TerminateProcess(GetCurrentProcess(),1);
      return( TRUE );
    default:
      return FALSE;
  }
}

#else // !_WIN32

void sig_handler(int Signo)
{
  if (Signo == SIGINT) {
      printf("\nReceived interrupt signal (SIGINT), exiting Extempore...\n");
      _exit(0);
  }
  else if (Signo == SIGTERM) {
      printf("\nReceived termination signal (SIGTERM), exiting Extempore...\n");
      exit(0);
  }
}

#endif

enum { OPT_SHAREDIR, OPT_NOBASE, OPT_SAMPLERATE, OPT_FRAMES,
       OPT_CHANNELS, OPT_IN_CHANNELS, OPT_INITEXPR, OPT_INITFILE,
       OPT_PORT, OPT_TERM, OPT_NO_AUDIO, OPT_TIME_DIV, OPT_DEVICE, OPT_IN_DEVICE,
       OPT_DEVICE_NAME, OPT_IN_DEVICE_NAME,
       OPT_PRT_DEVICES, OPT_REALTIME, OPT_ARCH, OPT_CPU, OPT_ATTR,
       OPT_LATENCY,
       OPT_HELP
     };

CSimpleOptA::SOption g_rgOptions[] = {
    // ID              TEXT                   TYPE
    { OPT_SHAREDIR,       "--runtime",       SO_REQ_SEP    },
    { OPT_SHAREDIR,       "--sharedir",      SO_REQ_SEP    },
    { OPT_NOBASE,         "--nobase",        SO_NONE       },
    { OPT_SAMPLERATE,     "--samplerate",    SO_REQ_SEP    },
    { OPT_FRAMES,         "--frames",        SO_REQ_SEP    },
    { OPT_CHANNELS,       "--channels",      SO_REQ_SEP    },
    { OPT_IN_CHANNELS,    "--inchannels",    SO_REQ_SEP    },
    { OPT_INITEXPR,       "--eval",          SO_REQ_SEP    },
    { OPT_INITFILE,       "--run",           SO_REQ_SEP    },
    { OPT_PORT,           "--port",          SO_REQ_SEP    },
    { OPT_TERM,           "--term",          SO_REQ_SEP    },
    { OPT_NO_AUDIO,       "--noaudio",       SO_NONE       },
    { OPT_TIME_DIV,       "--timediv",       SO_REQ_SEP    },
    { OPT_DEVICE,         "--device",        SO_REQ_SEP    },
    { OPT_IN_DEVICE,      "--indevice",      SO_REQ_SEP    },
    { OPT_DEVICE_NAME,    "--device-name",   SO_REQ_SEP    },
    { OPT_IN_DEVICE_NAME, "--indevice-name", SO_REQ_SEP    },
    { OPT_LATENCY,        "--latency",       SO_REQ_SEP    },
    { OPT_PRT_DEVICES,    "--print-devices", SO_NONE       },
    { OPT_REALTIME,       "--realtime",      SO_NONE       },
    { OPT_ARCH,           "--arch",          SO_REQ_SEP    },
    { OPT_CPU,            "--cpu",           SO_REQ_SEP    },
    { OPT_ATTR,           "--attr",          SO_MULTI      },
    { OPT_HELP,           "--help",          SO_NONE       },
    SO_END_OF_OPTIONS
};

int main(int argc, char** argv)
{
    std::string initexpr;
    std::string host("localhost");
    std::string primary_name("primary");
    std::string utility_name("utility");
    int primary_port = 7099;
    int utility_port = 7098;
#ifndef _WIN32
    // redirect stderr to NULL
    freopen("/dev/null", "w", stderr);

        // signal handlers for OSX/Linux
    if (signal(SIGINT, sig_handler) == SIG_ERR) {
                printf("\nWarning: can't catch SIGINT.\n");
    }
    if (signal(SIGTERM, sig_handler) == SIG_ERR) {
                printf("\nWarning: can't catch SIGTERM.\n");
    }
#else
    WSADATA wsadata;
    WSAStartup(0x0202, &wsadata); // I didn't seem to need to call this... but (?)
    SetConsoleCtrlHandler(PHANDLER_ROUTINE(CtrlHandler), TRUE);
#endif

    CSimpleOptA args(argc, argv, g_rgOptions);
    while (args.Next()) {
        if (args.LastError() == SO_SUCCESS) {
            switch(args.OptionId()) {
            case OPT_SHAREDIR:
                extemp::UNIV::SHARE_DIR = std::string(args.OptionArg());
                break;
            case OPT_SAMPLERATE:
                extemp::UNIV::SAMPLE_RATE = atoi(args.OptionArg());
                break;
            case OPT_FRAMES:
                extemp::UNIV::NUM_FRAMES = atoi(args.OptionArg());
                break;
            case OPT_CHANNELS:
                extemp::UNIV::CHANNELS = atoi(args.OptionArg());
                break;
            case OPT_IN_CHANNELS:
                extemp::UNIV::IN_CHANNELS = atoi(args.OptionArg());
                break;
            case OPT_INITEXPR:
                initexpr = std::string(args.OptionArg());
                break;
            case OPT_INITFILE:
                {
                    size_t start_pos = 0;
                    std::string str(args.OptionArg());
                    std::string from("\\");
                    std::string to("\\\\");
                    while((start_pos = str.find(from, start_pos)) != std::string::npos) {
                        str.replace(start_pos, from.length(), to);
                        start_pos += to.length();
                    }
                    initexpr = std::string("(sys:load \"") + str + std::string("\")");
                }
                break;
            case OPT_NOBASE:
                extemp::UNIV::EXT_LOADBASE = false;
                break;
            case OPT_PORT:
                primary_port = atoi(args.OptionArg());
                utility_port = primary_port - 1;
                break;
            case OPT_TERM:
                if (!strcmp(args.OptionArg(), "cmd")) {
                    extemp::UNIV::EXT_TERM = 1;
                } else if (!strcmp(args.OptionArg(), "basic")) {
                    extemp::UNIV::EXT_TERM = 2;
                } else if (!strcmp(args.OptionArg(), "nocolor")) {
                  extemp::UNIV::EXT_TERM = 3;
                } else if (!strcmp(args.OptionArg(), "ansi")) {
                  extemp::UNIV::EXT_TERM = 0;
                } else {
#ifdef _WIN32
                  extemp::UNIV::EXT_TERM = 1;
#else                  
                  extemp::UNIV::EXT_TERM = 0;
#endif                  
                }
                break;
            case OPT_NO_AUDIO:
                extemp::UNIV::AUDIO_NONE = true;
                break;
            case OPT_TIME_DIV:
                extemp::UNIV::TIME_DIVISION = atoi(args.OptionArg());
                break;
            case OPT_DEVICE:
                extemp::UNIV::AUDIO_DEVICE = atoi(args.OptionArg());
                break;
            case OPT_IN_DEVICE:
                extemp::UNIV::AUDIO_IN_DEVICE = atoi(args.OptionArg());
                break;
            case OPT_DEVICE_NAME:
                extemp::UNIV::AUDIO_DEVICE_NAME = args.OptionArg();
                break;
            case OPT_IN_DEVICE_NAME:
                extemp::UNIV::AUDIO_IN_DEVICE_NAME = args.OptionArg();
                break;
            case OPT_LATENCY:
                extemp::UNIV::AUDIO_OUTPUT_LATENCY = atoi(args.OptionArg()) / 1000.0;
                break;
#if !( defined (___ALSA_AUDIO___) || defined (COREAUDIO))
            case OPT_PRT_DEVICES:
                extemp::AudioDevice::printDevices();
                return 0;
#endif
            case OPT_REALTIME:
#ifdef _WIN32
                SetPriorityClass(GetCurrentProcess(), REALTIME_PRIORITY_CLASS);
#else
                std::cout << "Realtime priority setting not available on your platform" << std::endl;
#endif
                break;
            case OPT_ARCH:
                extemp::UNIV::ARCH = args.OptionArg();
                break;
            case OPT_CPU:
                extemp::UNIV::CPU = args.OptionArg();
                break;
            case OPT_ATTR:
                extemp::UNIV::ATTRS.push_back(args.OptionArg());
                break;
            case OPT_HELP:
            default:
                std::cout << "Extempore's command line options: " << std::endl;
                std::cout << "            --help: prints this menu" << std::endl;
                std::cout << "             --run: path to a scheme file to load at startup" << std::endl;
                std::cout << "            --port: port for primary process [7099]" << std::endl;
                std::cout << "            --term: either ansi, cmd (windows), basic (for simpler ansi terms), or nocolor" << std::endl;
                std::cout << "        --sharedir: location of the Extempore share dir (which contains runtime/, libs/, examples/, etc.)" << std::endl;
                std::cout << "         --runtime: [deprecated] use --sharedir instead" << std::endl;
                std::cout << "           --nobase: don't load base lib on startup" << std::endl;
                std::cout << "      --samplerate: audio samplerate" << std::endl;
                std::cout << "          --frames: attempts to force frames [1024]" << std::endl;
                std::cout << "        --channels: attempts to force num of output audio channels" << std::endl;
                std::cout << "      --inchannels: attempts to force num of input audio channels" << std::endl;
                std::cout << "         --noaudio: no audio output: use a \"dummy\" device (overrides --device option)" << std::endl;
                std::cout << "         --timediv: timed sub divisions of FRAMES for scheduling engine (1 = no division which is the defaul)" << std::endl;
                std::cout << "          --device: the index of the audio device to use (output or duplex)" << std::endl;
                std::cout << "        --indevice: the index of the audio input device to use" << std::endl;
                std::cout << "     --device-name: the name of the audio device to use (output or duplex) (overrides index)" << std::endl;
                std::cout << "   --indevice-name: the name of the audio input device to use (overrides index)" << std::endl;
                std::cout << "         --latency: attempts to force audio output latency" << std::endl;
                std::cout << "            --arch: the target architecture [current host]" << std::endl;
                std::cout << "             --cpu: the target cpu [current host]" << std::endl;
                std::cout << "            --attr: additional target attributes (allows multiple)" << std::endl;
                std::cout << "   --print-devices: print the available audio devices to console" << std::endl;
                std::_Exit(0);
            }
        } else {
            std::string key(args.OptionText());
            std::string val = (!args.OptionArg()) ? "" : args.OptionArg();
            if (key.substr(0, 2) != "--") {
                std::cout << "Poorly formed argument: " << key << std::endl;
                return 1;
            }
            extemp::UNIV::CMDPARAMS[key.substr(2)] = val;
            ascii_warning();
            std::cout << "**** WARNING: Setting non-standard option: " << key.substr(2);
            if (args.OptionArg()) {
                std::cout << " to " << val;
            }
            std::cout << std::endl << std::flush;
            ascii_default();
        }
    }
    ascii_normal();
    std::cout << std::endl;
    std::cout << "------------- Extempore -------------- " << std::endl;
    ascii_default();
    std::cout << "Andrew Sorensen (c) 2010-2016" << std::endl;
    std::cout << "andrew@moso.com.au, @digego" << std::endl;
    std::cout << std::endl;
    ascii_default();
#ifdef _WIN32
    // on Windows with MCJIT we need to add "-elf" to the target triple, see
    // http://lists.cs.uiuc.edu/pipermail/llvmdev/2013-December/068407.html
    if (extemp::UNIV::ARCH.empty()) {
        extemp::UNIV::ARCH = llvm::sys::getProcessTriple() + "-elf";
    }
#endif

    extemp::TaskScheduler::I()->start();
    extemp::EXTLLVM::initLLVM();
    extemp::SchemeProcess* primary = 0;
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
    } else {
#ifdef _WIN32
        printf("Sorry, the \"noaudio\" dummy device isn't yet supported on Windows.\n");
        exit(1);
#else
        // don't need this anymore, but we do need timediv to be > 1
        if (extemp::UNIV::TIME_DIVISION == 1) {
            extemp::UNIV::TIME_DIVISION = 4;
        }
#endif
    }
    ascii_normal();
#ifdef SUBSUME_PRIMARY
    ascii_info();
    std::cout << std::endl << "Primary on Thread 0" << std::endl;
    ascii_normal();
#endif    
    std::cout << "---------------------------------------" << std::endl;
    ascii_default();
    bool startup_ok = true;
    extemp::SchemeProcess* utility = new extemp::SchemeProcess(extemp::UNIV::SHARE_DIR, utility_name, utility_port, 0);
    startup_ok &= utility->start();
    extemp::SchemeREPL* utility_repl = new extemp::SchemeREPL(utility_name, utility);
    utility_repl->connectToProcessAtHostname(host, utility_port);

#ifndef SUBSUME_PRIMARY // if not subsume primary (i.e. primary NOT on thread 0)
    primary = new extemp::SchemeProcess(extemp::UNIV::SHARE_DIR, primary_name, primary_port, 0, initexpr);
    startup_ok &= primary->start();
    extemp::SchemeREPL* primary_repl = new extemp::SchemeREPL(primary_name, primary);
    primary_repl->connectToProcessAtHostname(host, primary_port);
    //std::cout << "primary started:" << std::endl << std::flush;    
    if (!startup_ok) {
        ascii_error();
        printf("Error");
        ascii_default();
        printf(": processes failed to start. Exiting...\n");
        fflush(NULL);
        exit(1);
    }
    while (true) {
      if (XTMMainCallback) { XTMMainCallback(); }
#ifdef _WIN32
      Sleep(2000);
#elif __APPLE__
      sleep(2);
#else
      sleep(2000);
#endif
    }      
#else
    primary = new extemp::SchemeProcess(extemp::UNIV::SHARE_DIR, primary_name, primary_port, 0, initexpr);

    // need to connect to primary from alternate thread (can be short lived simply puts repl on heap)
    extemp::EXTThread* replthread = new extemp::EXTThread(extempore_primary_repl_delayed_connect,primary);
    pass_primary_port = primary_port;
    replthread->start();
    // start the primary process running on this thread (i.e. process thread 0)
    primary->start(true); // this will not return
#endif // end SUBSUME_PRIMARY
    return 0;
}
