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
 * ARE DISCLEXTD. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE 
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
#include "SimpleOpt.h"
#include "SchemeProcess.h"
#include "AudioDevice.h"
#include "TaskScheduler.h"
#include "SchemeREPL.h"
#include "EXTLLVM.h"
#include <string>
#ifdef TARGET_OS_MAC
#include <Cocoa/Cocoa.h>
#include <AppKit/AppKit.h>
#endif


// WARNING EVIL WINDOWS TERMINATION CODE!
#ifdef TARGET_OS_WINDOWS
BOOL CtrlHandler( DWORD fdwCtrlType ) 
{ 
  switch( fdwCtrlType ) 
  { 
    case CTRL_C_EVENT: 
      //printf( "Ctrl-C event\n\n" );
      TerminateProcess(GetCurrentProcess(),1);
      return( TRUE );
 
    default: 
      return FALSE; 
  } 
} 
#endif


enum { OPT_RUNTIME, OPT_SAMPLERATE, OPT_FRAMES, 
       OPT_CHANNELS, OPT_IN_CHANNELS, OPT_INITFILE, 
       OPT_PORT, OPT_TERM, OPT_DEVICE, OPT_IN_DEVICE,
       OPT_PRT_DEVICES, OPT_REALTIME, OPT_HELP
     };

CSimpleOptA::SOption g_rgOptions[] = {
    // ID              TEXT                   TYPE
    { OPT_RUNTIME,     "--runtime",       SO_REQ_SEP    },
    { OPT_SAMPLERATE,  "--samplerate",    SO_REQ_SEP    },
    { OPT_FRAMES,      "--frames",        SO_REQ_SEP    },
    { OPT_CHANNELS,    "--channels",      SO_REQ_SEP    },
    { OPT_IN_CHANNELS, "--inchannels",    SO_REQ_SEP    },
    { OPT_INITFILE,    "--run",           SO_REQ_SEP    },
    { OPT_PORT,        "--port",          SO_REQ_SEP    },
    { OPT_TERM,        "--term",          SO_REQ_SEP    },
    { OPT_DEVICE,      "--device",        SO_REQ_SEP    },
    { OPT_IN_DEVICE,   "--indevice",      SO_REQ_SEP    },
    { OPT_PRT_DEVICES, "--print-devices", SO_NONE       },
    { OPT_REALTIME,    "--realtime",      SO_NONE       },
    { OPT_HELP,        "--help",          SO_NONE       },
    SO_END_OF_OPTIONS                       // END
};


int main(int argc, char** argv)
{
    std::string runtimedir("runtime");
    std::string initfile;    
    bool initfile_on = false;
    
    std::string host("localhost");
    std::string primary_name("primary");
    std::string utility_name("utility");
    int primary_port = 7099;
    int utility_port = 7098;

    // more evil windows termination code
    #ifdef TARGET_OS_WINDOWS
       SetConsoleCtrlHandler( (PHANDLER_ROUTINE) CtrlHandler, TRUE );
    #endif


    CSimpleOptA args(argc, argv, g_rgOptions);
    while (args.Next()) {
      if (args.LastError() == SO_SUCCESS) {
	switch(args.OptionId()) {
	case OPT_RUNTIME:
	  runtimedir = std::string(args.OptionArg());
	  break;
        case OPT_SAMPLERATE:
	  extemp::UNIV::SAMPLERATE = atoi(args.OptionArg());
	  break;
        case OPT_FRAMES:
	  extemp::UNIV::FRAMES = atoi(args.OptionArg());
	  break;
        case OPT_CHANNELS:
	  extemp::UNIV::CHANNELS = atoi(args.OptionArg());
	  break;
        case OPT_IN_CHANNELS:
	  extemp::UNIV::IN_CHANNELS = atoi(args.OptionArg());
	  break;
        case OPT_INITFILE:
	  initfile = std::string(args.OptionArg());
	  initfile_on = true;	  
	  break;
	case OPT_PORT:
	  primary_port = atoi(args.OptionArg());
	  utility_port = primary_port-1;
        case OPT_TERM:
          if(strcmp(args.OptionArg(),"cmd")==0) {
	    extemp::UNIV::EXT_TERM = 1;
	  }else{
	    extemp::UNIV::EXT_TERM = 0;
	  }
          break;
	case OPT_DEVICE:
	  extemp::UNIV::AUDIO_DEVICE = atoi(args.OptionArg());
          break;
	case OPT_IN_DEVICE:
	  extemp::UNIV::AUDIO_IN_DEVICE = atoi(args.OptionArg());
          break;
#if !(defined (JACK_AUDIO) || defined (___ALSA_AUDIO___) || defined (COREAUDIO))
	case OPT_PRT_DEVICES:
          extemp::AudioDevice::printDevices();
	  return -1;
#endif
        case OPT_REALTIME:
#ifdef TARGET_OS_WINDOWS          
          SetPriorityClass(GetCurrentProcess(), REALTIME_PRIORITY_CLASS);
#else
	  std::cout << "Realtime priority setting not available on your platform" << std::endl;
#endif
          break;
        case OPT_HELP:
	default:
	  std::cout << "Extempore's command line options: " << std::endl;
	  std::cout << "            --help: prints this menu" << std::endl;	
	  std::cout << "             --run: path to a scheme file to load at startup" << std::endl;
	  std::cout << "            --port: port for primary process [7099]" << std::endl;	
	  std::cout << "            --term: either ansi or cmd" << std::endl;	
	  std::cout << "         --runtime: path to runtime directory [runtime]" << std::endl; 	
	  std::cout << "      --samplerate: audio samplerate" << std::endl; 
	  std::cout << "          --frames: attempts to force frames [128]" << std::endl;
	  std::cout << "        --channels: attempts to force num of output audio channels" << std::endl;
	  std::cout << "      --inchannels: attempts to force num of input audio channels" << std::endl;
	  std::cout << "          --device: the index of the audio device to use (output or duplex)" << std::endl;
	  std::cout << "        --indevice: the index of the audio input device to use" << std::endl;	
	  std::cout << "   --print-devices: print the available audio devices to console" << std::endl;	
	  return -1;
	}
      } else {
	  char* key = (char*) args.OptionText();
	  char* val = args.OptionArg();
	  char a[256];
	  char b[256];
	  rsplit("--",key,a,b);
	  //std::cout << "ADD-ARG: " << b << " " << val << std::endl;
	  extemp::UNIV::CMDPARAMS[std::string(b)] = std::string(val);
      }
    }

    ascii_text_color(1,7,10);	    
    std::cout << "##########################################" << std::endl;
    std::cout << "##                                      ##" << std::endl;        
    std::cout << "##               EXTEMPORE              ##" << std::endl;        
    std::cout << "##                                      ##" << std::endl;        		    		
    std::cout << "##           andrew@moso.com.au         ##" << std::endl;            
    std::cout << "##                                      ##" << std::endl;
    std::cout << "##            (c) 2010-2012             ##" << std::endl;    
    std::cout << "##                                      ##" << std::endl;        
    std::cout << "##########################################" << std::endl;
    std::cout << "     ################################" << std::endl;
    std::cout << "          ######################" << std::endl;
    std::cout << "               ############" << std::endl;
    std::cout << "                    ##" << std::endl;
    std::cout << std::endl;
    ascii_text_color(0,9,10);
    fflush(NULL);    
		
    extemp::UNIV::PWD = runtimedir.c_str();
    extemp::EXTLLVM::I()->initLLVM();
    extemp::SchemeProcess* primary = 0;

    if(initfile_on) { // if a file needs to be loaded from the command line
       primary = new extemp::SchemeProcess(runtimedir, primary_name, primary_port, 0, initfile);
    }else{
       primary = new extemp::SchemeProcess(runtimedir, primary_name, primary_port, 0);
    }

    extemp::SchemeProcess* utility = new extemp::SchemeProcess(runtimedir, utility_name, utility_port, 0);
    extemp::AudioDevice* dev = extemp::AudioDevice::I();

    primary->start();
    utility->start();
    dev->start();

    extemp::SchemeREPL* primary_repl = new extemp::SchemeREPL(primary_name);
    primary_repl->connectToProcessAtHostname(host,primary_port);
    extemp::SchemeREPL* utility_repl = new extemp::SchemeREPL(utility_name);
    utility_repl->connectToProcessAtHostname(host,utility_port);


#ifdef TARGET_OS_MAC
    [[NSApplication sharedApplication] run];
#else
    while(1) {
#ifdef TARGET_OS_WINDOWS
      Sleep(5000);
#else
      sleep(5000);
#endif
    }
#endif

    return 0;
}
