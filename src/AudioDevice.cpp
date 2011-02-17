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

#include <time.h>  //just included for checking execution time of function - can be removed at any time.
#include <iostream>
#include <string.h>

#include "AudioDevice.h"
#include "math.h"
#include "TaskScheduler.h"
//#include "EXTMonitor.h"
#include "EXTLLVM.h"

#ifndef TARGET_OS_LINUX
#include <CoreAudio/HostTime.h>
#include <mach/mach_init.h>
#include <mach/task_policy.h>
#include <mach/thread_act.h>
#include <mach/thread_policy.h>
#include <sys/sysctl.h>
#endif

SAMPLE audio_sanity(SAMPLE x)
{
	if(isinf(x)) return 0.0f;
	else if(isnan(x)) return 0.0f;
	else if(x < -0.99f) return -0.99f; 
	else if(x > 0.99f) return 0.99f;
	else return x;
}

namespace extemp {
	
	AudioDevice AudioDevice::SINGLETON;

    bool first_callback = true;

    uint64_t start_time = 0;
    uint64_t device_time = 0;
	
	bool RUNNING = true;	

#ifdef TARGET_OS_LINUX
    //-----------------------------------
    //  PORT AUDIO
    //-----------------------------------
    int audioCallback(const void* inputBuffer, void* outputBuffer, unsigned long framesPerBuffer, const PaStreamCallbackTimeInfo *timeInfo, PaStreamCallbackFlags statusFlags, void *userData)
    {
        TaskScheduler* sched = static_cast<TaskScheduler*>(userData);
		UNIV::TIME = UNIV::TIME + UNIV::FRAMES;
		device_time = UNIV::TIME;
        if(UNIV::TIME != device_time) std::cout << "Timeing Sychronization problem!!!  UNIV::TIME[" << UNIV::TIME << "] DEVICE_TIME[ " << device_time << "]" << std::endl; 
        int channels = 2;
        uint64_t numOfSamples = (uint64_t) (framesPerBuffer * channels);
		sched->getGuard()->signal();
		
		memset(outputBuffer,0,numOfSamples);
		
		void* dsp_closure = AudioDevice::I()->getDSPClosure();
		void* cache_closure = 0;
		if(dsp_closure == 0) return 0;
		cache_closure = ((void*(*)()) dsp_closure)(); // get actual LLVM closure from _getter() !
		
		
		if(AudioDevice::I()->getDSPWrapper()) { // if true then we must be sample by sample
			dsp_f_ptr dsp_wrapper = AudioDevice::I()->getDSPWrapper();
			dsp_f_ptr cache_wrapper = dsp_wrapper;
			double* data = 0; 
			llvm_zone_t* zone = llvm_zone_create(1024*1024); // 1M
			for(uint32_t i=0;i<UNIV::FRAMES;i++)
			{
				uint32_t ii = i*UNIV::CHANNELS;
				SAMPLE* dat = (SAMPLE*) outputBuffer;
				for(uint32_t k=0; k<UNIV::CHANNELS; k++)
				{
					dat[ii+k] = audio_sanity((SAMPLE)cache_wrapper(zone, cache_closure, 0.0,(double)(i+UNIV::TIME),(double)k,data));
					llvm_zone_reset(zone);
				}
			}
			llvm_zone_destroy(zone);
		}else if(AudioDevice::I()->getDSPWrapperArray()) { // if true then we must be buffer by buffer
			dsp_f_ptr_array dsp_wrapper = AudioDevice::I()->getDSPWrapperArray();
			dsp_f_ptr_array cache_wrapper = dsp_wrapper;
			llvm_zone_t* zone = llvm_zone_create(1024*50);				
			cache_wrapper(zone, cache_closure, (SAMPLE*)inputBuffer,(SAMPLE*)outputBuffer,UNIV::FRAMES,UNIV::TIME,UNIV::CHANNELS);			
			llvm_zone_destroy(zone);
		}else{ 
			//nothin to do
		}
        return 0;
    }

	AudioDevice::AudioDevice() : started(false), buffer(0), dsp_closure(0), dsp_wrapper(0), dsp_wrapper_array(0)
    {
        Pa_Initialize();
        //std::cout << "Initializing AudioDevice: " << std::endl;
        PaError err;
        int inputDevice = Pa_GetDefaultInputDevice();
        int outputDevice = Pa_GetDefaultOutputDevice();
        std::cout << "Input Device: " << inputDevice << std::endl;
        std::cout << "Output Device: " << outputDevice << std::endl;
        err = Pa_OpenDefaultStream(&stream, 0, 2, paFloat32, 44100.0, UNIV::FRAMES, audioCallback, (void*)TaskScheduler::I());
		if(err != paNoError) {
			std::cerr << "PA Error: " << Pa_GetErrorText(err) << std::endl;
		}
        UNIV::CHANNELS = 2;
        UNIV::SAMPLERATE = 44100;
        std::cout << "Loaded Audio Device: " << std::endl;
        std::cout << "SampleRate\t: " << UNIV::SAMPLERATE << std::endl;
        std::cout << "Channels\t: " << UNIV::CHANNELS << std::endl;
    }
	
    AudioDevice::~AudioDevice()
	{
		PaError err;
		err = Pa_StopStream(stream);
		if(err != paNoError) std::cout << Pa_GetErrorText(err) << std::endl;
		err = Pa_CloseStream(stream);
		if(err != paNoError) std::cout << Pa_GetErrorText(err) << std::endl;
		err = Pa_Terminate();
		if(err != paNoError) std::cout << Pa_GetErrorText(err) << std::endl;
	}

    void AudioDevice::start()
    {
		if(started) return;
        UNIV::initRand();        
		PaError err;
        err = Pa_StartStream(stream);
		if(err != paNoError) { std::cout << "PortAudio ERROR: " << Pa_GetErrorText(err) << std::endl; }
		RUNNING = true;
		//queueThread->Start();
		started = true;
    }

    void AudioDevice::stop()
    {
		if(!started) return;
		PaError err = Pa_StopStream(stream);
		if(err != paNoError) std::cout << "PA Error: " << Pa_GetErrorText(err) << std::endl;    
		started = false;
	}
#else


    //-----------------------------------
    //  CORE AUDIO
     //----------------------------------- 
    OSStatus errorCallback(AudioDeviceID		inDevice,
						UInt32			inChannel,
						Boolean			isInput,
						AudioDevicePropertyID	inPropertyID,
						void*		inClientData)
    {
        std::cout << "This has been called because you've run out of processing time!!!" << std::endl;        
    }

    OSStatus audioCallback (AudioDeviceID		inDevice,
                            const AudioTimeStamp*	inNow,
                            const AudioBufferList*	inInputData,
                            const AudioTimeStamp*	inInputTime,
                            AudioBufferList*		outOutputData, 
                            const AudioTimeStamp*	inOutputTime,
                            void*			inClientData)
    {        
		TaskScheduler* sched = static_cast<TaskScheduler*>(inClientData);;
		//int channels = static_cast<int>(outOutputData->mBuffers[0].mNumberChannels);        
		int channels = static_cast<int>(outOutputData->mNumberBuffers);
        int numOfSamples = static_cast<int>(outOutputData->mBuffers[0].mDataByteSize) / sizeof(SAMPLE);
        long t = static_cast<long>(inOutputTime->mSampleTime);
        if(first_callback) { first_callback = false; start_time = t; }
        device_time = t - start_time;
		UNIV::TIME = UNIV::TIME + UNIV::FRAMES;
        
        if(UNIV::TIME != device_time) {
           std::cout << std::endl << "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=" << std::endl;
           std::cout << "UNIV Timeing Sychronization problem!!!:" << std::endl;
           std::cout << "UNIV::TIME [" << UNIV::TIME << "]" << std::endl;
           std::cout << "ADJUSTED_DEVICE_TIME [" << device_time << "]" << std::endl;
           std::cout << "NON_ADJUSTED_DEVICE_TIME [" << t << "]" << std::endl;
			  std::cout << "ADJUSTMENT_VALUE [" << start_time << "]" << std::endl;
           std::cout << "NumOfSamples [" << numOfSamples << "]" << std::endl;
           std::cout << std::endl << "Adjusting by " << (device_time - UNIV::TIME) << " samples to re-sync" << std::endl << std::endl;
           std::cout << "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=" << std::endl << std::endl;

           start_time += (device_time) - UNIV::TIME;
        } 
 		sched->getGuard()->signal();

		void* dsp_closure = AudioDevice::I()->getDSPClosure();
		void* cache_closure = 0;
		if(dsp_closure == 0) return 0;
		cache_closure = ((void*(*)()) dsp_closure)(); // get actual LLVM closure from _getter() !
		
		
		if(AudioDevice::I()->getDSPWrapper()) { // if true then we must be sample by sample
			dsp_f_ptr dsp_wrapper = AudioDevice::I()->getDSPWrapper();
			dsp_f_ptr cache_wrapper = dsp_wrapper;
			double* data = 0; 
			llvm_zone_t* zone = llvm_zone_create(1024*1024); // 1M				
			for(uint32_t i=0;i<UNIV::FRAMES;i++)
			{
				uint32_t ii = i*UNIV::CHANNELS;
				SAMPLE* dat = (SAMPLE*) outOutputData->mBuffers[0].mData;
				for(uint32_t k=0; k<UNIV::CHANNELS; k++)
				{   
					dat[ii+k] = audio_sanity((SAMPLE)cache_wrapper(zone, cache_closure, 0.0,(double)(i+UNIV::TIME),(double)k,data)); 
					llvm_zone_reset(zone);
				}
			}
			llvm_zone_destroy(zone);
		}else if(AudioDevice::I()->getDSPWrapperArray()) { // if true then we must be buffer by buffer
			dsp_f_ptr_array dsp_wrapper = AudioDevice::I()->getDSPWrapperArray();
			dsp_f_ptr_array cache_wrapper = dsp_wrapper;
			llvm_zone_t* zone = llvm_zone_create(1024*50);				
			SAMPLE* outputBuffer = (SAMPLE*) outOutputData->mBuffers[0].mData;
			SAMPLE* inputBuffer = (SAMPLE*) inInputData->mBuffers[0].mData;		
			cache_wrapper(zone, cache_closure, (SAMPLE*)inputBuffer,(SAMPLE*)outputBuffer,UNIV::FRAMES,UNIV::TIME,UNIV::CHANNELS);
			llvm_zone_destroy(zone);
		}else{ 
			//nothin to do
		}
		
        return 0;
    }

    AudioDevice::AudioDevice() : started(false), buffer(0), dsp_closure(0), dsp_wrapper(0), dsp_wrapper_array(0)
    {
        // get the default output device
        UInt32 count = (UInt32) sizeof(device);
        OSStatus err = AudioHardwareGetProperty(kAudioHardwarePropertyDefaultOutputDevice,&count,(void*)&device);
        if(err != kAudioHardwareNoError) {
            std::cout << "Error while trying to retrieve default audio device" << std::endl;
        }
        // get the default audio stream
        AudioStreamBasicDescription stream;
        count = sizeof(stream);
        err = AudioDeviceGetProperty(device,0,false,kAudioDevicePropertyStreamFormat,&count,(void*)&stream);
        if(err != kAudioHardwareNoError) {
            std::cout << "Error when reading audio device stream format" << std::endl;
        }
        err = AudioDeviceSetProperty(device,NULL,0,false,kAudioDevicePropertyBufferFrameSize,4,&UNIV::FRAMES);
        while(err != 0) {
            std::cout << "Error setting Frames To: " << UNIV::FRAMES << std::endl;
            UNIV::FRAMES = 1024;
            std::cout << "Adjusting Frames To: " << UNIV::FRAMES << std::endl;
            err = AudioDeviceSetProperty(device,NULL,0,false,kAudioDevicePropertyBufferFrameSize,4,&UNIV::FRAMES);            
        }
        err = AudioDeviceAddPropertyListener(device, 0, false, kAudioDeviceProcessorOverload, &errorCallback, NULL);
        if(err != 0) {
            std::cout << "Error setting Processor Overload Property Listener" << std::endl;
        }
        
        UNIV::CHANNELS = static_cast<int>(stream.mChannelsPerFrame);
        UNIV::SAMPLERATE = static_cast<int>(stream.mSampleRate);
        std::cout << "Loaded Audio Device With Stream: " << std::endl;
        std::cout << "Format\t: " << stream.mFormatID << std::endl;
        std::cout << "Format Flags\t: " << stream.mFormatFlags << std::endl;
        std::cout << "Bytes Per Packet\t: " << stream.mBytesPerPacket << std::endl;
        std::cout << "Frames Per Packet\t: " << stream.mFramesPerPacket << std::endl;
        std::cout << "Bytes Per Frame\t: " << stream.mBytesPerFrame << std::endl;
        std::cout << "Channels Per Frame\t: " << stream.mChannelsPerFrame << std::endl;
        std::cout << "Bits Per Channel\t: " << stream.mBitsPerChannel << std::endl;

        std::cout << "SampleRate\t: " << UNIV::SAMPLERATE << std::endl;
        std::cout << "Channels\t: " << UNIV::CHANNELS << std::endl;
		std::cout << "Frames:\t\t" << UNIV::FRAMES << std::endl;

		err = AudioDeviceAddIOProc(device, &audioCallback, TaskScheduler::I());
        if (err != kAudioHardwareNoError) { std::cout << "Audio harware setup error of some kind!!!!: " << err << std::endl; exit(1);} 
	}
	
    AudioDevice::~AudioDevice() 
	{
		std::cout << "DELETE AUDIOROUTER" << std::endl;
		RUNNING = false;
	}
	
    void AudioDevice::start() 
    {
		if(started) return;
		UNIV::initRand();
						
		OSStatus err = AudioDeviceStart(device, &audioCallback);
		if(err != kAudioHardwareNoError) {
			std::cerr << "Error loading audio hardware!!" << std::endl;
			exit(1);
		}
		started = false;
		RUNNING = true;
    }

    void AudioDevice::stop() 
    {
		if(!started) return;
		RUNNING = false;	
		sleep(1);
		OSStatus err = AudioDeviceStop(device, audioCallback);
		if (err != kAudioHardwareNoError) { std::cerr << "Audio harware stopping error of some kind" << std::endl; }
		err = AudioDeviceRemoveIOProc(device, &audioCallback);
		if (err != kAudioHardwareNoError) { std::cerr << "CoreAudio ERROR: removing IOProc" << std::endl; }
		started = false;
    }

#endif



} //End Namespace
