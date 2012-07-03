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
#include "TaskScheduler.h"
//#include "EXTMonitor.h"
#include "EXTLLVM.h"

#ifdef TARGET_OS_MAC
#include <CoreAudio/HostTime.h>
#include <mach/mach_init.h>
#include <mach/task_policy.h>
#include <mach/thread_act.h>
#include <mach/thread_policy.h>
#include <sys/sysctl.h>
#include <time.h>
#endif

#include <stdlib.h>
#include <math.h>

///////////////////////////////////////////////////////////////////////
//
//
// NOTE  !!!!!!!!!!!!!!!!!!!!!!!!!!!
//
// The contents of this file are a complete HACK!
// I have written some placeholder code to get the project
// started, but all of this code needs to be replaced.
//
// Nothing here is implemented properly AT ANY LEVEL!
// There is A LOT to do to make this stuff work properly.
// So please don't complain about how dodgy this code is
// Jump in and start to replace it !!!
//
///////////////////////////////////////////////////////////////////////

#ifdef EXT_BOOST
#include <boost/filesystem.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>

 boost::posix_time::ptime EXT_BOOST_JAN1970(boost::gregorian::date(1970,boost::gregorian::Jan,1));

  double time_to_double(boost::posix_time::ptime& time) {
	 boost::posix_time::time_period period(EXT_BOOST_JAN1970,time);
	 return period.length().total_microseconds()/D_MILLION;
  }
 
  struct boost::posix_time::ptime& double_to_time(double tm) {
     using namespace boost::posix_time;
     ptime p;
     int64_t seconds = (int64_t)tm;
     int64_t fractional = (tm-seconds)*time_duration::ticks_per_second();
     p = EXT_BOOST_JAN1970+time_duration(0,0,seconds,fractional);

     return p;
  }

  double getRealTime()
  {
	boost::posix_time::ptime pt = boost::posix_time::microsec_clock::local_time();
	return time_to_double(pt); // + SchemeFFI::CLOCK_OFFSET);
  }

#else
#ifdef TARGET_OS_LINUX
double time_to_double(struct timespec t) {
    return t.tv_sec + t.tv_nsec/D_BILLION;
}
 
struct timespec double_to_time(double tm) {
  struct timespec t;
 
  t.tv_sec = (long)tm;
  t.tv_nsec = (tm - t.tv_sec)*BILLION;
  if (t.tv_nsec == BILLION) {
    t.tv_sec++;
    t.tv_nsec = 0;
  }
  return t;
}

double getRealTime()
{
  struct timespec t;
  clock_gettime(CLOCK_REALTIME,&t);
  return time_to_double(t);
}
#endif //TARGET_OS_LINUX
#endif //EXT_BOOST

#ifdef TARGET_OS_MAC
double getRealTime()
{
   return CFAbsoluteTimeGetCurrent() + kCFAbsoluteTimeIntervalSince1970; 
}
#endif

#ifdef TARGET_OS_WINDOWS
#define isnan(x) ((x) != (x))
#define isinf(x) (isnan(x-x))
#elif TARGET_OS_MAC
#define isnan(x) ((x) != (x))
#define isinf(x) (isnan(x-x))
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
    double AudioDevice::REALTIME = 0.0;
    double AudioDevice::CLOCKBASE = 0.0;
    double AudioDevice::CLOCKOFFSET = 0.0;

    bool first_callback = true;

    uint64_t start_time = 0;
    uint64_t device_time = 0;
	
    bool RUNNING = true;	


  /////////////////////////////////////////////////////////////////
  //
  // This ALSA code doesn't work!!!
  // I've just put this here as I might come back to it later
  //
  ///////////////////////////////////////////////////////////////
#if defined (___ALSA_AUDIO___) // ALSA not currently supported

    static int xrun_recovery(snd_pcm_t *handle, int err)
    {
      if (err == -EPIPE) {    /* under-run */
	err = snd_pcm_prepare(handle);
	if (err < 0)
	  printf("Can't recovery from underrun, prepare failed: %s\n", snd_strerror(err));
	return 0;
      } else if (err == -ESTRPIPE) {
	while ((err = snd_pcm_resume(handle)) == -EAGAIN)
	  sleep(1);       /* wait until the suspend flag is released */
	if (err < 0) {
	  err = snd_pcm_prepare(handle);
	  if (err < 0)
	    printf("Can't recovery from suspend, prepare failed: %s\n", snd_strerror(err));
	}
	return 0;
      }
      return err;
    }

    void audioCallback(snd_async_handler_t *pcm_callback)
    {
      snd_pcm_t* handle = snd_async_handler_get_pcm(pcm_callback);
      AudioDevice* private_data = (AudioDevice*) snd_async_handler_get_callback_private(pcm_callback);
      SAMPLE* outputBuffer = private_data->getBuffer();

      snd_pcm_sframes_t avail;
      int err;
      int period_size = UNIV::FRAMES/16;

      TaskScheduler* sched = TaskScheduler::I(); //static_cast<TaskScheduler*>(userData);
      UNIV::DEVICE_TIME = UNIV::DEVICE_TIME + period_size; //UNIV::FRAMES;
      UNIV::TIME = DEVICE_TIME;
      int channels = 2;
      sched->getGuard()->signal();
      
      void* dsp_closure = AudioDevice::I()->getDSPClosure();
      void* cache_closure = 0;
      if(dsp_closure == 0) return;
      cache_closure = ((void*(*)()) dsp_closure)(); // get actual LLVM closure from _getter() !
      
      avail = snd_pcm_avail_update(handle);
      while (avail >= period_size) {
	if(AudioDevice::I()->getDSPWrapper()) { // if true then we must be sample by sample
	  dsp_f_ptr dsp_wrapper = AudioDevice::I()->getDSPWrapper();
	  dsp_f_ptr cache_wrapper = dsp_wrapper;
	  double (*closure) (double,double,double,double*) = * ((double(**)(double,double,double,double*)) cache_closure);
	  double* data = 0; 
	  //llvm_zone_t* zone = llvm_zone_create(1024*1024); // 1M
	  llvm_zone_t* zone = llvm_peek_zone_stack();
	  //llvm_push_zone_stack(zone);
	  for(uint32_t i=0;i<period_size;i++)
	    {
	      uint32_t ii = i*UNIV::CHANNELS;
	      SAMPLE* dat = (SAMPLE*) outputBuffer;
	      //SAMPLE* in = (SAMPLE*) inputBuffer;
	      for(uint32_t k=0; k<UNIV::CHANNELS; k++)
		{
		  dat[ii+k] = audio_sanity((SAMPLE)cache_wrapper(zone, (void*)closure, /*(double)in[ii+k]*/0.0,(double)(i+UNIV::DEVICE_TIME),(double)k,data));
		  llvm_zone_reset(zone);
		}
	    }
	  //llvm_pop_zone_stack();
	  //llvm_zone_destroy(zone);
	}else if(AudioDevice::I()->getDSPWrapperArray()) { // if true then we must be buffer by buffer
	  dsp_f_ptr_array dsp_wrapper = AudioDevice::I()->getDSPWrapperArray();
	  dsp_f_ptr_array cache_wrapper = dsp_wrapper;
	  //llvm_zone_t* zone = llvm_zone_create(1024*50);
          llvm_zone_t* zone = llvm_peek_zone_stack();
	  //llvm_push_zone_stack(zone);
	  cache_wrapper(zone, cache_closure, 0 /*(SAMPLE*)inputBuffer*/,(SAMPLE*)outputBuffer,period_size,UNIV::DEVICE_TIME,UNIV::CHANNELS);
	  //llvm_pop_zone_stack();
	  //llvm_zone_destroy(zone);
          llvm_zone_reset(zone);
	}else{ 
	  //nothin to do
	}
	
	// now write outputBuffer to the device
	err = snd_pcm_writei(handle, outputBuffer, period_size);
	if (err < 0) {
	  printf("Write error: %s\n", snd_strerror(err));
	  exit(EXIT_FAILURE);
	}
	if (err != period_size) {
	  printf("Write error: written %i expected %li\n", err, period_size);
	  exit(EXIT_FAILURE);
	}
	avail = snd_pcm_avail_update(handle);
      }
    }

    AudioDevice::AudioDevice() : started(false), buffer(0), dsp_closure(0), dsp_wrapper(0), dsp_wrapper_array(0)
    {
      UNIV::CHANNELS = 2;
      UNIV::SAMPLERATE = 44100;
      UNIV::FRAMES = 1024;


      /* This holds the error code returned */
      int err;
      /* Our device handle */
      pcm_handle = NULL;
      /* The device name */
      const char *device_name = "default";
      /* Open the device */
      err = snd_pcm_open (&pcm_handle, device_name, SND_PCM_STREAM_PLAYBACK, 0);
      /* Error check */
      if (err < 0) {
	fprintf (stderr, "cannot open audio device %s (%s)\n", 
		 device_name, snd_strerror (err));
	pcm_handle = NULL;
	return;
      }

      // first handle hardware params
      snd_pcm_hw_params_t *hw_params;
      snd_pcm_hw_params_malloc (&hw_params);
      snd_pcm_hw_params_any (pcm_handle, hw_params);
      unsigned int rrate = UNIV::SAMPLERATE;
      snd_pcm_hw_params_set_access (pcm_handle, hw_params, SND_PCM_ACCESS_RW_INTERLEAVED);
      //snd_pcm_hw_params_set_format (pcm_handle, hw_params, SND_PCM_FORMAT_FLOAT_LE); //SND_PCM_FORMAT_S16_LE);
      snd_pcm_hw_params_set_format (pcm_handle, hw_params, SND_PCM_FORMAT_S16_LE);
      snd_pcm_hw_params_set_rate_near (pcm_handle, hw_params, &rrate, NULL);
      snd_pcm_hw_params_set_channels (pcm_handle, hw_params, UNIV::CHANNELS);

      /* These values are pretty small, might be useful in
	 situations where latency is a dirty word. */
      snd_pcm_uframes_t buffer_size = UNIV::FRAMES;
      snd_pcm_uframes_t period_size = UNIV::FRAMES/16;
      snd_pcm_hw_params_set_buffer_size_near (pcm_handle, hw_params, &buffer_size);
      snd_pcm_hw_params_set_period_size_near (pcm_handle, hw_params, &period_size, NULL);
      // make it so!
      snd_pcm_hw_params (pcm_handle, hw_params);

      // now handle software params
      snd_pcm_sw_params_t *sw_params;
      snd_pcm_sw_params_malloc (&sw_params);
      snd_pcm_sw_params_current (pcm_handle, sw_params);
      snd_pcm_sw_params_set_start_threshold(pcm_handle, sw_params, buffer_size - period_size);
      snd_pcm_sw_params_set_avail_min(pcm_handle, sw_params, period_size);
      // make is so!
      snd_pcm_sw_params(pcm_handle, sw_params);
      snd_pcm_sw_params_free (sw_params);

      // allocate memory for audio buffer
      buffer = (float*) malloc(period_size * sizeof(SAMPLE) * UNIV::CHANNELS);
      
      //std::cout << "Input Device: " << inputDevice << std::endl;
      //std::cout << "Output Device: " << outputDevice << std::endl;
      ascii_text_color(1,7,10);
      std::cout << "---Alsa Audio---" << std::endl;
      std::cout << "Loaded Default Audio Device: " << std::endl;	
      ascii_text_color(0,7,10);	
      std::cout << "SampleRate\t: " << std::flush;
      ascii_text_color(1,6,10);	
      std::cout << UNIV::SAMPLERATE << std::endl << std::flush;
      ascii_text_color(0,7,10);	
      std::cout << "Channels\t: " << std::flush;
      ascii_text_color(1,6,10);	
      std::cout << UNIV::CHANNELS << std::endl << std::flush;
      ascii_text_color(0,7,10);	
    }
	
    AudioDevice::~AudioDevice()
    {
      free(buffer);
    }

    void AudioDevice::start()
    {
	if(started) return;
        UNIV::initRand();        
	// pepare device
	if(snd_pcm_prepare (pcm_handle) != 0) {
	  printf("Error preparing PCM for Alsa\n");
	}
	//do we really need to do this?
	//void* buf = malloc(2*64*4);
	//snd_pcm_writei (pcm_handle, buf, 2 * 64); //period_size);
	snd_async_handler_t *pcm_callback;
	int err;
	err = snd_async_add_pcm_handler(&pcm_callback, pcm_handle, &audioCallback, this);
	if(err != 0)
	  printf("Error setting PCM callback for ALSA: %s\n",snd_strerror(err));
	RUNNING = true;
	//queueThread->Start();
	if(snd_pcm_start(pcm_handle) != 0) {
	  printf("ERROR starting Alsa!!!");
	}else{
	  started = true;
	  printf("started!!!\n");
	}
    }

    void AudioDevice::stop()
    {
	if(!started) return;
	started = false;
    }



#elif defined (JACK_AUDIO)
    //-----------------------------------
    //  JACK
    //-----------------------------------
 
    void jack_shutdown (void *arg)
    {
      printf("Jack Server Died!!\n"); 
      exit (1);
    }

    int audioCallback(jack_nframes_t framesPerBuffer, void *userData)
    {
      AudioDevice* ad = AudioDevice::I();
      //jack_port_t* output_port_left = ad->out_port_left();
      jack_port_t** output_ports = ad->out_ports();
      jack_port_t** input_ports = ad->in_ports();

      jack_default_audio_sample_t* outputBuffers[UNIV::CHANNELS];
      //jack_default_audio_sample_t *inputBuffer = 0;
      for(int i=0;i<UNIV::CHANNELS;i++) {
	outputBuffers[i] = (jack_default_audio_sample_t *) jack_port_get_buffer (output_ports[i], framesPerBuffer);
      }

      TaskScheduler* sched = static_cast<TaskScheduler*>(userData);
      //sched->getGuard()->signal();	
      UNIV::DEVICE_TIME = UNIV::DEVICE_TIME + UNIV::FRAMES;
      UNIV::TIME = UNIV::DEVICE_TIME;

      if(AudioDevice::CLOCKBASE < 1.0) AudioDevice::CLOCKBASE = getRealTime();
      AudioDevice::REALTIME = getRealTime();

      //printf("DEVICE CALLBACK %lld\n",UNIV::DEVICE_TIME); 
      //int channels = 2;
      int channels = UNIV::CHANNELS;
      uint64_t numOfSamples = (uint64_t) (framesPerBuffer * channels);
      sched->getGuard()->signal();
      
      void* dsp_closure = AudioDevice::I()->getDSPClosure();
      void* cache_closure = 0;
      if(dsp_closure == 0) return 0;
      cache_closure = ((void*(*)()) dsp_closure)(); // get actual LLVM closure from _getter() !
      
      if(AudioDevice::I()->getDSPWrapper()) { // if true then we must be sample by sample
	dsp_f_ptr dsp_wrapper = AudioDevice::I()->getDSPWrapper();
	dsp_f_ptr cache_wrapper = dsp_wrapper;
	double (*closure) (double,double,double,double*) = * ((double(**)(double,double,double,double*)) cache_closure);
	double* data = 0; 
	//llvm_zone_t* zone = llvm_zone_create(1024*1024); // 1M
	llvm_zone_t* zone = llvm_peek_zone_stack(); //(1024*1024); // 1M
        //printf("zone:%p  size:%lld  offset:%lld\n",zone,zone->size,zone->offset);
	//llvm_push_zone_stack(zone);
	for(uint32_t i=0;i<UNIV::FRAMES;i++)
	  {
	    for(uint32_t k=0; k<UNIV::CHANNELS; k++)
	      {
		outputBuffers[k][i] = audio_sanity((SAMPLE)cache_wrapper(zone, (void*)closure, /*(double)in[ii+k]*/0.0,(double)(i+UNIV::DEVICE_TIME),(double)k,data));
		//datr[i] = audio_sanity((SAMPLE)cache_wrapper(zone, (void*)closure, /*(double)in[ii+k]*/0.0,(double)(i+UNIV::DEVICE_TIME),(double)1.,data));
		llvm_zone_reset(zone);
	      }
	  }
	//llvm_pop_zone_stack();
	//llvm_zone_destroy(zone);
      }else if(AudioDevice::I()->getDSPWrapperArray()) { // if true then we must be buffer by buffer
	dsp_f_ptr_array dsp_wrapper = AudioDevice::I()->getDSPWrapperArray();
	dsp_f_ptr_array cache_wrapper = dsp_wrapper;
	//llvm_zone_t* zone = llvm_zone_create(1024*50);
	llvm_zone_t* zone = llvm_peek_zone_stack(); //(1024*1024); // 1M
	//llvm_push_zone_stack(zone);
	//cache_wrapper(zone, cache_closure, (SAMPLE*)inputBuffer,(SAMPLE*)outputBuffer,UNIV::FRAMES,UNIV::DEVICE_TIME,UNIV::CHANNELS);
	llvm_zone_reset(zone);
	//llvm_pop_zone_stack();
	//llvm_zone_destroy(zone);
      }else{ 
	//nothin to do
      }
      return 0;
    }

    AudioDevice::AudioDevice() : started(false), buffer(0), dsp_closure(0), dsp_wrapper(0), dsp_wrapper_array(0)
    {
    }
	
    AudioDevice::~AudioDevice()
    {
      if(client) jack_client_close(client);
    }

    void AudioDevice::start()
    {
        //UNIV::CHANNELS = 2;
        //UNIV::SAMPLERATE = 44100;

	const char **ports;

	/* try to become a client of the JACK server */
	jack_status_t status;

	if ((client = jack_client_open ("Extempore",JackNullOption,&status)) == 0) {	  
	  fprintf (stderr, "jack server not running?  %d\n",client);
	  exit(1);
	}
	
        /* tell the JACK server to call `process()' whenever
	   there is work to be done.
	*/
	jack_set_process_callback (client, audioCallback, (void*)TaskScheduler::I());

	/* tell the JACK server to call `jack_shutdown()' if
	   it ever shuts down, either entirely, or if it
	   just decides to stop calling us.
	*/
	jack_on_shutdown (client, jack_shutdown, 0);

	/* get the current sample rate. */ 
	UNIV::SAMPLERATE = jack_get_sample_rate (client);	

	//UNIV::FRAMES = jack_get_buffer_size (client);
	jack_set_buffer_size(client,UNIV::FRAMES);

	/* create ports */
	//input_port = jack_port_register (client, "input", JACK_DEFAULT_AUDIO_TYPE, JackPortIsInput, 0);
	output_ports = (jack_port_t**) malloc(sizeof(jack_port_t*)*UNIV::CHANNELS);
	for(int i=0;i<UNIV::CHANNELS;i++) {	  
	  char name[256];
	  sprintf(name,"output_%d",i);
	  output_ports[i] = jack_port_register (client, name, JACK_DEFAULT_AUDIO_TYPE, JackPortIsOutput, 0);
	}
 	

	if(started) return;
       /* tell the JACK server that we are ready to roll */
	if (jack_activate (client)) {
	  ascii_text_color(1,1,10);	
	  fprintf (stderr, "Jack did not activate us :(\n");
	  ascii_text_color(0,9,10);	  
	  return;
	}
	
	//const char** ports;
	if ((ports = jack_get_ports (client, NULL, NULL, JackPortIsPhysical|JackPortIsInput)) == NULL) {
	   fprintf(stderr, "Cannot find any physical playback ports\n");
	   exit(1);
	}

	for(int i=0;i<UNIV::CHANNELS;i++) {
	  if(jack_connect (client, jack_port_name (output_ports[i]), ports[i])) {
	    ascii_text_color(1,1,10);
	    fprintf (stderr, "JACK initialization error: cannot connect to output port number %d\n", (i+1));
	    ascii_text_color(0,9,10);
	    exit(1);
	  }
	}

	free(ports);

        UNIV::initRand();
	RUNNING = true;
	//queueThread->Start();
	started = true;

	ascii_text_color(1,7,10);
	std::cout << "---         JACK         ----" << std::endl;
        std::cout << "Loaded Default Audio Device: " << std::endl;	
	ascii_text_color(0,7,10);	
        std::cout << "SampleRate\t: " << std::flush;
	ascii_text_color(1,6,10);	
	std::cout << UNIV::SAMPLERATE << std::endl << std::flush;
	ascii_text_color(0,7,10);	
        std::cout << "Channels\t: " << std::flush;
	ascii_text_color(1,6,10);	
	std::cout << UNIV::CHANNELS << std::endl << std::flush;
	ascii_text_color(0,7,10);	
        std::cout << "Frames\t\t: " << std::flush;
	ascii_text_color(1,6,10);	
	std::cout << UNIV::FRAMES << std::endl << std::flush;
	ascii_text_color(0,7,10);	

    }

    void AudioDevice::stop()
    {
	if(!started) return;
	started = false;
    }

#elif defined (COREAUDIO) //(TARGET_OS_MAC)


    //-----------------------------------
    //  CORE AUDIO
    //----------------------------------- 
    OSStatus errorCallback(AudioDeviceID		inDevice,
			   UInt32			inChannel,
			   Boolean			isInput,
			   AudioDevicePropertyID	inPropertyID,
			   void*		inClientData)
    {
       if(UNIV::DEVICE_TIME>4096) std::cout << "This has been called because you've run out of processing time!!!" << std::endl;        
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
	UNIV::DEVICE_TIME = UNIV::DEVICE_TIME + UNIV::FRAMES;
	UNIV::TIME = UNIV::DEVICE_TIME;

	if(AudioDevice::CLOCKBASE < 1.0) AudioDevice::CLOCKBASE = CFAbsoluteTimeGetCurrent() + kCFAbsoluteTimeIntervalSince1970; 
	AudioDevice::REALTIME = CFAbsoluteTimeGetCurrent()+kCFAbsoluteTimeIntervalSince1970;
        
        if(false) { //UNIV::TIME != device_time) {

	    std::cout << std::endl << "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=" << std::endl;
	    std::cout << "UNIV Timeing Sychronization problem!!!:" << std::endl;
	    std::cout << "UNIV::DEVICE_TIME [" << UNIV::DEVICE_TIME << "]" << std::endl;
	    std::cout << "ADJUSTED_DEVICE_TIME [" << device_time << "]" << std::endl;
	    std::cout << "NON_ADJUSTED_DEVICE_TIME [" << t << "]" << std::endl;
	    std::cout << "ADJUSTMENT_VALUE [" << start_time << "]" << std::endl;
	    std::cout << "NumOfSamples [" << numOfSamples << "]" << std::endl;
	    std::cout << std::endl << "Adjusting by " << (device_time - UNIV::DEVICE_TIME) << " samples to re-sync" << std::endl << std::endl;
	    std::cout << "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=" << std::endl << std::endl;

	    start_time += (device_time) - UNIV::DEVICE_TIME;
        } 
	sched->getGuard()->signal();

	void* dsp_closure = AudioDevice::I()->getDSPClosure();
	void* cache_closure = 0;
	if(dsp_closure == 0) return 0;
	cache_closure = ((void*(*)()) dsp_closure)(); // get actual LLVM closure from _getter() !
		
		
	if(AudioDevice::I()->getDSPWrapper()) { // if true then we must be sample by sample
	    dsp_f_ptr dsp_wrapper = AudioDevice::I()->getDSPWrapper();
	    dsp_f_ptr cache_wrapper = dsp_wrapper;
	    double (*closure) (double,double,double,double*) = * ((double(**)(double,double,double,double*)) cache_closure);
	    double* data = 0; 
	    //llvm_zone_t* zone = llvm_zone_create(1024*1024); // 1M 
	    llvm_zone_t* zone = llvm_peek_zone_stack();
	    //llvm_push_zone_stack(zone);
	    for(uint32_t i=0;i<UNIV::FRAMES;i++) {
	      uint32_t ii = i*UNIV::CHANNELS;
	      SAMPLE* dat = (SAMPLE*) outOutputData->mBuffers[0].mData;
	      for(uint32_t k=0; k<UNIV::CHANNELS; k++) {   
		dat[ii+k] = audio_sanity((SAMPLE)cache_wrapper(zone, (void*)closure, 0.0,(double)(i+UNIV::DEVICE_TIME),(double)k,data)); 
		llvm_zone_reset(zone);
	      }
	    }
	    //llvm_pop_zone_stack();
	    //llvm_zone_destroy(zone);
	}else if(AudioDevice::I()->getDSPWrapperArray()) { // if true then we must be buffer by buffer
	    dsp_f_ptr_array dsp_wrapper = AudioDevice::I()->getDSPWrapperArray();
	    dsp_f_ptr_array cache_wrapper = dsp_wrapper;
	    //llvm_zone_t* zone = llvm_zone_create(1024*50);
	    llvm_zone_t* zone = llvm_peek_zone_stack();
	    //llvm_push_zone_stack(zone);
	    SAMPLE* outputBuffer = (SAMPLE*) outOutputData->mBuffers[0].mData;
	    SAMPLE* inputBuffer = (SAMPLE*) inInputData->mBuffers[0].mData;
	    cache_wrapper(zone, cache_closure, (SAMPLE*)inputBuffer,(SAMPLE*)outputBuffer,UNIV::FRAMES,UNIV::DEVICE_TIME,UNIV::CHANNELS);
	    llvm_zone_reset(zone);
	    //llvm_pop_zone_stack();
	    //llvm_zone_destroy(zone);
	}else{ 
	    //nothin to do
	}
		
        return 0;
    }

    AudioDevice::AudioDevice() : started(false), buffer(0), dsp_closure(0), dsp_wrapper(0), dsp_wrapper_array(0)
    {

        // // get the default output device
        // UInt32 count = (UInt32) sizeof(device);
        // OSStatus err = AudioHardwareGetProperty(kAudioHardwarePropertyDefaultOutputDevice,&count,(void*)&device);
	// ascii_text_color(1,1,10);
        // if(err != kAudioHardwareNoError) {
        //     std::cout << "Error while trying to retrieve default audio device" << std::endl;
        // }
        // // get the default audio stream
        // AudioStreamBasicDescription stream;
        // count = sizeof(stream);
        // err = AudioDeviceGetProperty(device,0,false,kAudioDevicePropertyStreamFormat,&count,(void*)&stream);
        // if(err != kAudioHardwareNoError) {
        //     std::cout << "Error when reading audio device stream format" << std::endl;
        // }
        // err = AudioDeviceSetProperty(device,NULL,0,false,kAudioDevicePropertyBufferFrameSize,4,&UNIV::FRAMES);
        // while(err != 0) {
        //     std::cout << "Error setting Frames To: " << UNIV::FRAMES << std::endl;
        //     UNIV::FRAMES = 1024;
        //     std::cout << "Adjusting Frames To: " << UNIV::FRAMES << std::endl;
        //     err = AudioDeviceSetProperty(device,NULL,0,false,kAudioDevicePropertyBufferFrameSize,4,&UNIV::FRAMES);            
        // }
        // err = AudioDeviceAddPropertyListener(device, 0, false, kAudioDeviceProcessorOverload, &errorCallback, NULL);
        // if(err != 0) {
        //     std::cout << "Error setting Processor Overload Property Listener" << std::endl;
        // }


	// UNIV::CHANNELS = static_cast<int>(stream.mChannelsPerFrame);
        // UNIV::SAMPLERATE = static_cast<int>(stream.mSampleRate);
        // //ascii_text_color(0,7,10);        
        // //std::cout << "Loaded Audio Device With Stream: " << std::endl;
        // //std::cout << "Format\t: " << stream.mFormatID << std::endl;
        // //std::cout << "Format Flags\t: " << stream.mFormatFlags << std::endl;
        // //std::cout << "Bytes Per Packet\t: " << stream.mBytesPerPacket << std::endl;
        // //std::cout << "Frames Per Packet\t: " << stream.mFramesPerPacket << std::endl;
        // //std::cout << "Bytes Per Frame\t: " << stream.mBytesPerFrame << std::endl;
        // //std::cout << "Channels Per Frame\t: " << stream.mChannelsPerFrame << std::endl;
        // //std::cout << "Bits Per Channel\t: " << stream.mBitsPerChannel << std::endl;

        // //std::cout << "SampleRate\t: " << UNIV::SAMPLERATE << std::endl;
        // //std::cout << "Channels\t: " << UNIV::CHANNELS << std::endl;
	// //std::cout << "Frames:\t\t" << UNIV::FRAMES << std::endl;

        // ascii_text_color(1,7,10);
	// std::cout << "---      CoreAudio     ----" << std::endl;
        // std::cout << "Loaded Default Audio Device: " << std::endl;	
	// ascii_text_color(0,7,10);	
        // std::cout << "SampleRate\t: " << std::flush;
	// ascii_text_color(1,6,10);	
	// std::cout << UNIV::SAMPLERATE << std::endl << std::flush;
	// ascii_text_color(0,7,10);	
        // std::cout << "Channels\t: " << std::flush;
	// ascii_text_color(1,6,10);	
	// std::cout << UNIV::CHANNELS << std::endl << std::flush;
	// ascii_text_color(0,7,10);	
        // std::cout << "Frames\t\t: " << std::flush;
	// ascii_text_color(1,6,10);	
	// std::cout << UNIV::FRAMES << std::endl << std::flush;
	// ascii_text_color(0,7,10); 

	// err = AudioDeviceAddIOProc(device, &audioCallback, TaskScheduler::I());
	
	// ascii_text_color(1,1,10);
        // if (err != kAudioHardwareNoError) { std::cout << "Audio harware setup error of some kind!!!!: " << err << std::endl; exit(1);} 
	// ascii_text_color(0,7,10);
    }
	
    AudioDevice::~AudioDevice() 
    {
	RUNNING = false;
    }
	
    void AudioDevice::start() 
    {
        // get the default output device
        UInt32 count = (UInt32) sizeof(device);
        OSStatus err = AudioHardwareGetProperty(kAudioHardwarePropertyDefaultOutputDevice,&count,(void*)&device);
	ascii_text_color(1,1,10);
        if(err != kAudioHardwareNoError) {

            std::cout << "Error while trying to retrieve default audio device" << std::endl;
	    exit(1);
        }
        // get the default audio stream
        AudioStreamBasicDescription stream;
        count = sizeof(stream);
        err = AudioDeviceGetProperty(device,0,false,kAudioDevicePropertyStreamFormat,&count,(void*)&stream);
        if(err != kAudioHardwareNoError) {
            std::cout << "Error when reading audio device stream format" << std::endl;
	    exit(1);
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

        //ascii_text_color(0,7,10);        
        //std::cout << "Loaded Audio Device With Stream: " << std::endl;
        //std::cout << "Format\t: " << stream.mFormatID << std::endl;
        //std::cout << "Format Flags\t: " << stream.mFormatFlags << std::endl;
        //std::cout << "Bytes Per Packet\t: " << stream.mBytesPerPacket << std::endl;
        //std::cout << "Frames Per Packet\t: " << stream.mFramesPerPacket << std::endl;
        //std::cout << "Bytes Per Frame\t: " << stream.mBytesPerFrame << std::endl;
        //std::cout << "Channels Per Frame\t: " << stream.mChannelsPerFrame << std::endl;
        //std::cout << "Bits Per Channel\t: " << stream.mBitsPerChannel << std::endl;

        //std::cout << "SampleRate\t: " << UNIV::SAMPLERATE << std::endl;
        //std::cout << "Channels\t: " << UNIV::CHANNELS << std::endl;
	//std::cout << "Frames:\t\t" << UNIV::FRAMES << std::endl;

        ascii_text_color(1,7,10);
	std::cout << "---      CoreAudio     ----" << std::endl;
        std::cout << "Loaded Default Audio Device: " << std::endl;	
	ascii_text_color(0,7,10);	
        std::cout << "SampleRate\t: " << std::flush;
	ascii_text_color(1,6,10);	
	std::cout << UNIV::SAMPLERATE << std::endl << std::flush;
	ascii_text_color(0,7,10);	
        std::cout << "Channels\t: " << std::flush;
	ascii_text_color(1,6,10);	
	std::cout << UNIV::CHANNELS << std::endl << std::flush;
	ascii_text_color(0,7,10);	
        std::cout << "Frames\t\t: " << std::flush;
	ascii_text_color(1,6,10);	
	std::cout << UNIV::FRAMES << std::endl << std::flush;
	ascii_text_color(0,7,10); 

	err = AudioDeviceAddIOProc(device, &audioCallback, TaskScheduler::I());
	
	ascii_text_color(1,1,10);
        if (err != kAudioHardwareNoError) { std::cout << "Audio harware setup error of some kind!!!!: " << err << std::endl; exit(1);} 
	ascii_text_color(0,7,10);


	if(started) return;
	UNIV::initRand();
						
	err = AudioDeviceStart(device, &audioCallback);
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
#else
    //-----------------------------------
    //  PORT AUDIO
    //-----------------------------------  
    int audioCallback(const void* inputBuffer, void* outputBuffer, unsigned long framesPerBuffer, const PaStreamCallbackTimeInfo *timeInfo, PaStreamCallbackFlags statusFlags, void *userData)
    {        
        TaskScheduler* sched = static_cast<TaskScheduler*>(userData);
	//sched->getGuard()->signal();
	UNIV::DEVICE_TIME = UNIV::DEVICE_TIME + UNIV::FRAMES;
	UNIV::TIME = UNIV::DEVICE_TIME;

	if(AudioDevice::CLOCKBASE < 1.0) AudioDevice::CLOCKBASE = getRealTime(); 
	AudioDevice::REALTIME = getRealTime();

	device_time = UNIV::DEVICE_TIME;
        if(UNIV::DEVICE_TIME != device_time) std::cout << "Timeing Sychronization problem!!!  UNIV::TIME[" << UNIV::TIME << "] DEVICE_TIME[ " << device_time << "]" << std::endl; 

        int channels = 2;
        uint64_t numOfSamples = (uint64_t) (framesPerBuffer * channels);
	sched->getGuard()->signal();	
	
	void* dsp_closure = AudioDevice::I()->getDSPClosure();
	void* cache_closure = 0;
	if(dsp_closure == 0) { memset(outputBuffer,0,(UNIV::CHANNELS*UNIV::FRAMES*sizeof(SAMPLE))); return 0; }
	cache_closure = ((void*(*)()) dsp_closure)(); // get actual LLVM closure from _getter() !

	//double indata[UNIV::IN_CHANNELS];
	double* indata = (double*) malloc(UNIV::IN_CHANNELS*8);
				
	if(AudioDevice::I()->getDSPWrapper()) { // if true then we must be sample by sample
	    dsp_f_ptr dsp_wrapper = AudioDevice::I()->getDSPWrapper();
	    dsp_f_ptr cache_wrapper = dsp_wrapper;
	    double (*closure) (double,double,double,double*) = * ((double(**)(double,double,double,double*)) cache_closure);
	    double* data = 0; 
	    //llvm_zone_t* zone = llvm_zone_create(1024*1024); // 1M
	    llvm_zone_t* zone = llvm_peek_zone_stack();
	    //llvm_push_zone_stack(zone);
	    for(uint32_t i=0;i<UNIV::FRAMES;i++)
	    {
		uint32_t iout = i*UNIV::CHANNELS;
		uint32_t iin = i*UNIV::IN_CHANNELS;
		SAMPLE* dat = (SAMPLE*) outputBuffer;
		SAMPLE* in = (SAMPLE*) inputBuffer;
		for(int k=0;k<UNIV::IN_CHANNELS;k++) indata[k]=(double)in[iin+k];

		if(UNIV::IN_CHANNELS==UNIV::CHANNELS) {
		  for(uint32_t k=0; k<UNIV::CHANNELS; k++)
		    {		  
		      dat[iout+k] = audio_sanity((SAMPLE)cache_wrapper(zone, (void*)closure, (double)in[iin+k], (double)(i+UNIV::DEVICE_TIME),(double)k,&(indata[0])));
		      llvm_zone_reset(zone);
		    }
		}else if(UNIV::IN_CHANNELS==1){
		  for(uint32_t k=0; k<UNIV::CHANNELS; k++)
		    {		  
		      dat[iout+k] = audio_sanity((SAMPLE)cache_wrapper(zone, (void*)closure, (double)in[iin], (double)(i+UNIV::DEVICE_TIME),(double)k,&(indata[0])));
		      llvm_zone_reset(zone);
		    }		  
		}else{
		  for(uint32_t k=0; k<UNIV::CHANNELS; k++)
		    {		  
		      dat[iout+k] = audio_sanity((SAMPLE)cache_wrapper(zone, (void*)closure, 0.0,(double)(i+UNIV::DEVICE_TIME),(double)k,&(indata[0])));
		      llvm_zone_reset(zone);
		    }
		}
	    }
	    //llvm_pop_zone_stack();
	    //llvm_zone_destroy(zone);
	}else if(AudioDevice::I()->getDSPWrapperArray()) { // if true then we must be buffer by buffer
	    dsp_f_ptr_array dsp_wrapper = AudioDevice::I()->getDSPWrapperArray();
	    dsp_f_ptr_array cache_wrapper = dsp_wrapper;
	    void (*closure) (SAMPLE*,SAMPLE*,SAMPLE,void*) = * ((void(**)(SAMPLE*,SAMPLE*,SAMPLE,void*)) cache_closure);
	    llvm_zone_t* zone = llvm_peek_zone_stack();
	    SAMPLE* indat = (SAMPLE*) inputBuffer;
	    SAMPLE* outdat = (SAMPLE*) outputBuffer;
	    cache_wrapper(zone, (void*)closure, indat, outdat, (SAMPLE)UNIV::DEVICE_TIME, userData);

	    // static SAMPLE bufin[32]; 
	    // static SAMPLE bufout[32];
	    // int soffset = 0; // sample chunk offset
	    // for(int i=0;i<UNIV::FRAMES/32;i++) { // how many chunks of 32 do we process?
	    //   soffset = i*32*UNIV::CHANNELS;
	    //   for(int j=0;j<UNIV::CHANNELS;j++) {
	    // 	// turn interleaved into non-interleaved
	    // 	if(inputBuffer) for(int k=0;k<32;k++) bufin[k] = indat[soffset+j+(k*UNIV::CHANNELS)];
	    // 	cache_wrapper(zone, (void*)closure, bufin, bufout, (SAMPLE)(UNIV::DEVICE_TIME+(i*32)),(SAMPLE)j,userData);
	    // 	// turn non-interleaved back into interleaved
	    // 	for(int k=0;k<32;k++) outdat[(soffset+j+(k*UNIV::CHANNELS))] = bufout[k];
	    //   }
	    // }
	    //printf("soffset: %d\n",soffset);
	    llvm_zone_reset(zone);
	}else{ 
	    //zero out audiobuffer
	    memset(outputBuffer,0,(UNIV::CHANNELS*UNIV::FRAMES*sizeof(SAMPLE)));
	    //nothin to do
	}
        return 0;
    }

    AudioDevice::AudioDevice() : started(false), buffer(0), dsp_closure(0), dsp_wrapper(0), dsp_wrapper_array(0)
    {
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

	Pa_Initialize();
        //printf("\n-----Available Audio Drivers-------\n");
        PaError err;

	int numDevices = Pa_GetDeviceCount();
	if( numDevices < 0 ) {
   	  printf("No audio devices found!\n");
	  printf( "ERROR: Pa_CountDevices returned 0x%x\n", numDevices );
          exit(1);
	}

        
	if( (UNIV::IN_CHANNELS != UNIV::CHANNELS) &&
            (UNIV::IN_CHANNELS != 1) &&
            (UNIV::IN_CHANNELS > 0)) {
	  ascii_text_color(1,5,10);
	  printf("Warning: dsp input will be 0.0, use data* for channel data\n");
	  ascii_text_color(0,7,10);
          printf("");
        }
           
        
        const   PaDeviceInfo *deviceInfo;
        const   PaHostApiInfo* apiInfo;
        /*
	for( int i=0; i<numDevices; i++ ) {
	  deviceInfo = Pa_GetDeviceInfo( i );
          apiInfo = Pa_GetHostApiInfo(deviceInfo->hostApi);
	  printf("audio device[%d]:%s api[%d]:%s inchan[%d] outchan[%d]\n",i,deviceInfo->name,deviceInfo->hostApi,apiInfo->name,deviceInfo->maxInputChannels,deviceInfo->maxOutputChannels);
	} 
        */   
        //printf("-----------------------------------\n\n");
	int inputDevice = Pa_GetDefaultInputDevice();
	int outputDevice = Pa_GetDefaultOutputDevice();     

	if(UNIV::AUDIO_DEVICE != -1) {
          PaStreamParameters pain;
          PaStreamParameters paout;

	  //std::cout << "INC: " << UNIV::IN_CHANNELS << "  OUTC: " << UNIV::CHANNELS << "  name: " << deviceInfo->name <<  std::endl;
	  deviceInfo = Pa_GetDeviceInfo( UNIV::AUDIO_DEVICE );
          pain.device=UNIV::AUDIO_DEVICE;
          if(UNIV::AUDIO_IN_DEVICE != -1) {
             deviceInfo = Pa_GetDeviceInfo( UNIV::AUDIO_IN_DEVICE );
   	     inputDevice = UNIV::AUDIO_IN_DEVICE;
             pain.device=UNIV::AUDIO_IN_DEVICE;
	  }
          pain.channelCount=UNIV::IN_CHANNELS;
          pain.hostApiSpecificStreamInfo=NULL;
          pain.sampleFormat=paFloat32; //|((UNIV::INTERLEAVED==0) ? 0 : paNonInterleaved);
          pain.suggestedLatency = deviceInfo->defaultLowInputLatency;
          pain.hostApiSpecificStreamInfo = NULL;
          PaStreamParameters* painptr = &pain;
          if(UNIV::IN_CHANNELS<1) painptr=NULL;	  

	  deviceInfo = Pa_GetDeviceInfo( UNIV::AUDIO_DEVICE );
	  outputDevice = UNIV::AUDIO_DEVICE;
          paout.channelCount=UNIV::CHANNELS;
          paout.device=UNIV::AUDIO_DEVICE;
          paout.sampleFormat=paFloat32; //|((UNIV::INTERLEAVED==0) ? 0 : paNonInterleaved);
          paout.suggestedLatency = deviceInfo->defaultLowOutputLatency;
          paout.hostApiSpecificStreamInfo = NULL;
          PaStreamParameters* paoutptr = &paout;
          if(UNIV::CHANNELS<1) paoutptr=NULL;

          err = Pa_OpenStream(&stream, painptr, paoutptr, UNIV::SAMPLERATE, UNIV::FRAMES, paNoFlag, audioCallback, (void*)TaskScheduler::I());
	}else{
          err = Pa_OpenDefaultStream(&stream, 0, UNIV::CHANNELS, paFloat32, UNIV::SAMPLERATE, UNIV::FRAMES, audioCallback, (void*)TaskScheduler::I());
	}
        // std::cout << "Input Device: " << inputDevice << std::endl;
        // std::cout << "Output Device: " << outputDevice << std::endl;

	if(err != paNoError) {
   	    ascii_text_color(1,1,10);            
	    std::cerr << "PortAudio Initialization Error: " << Pa_GetErrorText(err) << std::endl;
	    std::cerr << "PortAudio Device: " << (Pa_GetDeviceInfo( outputDevice ))->name << std::endl;
	    ascii_text_color(0,7,10); 
	    exit(1);
	}
        //UNIV::CHANNELS = 2;
        //UNIV::SAMPLERATE = 44100;

	if(started) return;
        UNIV::initRand();        

        err = Pa_StartStream(stream);
	
	if(err != paNoError) {        
           ascii_text_color(1,1,10);    
           std::cout << "PortAudio ERROR: " << Pa_GetErrorText(err) << std::endl; 
           std::cerr << "PortAudio Device: " << (Pa_GetDeviceInfo( outputDevice ))->name << std::endl;
	   ascii_text_color(0,7,10); 
	   exit(1);
        }
	
        const PaStreamInfo* info = Pa_GetStreamInfo(stream);
	//std::cout << "Stream latency: " << info->outputLatency << std::endl;       

	ascii_text_color(0,9,10);
	RUNNING = true;
	//queueThread->Start();
	started = true;

	ascii_text_color(1,7,10);
	std::cout << "---PortAudio---" << std::endl;
	ascii_text_color(0,7,10);
        std::cout << "Output Device\t: " << std::flush;
	ascii_text_color(1,6,10);	
	std::cout << (Pa_GetDeviceInfo( outputDevice ))->name << std::endl;	
	ascii_text_color(0,7,10);
        std::cout << "Input Device\t: " << std::flush;
	ascii_text_color(1,6,10);	
	std::cout << (Pa_GetDeviceInfo( inputDevice ))->name << std::endl;	
	ascii_text_color(0,7,10);
        std::cout << "SampleRate\t: " << std::flush;
	ascii_text_color(1,6,10);	
	std::cout << UNIV::SAMPLERATE << std::endl << std::flush;
	ascii_text_color(0,7,10);	
        std::cout << "Channels Out\t: " << std::flush;
	ascii_text_color(1,6,10);	
	std::cout << UNIV::CHANNELS << std::endl << std::flush;
	ascii_text_color(0,7,10);	
        std::cout << "Channels In\t: " << std::flush;
	ascii_text_color(1,6,10);	
	std::cout << UNIV::IN_CHANNELS << std::endl << std::flush;
	ascii_text_color(0,7,10);	
        std::cout << "Frames\t\t: " << std::flush;
	ascii_text_color(1,6,10);	
	std::cout << UNIV::FRAMES << std::endl << std::flush;
	ascii_text_color(0,7,10); 
        std::cout << "Latency\t\t: " << std::flush;
	ascii_text_color(1,6,10);	
	std::cout << info->outputLatency << std::endl << std::flush;
	// ascii_text_color(0,7,10); 
        // std::cout << "Interleaved\t: " << std::flush;
	// ascii_text_color(1,6,10);	
	// std::cout << ((UNIV::INTERLEAVED==0) ? "TRUE" : "FALSE") << std::endl << std::flush;
	// ascii_text_color(0,7,10);	
	std::cout << std::endl << std::flush;
	//ascii_text_color(0,7,10);

    }

    void AudioDevice::stop()
    {
	if(!started) return;
	PaError err = Pa_StopStream(stream);
	if(err != paNoError) std::cout << "PA Error: " << Pa_GetErrorText(err) << std::endl;    
	started = false;
    }

  void AudioDevice::printDevices() {
	Pa_Initialize();
        printf("\n-----Available Audio Drivers-------\n");
        PaError err;

	int numDevices = Pa_GetDeviceCount();
	if( numDevices < 0 ) {
   	  printf("No audio devices found!\n");
	  printf( "ERROR: Pa_CountDevices returned 0x%x\n", numDevices );
          exit(1);
	}
        
        const   PaDeviceInfo *deviceInfo;
        const   PaHostApiInfo* apiInfo;
	for( int i=0; i<numDevices; i++ ) {
	  deviceInfo = Pa_GetDeviceInfo( i );
          apiInfo = Pa_GetHostApiInfo(deviceInfo->hostApi);
	  printf("audio device[%d]:%s api[%d]:%s inchan[%d] outchan[%d]\n",i,deviceInfo->name,deviceInfo->hostApi,apiInfo->name,deviceInfo->maxInputChannels,deviceInfo->maxOutputChannels);
	}    
        printf("-----------------------------------\n\n");
        Pa_Terminate();
        return;
  }

#endif



} //End Namespace
