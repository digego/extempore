---
title: Reading audio files in Extempore
---

Extempore's `AudioBuffer` library bindings provide functionality for reading and
playing back audio files. They read uncompressed WAV, AIFF and AIFF-C files (8
to 32-bit integer or 32 and 64-bit float samples); convert anything else, such
as an MP3 or FLAC file, to WAV first. Here's a short example of how to read
audio files.

First, load the `audiobuffer.xtm` library and create an `AudioBuffer` variable:

```xtlang
(sys:load "libs/core/audiobuffer.xtm")

(bind-func dsp:DSP
  (let ((ab (AudioBuffer "assets/samples/christmas_carol.aif")))
    (lambda (in time chan dat)
      (AudioBuffer_read ab chan))))

(dsp:set! dsp)
```

Once you run the code, you should hear the (long-ish) christmas carol audio file
start playing (in stereo). Notice that in the body of the `dsp` closure there's
no explicit mention of where in the file we're up to, because the current
"playhead" is stored in the `AudioBuffer` type.

You can play back from a different position in the file by supplying an extra
argument to `AudioBuffer_read`. For example, this (slightly weird) playback
approach will only advance the playhead 10% of the time:

```xtlang
(bind-func dsp:DSP
  (let ((ab (AudioBuffer "assets/samples/christmas_carol.aif"))
        (pos 0))
    (lambda (in time chan dat)
      (if (< (random) 0.1:f)
          (set! pos (+ pos 1)))
      (AudioBuffer_read ab pos chan))))
```

All the `AudioBuffer_read` functions will stop playback when they reach the end
of the audio data from the file. If you'd like to _loop_ the audio file instead,
you can use `AudioBuffer_read_looped`, which wraps the playhead back to the
start of the buffer when it reaches the end so the audio plays in a continuous
loop.

One other thing to say here is that `AudioBuffer` is both the name of the
(overloaded) constructor function and the name of the type which holds the audio
data and metadata. `AudioBuffer` is a polymorphic function, and there are
versions for e.g. reading files with non-standard samplerates, and copying audio
data from other `AudioBuffer` variables, but the simple case (shown above) just
takes a single filename argument and returns an initialised `AudioBuffer` object
(the audio data is heap-allocated).

To mess with this audio stream, let's add a low-pass filter to the left channel,
and a high-pass filter to the right channel (both with the resonance paramater
cranked right up).

```xtlang
;; need to load this library as well to get the lpf/hpf
(sys:load "libs/core/audio_dsp.xtm")

(bind-func dsp:DSP
  (let ((ab (AudioBuffer "assets/samples/christmas_carol.aif"))
        (lp (lpf_c))
        (hp (hpf_c)))
    (lambda (in time chan dat)
      (cond ((= chan 0)
             (lp (AudioBuffer_read ab chan) 400. 0.99))
            ((= chan 1)
             (hp (AudioBuffer_read ab chan) 5000. 0.99))
            (else 0.0:f)))))
```

If you're looking for more options for messing with the signal at this low
level, have a look at the functions in the `audio_dsp.xtm` library.

## Reading into your own buffer, and writing WAV files

`AudioBuffer` reads a whole file. To read part of one into memory of your own,
or to write a WAV file, use `libs/core/audiofile.xtm` (which `audiobuffer.xtm`
loads). `audiofile_read` reads interleaved samples, scaled so that full scale
is 1.0, and returns the number of frames it read (or -1 if it couldn't read the
file). This copies a file to a 24-bit WAV:

```xtlang
(sys:load "libs/core/audiofile.xtm")

(bind-func copy_to_wav
  (lambda (inpath:i8* outpath:i8*)
    (let ((frames (audiofile_frames inpath))
          (channels (audiofile_channels inpath))
          (data:SAMPLE* (halloc (* frames channels))))
      (audiofile_read inpath data 0 frames)
      (audiofile_write_wav outpath data frames channels
                           (convert (audiofile_samplerate inpath) i64)
                           AUDIOFILE_PCM_24)
      (free (cast data i8*)))))
```

`audiofile_write_wav` takes `AUDIOFILE_PCM_16`, `AUDIOFILE_PCM_24`,
`AUDIOFILE_PCM_32`, `AUDIOFILE_FLOAT_32` or `AUDIOFILE_FLOAT_64`, and clips
integer samples to [-1, 1].
