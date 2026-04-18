---
title: Loading and using a sampler
---

A sampler is an instrument which stores chunks of audio which can be
*triggered*---played back. Samplers are used in many different ways, such as
providing digital simulations of acoustic instruments (e.g. a piano sampler
which plays recorded samples of a *real* piano when triggered by messages from a
MIDI keyboard) or they can be used to play and manipulate non-acoustic sounds.
Since samplers are so useful, Extempore provides a built-in sampler in
`libs/external/instruments_ext.xtm`.

## Samplers 101 {#samplers-101}

You can think of a sampler as a series of 'slots', each of which contains a
sound file.

![image](/images/sampler/piano-full.png)

Each slot has a unique index, and playing the sampler generally involves
specifying the index of the sample to play, the loudness/velocity and the
duration. There are lots of subtleties to this process, such as what to do when
the duration is longer than the sample length, what to do if there isn't a
sample at the given index, etc., but at a high level that's how it works.

If you look at the filenames in each slot (although it's the audio *data* in the
slots, not the filename strings), you'll notice that they refer to
[wave](http://en.wikipedia.org/wiki/WAV) (audio) files named with [scientific
pitch notation](http://en.wikipedia.org/wiki/Scientific_pitch_notation). These
wave files could be recordings of a piano, or of a violin, or even of abstract
sound effects (although at that point we'd have to wonder why they were named
with pitches from the musical scale). The sampler doesn't care what audio data
is stored in each slot, and it's up to you to make sure that both the slot the
audio is stored in and the audio data itself make sense for whatever musical
purpose you have in mind.

For sampling pitched instruments, using [MIDI note
numbers](http://www.phys.unsw.edu.au/jw/notes.html) (middle C = 60) make sense,
and that's the convention I've used in the diagram above. If you want to trigger
the sample for middle C, just use `play-note` with a pitch argument of `60`.

The Extempore sampler doesn't *have* to be full---there can be empty slots.

![image](/images/sampler/piano-gaps.png)

In this situation, when the sampler is told to play a pitch value corresponding
to an empty slot, looks for the closest 'filled' slot, grabs that audio data,
and linearly pitch-shifts it to play at the required pitch.

For example, say the sampler is told to play sample `72` (one octave above
middle C). If the closest filled slot was middle C (slot `60`), then the sampler
would play the middle C audio file pitch-shifted up one octave. If you've ever
looked into the maths behind musical notes, you'll know that for every octave
increase in pitch, the frequency of the waveform doubles. So, to play a sample
*up an octave*, we play it back at double speed. In practice, because the audio
sample rate is constant, we only play every second sample, which has the same
effect---we're progressing through the audio data twice as fast. This is the
same reason that when you hit fast-forward on a tape player (do people still
remember tape players) while it's playing then you get a hilarious chipmunk
effect, because everyone's voices are pitch-shifted up.

If we're not pitch-shifting by a whole octave, then the maths is a bit trickier
(it's not just a matter of doubling the frequency), but the Extempore sampler
takes care of that for us (including all sorts of fancy stuff like interpolating
between individual audio samples). When `play-note` calls an empty slot in the
sampler, the pitch-shifting up or down is taken care of, and it works as though
we *did* have an audio file in that slot. The exception to this rule is if the
audio sample has a meaningful tempo---such as a full drum loop. In this case,
because the pitch-shifting is also speeding up or slowing down the sample
playback, the tempo will be altered as well. Which may be fine, but it may also
be a problem. If you're in that situation, then you'll probably want to make
sure you have audio data in all the slots you're going to trigger.

There is the potential for a loss in audio quality the further (in pitch terms)
an audio file has to be shifted. Still, the 'missing sample interpolation'
allows us to make a trade-off between sound quality and memory footprint. If
sound quality is more important, use more slots, but if low memory use is more
important, use fewer slots and let the sampler interpolate in the gaps.

## Creating a drum sampler {#creating-a-drum-sampler}

::: info
If you've loaded the sharedsystem (i.e. with `(sys:load
"examples/sharedsystem/setup.xtm")`) then you don't have to do any of the "DSP"
signal chain setup stuff (i.e. you don't have to load the `instruments_ext.xtm`
file, create the sampler `samp1`, evaluate the `dsp` closure, or call
`dsp:set!`). Basically you can just ignore the following code block, although
the ones after that are still relevant---we'll try and make it clear 😜

Ok, enough background material---let's make some noise. We'll create an instance
of Extempore's sampler called `samp1`. To do this, we use the `make-instrument`
Scheme macro (once we've loaded it from the `libs/external/instruments_ext.xtm`
library file).
:::

```xtlang
(sys:load "libs/external/instruments_ext.xtm")

;; define a sampler (called samp1) using the default sampler note kernel and effects
(make-instrument samp1 sampler)

;; add the sampler to the dsp output callback
(bind-func dsp:DSP
  (lambda (in time chan dat)
    (cond ((< chan 2)
           (samp1 in time chan dat))
          (else 0.0))))

(dsp:set! dsp)
```

But we're not done yet: the xtlang closure `samp1` has been compiled, but it
hasn't had any samples loaded into it's slots yet. So we need some drum samples,
then. Extempore (by default) ships with a set of
[808](https://en.wikipedia.org/wiki/Roland_TR-808) samples (in the
`assets/samples/808` folder) which will do the job nicely.

::: info
If you want to use other drum samples, e.g. the [Salamander
drumkit](http://download.linuxaudio.org/musical-instrument-libraries/sfz/salamander_drumkit_v1.tar.7z)
then it shouldn't be too hard to modify these steps---although obviously the
filenames will be different.

Let's load those samples into the `samp1` sampler. First, download the
Salamander drum kit samples, unzip and untar them and put the files somewhere.
On my computer, I've put them into
`/Users/ben/Music/sample-libs/drums/salamander`, but you can put them wherever
you like. Just make sure that you set the right path in your code if you're
following along.

We're going to load (some of) these files into our `samp1` sampler one at a time
using the `sampler-populate-slot` function (note that the path to each `.aif` file
is relative to your Extempore folder---if you're loading samples from somewhere
else you'll need to include the full path).
:::

```xtlang
(sampler-populate-slot samp1 "assets/samples/808/36.aif" *gm-kick-2* 0 0 0 1)
(sampler-populate-slot samp1 "assets/samples/808/37.aif" *gm-side-stick* 0 0 0 1)
(sampler-populate-slot samp1 "assets/samples/808/38.aif" *gm-snare* 0 0 0 1)
(sampler-populate-slot samp1 "assets/samples/808/39.aif" *gm-hand-clap* 0 0 0 1)
(sampler-populate-slot samp1 "assets/samples/808/40.aif" *gm-snare-2* 0 0 0 1)
(sampler-populate-slot samp1 "assets/samples/808/41.aif" *gm-low-floor-tom* 0 0 0 1)
(sampler-populate-slot samp1 "assets/samples/808/42.aif" *gm-closed-hi-hat* 0 0 0 1)
(sampler-populate-slot samp1 "assets/samples/808/43.aif" *gm-hi-floor-tom* 0 0 0 1)
(sampler-populate-slot samp1 "assets/samples/808/44.aif" *gm-pedal-hi-hat* 0 0 0 1)
(sampler-populate-slot samp1 "assets/samples/808/45.aif" *gm-low-tom* 0 0 0 1)
(sampler-populate-slot samp1 "assets/samples/808/46.aif" *gm-open-hi-hat* 0 0 0 1)
(sampler-populate-slot samp1 "assets/samples/808/47.aif" *gm-low-mid-tom* 0 0 0 1)
(sampler-populate-slot samp1 "assets/samples/808/48.aif" *gm-hi-mid-tom* 0 0 0 1)
(sampler-populate-slot samp1 "assets/samples/808/49.aif" *gm-crash* 0 0 0 1)
(sampler-populate-slot samp1 "assets/samples/808/50.aif" *gm-hi-tom* 0 0 0 1)
(sampler-populate-slot samp1 "assets/samples/808/51.aif" *gm-ride* 0 0 0 1)
(sampler-populate-slot samp1 "assets/samples/808/52.aif" *gm-chinese* 0 0 0 1)
(sampler-populate-slot samp1 "assets/samples/808/53.aif" *gm-ride-bell* 0 0 0 1)
(sampler-populate-slot samp1 "assets/samples/808/54.aif" *gm-tambourine* 0 0 0 1)
(sampler-populate-slot samp1 "assets/samples/808/55.aif" *gm-splash* 0 0 0 1)
(sampler-populate-slot samp1 "assets/samples/808/56.aif" *gm-cowbell* 0 0 0 1)
(sampler-populate-slot samp1 "assets/samples/808/57.aif" *gm-crash-2* 0 0 0 1)
(sampler-populate-slot samp1 "assets/samples/808/58.aif" *gm-vibraslap* 0 0 0 1)
(sampler-populate-slot samp1 "assets/samples/808/59.aif" *gm-ride-2* 0 0 0 1)
(sampler-populate-slot samp1 "assets/samples/808/60.aif" *gm-hi-bongo* 0 0 0 1)
(sampler-populate-slot samp1 "assets/samples/808/61.aif" *gm-low-bongo* 0 0 0 1)
(sampler-populate-slot samp1 "assets/samples/808/62.aif" *gm-mute-hi-conga* 0 0 0 1)
(sampler-populate-slot samp1 "assets/samples/808/63.aif" *gm-hi-conga* 0 0 0 1)
(sampler-populate-slot samp1 "assets/samples/808/64.aif" *gm-low-conga* 0 0 0 1)
(sampler-populate-slot samp1 "assets/samples/808/65.aif" *gm-hi-timbale* 0 0 0 1)
(sampler-populate-slot samp1 "assets/samples/808/66.aif" *gm-low-timbale* 0 0 0 1)
```

If that works properly, some info will be printed to the log about the audio
files which have been loaded into the sampler. They should look something like
this:

```
file name:     assets/samples/808/36.aif
samplerate:    44100
channels:      2
frames:        11473
samples read:  22946
---------------
file name:     assets/samples/808/37.aif
samplerate:    44100
channels:      2
frames:        1739
samples read:  3478
---------------

and lots more like this ...
```

If the log doesn't show something like that, then there are a few things which
could have gone wrong:

-   have you set up `libsndfile` properly on your system?
-   are the pathnames to to samples correct?
-   did you define the `samp1` sampler and did it compile properly?

Assuming things worked properly, we should be able to play our `samp1` sampler.

```xtlang
;; evaluate these as you see fit!
(play-note (now) samp1 *gm-kick-2* 80 44100)
(play-note (now) samp1 *gm-snare* 80 44100)
(play-note (now) samp1 *gm-closed-hi-hat* 80 44100)
```

Using the extempore [pattern language](pattern-language.md) you can make loops really easily. The
[pattern language guide](pattern-language.md) has much more info on how it all works, but
if you just want a teaser here's a standard back-beat pattern.

```xtlang
(sys:load "libs/core/pattern-language.xtm")
(:> backbeat 4 0 (play samp1 @1 80 dur) (list *gm-kick-2* *gm-snare* *gm-kick-2* *gm-snare*))
(:> backbeat-hats 1 1/2 (play samp1 @1 80 dur) (list *gm-closed-hi-hat*))
```

If you want to turn it off, just change the `:>`s to `:|` and re-evaluate.

## Play-note parameters for the Extempore sampler

Triggering a series of drum samples like this is just one use-case for a
sampler, and (arguably) not even the most interesting one. There are a couple of
features of the sampler which are relevant here:

1. the sampler supports interpolating (with pitch-shifting) from the nearest
   slot, so you can load as little as one sample into the slot and then
   re-trigger it at lots of different pitches

2. during playback, you can provide three _extra_ arguments to
   `play`/`play-note` (i.e. after the `dur` argument):
   1. pan (0--1)
   2. offset (in samples) to specify how far into the audio file to start playback
   3. reverse? (yes for value > 0) to reverse playback

If you want to see how this might work, try loading just _one_ of the drum
samples above into bank 1 (a new bank---in the previous section we loaded them
all into bank 0) of our `samp1` sampler:

```xtlang
(sampler-populate-slot samp1 "assets/samples/808/38.aif" *gm-snare* 0 0 1 1)
```

Then, we can mess around with the playback:
   
```xtlang
;; this is just "normal" playback
(play-note (now) samp1 *gm-snare* 80 44100 1 0.5 0 0)

;; random offset (not so interesting with such a short sample)
(play-note (now) samp1 *gm-snare* 80 44100 1 0.5 (random 100) 0)

;; playback sample in reverse
(play-note (now) samp1 *gm-snare* 80 44100 1 0.5 0 1)
```

Of course, these examples are still pretty limited when we've only got the
(short) 808 samples to work with---you can imagine that you can have much more
fun if you load & manipulate your own samples.

## Creating a piano sampler {#creating-a-piano-sampler}

::: info
Note: Extempore actually ships with a bunch of piano samples in
`assets/samples/piano/`, which are loaded by default as part of the
sharedsystem. But this example is instructive if you want to load some other
samples.

Let's add one more sampler---this time a `samp2`.
:::

```xtlang
(make-instrument samp2 sampler)

;; add the samp2 sampler to the dsp output callback
;; note how the samp1 is still there from before
(bind-func dsp:DSP
  (lambda (in time chan dat)
    (cond ((< chan 2)
           (+ (samp1 in time chan dat)
              (samp2 in time chan dat)))
          (else 0.0))))
```

You might be wondering why we need another sampler (`samp2`) when we could just
use the next available bank from `samp1` (each sampler has 16 banks in total,
indexed from `0` to `15`). Sometimes that _is_ the best option, although the
main reason that you'd use a new one is to set different envelopes and/or
effects for each (since they're set on a per-sampler basis).

The Salamander piano is freely available from <https://linuxaudio.org> (direct
download link
[here](http://download.linuxaudio.org/lau/SalamanderGrandPianoV2/SalamanderGrandPianoV2_44.1khz16bit.tar.bz2))
which will do just fine for today, so ahead and download that (pretty big
though---around half a gigabyte). Again, unzip and untar the files to wherever
you put that sort of thing. This time, the wave audio files should be in a
`44.1khz16bit` subdirectory. Looking at the files in that directory (e.g. with
`ls`), we get something like

```
A0v1.wav   A5v6.wav   C4v2.wav    D#2v13.wav  F#1v1.wav   F#6v6.wav
A0v10.wav  A5v7.wav   C4v3.wav    D#2v14.wav  F#1v10.wav  F#6v7.wav
A0v11.wav  A5v8.wav   C4v4.wav    D#2v15.wav  F#1v11.wav  F#6v8.wav
A0v12.wav  A5v9.wav   C4v5.wav    D#2v16.wav  F#1v12.wav  F#6v9.wav
A0v13.wav  A6v1.wav   C4v6.wav    D#2v2.wav   F#1v13.wav  F#7v1.wav
A0v14.wav  A6v10.wav  C4v7.wav    D#2v3.wav   F#1v14.wav  F#7v10.wav

... plus many more files
```

So it looks like the files are named with a simple naming convention, which
makes use of scientific pitch notation. For example, `C4v5.wav` looks like it's
a recording of C4 (middle C) on the piano, and the `v5` part probably means that
it's the 5th velocity layer for the note C4. This means that there are multiple
sound files (called _layers_) for each note, and the sampler will choose which
one to play based on the velocity argument in the triggering call. Not all
sample libraries will have multiple velocity layers, but they're a way of
capturing the differences in sound between soft notes and loud
notes---particularly on instruments where the difference between soft and loud
is in more than just volume (such as a [Fender
Rhodes](http://en.wikipedia.org/wiki/Rhodes_piano)).

Extempore's built-in sampler doesn't support layers, in that you can't load up a
bunch of sound files (with multiple layer files per note) and have it
automatically pick the correct "layer" based on the velocity. However it does
support multiple "banks" of samplers per sampler, so you could always load each
layer into a separate bank and then write your own code to trigger the correct
bank (based on velocity, or on whatever criteria you like---it's your sampler).

The default bank is bank `0`, so if you don't want to use multiple sound banks
just load into bank `0` and forget about it. Let's take that approach for now,
so we'll choose just one of the layer files for each note to load into the
sampler. We could choose the loudest layer, or the softest layer, or a random
layer for each note (although this would lead to uneven loudness when playing
the sampler with `play-note`).

If the audio files are named according to a meaninful convention, is there a way
to make use of that? Loading each audio file individually can be pretty
time-consuming---not to mention error-prone! How do we take a list of files
(such as the output of `ls` above) and tell our sampler which files to load into
which slots?

To do this, `external/instruments.xtm` provides a helper macro called
`load-sampler`. Looking at the definition for `load-sampler`, we see that it
takes the three arguments:

1.  `sampler`, the sampler closure
2.  `path`, the path to the directory where the audio files are
3.  `parser`, a (Scheme) function

The first two arguments are fairly self-explanatory, but the third one (the
parser function) is where the magic happens.

`load-sampler` first creates a list of all the files (including hidden files) in
the `path` directory. Each filename from this list is then passed to the
function which was passed in as the `parser` argument to `load-sampler`. This
parser function's job is to take the sound file name and return a data structure
(still a list, but with a specific format), telling the sampler which slot to
load the audio data into. In particular, the parser function should return a
list with four elements:

1.  the filename
2.  the slot (midi note number) to load the file into
3.  the sample offset (i.e. if the sample should start playing part-way
    into the audio file)
4.  the sample length (or `0` to load the whole file, which is what
    you'll want to do in most cases).

So, going back to our filename example earlier, we want a filename like
`C4v5.wav` to get mapped into a list like `("C4v5.wav" 60 0 0)`. The `60` is for
middle C (C4), and the two `0` arguments mean a sample offset of `0` (so the
file starts playing from the start) and plays for its whole length. Writing a
Scheme function which can do this parsing isn't too difficult, and would look
something like this

```xtlang
(define parse-salamander-piano
  (lambda (fname)
    (let ((result (regex:matched fname "^.*([ABCDEFG][#b]?[0-9])v([0-9]+)\.(wav|aif|aiff|ogg)$")))
      (if (null? result)
          (begin (println 'Cannot 'parse 'filename: fname)
                 #f)
          ;; load 4th velocity layer only
          (if (= (string->number (caddr result)) 4)
              (list fname
                    (scientific-pitch-notation-to-midi-number (cadr result))
                    0
                    0)
              #f)))))

(load-sampler samp2
              "/Users/ben/Music/sample-libs/piano/salamander/44.1khz16bit"
              0 ;; 'sound bank' index
              parse-salamander-piano)
```

When you call `load-sampler` at the bottom of that code chunk, it should load
all the 4th velocity layers into bank `0` of the `samp2` sampler. Then try it
out:

```xtlang
(play-note (now) samp2 (random 40 80) 80 44100)
```

Awesome, we've got a piano. Success!

## Doing cool things with samplers {#doing-cool-things-with-samplers}

There are lots of possibilities at this stage. If you're interested in seeing
how to make vaguely 'conventional' musical material, then learning about the
[pattern language](pattern-language.md)
or [note-level music](note-level-music.md) would be good next steps. And if you'd like to see all this in an example
code file, see `examples/external/sampler.xtm` and other files in that
directory.
