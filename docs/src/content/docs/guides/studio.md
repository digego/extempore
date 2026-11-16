---
title: The studio
---

The studio is a set of instruments, a mixer and a pattern language which work
together out of the box. It's the quickest way to get from a fresh Extempore to
something which sounds like music.

Everything in it is written in xtlang and Scheme, in `libs/studio`. Each
instrument is one file, written to be read.

Before you start, make sure you've read the
[quickstart guide](/overview/quickstart/) and can evaluate Extempore code from
your editor.

## First sounds

```extempore
(sys:load "libs/studio/studio.xtm")
(studio:start)
```

`instruments` lists what there is to play, and `track` puts one on a mixer track
with a name of your choosing.

```extempore
(instruments)
(track 'bass 'acid)
```

The first time you use an instrument Extempore compiles it, which takes a few
seconds. Now play a note: the track, a pitch (a MIDI note number, where 60 is
middle C), a velocity from 0.0 to 1.0 and a length in seconds.

```extempore
(note-on 'bass 36 0.7 0.5)
```

Every instrument has parameters, which you can list and change while it plays,
and presets, which set them all at once.

```extempore
(params 'bass)
(ctl 'bass 'cutoff 900 'res 0.9)

(presets 'bass)
(preset 'bass 'squelchy)

;; or ask for a preset when you make the track
(track 'keys 'epiano 'suitcase)
```

## The instruments

Synthesised, needing nothing but Extempore:

| name        | what it is                                                            |
| ----------- | --------------------------------------------------------------------- |
| `acid`      | a squelchy monophonic bassline synth in the style of the TB-303       |
| `polysynth` | an eight-voice analogue polysynth with a Juno-style chorus            |
| `fm`        | a six-operator FM synth in the style of the Yamaha DX7                |
| `strings`   | a string machine with an ensemble chorus, in the style of the Solina  |
| `organ`     | a tonewheel organ with drawbars, through a rotary speaker             |
| `epiano`    | an electric piano: struck tines through a pickup, with tremolo        |
| `guitar`    | plucked strings through an amplifier: acoustic, clean and overdriven  |
| `macro`     | a macro-oscillator: six engines from Mutable Instruments Plaits       |
| `resonator` | struck and plucked physical models, from Mutable Instruments Rings    |
| `chip`      | chiptune leads, arpeggios and basses in the style of 8-bit consoles   |
| `kit808`    | a synthesised drum machine in the style of the TR-808                 |
| `kit909`    | a synthesised drum machine in the style of the TR-909                 |
| `chipkit`   | drums from pitch sweeps and a shift-register noise channel            |

Sampled, needing the Extempore assets (build with `-DASSETS=ON`, or put a copy
of the assets in an `assets` folder next to `libs`):

| name      | what it is                                                    |
| --------- | ------------------------------------------------------------- |
| `piano`   | the Salamander grand piano                                    |
| `drumkit` | the Salamander acoustic drum kit                              |
| `kit808s` | a sampled TR-808                                              |
| `sampler` | an empty sampler, for your own samples or SFZ instruments     |

Load your own sounds into a `sampler` track with
`(sampler-load 'track "path/to/instrument.sfz")` or
`(sampler-load-folder 'track "path/to/folder")`.

There are also wrappers for a set of public-domain sample libraries
(`vibraphone`, `marimba`, `glockenspiel`, `kalimba`, `harp`, `violins`, `flute`,
`trumpet`, `pizzicato`, `timpani`, `upright`, `nylon`, `bass-guitar` and
`ukulele`). Their samples live in `assets/studio`, which isn't part of the
assets download yet: without it, each says what's missing and stays silent.

Each instrument has a demo in `examples/studio/demos` which plays through its
presets, and `examples/studio/songs` has short pieces in a range of styles
(house, lo-fi, funk, dub, ambient, synthwave, rock, jazz, chamber music), each
written with the pattern language.

## The mixer

Every track has a mixer strip with the same parameters, whatever its instrument.

| parameter | what it does                               |
| --------- | ------------------------------------------ |
| `gain`    | level (1.0 is unity)                       |
| `pan`     | 0.0 left, 0.5 centre, 1.0 right            |
| `reverb`  | how much of the track goes to the reverb   |
| `delay`   | how much goes to the delay                 |
| `lpf`     | lowpass filter cutoff in Hz (20000 is off) |
| `hpf`     | highpass filter cutoff in Hz (20 is off)   |
| `lpq`     | resonance of those two filters, 0.0 to 1.0 |
| `shape`   | saturation, 0.0 to 1.0                     |
| `mute`    | 1.0 mutes the track                        |

```extempore
(ctl 'keys 'reverb 0.4 'pan 0.3)
```

There is one reverb and one delay, shared by all the tracks, and the master bus
has a compressor which glues the mix together. Their settings are parameters of
`master`.

```extempore
(params 'master)
(master 'reverb-size 0.9)
(master 'delay-time 0.25 'delay-feedback 0.6)
```

There are two reverbs to choose from: a plate, which is the default, and a
concert hall, which is bigger and smoother: `(master 'reverb-type 1)`.

`(tracks)` lists the mixer's tracks, `(levels)` prints each track's peak level,
and `(untrack 'keys)` takes a track away.

## Patterns

The studio's pattern language follows [TidalCycles](https://tidalcycles.org). A
pattern repeats every cycle (two seconds, to begin with), and you change the
music by evaluating a pattern again with something different in it.

```extempore
(track 'drums 'kit808)

(d1 (sound '(bd sd bd sd)))
```

`d1` to `d8` each play a pattern, and `(hush)` stops them all. A quoted list is
a sequence whose steps share the cycle equally. Here the steps are sounds from
the kit; `(sounds 'drums)` lists them.

### Sequences

| notation              | meaning                                              |
| --------------------- | ---------------------------------------------------- |
| `'(a b c)`            | three steps in one cycle                             |
| `'(a (b c) d)`        | a sublist squeezes its steps into one step           |
| `'(a ~ c)`            | `~` is a rest                                        |
| `'(a _ c)`            | `_` makes the step before it longer                  |
| `'(a*2 b)`            | play `a` twice in its step                           |
| `'(a/2 b)`            | stretch `a` over two cycles                          |
| `'(a!3 b)`            | three steps of `a` (`!` alone repeats the last step) |
| `'(a@3 b)`            | `a` lasts as long as three steps                     |
| `'(a? b)`             | play `a` half the time (`a?0.2`: one time in five)   |
| `'(<a b c> d)`        | one of `a`, `b`, `c` each cycle, in turn             |
| `'(#(a b c) d)`       | a vector plays its elements together                 |
| `'((euclid 3 8 a) b)` | 3 hits spread evenly over 8 steps                    |

The modifiers work on sublists too: `'((a b)*2 <c d>!2)`. Inside a quasiquoted
list, an unquoted pattern is a step.

```extempore
(d1 (sound '(bd*2 <sd cp> hh*4 <sd (sd sd)>)))
```

### Notes and parameters

A track's own name is a sound. Give it notes by name (`c4` is middle C; sharps
are `c#4` or `cs4`, flats `eb4` or `ef4`) or by MIDI number.

```extempore
(d2 (note '(c2 c2 <eb2 g2> c3))
    (sound 'bass)
    (vel '(0.6 0.6 1 0.6)))
```

`d2` takes a list of things. Patterns are merged: the first says when things
happen, and the others add to each event. So `vel` here sets the velocity of
each note, cycling through its four values.

Anything about the note or its track can be patterned.

| function                                                | what it sets                                            |
| ------------------------------------------------------- | ------------------------------------------------------- |
| `sound`                                                 | which track or kit sound plays                          |
| `note` (or `n`)                                         | pitch                                                   |
| `vel`                                                   | velocity, 0.0 to 1.0                                    |
| `legato`                                                | length as a fraction of the step (more than 1 overlaps) |
| `dur`                                                   | length in seconds                                       |
| `nudge`                                                 | seconds to shift the note later                         |
| `gain` `pan` `reverb` `delay` `lpf` `hpf` `lpq` `shape` | the track's mixer strip                                 |
| `(param 'name ...)`                                     | any parameter of the track's instrument                 |

`sine`, `saw`, `tri`, `square`, `rand` and `perlin` are signals: patterns with a
value at every moment, which run from 0 to 1 once per cycle. `between` stretches
one to the range you want.

```extempore
(d2 (note (scale 'c2 'minor '(0 0 <2 4> 7 0 <6 5> 4 7)))
    (sound 'bass)
    (param 'cutoff (between 150 1500 (slow 4 sine)))
    (pan (between 0.3 0.7 perlin)))
```

`scale` turns scale degrees into notes, and `chord` turns each root into a
chord: `(note (chord 'minor7 '(c3 f3)))`.

### Transformations

A function in the list transforms the pattern above it.

```extempore
(d1 (sound '(bd*2 <sd cp> hh*4 sd))
    (every 4 (fast 2))
    (off 1/16 (vel 0.3)))
```

| function                                  | what it does                                             |
| ----------------------------------------- | -------------------------------------------------------- |
| `(fast n)` `(slow n)`                     | play n times faster or slower                            |
| `rev`                                     | play each cycle backwards                                |
| `(late t)` `(early t)`                    | shift by `t` cycles                                      |
| `(every n f)`                             | apply `f` every `n`th cycle                              |
| `(every-from n i f)`                      | ... starting from cycle `i` of each `n`                  |
| `(off t f)`                               | layer a copy `t` cycles later, with `f` applied to it    |
| `(superimpose f)`                         | layer a copy with `f` applied                            |
| `(sometimes f)` `(often f)` `(rarely f)`  | apply `f` to some events at random                       |
| `(degrade-by p)`                          | drop events at random with probability `p`               |
| `(ply n)`                                 | repeat each event `n` times                              |
| `(echo n t fb)`                           | `n` copies, each `t` later and `fb` times as loud        |
| `(swing amount n)`                        | delay every second step of `n` per cycle                 |
| `(euclid k n)`                            | play on `k` of `n` steps, spread evenly                  |
| `(struct bools)` `(mask bools)`           | impose or mask a rhythm: `(struct '(t ~ t t))`           |
| `(within start end f)`                    | apply `f` to part of each cycle                          |
| `(chunk n f)`                             | apply `f` to one `n`th of the cycle, moving each cycle   |
| `(iter n)` `(palindrome)` `(linger frac)` | rotate, mirror, or loop the start of each cycle          |
| `(add p)` `(sub p)` `(mul p)`             | arithmetic on numbers: `(add (note 12))` is an octave up |
| `(arp n)`                                 | play a chord's notes one after another                   |
| `(strum seconds)`                         | spread a chord's notes out in time, as a strum does      |
| `(segment n)`                             | sample a signal `n` times per cycle                      |

Where one of these takes a function, it also takes a pattern, which is merged
in: `(off 1/8 (vel 0.3))` is an echo at a velocity of 0.3. And each of them
works as a plain function of a pattern as well: `(fast 2 '(a b))`.

`stack` plays patterns together, `cat` plays them one cycle each in turn, and
`seq` squeezes them into one cycle.

`(show pattern)` prints the events in a pattern's first cycle, which is the
quickest way to see what a transformation does.

### Tempo

```extempore
(cps 0.6)   ; cycles per second
(bpm 132)   ; beats per minute, at four beats to the cycle
```

`(once pattern)` plays one cycle of a pattern straight away, without looping it.

## Rendering to a file

Extempore can write its audio to a WAV file instead of a sound card, much faster
than real time, with `--audio-outfile`. Time runs too fast then to schedule
anything on the fly, so queue the whole piece first as a _score_.

```extempore
(sys:load "libs/studio/studio.xtm")
(score-begin)
(track 'drums 'kit808)
(track 'bass 'acid)
(score-pattern 0 8 (sound '(bd*2 <sd cp> hh*4 sd)))        ; cycles 0 to 8
(score-pattern 4 8 (note '(c2 c2 eb2 c3)) (sound 'bass))
(score-note 16.0 'bass 36 1.0 2.0)                         ; at 16 seconds
(score-ctl 16.0 'bass 'reverb 0.5)
(score-end)
```

```bash
extempore --audio-outfile song.wav --duration 20 --run song.xtm
```

The same file plays in real time in a live session.

## Writing an instrument

`examples/studio/04-make-an-instrument.xtm` builds an instrument from scratch
and then changes it while it plays, and `libs/studio/README.md` describes the
instrument interface in full. To keep an instrument, put it in a file of its own
in `libs/studio/instruments`: then `(instruments)` lists it and `track` loads it
by name.
