# The studio

A self-contained audio path for Extempore: polyphonic instruments written in
xtlang, a mixer with send effects, and (in `pattern.xtm`) a pattern language for
playing them. It sits alongside the older instrument libraries and shares
nothing with them except `libs/core/audio_dsp.xtm`.

```scheme
(sys:load "libs/studio/studio.xtm")
(studio:start)
(track 'bass 'acid)          ; put the acid synth on a track called bass
(note-on 'bass 36 0.8 0.5)   ; pitch, velocity, seconds
(ctl 'bass 'cutoff 900)
(preset 'bass 'squelchy)
```

`examples/studio` is the place to start as a user. The rest of this file is for
people who want to write an instrument.

## How the pieces fit

| file           | what's in it                                                 |
| -------------- | ------------------------------------------------------------ |
| `engine.xtm`   | voices, instruments, the timestamped event queue             |
| `dsp.xtm`      | delay lines, a ladder filter, smoothing, a soft clipper      |
| `fx.xtm`       | channel strip, stereo delay, plate reverb, compressor, limiter |
| `mixer.xtm`    | sixteen tracks, the send effects, the master bus, the DSP    |
| `api.xtm`      | the Scheme side: `track`, `ctl`, `preset`, `def-instrument`  |
| `pattern.xtm`  | the pattern language and its clock                           |
| `sampler.xtm`  | the sample-playback engine, and its SFZ and folder loaders   |
| `studio.xtm`   | loads all of the above                                       |
| `instruments/` | one file per instrument, loaded on first use                 |

A _voice_ is one sounding note. An _instrument_ is a set of voices, a table of
parameters they all read, and an optional effect on their sum. The engine
allocates voices, counts down note lengths and delivers parameter changes on the
right frame. An instrument file says only what one voice sounds like.

## Writing an instrument

An instrument called `beep` lives in `instruments/beep.xtm` and has three parts.

### 1. Describe it

```scheme
(sys:load-preload-check 'studio-beep)
(define *xtmlib-studio-beep-loaded* #t)

(sys:load "libs/studio/api.xtm")

(def-instrument beep
  "a sine wave with a click-free envelope"
  (params
    (attack 0.005 "seconds to fade in")
    (release 0.3 "seconds to fade out after the note ends")
    (level 0.25 "output level"))
  (presets
    (short (release 0.05))
    (long (attack 0.2) (release 1.5))))
```

Each parameter has a name, a default and a description. Use real units (Hz,
seconds, semitones) where there is one and 0.0 to 1.0 otherwise, and say which
in the description. A preset lists only the parameters it changes from the
defaults.

`def-instrument` defines, for use in xtlang:

- `(beep_attack p)` and so on: the value of each parameter, given the parameter
  array `p`. A hyphen in a parameter name becomes an underscore (`env-mod` is
  read with `(beep_env_mod p)`).
- `(beep_nparams)`: the number of parameters.
- `(beep_defaults p)`: sets every parameter to its default.

The names `gain`, `pan`, `reverb`, `delay`, `lpf`, `hpf`, `lpq`, `shape` and
`mute` belong to every track's mixer strip, so an instrument can't use them.

### 2. Write a voice

A voice function is a closure which is called once per frame for as long as its
note sounds. It reads the note from a `Voice*` and _adds_ its output to a stereo
frame.

```scheme
(bind-func beep_voice_c
  (lambda (p:SAMPLE*)
    (let ((phase:SAMPLE 0.0)
          (env:SAMPLE 0.0)
          (rate:SAMPLE 0.0))
      (lambda (v:Voice* out:SAMPLE*)
        (if (voice_start? v)
            (set! phase 0.0))
        (set! phase (+ phase (/ (voice_freq v) SRf)))
        (if (>= phase 1.0) (set! phase (- phase 1.0)))
        ;; glide the envelope towards the gate: up while held, down once released
        (set! rate (smoothing_coeff (if (> (voice_gate v) 0.5) (beep_attack p) (beep_release p))))
        (set! env (+ env (* rate (- (voice_gate v) env))))
        (frame_add out (* (beep_level p) (voice_vel v) env (sin (* STWOPI phase))))
        (if (and (< (voice_gate v) 0.5) (< env 0.001))
            (voice_done v))
        void))))
```

What a voice can ask about its note:

| macro              | meaning                                                 |
| ------------------ | ------------------------------------------------------- |
| `(voice_freq v)`   | frequency in Hz                                         |
| `(voice_pitch v)`  | the same as a MIDI note number (may be fractional)      |
| `(voice_vel v)`    | velocity, 0.0 to 1.0                                    |
| `(voice_gate v)`   | 1.0 while the note is held, 0.0 once it's released      |
| `(voice_age v)`    | frames since the note started                           |
| `(voice_start? v)` | is this the note's first frame?                         |
| `(voice_legato v)` | did the note start while the last was held? (mono only) |

The rules:

- **Restart on `voice_start?`.** The same closure plays note after note, and a
  busy instrument will steal a voice which is still sounding. Reset phases and
  retrigger envelopes on the first frame. Let envelopes restart from their
  current level, not from zero, so that a stolen voice doesn't click.
- **Call `(voice_done v)` when the sound has finished**, which for most
  instruments is when the gate is off and the envelope has faded out. A voice
  which never finishes is never freed.
- **Add to `out`, don't overwrite it.** `(frame_add out x)` adds a mono sample
  to the centre, `(frame_add_mono out x pan)` places it (0.0 left, 1.0 right)
  and `(frame_add_stereo out l r)` adds a stereo pair.
- **Read parameters every frame** (or every few frames) so that `ctl` changes
  are heard straight away. Do expensive conversions at a lower rate:
  `(if (= (% (voice_age v) 32) 0) ...)` runs on the first frame and every 32nd
  after it.
- **Keep levels sensible.** One note at full velocity should peak around -12
  dBFS (0.25), so that a chord doesn't hit the master compressor hard.

### 3. Make the instrument

```scheme
(bind-func beep_make
  (lambda ()
    (let ((inst (inst_make "beep" 8 (beep_nparams)))
          (i:i64 0))
      (beep_defaults (inst_params inst))
      (dotimes (i 8)
        (inst_set_voice inst i (beep_voice_c (inst_params inst))))
      inst)))
```

`inst_make` takes the name, the number of voices and the number of parameters.
The function must be called `<name>_make`: that's how `(track 'x 'beep)` finds
it. Each instance is made in its own 32 MB memory zone, so `zalloc` whatever
buffers the instrument needs here or in the voice constructor.

Three more things `<name>_make` can set:

- `(inst_set_mode inst INST_MONO)` for a monophonic instrument. A note which
  starts while the last is still held then arrives with `(voice_legato v)` true
  and without `voice_start?`, so the voice can slide to it. `acid.xtm` does
  this.
- `(inst_set_mode inst INST_KEYED)` with `(inst_map_key inst pitch voice-index)`
  for a kit, where each pitch is a different sound with its own voice function.
  Two pitches mapped to the same voice cut each other off, which is how an open
  hi-hat gets choked by a closed one. List the sounds in `def-instrument` with a
  `(sounds (bd 36) (sd 38) ...)` clause so that patterns can use their names.
- `(inst_set_fx inst fx)` for an effect on the summed voices (a chorus, say),
  where `fx` is a closure which takes a stereo frame (`SAMPLE*`) and processes
  it in place.

## xtlang traps

The compiler is strict in ways that are easy to trip over.

- **Both branches of an `if` must have the same type.** When the branches are
  statements, end each with `void`:
  `(if c (begin (set! i 1) void) (begin (set! flag #f) void))`. A one-armed `if`
  is fine.
- **Give float state a type.** `SAMPLE` is `float`, but a bare literal which
  nothing pins down becomes a `double` and then fails to unify. Write
  `(x:SAMPLE 0.0)` in `let` bindings. `clamp`, `min` and `max` are macros which
  expand to `if`, so `(clamp x 0.0 0.24)` in the middle of an expression can
  fail the same way: assign it to a typed variable first.
- **Keep structs free of padding.** The compiler works out a struct's size as if
  an `i64`, `double` or pointer only needed a 4-byte boundary, but LLVM puts
  them on 8-byte boundaries. A struct whose layout needs padding (an `i1` or an
  odd number of `SAMPLE`s before an `i64`, say) is then allocated a few bytes
  short, and an array of them overruns its memory. In any `bind-type`, put the
  8-byte fields first and the 4-byte fields after them in pairs, and use `i64`
  for flags.
- **"floating point constant invalid for type".** A literal which a `float`
  can't hold exactly (`0.004`, say), multiplied by a `SAMPLE` inside a
  `convert`, fails with this error. Divide by a value which is exact instead:
  `(convert (/ SRf 250.0) i64)`, not `(convert (* 0.004 SRf) i64)`.
- **A parameter's accessor is a macro, and a function of the same name loses.**
  An instrument `chip` with a parameter `ring` gets the macro `(chip_ring p)`,
  so don't also call a function `chip_ring`.
- **Declare loop variables.** `(dotimes (i n) ...)` needs `(i:i64 0)` in an
  enclosing `let`.
- **Convert explicitly.** `(convert x i64)` and `(convert n SAMPLE)`.
- **Names are global.** Prefix every function and type with the instrument's
  name.
- **A failed `bind-func` doesn't stop the file loading.** The error scrolls past
  and the next function fails with "cannot find symbol". Search the log for the
  first `Error`.
- **Macros evaluate their arguments more than once**, so don't pass
  `clamp`/`min`/`max` an expression with a side effect.

Useful constants and functions: `SRf` (sample rate), `SPI`, `STWOPI`,
`midi2frq`, `(random)` (give the result a type: `(r:SAMPLE (random))`), and
`sin cos tan tanh exp log pow sqrt fabs floor`. `poly_blep` in `audio_dsp.xtm`
rounds off the corner of a saw or square so that it doesn't alias.

## Hearing it without a sound card

`extras/studio/render` renders a file offline, far faster than real time, and
`extras/studio/analyse.py` reports on the result: peak and RMS level, clipping,
stereo balance, where the energy sits, pitch and level over time, and a
spectrogram. `extras/studio/levels.py <instrument>` plays one note with each of
an instrument's presets and prints how loud each is, which is the quick way to
check that a new preset sits level with the others.

```bash
extras/studio/render examples/studio/demos/acid.xtm /tmp/acid.wav 12
```

Time runs fast in an offline render, so nothing can be scheduled on the fly.
Queue the whole piece first as a _score_:

```scheme
(sys:load "libs/studio/studio.xtm")
(score-begin)
(track 'bass 'acid)
(score-note 0.0 'bass 36 0.8 0.2)   ; seconds from the start, then as note-on
(score-preset 2.0 'bass 'squelchy)
(score-ctl 3.0 'bass 'cutoff 900)
(score-end)
```

The same file plays in real time in a live session.
