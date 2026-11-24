---
title: MIDI
description: Sending and receiving MIDI, from a temporal recursion or from the studio.
---

Extempore talks MIDI through the operating system's own API: the ALSA sequencer
on Linux, CoreMIDI on macOS and the multimedia API on Windows. There's nothing
to install. The library is `libs/core/midi.xtm`, and the studio builds MIDI
tracks on top of it.

## Finding and opening ports

```extempore
(sys:load "libs/core/midi.xtm")

(midi-ports)
;; (("Midi Through Port-0" in out) ("minilogue xd MIDI 1" in out))
```

Open a port by part of its name, in any case:

```extempore
(define syn (midi-out "mini"))
(define keys (midi-in "keystep"))
```

Evaluating those lines again gives back the same ports, so they're safe to
leave in a file you evaluate often. If nothing matches, you get a message and
`#f`, and sending to `#f` does nothing: a synth which isn't plugged in won't
stop the rest of your code.

On Linux and macOS you can also make a _virtual_ port, which other programs
see and can connect to:

```extempore
(define to-daw (midi-virtual-out "Extempore"))
```

## Sending

```extempore
(midi-note syn 1 60 0.8 0.5)     ; channel, pitch, velocity, seconds
(midi-note syn 1 60 0.8)         ; held, until...
(midi-note-off syn 1 60)

(midi-control syn 1 74 0.5)      ; controller 74 to halfway
(midi-bend syn 1 -0.25)          ; pitch bend, -1 to 1
(midi-program syn 1 5)
(midi-send syn #xFA)             ; any other message, as raw bytes
(midi-sysex syn (byte-vector #xF0 #x7D #x01 #xF7))
```

Channels are 1 to 16, as they are on the front of a synth, and velocities and
controller values run from 0 to 1.

Those all send straight away. To send at a particular time, there are `-at`
versions which take a time first (in frames, as `(now)` counts):

```extempore
(midi-note-at (+ (now) 44100) syn 1 60 0.8 0.5)
```

## In a temporal recursion

`mplay` takes what `play` takes, with a port and a channel: pitch, velocity (0
to 127), a length in beats, and a channel from 0 to 15. It needs a variable
called `beat` in scope.

```extempore
(define midi-loop
  (lambda (beat dur)
    (mplay syn (random '(60 67 70 72)) 80 dur 0)
    (callback (*metro* (+ beat (* .5 dur))) 'midi-loop (+ beat dur) dur)))

(midi-loop (*metro* 'get-beat 4) 1/4)
```

`mcc` does the same for a controller: `(mcc syn 74 100 0)`. Like `play`, both
take an offset in beats as an optional first argument.

Each message is sent at the frame of its beat, not when the callback happens to
run, so the timing is tight: within a millisecond or so of the beat. (A
callback runs at the start of an audio block, which at the default block size
can be 23 ms early or late.) These older names keep the conventions they've
always had, which is why their velocities and channels are numbered differently
from `midi-note`'s.

## Receiving

Give a port a function, and it's called with each message which arrives:

```extempore
(define on-midi
  (lambda (msg)
    (println msg)))

(midi-listen keys 'on-midi)
;; #(note-on 1 60 0.787402 188445)
;; #(note-off 1 60 0.000000 193029)
;; #(control 1 74 0.503937 201332)
```

A message is a vector: its type, channel, two values and the frame it arrived
on. Because the listener was given by name, redefining `on-midi` changes what
happens straight away. `(midi-unlisten keys)` stops it.

A listener runs on the Scheme thread, a couple of hundred times a second. For
the lowest latency (a synth you play from a keyboard, say) read the port from
your audio callback instead, with `midi_receive` in xtlang;
`examples/core/midi_synth.xtm` does that.

## In the studio

A MIDI track plays an external synth with everything that plays any other
[studio](/guides/studio/) track.

```extempore
(sys:load "libs/studio/studio.xtm")
(studio:start)

(midi-track 'lead "mini" 1)          ; the port, and the channel
(note-on 'lead 48 0.8 0.5)
(ctl 'lead 'cc74 0.5)                ; any controller, 0 to 1
(ctl 'lead 'bend -0.5 'program 12)
```

Patterns and scores play it too. Give the controllers you use names of their
own:

```extempore
(midi-controls 'lead '((cutoff 43) (resonance 44)))
(ctl 'lead 'cutoff 0.7)
```

If the synth sounds early against Extempore's own audio, hold its MIDI back
with `(midi-offset 'lead 0.012)` (in seconds).

Going the other way, a MIDI keyboard can play a track:

```extempore
(track 'keys 'epiano)
(midi-route "keystep" 'keys)         ; every channel
(midi-route "keystep" 'keys 2)       ; or just channel 2
(midi-map "keystep" 74 'keys 'release 0.05 2.0)  ; controller 74 sets a parameter
(midi-unroute "keystep")
```

`midi-map` takes the parameter's values at the bottom and top of the
controller's travel.

## Older code

`libs/external/portmidi.xtm` still has the whole PortMidi API (`Pm_Read`,
`pm_send` and the rest), now written on top of this library, so code which
uses it carries on working. The old `libs/external/midi.xtm` is
`libs/external/midi-legacy.xtm`.

## Platform notes

- on Linux, MIDI needs the ALSA sequencer (`/dev/snd/seq`), which a desktop
  has; your user may need to be in the `audio` group
- Windows has no virtual ports
- MIDI input on Windows is written but hasn't been run on a real device yet
