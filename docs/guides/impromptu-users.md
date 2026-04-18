---
title: Extempore for Impromptu users
---

If you're an [Impromptu](http://impromptu.moso.com.au/) user and you're thinking
about making the switch to Extempore, you've probably got a few questions about
what the difference is between Impromptu and Extempore.
[Philosophy](../overview/philosophy.md) covers
this to some extent, but this post is designed to be short and sweet, answering
three questions that Impromptu users might have:

1.  what's the same
2.  what's new or different
3.  what will I have to do to switch over

## What's the same {#whats-the-same}

-   Impromptu's Scheme interpreter has been brought into Extempore
    largely unchanged. So any vanilla Scheme code or libraries should
    work fine (e.g. the pitch class library `pc_ivl.xtm` is unchanged).
-   [time](../overview/time.md) (including
    `(now)`, `callback`, temporal recursion etc.) is exactly the same as it was
    in Impromptu.

## What's new or different {#whats-new-or-different}

There are some obvious 'big picture' differences:

-   Extempore is cross-platform (OS X, Linux & Windows)
-   Extempore is [open source](https://github.com/extemporelang/extempore)
-   Extempore is now just an executable, it no longer has a built in editor &
    log view (so [you can use your favourite editor](editor-support.md))

The biggest difference, though, is the addition of *xtlang*: a new programming
language (and compiler). Whereas Scheme was the *only* language supported in
Impromptu, in Extempore you can write Scheme code and xtlang code, and the
languages can share data and call into one another, and everyone pretty much
just gets along fine.

The other main difference between Impromptu and Extempore are in audio and
graphics. Impromptu was strongly tied to the [Core
Audio](https://developer.apple.com/library/mac/#documentation/MusicAudio/Conceptual/CoreAudioOverview/Introduction/Introduction.html)
framework for audio,
[Quartz](https://developer.apple.com/library/mac/#documentation/GraphicsImaging/Conceptual/drawingwithquartz2d/Introduction/Introduction.html)
for 2D graphics, and [OpenGL](http://www.opengl.org) for 3D graphics. Core Audio
and Quartz are both OSX-only, so there was a need to change this tight coupling
in the move to make Extempore cross-platform.

In the case of audio, audio signal processing in Extempore hasn't been replaced
by another monolithic framework---it's been replaced by home-grown xtlang code.
This 'turtles all the way down' approach brings a huge win over the previous
approach: the dynamic run-time modifiability of the whole DSP chain. If you
don't want to write your own instruments from scratch that's ok, too: Extempore
ships with some synths and samplers, and hopefully the number of built-in
instruments will grow as the community grows.

In the graphics case, Extempore uses [WebGPU](https://www.w3.org/TR/webgpu/)
(via wgpu-native) for cross-platform graphics programming. The previous
OpenGL/nanovg graphics stack has been deprecated and replaced with WebGPU, which
provides a modern, portable GPU API.

In fact, one of the key tenets of Extempore's design philosophy was to keep
functionality out of the compiled `extempore` binary and provide it in libraries
wherever possible. So Extempore's audio and graphics support, which was baked in
to Impromptu, is now user-editable. Some examples you might find interesting
are:

**Audio**

-   `examples/core/audio_101.xtm`
-   `examples/core/polysynth.xtm`
-   `examples/external/electrofunk.xtm`
-   `examples/external/portmidi.xtm`
-   `examples/core/sampler.xtm`
-   `examples/core/sndfile.xtm`

**Graphics**

-   `examples/external/webgpu-triangle.xtm`

## What will I have to change to switch from Impromptu to Extempore? {#what-will-i-have-to-change-to-switch-from-impromptu-to-extempore}

**Audio**

All AudioUnit code (i.e. any functions starting with `au:`) will have to be
changed. I know that's a pain---sorry!---but it was an inevitable consequence of
making things cross-platform. The 'setting up the AU graph' section of your code
will therefore need to be rewritten. You can either use the built-in synths and
sampler, or you can build your own instruments, or you could even use
Extempore's MIDI in/out (see `libs/external/portmidi.xtm`) to play an instrument
running *outside* of Extempore.

The good news is that from `play-note` 'up', Extempore's audio infrastructure
works pretty much exactly the same as Impromptu's. In fact, that truth is the
starting point for the tutorials on 'playing an instrument in Extempore' (coming
soon). Once the audio graph is set up, the code for *playing* music in Extempore
should be the same as it was in Impromptu.

**Graphics**

If you're using any Quartz or OpenGL functions, you'll need to rewrite them
using the new WebGPU graphics pipeline. See `examples/external/webgpu-triangle.xtm`
for a starting point.

## Getting help {#getting-help}

Many of the folks on the Extempore [mailing
list](https://groups.google.com/extemporelang) (including myself) were once
Impromptu users, so we're probably in a pretty good position to help you out if
you have any problems. Have a look around elsewhere on this blog, too. Hopefully
you'll get excited about the cool stuff that you can do in Extempore which you
just *can't* do in Impromptu, and that excitement might dull the (hopefully
small) pain of making the switch!
