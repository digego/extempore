Extempore for Impromptu users
=============================

.. note:: This was once a blog post---corrections/improvements
          welcome.

If you're an `Impromptu`_ user and you're thinking about making the
switch to Extempore, you've probably got a few questions about what
the difference is between Impromptu and Extempore. The `Extempore
Philosophy`_ document covers this to some extent, but this post is
designed to be short and sweet, answering three questions that
Impromptu users might have:

 .. _Impromptu: http://impromptu.moso.com.au

#. what's the same
#. what's new or different
#. what will I have to do to switch over

What's the same
---------------

-  Impromptu's Scheme interpreter has been brought into Extempore
   largely unchanged. So any vanilla Scheme code or libraries should
   work fine (e.g. the pitch class library ``pc_ivl.xtm`` is unchanged).
-  `Time in Extempore`_ (including ``(now)``, ``callback``, temporal
   recursion etc.) is exactly the same as it was in Impromptu.

What's new or different
~~~~~~~~~~~~~~~~~~~~~~~

There are some obvious 'big picture' differences:

-  Extempore is cross-platform (OS X, Linux & Windows)
-  Extempore is `open source`_
-  Extempore is now just an executable, it no longer has a built in
   editor & log view (so `you can use your favourite editor`_)

The biggest difference, though, is the addition of *xtlang*: a new
programming language (and compiler). Whereas Scheme was the *only*
language supported in Impromptu, in Extempore you can write Scheme code
and xtlang code, and the languages can share data and call into one
another, and everyone pretty much just gets along fine. There are
*heaps* of `other posts`_ which cover both the reasons for the
introduction of a new language and also how it works.

The other main difference between Impromptu and Extempore are in audio
and graphics. Impromptu was strongly tied to the `Core Audio`_ framework
for audio, `Quartz`_ for 2D graphics, and `OpenGL`_ for 3D graphics.
Core Audio and Quartz are both OSX-only, so there was a need to change
this tight coupling in the move to make Extempore cross-platform.

In the case of audio, audio signal processing in Extempore hasn't been
replaced by another monolithic framework---it's been replaced by
home-grown xtlang code. This 'turtles all the way down' approach brings
a huge win over the previous approach: the dynamic run-time
modifiability of the whole DSP chain. If you don't want to write your
own instruments from scratch that's ok, too: Extempore ships with some
synths and samplers, and hopefully the number of built-in instruments
will grow as the community grows.

In the graphics case, Extempore uses `cairo`_ (for 2D) and OpenGL (for
2D and 3D) cross-platform graphics programming. This involves a few
platform-specific hacks in the source code for Extempore itself, but any
Scheme/xtlang code you write to manipulate cairo or OpenGL graphics
objects should be portable to any platform where Extempore runs. Some of
Impromptu's OpenGL bindings have made it across to Extempore unchanged,
while others may be slightly different.

In fact, one of the key tenets of Extempore's design philosophy was to
keep functionality out of the compiled ``extempore`` binary and provide
it in libraries wherever possible. So Extempore's audio and graphics
support, which was baked in to Impromptu, is now user-editable. Some
libraries and examples you might find interesting are:

**Audio**

-  ``libs/core/audio_dsp.xtm``
-  ``libs/core/instruments.xtm``
-  ``libs/external/instruments_ext.xtm``
-  ``libs/external/sndfile.xtm``

-  ``examples/core/audio_101.xtm``
-  ``examples/core/polysynth.xtm``
-  ``examples/core/sampler.xtm``
-  ``examples/core/sndfile.xtm``

\*Graphics\*

-  ``libs/external/cairo.xtm``
-  ``libs/external/opengl.xtm``
-  ``libs/external/shaders.xtm``

-  ``examples/core/polysynth.xtm``
-  ``examples/external/drawing.xtm``
-  ``examples/external/opengl_101.xtm``
-  ``examples/external/gpu_image.xtm``
-  ``examples/external/horde3d_knight.xtm``

What will I have to change to switch from Impromptu to Extempore?
-----------------------------------------------------------------

**Audio**

All AudioUnit code (i.e. any functions starting with ``au:``) will have
to be changed. I know that's a pain---sorry!---but it was an inevitable
consequence of making things cross-platform. The 'setting up the AU
graph' section of your code will therefore need to be rewritten. You can
either use the built-in synths and sampler, or you can `build your own
instruments`_, or you could even use Extempore's MIDI in/out (see
``libs/external/rtmidi.xtm``) to play an instrument running *outside* of
Extempore. [1]_

The good news is that from ``play-note`` 'up', Extempore's audio
infrastructure works pretty much exactly the same as Impromptu's. In
fact, that truth is the starting point for the tutorials on 'playing an
instrument in Extempore' (`part I`_ and `part II`_). Once the audio
graph is set up, the code for *playing* music in Extempore should be the
same as it was in Impromptu.

**Graphics**

If you're using any Quartz functions (e.g. ``gfx:make-square``,
``gfx:draw-path``) you'll need to change them over to the equivalent
cairo drawing commands. Cairo's API and drawing model is quite similar
to Quartz's, but there may be a couple of tweaks required.

If you're using OpenGL, then you may not need to change much. But it's
definitely worth going over the code carefully to see if it's still
doing what you expect.

If you were using the video playback and recording functions in
Impromptu (e.g. ``gfx:capture-canvas``) then that is a bit of a dog's
breakfast in Extempore at the moment. Work is ongoing to add bindings
for the gstreamer multimedia library, which will allow for all sorts of
fancy AV encoding, decoding and manipulation. However, that's not in
there yet, so as a stop-gap measure the best way to record an Extempore
graphics canvas (or editor window) is to use standalone screen recording
software.

Getting help
------------

Many of the folks on the Extempore `mailing list`_ (including myself)
were once Impromptu users, so we're probably in a pretty good position
to help you out if you have any problems. Have a look around elsewhere
on this blog, too. Hopefully you'll get excited about the cool stuff
that you can do in Extempore which you just *can't* do in Impromptu, and
that excitement might dull the (hopefully small) pain of making the
switch!

.. [1]
   There's also the possibility of writing an xtlang harness for running
   `VST`_ or AU plugins directly from Impromptu. Setting that up is a
   big job, though, and I can't provide a timeline on it at this stage.
   If you end up adding VST/AU support yourself, *that* would be a very
   welcome pull request :)

.. _Extempore Philosophy: 2012-08-07-extempore-philosophy.org
.. _Time in Extempore: 2012-10-15-time-in-extempore.org
.. _open source: https://github.com/digego/extempore
.. _you can use your favourite editor: 2012-09-26-interacting-with-the-extempore-compiler.org
.. _other posts: ../extempore-docs/index.org
.. _Core Audio: https://developer.apple.com/library/mac/#documentation/MusicAudio/Conceptual/CoreAudioOverview/Introduction/Introduction.html
.. _Quartz: https://developer.apple.com/library/mac/#documentation/GraphicsImaging/Conceptual/drawingwithquartz2d/Introduction/Introduction.html
.. _OpenGL: http://www.opengl.org
.. _cairo: http://www.cairographics.org
.. _build your own instruments: 2012-10-16-a-really-simple-instrument.org
.. _part I: 2012-10-15-playing-an-instrument-part-i.org
.. _part II: 2012-10-15-playing-an-instrument-part-ii.org
.. _mailing list: https://groups.google.com/extemporelang
.. _VST: http://en.wikipedia.org/wiki/Virtual_Studio_Technology
