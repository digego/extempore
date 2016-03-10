Turning the sound off in Extempore
==================================

Once you’ve got sound playing in Extempore, you may well want to turn it
off. Understanding how to do that is helpful for understanding how the
audio signal chain works in Extempore, especially since Extempore
differs from other similar multimedia programming environments.

In Extempore you don’t need to set up a signal chain with pre-defined
UGens, to make sound at the most basic level you just need to understand
one thing:

The sound that comes out of the speakers is made up of successive return
values of the ``dsp`` function. This audio ‘sink’ function is a regular
xtlang function, and once it has been set with ``(dsp:set!
function_name)`` Extempore calls this function (in this case
``function_name``, but it can have any name) at the audio sample rate
(usually 44.1kHz). Whatever signal/sound you want; if it ain’t in the
dsp callback, you won’t hear it.

In code:

.. code:: extempore

      ;; a really simple white noise dsp function
      ;; will return (pseudo)random values in [0.0,0.1]
      (bind-func dsp:DSP
        (lambda (in time chan dat)
          (random .1)))

      ;; need to set the dsp callback to start the sound playing
      (dsp:set! dsp)

      ;; and to turn it off
      (bind-func dsp:DSP
        (lambda (in time chan dat)
          0.0))

To test your understanding, think about this question: why does the
second definition of ``dsp`` (at the bottom) turn the sound off?

That’s really the core concept for audio signal processing in
Extempore—all the instruments, samplers, and everything else is built on
top of that ‘call this function once per channel per sample, and send
the return values to the DAC’ notion [1]_.

.. [1]
   There are some exceptions to this—for example Extempore can do
   buffered (rather than sample-by-sample) audio processing. However,
   because of the efficiency of the generated code you’ll be surprised
   at how far you can get without having to go down this route. All my
   livecoding screencasts, for instance, use a sample-by-sample audio
   chain.
