Time in Extempore
=================

.. note:: This was once a blog post---corrections/improvements
          welcome.

Most programming environments operate using an iterative 'generate and
test' style of programming. First you write the program, then you run
the program. If there are problems with the program you re-write the
program and then re-run the program, etc…

Extempore is different. Extempore is a dynamic, interactive programming
environment where programs are modified and extended while they are
running. As a simple example: type the following code into your editor
and :doc:`evaluate <caas>` it:

.. code-block:: extempore

    (now)

Your editor's echo area should display the result of evaluating the
expression, and it should be a big (integer) number---when I did it on my
computer just now it came back with ``770432``.

Now try pressing the eval button again. It's a different result!
``(now)`` is a function that returns the time (in audio samples) since
Extempore was started. The important thing to keep in mind for the
present is that Extempore is a program that is already running.
Extempore isn't just a programming language, or just a compiler, it's a
run-time as well. And one of the primary differences between Extempore
and most other general purpose programming environments is an emphasis
on *precisely scheduled code execution*.

Each ``extempore`` process has a scheduling engine built-in, allowing
tasks to be scheduled for execution at a precise time in the future.
Unlike threading constructs such as ``sleep()`` or ``wait()``, which
don't provide strong guarantees about temporal accuracy (i.e. the time
they'll sleep/wait for), Extempore's scheduling engine is *guaranteed*
to execute its tasks at the requested time (the number of audio samples
since Extempore was started). This temporal guarantee is significant for
time critical domains such as audio and graphic, and real-time systems
programming.

Extempore's scheduler (which what we're accessing when we call ``(now)``
is driven by the audio device which the Extempore process connected to
upon startup. This isn't the same as the time value on the system clock,
although you can access the system clock as well through
``(clock:clock)``. Instead, the audio device will (usually) have a much
more accurate clock, running at the audio sample rate (which will
usually be 44.1kHz). That's why Extempore *always* connects to an audio
device on startup---even if you're not producing any audio output.

Scheduling events for future execution
--------------------------------------

Being able to schedule events (e.g. for music playback or even arbitrary
code execution) for execution in the future is *super handy*. As an
example, let's load up the default synth instrument and play three notes
in sequence (an arpeggiated triad).

.. code-block:: extempore

      ;; load the instruments file
      (sys:load "libs/core/instruments.xtm")

      ;; define a synth using the provided components
      ;; synth_note_c and synth_fx
      (bind-instrument synth synth_note_c synth_fx)

      ;; add the instrument to the DSP output callback
      (bind-func dsp:DSP
        (lambda (in time chan dat)
          (synth in time chan dat)))
      (dsp:set! dsp)

      ;; schedule three nodes to play in succession
      (define play-seq
        (lambda ()
          (play-note (now) synth 60 80 10000)
          (play-note (+ (now) 22050) synth 64 80 10000)
          (play-note (+ (now) 44100) synth 67 80 10000)))

      ;; play
      (play-seq)

Extempore uses an asynchronous 'schedule and forget' style of
programming, often in conjunction with a design pattern called temporal
recursion---a concept I'll come back to shortly. This is different from
languages such as ChucK which use a synchronous approach. What *is* the
difference? Synchronous timing works by holding up code until some
specified time in the future. This is basically the same concept as
using ``sleep()``, although strongly timed languages like ChucK
guarantee the length of the sleep. Extempore, on the other hand, works
by scheduling tasks to be executed at some time in the future. Once a
task has been scheduled, thread execution moves immediately onto the
next expression. A pseudocode example may help to illustrate this
difference.

.. code::

    //synchronous timing
    play-note(now)
    time = (now) + 44100
    play-note(now)

    //asynchronous timing
    play-note(now)
    play-note(now + 44100)

Strongly timed code holds up thread execution until the global time is
equal to the value ``time``, and multitasking is achieved by running
multiple concurrent threads of execution. The asynchronous code example
schedules tasks into the future and immediately continues execution.
Multitasking in Extempore is achieved with very little effort by
evaluating multiple simultaneous temporal recursions.

Asynchronous event scheduling is a fairly common programming technique,
and there wouldn't be much else to say if Extempore wasn't a dynamic
language. However, Extempore allows us to create and schedule code for
future execution. This turns out to be very useful in time-based
programming.

Temporal recursion
------------------

There is a common design pattern in Extempore programming called
**temporal recursion**. By writing a function which *schedules itself*
as its final action, a temporally recursive callback loop is
established. Here is an example demonstrating a ``foo`` function that
will play a note and then schedule itself to be called back in one
second (this loop will continue indefinitely).

.. code-block:: extempore

      (define foo
        (lambda ()
          (play-note (now) synth 60 80 *second*)
          (callback (+ (now) *second*) 'foo)))

      (foo)

You can create as many of these temporal recursion loops as you like---try
evaluating ``foo`` multiple times. Notice that you get multitasking for
free, you don't need to do anything special to run two event streams.
You can even create temporal recursions *inside* temporal recursions.

A temporal recursion need not 'recur' at a constant rate. By adjusting
the time increment on each cycle the ``callback`` rate (control rate)
can be constantly adjusted. Here is an extension to the previous example
that will randomize the note length. Note that each ``callback`` is now
scheduled at ``(now)`` + the duration of the note.

In making this change, we're also taking advantage of the fact that you
can re-evaluate a function while it is temporally recursing, changing
its functionality on the fly (provided that the signature of the method
does not change, i.e. same arguments and same name). Try evaluating the
code below while the old version of foo is running.

.. code-block:: extempore

      ;; re-define foo
      (define foo
        (lambda ()
          (let ((note-length (random '(0.25 0.5 1.0 2.0))))
            (play-note (now) synth (random 60 80) 80 (* *second* note-length))
            (callback (+ (now) (* note-length *second*)) 'foo))))

One-off anonymous functions can also be scheduled for future evaluation.
The code example below shows a one off anonymous function scheduled for
evaluation one minute from ``(now)``.

.. code-block:: extempore

      (callback (+ (now) *minute*)
                (lambda () (play-note (now) synth 60 80 *second*)))

There are a couple of gotchas to keep in mind when doing 'schedule and
forget' programming. The first is that ``(now)`` can be a slippery
thing. In the example below, the two notes *may* be scheduled to play on
the same sample, but then again, they may not! ``(now)`` may have moved
forward in time between the two calls, even if they were evaluated at
the same time.

.. code-block:: extempore

      (play-note (now) synth 60 80 *second*)
      (play-note (now) synth 72 80 *second*)

Often this lack of precision is fine (i.e. too small a change to be
noticeable) but where absolute accuracy is required a time variable
should be used.

.. code-block:: extempore

      (let ((time (now)))
        (play-note time synth 60 80 *second*)
        (play-note time synth 72 80 *second*))

This inaccuracy becomes more of an issue when amplified over time, such
as using ``(now)`` inside a recursive callback loop. We can avoid the
problem by precisely incrementing a ``time`` value between each
recursive callback (note that any arguments required by the function
being called back must also be passed to ``callback``).

.. code-block:: extempore

      ;; This is bad
      (define loop
        (lambda ()
          (play-note (now) synth 60 80 *second*)
          (callback (+ (now) *second*) 'loop)))

      (loop)

      ;; This is good (precise time arg is now incremented each recursion)
      (define loop
        (lambda (time)
          (play-note time synth 60 80 *second* )
          (callback (+ time *second*) 'loop (+ time *second*))))

      (loop (now))

The second major gotcha in recursive callback loops is that ``(now)`` is
*now*. Code requires some time to execute. If you are executing a call
to evaluate a note ``(now)``, by the time the code is evaluated it will
already be late: ``(now)`` will have moved on. You should always try to
schedule your code execution *ahead* of the scheduled time of your
tasks.

.. code-block:: extempore

      ;; This is best (callback happens 4100 samples earlier than new time)
      (define loop
        (lambda (time)
          (play-note time synth 60 80 1.0)
          (callback (+ time 40000) 'loop (+ time 44100))))

      (loop (now))

In the 'good' version of ``loop``, the ``time`` passed as an argument to
``loop`` is exactly the same time as the scheduled callback time. The
problem with this is that the next note needs to be scheduled at exactly
the same time that the function is called. The note will always be late.
The 'best' version schedules the callback just ahead of the time that we
want the note to play. This gives us ``4100`` samples to execute the
code to schedule the note before the note is required to sound.

..
   Temporal recursion is a fundamental pattern in Extempore and something
   that you will use all the time. Take a look at the `playing an
   instrument`_ tutorial to get more of an idea about using temporal
   recursion in a practical setting.

   .. _playing an instrument: TODO
