Concurrency
===========

.. warning:: Some of the threading stuff (particularly in xtlang) has
             changed slightly since this was written, although as a
             high-level overview it's still accurate. It'll be brought
             up to date as soon as possible, but if you find anything
             in here which isn't clear or doesn't work, :doc:`let us
             know <about-this-documentation>`

Every Extempore *process* (e.g. the "primary process" or "utility
process") is a *Scheme process*, which is actually an operating-system
(OS) thread running in the main Extempore OS process (i.e. the
thing with the PID number). Each top level Scheme process runs its own
scheme interpreter with its own managed memory, own garbage collector
etc. Additionally, each Scheme process has its own network port. To
evaluate a Scheme expression you send it to a given scheme process
using this network port, i.e. over a TCP connection. Extempore
provides a nice inter-process communication (IPC) layer to make
communication between Scheme processes reasonably straightforward,
regardless of whether they are running locally or remotely.

To summarize, each *Scheme process* is an OS thread, but in reality
behaves more like an OS process because it has its own managed memory,
scheme process control, etc..

Here is a starting example of a Scheme function that mutates a Scheme
global variable. The function includes a *blocking* sleep---it will
``println`` then sleep for one second, test conditional and repeat.

.. code-block:: extempore

  (define global-val 0)

  (define my-scm-func
    (lambda (name x)
      (println name (ipc:get-process-name) global-val)
      (sys:sleep *second*)
      (if (< global-val x)
          (begin (set! global-val (+ global-val 1))
                 (my-scm-func name x)))))

  (my-scm-func 'a 5)

It is important to note though that even though an Extempore process
(i.e. Scheme process) is a single OS thread, much of Extempore's
day-to-day concurrency occurs within this context using cooperative
concurrency. So, for example, we can happily run this code
concurrently. Eval each of the following lines in turn using

.. code-block:: extempore

  (set! global-val 0)
  (my-scm-func 'a 10)
  (my-scm-func 'b 10)

Note that ``global-val`` is shared memory, however access to that
shared memory is strictly ordered, i.e. is not pre-emptive. This type
of concurrency makes concurrent programming in Extempore safe and
straightforward. Many of Extempore's libraries are built around this
type of cooperative concurrency (i.e. ``sys:sleep`` above, which makes this
example possible).

However, one downside to this approach is that it is only able to
utilizes a single CPU core. So, Extempore also supports *multiple*
Scheme processes. There are always two started by default (primary and
utility), but you can spawn as many as you like at runtime, although
these are relatively heavy-weight so generally you are not likely to
want any more processes than your total number of CPU cores.

You can spawn your own process with a name and a network port like this:

.. code-block:: extempore

  (ipc:new "myproc" 7090)

or you can connect to a remote process like this:

.. code-block:: extempore

  ;; assuming a host at 192.168.1.1 is running Extempore
  (ipc:connect "192.168.1.1" "myproc" 7099)

Either way, *myproc* is now a local name which defines a process
running *somewhere*. IPC calls work regardless of whether the process
is local or remote.

You can explicitly call into ``my-scm-func`` in the primary process like
so:

.. code-block:: extempore

  (ipc:call "primary" 'my-scm-func 'a 5)

which is essentially the same thing as calling ``(my-scm-func 'a 5)``
while *connected* to the primary process. Being explicit though, means
that we can make this call into primary no matter what scheme
process we are currently *connected* to.

We can try to run ``my-scm-func`` in the *myproc* process as follows:

.. code-block:: extempore

  (ipc:call "myproc" 'my-scm-func 'a 5)

but we will get an error, because ``my-scm-func`` (and ``global-val``
for that matter) do not exist in the memory space of *myproc*. We can
fix that using Extempore's IPC infrastructure simply enough by
defining both ``global-val`` and ``my-scm-func`` in *myproc*.

.. code-block:: extempore

  (ipc:define "myproc" 'global-val global-val)
  (ipc:define "myproc 'my-scm-func my-scm-func)

now the ``ipc:call`` works as expected---i.e. executing
``my-scm-func`` in the *myproc* process.

Note that we defined ``global-val`` in *myproc* to be whatever value
``global-val`` was currently bound to in our *connected* process,
which in this instance was *primary* but could be whatever process our
text buffer was connected to. We could just as easily have defined a
different value into *myproc*, e.g.

.. code-block:: extempore

  (ipc:define "myproc" 'global-val 0)

So, now try evaluating the next four lines one after the other

.. code-block:: extempore

  (ipc:define "primary" 'global-val 0)
  (ipc:define "myproc" 'global-val 0)
  (ipc:call "primary" 'my-scm-func 'a 5)
  (ipc:call "myproc" 'my-scm-func 'a 5)

These are again executing concurrently but now also in parallel (i.e.
on different cores). Importantly, ``global-val`` is independent, not
shared. Anyway, so far so good, the main point being the independence
of the memory spaces, and Extempore's IPC layer for communication
between Scheme processes.

Concurrency in xtlang
---------------------

Things get more interesting when we introduce xtlang.

Firstly, all calls into xtlang code are always initiated at some point
by a top level scheme expression (see :doc:`scheme-xtlang-interop` for
more detail). Under normal Extempore operating conditions, xtlang code
is always executing in some Scheme process or other. Generally this
xtlang code will behave as expected with regards to concurrency, i.e.
will generally behave as if it were just another Scheme call inside
the Scheme process. As a trivial example, consider the xtlang
function:

.. code-block:: extempore

  (bind-func times2
     (lambda (x)
         (* x 2)))

Compiling this xtlang function automatically creates a Scheme binding
with exactly same name, which allows us to call it like any other
scheme call:

.. code-block:: extempore

  ;; try evaluating this line
  (times2 4)

Of course, we can incorporate this Scheme wrapper call into our normal
Scheme code, for example we can modify the ``my-scm-func`` from above:

.. code-block:: extempore

  (define my-scm-func
    (lambda (name x)
      (println name (ipc:get-process-name) (times2 global-val))
      (sys:sleep *second*)
      (if (< global-val x)
          (begin (set! global-val (+ global-val 1))
                 (my-scm-func name x)))))

and all of our existing examples will work just fine. For example,
cooperative concurrency as before:

.. code-block:: extempore

  (define global-val 0)
  (my-scm-func 'a 10)
  (my-scm-func 'b 10)

also using IPC, although we will need to re-define ``my-scm-func`` in
*myproc* because we have changed its definition. Also note that we
need to tell *myproc* about ``times2`` (note that ``ipc:bind-func`` has a
slightly different signature from ``ipc:define``):

.. code-block:: extempore

  (ipc:bind-func "myproc" times2)
  (ipc:define "myproc" 'my-scm-func my-scm-func)

now we can re-run the same ipc example as earlier (evaluating each
line one after the other)

.. code-block:: extempore

  (ipc:define "primary" 'global-val 0)
  (ipc:define "myproc" 'global-val 0)
  (ipc:call "primary" 'my-scm-func 'a 5)
  (ipc:call "myproc" 'my-scm-func 'a 5)

OK, so far the behaviour of xtlang fits in with our existing
understanding of both Extempore's cooperative concurrency and
Extempore's Scheme process architecture. Now things will begin to
diverge somewhat.

Firstly, the ``(ipc:bind-func "myproc" times2)`` call from above is
needed to define the "scheme times2 wrapper" in *myproc*---**not** the
xtlang times2 function itself which is bound globally across the
entire Extempore OS process and so is automatically available to all
Scheme processes, and indeed potentially to *any* other OS thread
running in the Extempore OS process (the thing with the PID). In
practice this means that if an xtlang function closes over some value
at the top level, then that closed value is shared between all Scheme
processes (which is not the case with a Scheme closure which is unique
in every Scheme process).

For example:

.. code-block:: extempore

  (bind-func xtlang_inc
    (let ((y 0))
      (lambda (inc)
        (set! y (+ y inc))
        y)))

  (ipc:bind-func "myproc" 'xtlang_inc)
  (println (ipc:call "primary" 'xtlang_inc 1))
  (println (ipc:call "myproc" 'xtlang_inc 1))

Note that ``xtlang_inc`` is shared between primary and *myproc* and
therefore ``y`` is shared data, and is therefore subject to all of the
potential pitfalls associated with shared mutable memory (as well as
all of the potential performance optimizations etc.

This also goes for any globally bound xtlang variables. Consider this
code for example.

.. code-block:: extempore

  (bind-val my_xtlang_global i64 0)

  (bind-func get_global
    (lambda ()
      my_xtlang_global))

  (bind-func set_global
    (lambda (x)
      (set! my_xtlang_global x)))

  (ipc:bind-func "myproc" 'get_global)
  (ipc:bind-func "myproc" 'set_global)

  (println (ipc:call "primary" 'get_global))
  (println (ipc:call "myproc" 'get_global))

  (println (ipc:call "primary" 'set_global 55))

  (println (ipc:call "primary" 'get_global))
  (println (ipc:call "myproc" 'get_global))

So Scheme is all about message passing, and xtlang is all about shared
memory. This is by design, as xtlang is there to let you break all the
rules when performance matters. Now this does not mean that your
xtlang code is definitely *not* Scheme process (i.e. thread) safe.
xtlang code can be Scheme process (i.e. thread) safe if you stick to
the following three principles:

#. Don't access global xtlang variables in your xtlang functions.
#. Don't close over variables with top-level xtlang functions.
#. Don't allocate heap :doc:`memory <memory>` in xtlang functions
   (zone and stack memory is OK)

If you stick to those three principles then your xtlang code should be
Scheme process safe, although obviously you also need to be careful
about what **other** xtlang and native code that you call into.

Having said that, xtlang is there to allow you to break the
rules---with great power comes great responsibility, and all that
rubbish. Indeed xtlang allows you to *completely* break the rules by
giving you direct access to native threads. Here's an xtlang example
that completely breaks out of Extempore's "normal environment" by
managing its own native OS threads using standard fork/join semantics.

.. code-block:: extempore

  ;; sleep for 0-3 seconds
  (bind-func my_os_thread
    (lambda ()
      (thread_sleep (dtoi64 (* 3. (random))) 0)
      (printf "thread: %p\n" (thread_self))))

  (bind-func native_threads
    (lambda (num:i64)
      (let ((i 0)
            (threads:i8** (salloc num)))
        (dotimes (i num)
          (pset! threads i
                 (thread_fork
                  (llvm_get_function_ptr "my_os_thread_native")
                  null)))
        (dotimes (i num)
          (thread_join (pref threads i)))
        (printf "DONE!\n"))))

  (native_threads 5)

Note the use of ``(llvm_get_function_ptr "my_os_thread_native")``.
This call returns a sanitized C wrapper function around our xtlang
closure ``my_os_thread``. Like Scheme wrappers, C wrappers are also
automatically generated for toplevel xtlang closures, and are required
if you wish to call an xtlang closure from an external C
library---:doc:`xtlang knows how to call C natively
<c-xtlang-interop>` but C cannot call an xtlang closure without an
appropriate C wrapper. C wrappers have the same name as the xtlang
closure with ``_native`` appended to the end. By passing a C wrapper
around we can have the OS callback into native xtlang code and still
enjoy full on-the-fly hot-swappability. In other words, once you have
passed the C wrapper as a callback you can re-compile (change the
behaviour) of the original xtlang closure on-the-fly, whenever you
like!

The same principle applies to any other C library code that you may
pass xtlang closure C wrappers to. These callbacks are then subject to
whatever threading context that library code implements, although all
obviously within the context of the global Extempore OS process.
