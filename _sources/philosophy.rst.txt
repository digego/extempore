The Extempore philosophy
========================

.. note:: This was once a blog post---corrections/improvements
          welcome.

Extempore is a programming language and runtime environment designed
with *live* programming in mind. It supports interactive programming
in a REPL style, compiling and binding code just-in-time. Although
Extempore has its roots in 'live coding' of audiovisual media art, it
is suitable for any task domain where dynamic run-time modifiability
and good numerical performance are required. Extempore also has strong
timing and concurrency semantics, which are helpful when working in
problem spaces where timing is important (such as audio and video).

These two goals---dynamic flexibility and close-to-the-metal
control---seem at odds. Extempore tries to offer both by supporting
both a high-level dynamic language (Scheme) and a low-level 'C like'
language (xtlang) simultaneously, with tight integration and
transparency between the two. A running Extempore process will compile
both valid scheme and xtlang forms. Scheme objects (lists, closures,
continuations, etc.) coexist with the :doc:`xtlang's types
<types>` and :ref:`pointers <pointer-doc>` to :doc:`allocated
memory <memory>`, and with a few 'helper functions' data can even flow
through both languages fluidly.
   
What's scheme, and what's xtlang?
---------------------------------

This is all a bit abstract, so let's look at a couple of examples:

.. code-block:: extempore

      (define scheme-closure
        (lambda (a b)
          (let ((result (* a b)))
            (print "result = " result)
            result)))

      (scheme-closure 4 5) ;; prints "result = 20", returns 20
      (scheme-closure 2.4 2) ;; prints "result = 4.8", returns 4.8

      (bind-func xtlang_closure
        (lambda (c:double d:i64)
          (let ((result (* c (i64tod d))))
            (printf "result = %f\n" result)
            result)))

      (xtlang_closure 4.5 2) ;; prints "result = 9.000000", returns 9.0

Here, ``scheme-closure`` is a Scheme `closure`_ (a closure is a function
along with its enclosing scope). It's just a regular Scheme closure, it
takes two arguments (``a`` and ``b``), which can be any number; anything
for which ``number?`` returns ``#t``. Closures are first-class objects
in Scheme, and ``scheme-closure`` is no exception. It can be passed to
``map``, ``apply``, and friends.

.. _closure: http://en.wikipedia.org/wiki/Closure_(computer_science)

``xtlang_closure``, on the other hand, is an xtlang closure. xtlang
(unlike Scheme) is a *new* language, and the Extempore executable
provides the xtlang compiler. Like Scheme, xtlang is has an
`s-expression`_ based syntax.

.. _s-expression: http://en.wikipedia.org/wiki/S-expression

``xtlang_closure`` is also a closure which takes two arguments, and
xtlang uses the ``lambda`` form to build closures, just like Scheme. In
fact, ``xtlang_closure`` does the exact same thing as ``scheme-closure``
does---it takes two arguments, multiplies them together, then both prints
and returns the result. One thing that's different in the xtlang
version, though, is the presence of type annotations for the arguments:
they're the (blue) parts of the symbol name following the colon. The
types should be familiar: ``double`` for a double-precision floating
point number, and ``i64`` for a 64-bit (signed) integer. Unlike
Scheme---which is dynamically typed, and will silently coerce floats into
ints and other things like that---xtlang is statically typed. Not every
type needs to be specified, the compiler will infer types when it is
*unambiguous*, but the compiler will never silently coerce types. This
is by design---the whole point of using xtlang in Extempore is to make
things explicit. If you want more dynamic typing, then there's always
Scheme.

The xtlang compiler uses an `LLVM`_ backend to generate high-performance
machine code. Basically, Extempore's xtlang compiler generates the LLVM
IR, and this is then passed to LLVM for compiling and linking.

.. _LLVM: http://llvm.org

So why two languages?
---------------------

Why introduce this confusion? Why not just pick one language or the
other (or design a new language which has aspects of both)? By way of
explanation, let's do a bit of numerical processing. Say we want to
calculate the highest common factor of two integers ``a`` and ``b``
using a brute-force approach:

.. code-block:: extempore

      (define hcf-scheme
        (lambda (a b)
          (letrec ((hcf (lambda (i)
                          (if (and (= (modulo a i) 0)
                                   (= (modulo b i) 0))
                              i
                              (hcf (- i 1))))))
            (hcf (if (< a b) b a)))))

      (hcf-scheme 10 15) ;; returns 5

      (bind-func hcf_xtlang
        (lambda (a:i64 b)
          (let ((hcf (lambda (i)
                       (if (and (= (modulo a i) 0)
                                (= (modulo b i) 0))
                           i
                           (hcf (- i 1))))))
            (hcf (if (< a b) b a)))))

      (hcf_xtlang 10 15)  ;; returns 5

The code for Scheme (``hcf-scheme``) and xtlang (``hcf_xtlang``) is
identical except for an ``i64`` type annotation on the first argument
``a`` in ``hcf_xtlang`` and a ``letrec`` instead of a ``let`` in
``hcf-scheme``. Both functions use tail call recursion, and are
written in a fairly 'scheme-like' way. Although there is only the one
type annotation, ``hcf_xtlang`` is strongly (and fully) typed. The
types of all the other variables and the return type of the closure
are all inferred by the compiler from the type of ``a``: the function
``hcf_xtlang`` takes two ``i64`` arguments and returns another
``i64``. In more complex functions there may be a greater need to
specify the types of the variables, but often just a few type
annonations can unambiguously determine everything in scope. The
:doc:`xtlang type reference <types>` has more info on how
type inferencing works in the xtlang compiler.

.. code-block:: extempore

      ;; first, figure out two large numbers with a common factor (133)
      (println (map (lambda (x) (* x 133)) '(125219 123711))) ;; prints (16654127 16453563)

      ;; profile the scheme version
      (let ((t (clock:clock)))
        (println 'HCF '= (hcf-scheme 16654127 16453563))
        (println 'elapsed 'time '= (- (clock:clock) t) 'seconds))

      ;; --result--
      ;; HCF = 133
      ;; elapsed time = 82.085036 seconds

      ;; profile the xtlang version
      (let ((t (clock:clock)))
        (println 'HCF '= (hcf_xtlang 16654127 16453563))
        (println 'elapsed 'time '= (- (clock:clock) t) 'seconds))

      ;; --result--
      ;; HCF = 133
      ;; elapsed time = 0.257790 seconds

In a direct comparison, here I've calculate the HCF of the integers
``16654127`` and ``16453563``, which are (by design) known to have at
least one non-trivial factor (``133``). Both functions return ``133``,
but the xtlang one finishes over 300 times faster. I tried to use even
bigger integers as input, but the Scheme version blew past the maximum
runtime timeout, while the xtlang one finished in about 2 seconds :)

Now, this comparison is one datapoint: it isn't meant to start a flame
war about dynamic vs statically typed languages or anything like that.
It's a brute-force algorithm for a problem with many more elegant
algorithms. What it does show, though, is that *Extempore's* Scheme
interpreter is *slow*. There are some crazy fast and efficient Scheme
compilers, but Extempore's isn't one of them---it's dog slow.

You may now be thinking that this pretty much rules Scheme out for
anything computationally intensive in Extempore, such as audio and
graphics. Well, late one night in about 2010 Andrew (Extempore's
creator) had pretty much the same realisation. At the time he was
working on Impromptu, Extempore's predecessor, which had the same Scheme
interpreter. And he realised that the Scheme interpreter would need some
*serious* work to bring it up to speed if it was going to be used for
any number-crunching. At that point, he figured that he might as well
write a new language, leveraging the LLVM compiler. And lo, xtlang was
born (although it wasn't called that straight away).

After working on xtlang inside of Impromptu for a while, it became clear
that introducing a whole new language to a programming environment is
kindof a big change. So he decided to fork the project, give it a new
name, and also make a couple of other fundamental changes (open source
and cross-platform) as well. Impromptu is still supported, and some of
the improvements to Extempore are back-ported, but Extempore is the
future. And that's the history of Extempore and the genesis of xtlang in
two paragraphs.

xtlang's types include tuples (like C structs), arrays, SIMD vectors and
pointers in addition to the float and int primitives shown in these
examples. The upside of having to worry about these types is the
increased performance and low-level expressiveness, which is
particularly important in real-time and computationally intensive
settings such as digital audio, graphics and interfacing directly with
hardware. The other benefit of having a low-level type system (like C)
is that it's easy to bind to shared libraries (``.dll``, ``.so`` or
``.dylib`` depending on your platform) and then call into them in
xtlang. You can even bind and rebind these shared libraries dynamically,
switching the bindings around as you please. There's more details about
binding to C shared libraries in the ``examples/external`` directory,
and in :doc:`c-xtlang-interop`.

There's heaps more to say about the Scheme/xtlang interop in Extempore
(as well as the details of xtlang itself!), but the key point is that
it's nice to have the choice. Scheme is a great control/scripting
language for triggering events, and xtlang is a nice 'systems' language
for building infrastructure and for doing computational heavy lifting.
Extempore allows the programmer to live in both worlds, as long as they
have some understanding of what's going on under the covers. And as I
work with Extempore (and as xtlang matures) I find myself using Scheme
less and less and xtlang more and more. The code I'm writing is almost
the same (since they're syntactically so similar), but with the
performance benefits and bit-level control of working much closer to the
metal. It's even nice (most of the time, at least!) to get the compile
errors, it's better to catch type mismatches earlier rather than later.

**Live** programming: Interacting with the Extempore compiler/runtime
---------------------------------------------------------------------

Remember the claim in the opening paragraph that Extempore is a language
designed with 'live programming' in mind? Now, 'live programming' is a
pretty loaded term (is the insinuation that all other programming is
*dead?*) and as such needs some unpacking. Extempore is designed to
support (and indeed make it easy for) the programmer to interact with,
modify, and extend their program as it runs.

This is obviously possible in any REPL-based development environment,
but often this interaction is limited to the building and debugging
phase of software development, with the program being frozen (possibly
compiled) upon completion and then left to run unmolested. In Extempore,
on the other hand, this interactive development style is supported (and
encouraged) through the whole software lifecycle---up to and including the
deployment and running of the 'final' code. An Extempore codebase is not
necessarily a static artefact: the behaviour of the system is determined
by the development of the code over the whole time the system is
running, and this behaviour may be differ substantially between the
commencement and completion of this process.

This human-in-the-loop programming approach is exemplified by the
practice of live coding or `laptop performance`_, a "new direction in
electronic music and video: live coders expose and rewire the innards of
software while it generates improvised music and/or visuals. All code
manipulation is projected for your pleasure." In an artistic context
this idea of improvisational live programming makes sense, but there are
also many other contexts where having a human in the loop even at
program execution time (to catch unforseen bugs or add hitherto
unplanned functionality) is advantageous. This is a tough job for the
programmer---there's no safety net when you're modifying the program as
it's being run---but that's exactly why Extempore is being designed as it
is: to provide as much support as possible to the programmer as they
deal with this difficult (and exciting) challenge.

.. _laptop performance: http://toplap.org

This 'everything should be hot-swappable at runtime' philosophy has a
couple of implications for the architecture of the Extempore compiler
and programming environment:

#. Compilation/binding should happen as late as possible. Extempore has
   a couple of static dependencies baked in at compile time, but the
   rest of the functionality is loaded on-the-fly.
#. Compiler-as-a-service (CaaS): the Extempore compiler is a running
   process, and compilation happens by interactively sending Scheme or
   xtlang code to the appropriate address/port. The compiler need not be
   running on the same machine as the programmer, and the code can also
   be executed in any number of running Extempore processes. And because
   it's written in Scheme, even the compiler *itself* is reconfigurable
   at runtime.

