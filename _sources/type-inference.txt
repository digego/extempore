Type inferencing
================

In xtlang, type annotations can be attached to the declaration of any
variable using a colon, e.g.

-  ``int_var:i64`` (64-bit integer)
-  ``double_ptr:double*`` (pointer to a double precision float)
-  ``closure_ptr:[i64,i32,i32]*`` (pointer to a closure with two
   arguments)

Now, most of the examples in this file have been fairly explicit about
the types of the variables. Look at the code for ``xt_add`` above---in the
argument list ``(a:i64 b:i64)`` both arguments are identified as
``i64``. What happens, though, if we take out just one of these type
annotations?

.. code-block:: extempore

      (bind-func xt_add2
        (lambda (a:i64 b)
          (+ a b)))

      ;; log shows "Compiled xt_add2 >>> [i64,i64,i64]*"

      (xt_add2 2 4) ;; returns 6

Even though we didn't specify the type of ``b``, everything still
compiled fine and the closure returns the correct result. What's the go
with that? Well, it's because the xtlang compiler in Extempore is a
`type inferencing`_ compiler. The addition function ``+`` in the body of
``xt_add2`` can only add values of the *same* type. Since the compiler
knows the type of ``a``, things will only work out if ``b`` is also an
``i64``. And since this guess doesn't conflict with any other
information it has about ``b`` (because there isn't any), then the
compiler can infer that the only acceptable type signature for the
closure pointer is ``[i64,i64,i64]*``.

.. _type inferencing: http://en.wikipedia.org/wiki/Type_inference

How about if we try removing the ``a`` type annotation as well?

.. code-block:: extempore

      (bind-func xt_add3
        (lambda (a b)
          (+ a b)))

This time, the compiler prints the message:

.. code::

    Compiler Error: could not resolve ("a" "b" "xt_add3") you could try
    forcing the type of one or more of these symbols

There just isn't enough info to unambiguously determine the types of
``a`` and ``b``. They could be both ``i32``, or both ``floats``---the
compiler can't tell. And rather than guess, it throws a compile error.

It's also worth mentioning that we could have specified the closure's
type directly with the definition of the ``xt_add3`` symbol

.. code-block:: extempore

      (bind-func xt_add4:[i64,i64,i64]*
        (lambda (a b)
          (+ a b)))

      (xt_add4 2 9) ;; returns 11

Scheme and xtlang types
-----------------------

Extempore can run both Scheme and xtlang code, but Scheme doesn't know
anything about xtlang's types---things like tuples, arrays, vectors,
closures, and user-defined types through ``bind-type``. Scheme only
knows about `Scheme types`_ like symbols, integers, reals,
strings, c pointers, etc.

.. _Scheme types: https://groups.csail.mit.edu/mac/ftpdir/scheme-reports/r5rs-html/r5rs_8.html#SEC48

There is some (approximate) overlap in these type systems, for ints,
floats, strings and c pointers, although even in these cases there are
some caveats, e.g. Scheme only supports *double precision* floats, while
Extempore can work with both ``floats`` and ``doubles`` natively.
Similarly, xtlang's pointers are typed, but Scheme only supports void
(opaque) c pointers. Where possible, Extempore will do the work to allow
xtlang code from Scheme (coercing argument types), but for any composite
types (e.g. list) you can't call xtlang code from Scheme.

Here's an example to make things a bit clearer:

.. code-block:: extempore

    ;; tuple-maker returns a pointer to a tuple and tuple-taker takes
    ;; a pointer to a tuple as an argument.

    (bind-func tuple-maker
      (lambda ()
        (let ((a:<i64,double>* (alloc)))
              (tset! a 0 42)
              a)))

    (bind-func tuple-taker
      (lambda (a:<i64,double>*)
        (tuple-ref a 0)))

    (tuple-maker)               ;; Returns a CPTR (to a tuple, but scheme doesn't know that)
    (tref (tuple-maker) 0)      ;; error, scheme doesn't know about xtlang types
    (tuple-taker (tuple-maker)) ;; returns 42. scheme can pass *pointers* to tuples around
                                ;; as void pointers, but you lose the type checking

Have a look at ``examples/core/extempore_lang.xtm`` for more examples.
