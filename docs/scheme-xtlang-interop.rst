Scheme-xtlang interop
=====================

The rationale behind having the two languages is covered in
:doc:`philosophy`, but there are a few other things worth knowing
about the relationship between Scheme and xtlang in Extempore.

.. note:: The **tl;dr** of this page is: at the top-level (i.e. not
          called by/in another function) all the ``bind-*`` forms
          (e.g. ``bind-func``, ``bind-val``) are xtlang, as is
          anything inside a ``call-as-xtlang`` / ``xtmX`` / ``$``
          special form. Other than that, it's Scheme code.

A Scheme function (for example named ``scheme-foo``) is created by
using the ``define`` keyword and a Scheme ``lambda`` expression:

.. code-block:: extempore

  (define scheme-foo
  (lambda (argument1 argument2)
    ;; do some manipulations of the arguments
    ...
    ;; the last calculated expression is the 'return' value for the function scheme-foo
    (some-expression)))

An xtlang function (for example named ``xtlang_bar``) is created by using
the ``bind-func`` keyword and an xtlang ``lambda`` expression:

.. code-block:: extempore

  (bind-func xtlang_bar:[type_of_return_value,type_of_argument1,type_of_argument2]*
    (lambda (argument1:type_of_argument1 argument2:type_of_argument2)
      ;; do some manipulations of the arguments
      ...
      ;; the last calculated expression is the 'return' value for the function xtlang_bar
      (some_expression)))

So there are already a few potentially confusing things.

#. the Scheme lambda expression and the xtlang lambda function have the
   same name, but are different. The Scheme version only makes sense
   in a Scheme expression, whilst the xtlang version only makes sense
   in an xtlang expression (such as the body of a ``bind-func``)
#. its important to understand when you are in a Scheme context vs an
   xtlang context. If you are typing into your text editor, and you
   are on a new line, and you type an opening ``(`` then what comes next
   should be a Scheme function.
#. `extra confusing` is that when you create the xtlang function
   ``xtlang_bar``, extempore also creates a Scheme function
   ``xtlang_bar`` which is simply a wrapper for the xtlang version.
   The reason for doing this is that otherwise there would be no way
   to actually call the xtlang version (other than in other
   ``bind-func`` expressions)

When mixing Scheme and xtlang code, remember that Scheme is
dynamically typed (meaning you don't have to specify what data type
the input arguments or return value have when you define the closure)
but xtlang functions are statically typed, so when you ``bind-func`` a
new closure you must specify the type of the input arguments and the
return argument. Because closures in xtlang are "first-class
citizens", the newly created closures `itself` has a type (which is
basically a closure type with certain input types and return type).
Again, see :doc:`types` for a more detailed discussion.

More caveats
------------

Sometimes, the Scheme-xtlang interop stuff can be a bit too tricky for
its own good. Consider this example:

.. code-block:: extempore

  (bind-func make_squarer
    (lambda ()
      (lambda (x:double)
        (* x x))))

  (bind-func test_apply:[double,[double,double]*,double]*
    (lambda (f:[double,double]* x:double)
      (f x)))

  (define mysquarer (make_squarer))

  (test_apply mysquarer 2.0)

Here, calling ``(make_squarer)`` from Scheme, returns a Scheme cptr
object---which is the actual xtlang closure. When you call ``test_apply``
with the ``mysquarer`` argument the generic Scheme cptr gets cast back to
the appropriate xtlang closure type as required by ``test_apply``. So far all
good.

Sometimes, however, Extempore gets in the road by trying to help you
out :) As discussed above, all top level xtlang closures (i.e.
anything created with ``bind-func``) have a Scheme wrapper function
automatically bound to the same name. So in this example:

.. code-block:: extempore

  (bind-func squarer:[double,double]*
    (lambda (x)
      (* x x)))

  (bind-func test_apply:[double,[double,double]*,double]*
    (lambda (f:[double,double]* x:double)
      (f x)))

  (test_apply squarer 2.0)

``squarer``, when used as an argument to ``test_apply`` (in Scheme
land), is actually a Scheme function, not a Scheme cptr. This is handy
because it lets you call ``squarer`` directly from Scheme, but it also
means that if you try to pass ``squarer`` (the "Scheme variable" bound to
a "Scheme function") to xtlang it rightfully complains that it doesn't
know what to do with it.

The slightly kludgy way around this is to ask Extempore to give you
the xtlang closure as a cptr---you can do this by calling

.. code-block:: extempore

  (llvm:get-native-closure "squarer")

so calling

.. code-block:: extempore

  (test_apply (llvm:get-native-closure "squarer") 2.0)

will do what you probably want, or if you are going to call it a lot
you could bind to a different Scheme variable:

.. code-block:: extempore

  (define squarer2 (llvm:get-native-closure "squarer"))

  (test_apply squarer2 5.0)
