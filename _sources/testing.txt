Unit testing in Extempore
=========================

Extempore's unit testing library (``libs/core/test.xtm``) provides a
few functions/macros for writing unit tests for xtlang code. The most
important of these is the ``xtmtest`` macro, which takes (up to) three
arguments:

#. the xtlang function to test (e.g. a ``bind-func``)
#. a call to said function
#. (optional) the expected return value

Here's an example:

.. code-block:: extempore

   ;; note the quote (') mark
   (xtmtest
    '(bind-func add_two_integers
       (lambda (a:i64 b)
         (+ a b)))
    (add_two_integers 2 3)
    5)

If things don't work as you'd expect, make sure you've quoted the
``bind-func`` form.

To test an xtlang function multiple times with different arguments,
use ``xtmtest-result`` (call this as many times as you
like with different arguments and return values).

.. code-block:: extempore

  (xtmtest-result (add_two_integers 1 5) 6)
  (xtmtest-result (add_two_integers 10 5000) 5010)

xtlang already has a bunch of tests in the ``tests/`` directory. The
general idea is that this directory structure will grow to mirror the
``libs/`` one, so that ``tests/core/adt.xtm`` has tests for the code
in ``libs/core/adt.xtm``, etc. Some of these test files exist already
(although more tests are necessary in these cases), while others need
to be created as new tests are written.

In addition, the examples in ``examples/core`` and
``examples/external`` are an additional level of "testing"---although
it's sometimes hard verify that e.g. the music/graphics sounds/looks
ok. Still, running these files from top-to-bottom and making sure they
don't crash and produce the right output is important as well.

Running the tests
-----------------

The easiest way to run the test suite is using CMake/CTest. By
default, there's a **test** target created in the Extempore configure
process, so that on e.g. OSX/Linux, you can run all the tests &
examples with::

  make test

This will take a while, but will give a full report on what works and
what's broken.

If you want more control over how the tests are run, then the CMake
configure process will also generate a ``CTestTestfile.cmake`` file in
your build directory, and you can execute ``ctest`` in that directory with
additional arguments. One use case of this is to only run tests with a
certain label::

  cd /path/to/CTestTestfile.cmake
  ctest -L libs-core

The tests are partitioned into the following (hopefully
self-explanatory) labels:

* libs-core
* libs-external
* examples-core
* examples-external
* examples-external-shader

To **include** only the labels matching ``<regex>``, use::

  ctest -L <regex>

To **exclude** only the labels matching ``<regex>``, use::

  ctest -LE <regex>

Get involved
------------

We really appreciate `bug reports`_, and the best way to submit them
these days is in the form of a failing test.

.. _bug reports: https://github.com/digego/extempore/issues

You can just paste the failing test into an email to the `mailing
list`_, or you can submit a pull request with the test in it. If
you're not sure where your test should go, you can still submit it as
a pull request---just add the test code to the bottom of the
``tests/failing.xtm`` file.

.. _mailing list: mailto:extemporelang@googlegroups.com
