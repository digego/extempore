---
title: Testing in Extempore
---

Extempore's unit-testing framework lives in a single file, `libs/core/test.xtm`,
which is loaded at startup. It provides a handful of macros for asserting things
about xtlang code, and a runner (`xtmtest-run-tests`) that CMake/CTest drives.

## Writing tests {#writing-tests}

The preferred way to write a test is `xtmtest-result`: define the xtlang you want
to exercise with a normal `bind-func`, then assert on a call. The third argument
is an optional label.

```xtlang
(bind-func add_two_integers
  (lambda (a:i64 b:i64)
    (+ a b)))

(xtmtest-result (add_two_integers 2 3) 5 "add-two")
(xtmtest-result (add_two_integers 10 5000) 5010)
```

Numbers are compared with `=` (so `4` and `4.0` match); everything else with
`equal?`.

To group related assertions under shared setup, use `xtmtest-with-fixture`. It
runs the fixture body, then the tests, in a fresh environment, with an `is?`
macro in scope (its optional third argument labels the assertion within the
suite). Fixtures nest.

```xtlang
(xtmtest-with-fixture arithmetic
  (bind-func double (lambda (x:i64) (* x 2)))

  (is? (double 3) 6)
  (is? (double 0) 0 "zero"))
```

For tests about compilation itself, use `xtmtest-compile` (asserts a form
compiles) or `xtmtest-compile-fails` (asserts it does *not* --- e.g. a type error
the checker is meant to catch):

```xtlang
(xtmtest-compile
 (bind-type TupleWithZone <i64,mzone*>))

(xtmtest-compile-fails
 (bind-func bad_return:[i64]*
   (lambda () 3.5)))
```

`xtmtest` is the older "define, call, and check in one form" macro, still used by
much of the existing corpus. New tests should prefer `xtmtest-result`.

```xtlang
(xtmtest
 (bind-func add_three (lambda (a:i64 b:i64 c:i64) (+ a (+ b c))))
 (add_three 1 2 3)
 6)
```

## Outcomes {#outcomes}

Each assertion records one of these outcomes:

-   `pass` --- the call ran and its value matched
-   `fail` --- the call ran but its value did not match
-   `runtime-error` --- it compiled, but evaluating the call threw
-   `compile-error` --- the definition failed to compile
-   `compile-ok` --- a compile-only test compiled
-   `fixture-error` --- a fixture's setup threw

A run fails (non-zero exit) if any outcome is not a `pass` or `compile-ok`.

## Where tests live {#where-tests-live}

Test files are `.xtm` files under `tests/`, and the directory structure mirrors
`libs/`: `tests/core/adt.xtm` tests `libs/core/adt.xtm`, and so on. Compiler-
internal tests live in `tests/compiler/`.

Every `tests/core/*.xtm` and `tests/compiler/*.xtm` file is discovered and
registered automatically, so adding a test is just a matter of dropping a file
in the right directory --- no build files to edit. (A few tests with bespoke
harnesses, such as the IPC and AOT round-trip tests, are registered explicitly
in `extras/cmake/tests.cmake`.)

The examples in `examples/core` and `examples/external` are also run as a
coarser level of testing: loading each one top-to-bottom and checking it doesn't
crash. The offline audio-rendering tests go further, rendering a DSP to a WAV and
asserting on its frequency content.

## Running the tests {#running-the-tests}

The easiest way to run everything is via CTest. By default a **test** target is
created, so on Linux/macOS:

```
    make test
```

This takes a while but gives a full report. For finer control, run `ctest`
directly in your build directory and filter by label:

```
    ctest -L libs-core
```

The labels are:

-   `libs-core` --- core standard-library tests
-   `compiler-unit` --- compiler-internal unit tests (including the AOT
    round-trip)
-   `libs-external` --- external-library tests
-   `cpp-unit` --- C++ unit tests (GoogleTest)
-   `examples-core`, `examples-audio`, `examples-graphics` --- examples run as
    smoke tests
-   `audio-offline` --- offline WAV-rendering assertions

Use `-L <regex>` to include, or `-LE <regex>` to exclude, matching labels.

Each test file also writes a JUnit XML report to `test-results/` in the build
directory (one file per test file), so CI can surface individual assertion
failures rather than just which `.xtm` file failed.

## Failing tests {#failing-tests}

`tests/failing.xtm` is a quarantine for known-broken tests. It is registered as
an *expected* failure: CTest stays green while the bugs persist, and flips to
failing the moment a fix makes the file pass --- the cue to move that test into
its proper home.

## Get involved {#get-involved}

We really appreciate [bug reports](https://github.com/digego/extempore/issues),
and the best way to submit one is as a failing test. Open a pull request with the
test added to the bottom of `tests/failing.xtm`; if you're not sure where it
should eventually live, that's fine --- we'll find it a home when the bug is
fixed.
