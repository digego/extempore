---
title: xtlang tutorial
---

This tutorial picks up where the [quickstart](/overview/quickstart/) leaves off.
You've got Extempore running, you can send `(println "Hello, World!")` to it
from your editor, and now you want to actually write xtlang---the
statically-typed language that compiles to LLVM IR at runtime.

By the end of this tutorial you'll know how to:

- define typed functions with `bind-func`
- read xtlang type signatures and let the inferencer fill in the blanks
- allocate memory and work with pointers, tuples, and named types
- write closures that carry state between calls
- choose between stack, zone, and heap allocation

When you're done, the [types](/reference/types/) and
[memory-management](/reference/memory-management/) reference pages cover
everything here in more depth.

:::note[Code snippets are verified]

Every snippet in this tutorial is run through `./extempore --batch` and the
output shown is what it actually prints. If something here doesn't work, that's
a bug---please [open an issue](https://github.com/digego/extempore/issues).

:::

## Your first bind-func

`bind-func` is how you give a name to an xtlang function. Every `bind-func` has
the same shape: a name, and a `lambda` that does the work.

```xtlang
(bind-func xt_add
  (lambda (a:i64 b:i64)
    (+ a b)))

(println (xt_add 3 6))
```

Two things happen when you evaluate this. First, the compiler reports what it
built:

```
Compiled:  xt_add >>> [i64,i64,i64]*
```

The `[i64,i64,i64]*` is the type signature of `xt_add`. Read it right-to-left
with the first slot as the return type: "a pointer to a closure that takes two
`i64`s and returns an `i64`". This is the notation you'll see everywhere.

Second, the call `(xt_add 3 6)` returns `9`.

:::note[Why the colons?]

`a:i64` means "the argument `a` is an `i64`". xtlang is statically typed, so the
compiler needs to know the type of everything. For function arguments you
usually say so explicitly; for locals, the compiler can often figure it out
(next section).

:::

## Types and inference

xtlang's primitive types will look familiar if you've written any C:

| Type     | What it is                                     |
| -------- | ---------------------------------------------- |
| `i1`     | 1-bit int (used for booleans)                  |
| `i8`     | 8-bit int (used for chars and `i8*` C strings) |
| `i32`    | 32-bit int                                     |
| `i64`    | 64-bit int (default for int literals)          |
| `float`  | 32-bit float                                   |
| `double` | 64-bit float (default for float literals)      |

You don't have to annotate everything. The compiler will infer types when it can
work them out from context:

```xtlang
(bind-func xt_add_partial
  (lambda (a:i64 b)
    (+ a b)))

(println (xt_add_partial 4 5))
```

```
Compiled:  xt_add_partial >>> [i64,i64,i64]*
9
```

Since `a` is `i64` and `b` is being added to it, `b` must also be `i64`. The
compiler fills in the gap.

But drop _all_ the annotations and there's nothing for it to go on:

```xtlang
(bind-func xt_add_bad
  (lambda (a b)
    (+ a b)))
```

```
Could not resolve types!::xt_add_bad
unresolved: a
unresolved: b
```

A good rule of thumb: annotate at the edges (function arguments, `bind-val`
globals, places where you allocate memory). The middle usually takes care of
itself.

### Deliberate type errors

If you give the compiler conflicting information, it will tell you so:

```xtlang
(bind-func type_mismatch
  (lambda ()
    (xt_add 3.0 6.0)))
```

```
Type Error bad numeric value 3.000000, should be i64
```

`xt_add` wants `i64` arguments; `3.0` is a `double`. xtlang won't silently
coerce, so you either change the literal (`3`) or convert explicitly
(`(dtoi64 3.0)`).

The [error-messages glossary](/reference/error-messages/) catalogues more of
these.

## Pointers and memory

xtlang is manually memory-managed. When you need a chunk of memory, you ask for
it with one of three allocator functions:

- `salloc` --- allocate on the **stack** (freed when the enclosing function
  returns)
- `zalloc` --- allocate from the current **zone** (freed when the zone ends)
- `halloc` --- allocate on the **heap** (freed when you call `free`, or never)

`alloc` is an alias for `zalloc`, so when you see a bare `alloc` you're
allocating from a zone.

A pointer is indicated with a trailing `*`. So `i64*` is a pointer to an `i64`,
and `double*` is a pointer to a `double`. The functions for reading and writing
through pointers are:

- `pref` --- get the value at an offset (`(pref p i)` is like `p[i]` in C)
- `pset!` --- set the value at an offset (`(pset! p i v)` is like `p[i] = v`)
- `pfill!` --- fill several slots at once

Here's a function that averages three doubles:

```xtlang
(bind-func my_mean
  (lambda ()
    (let ((buf:double* (zalloc 3)))
      (pfill! buf 4.5 3.3 7.9)
      (/ (+ (pref buf 0)
            (pref buf 1)
            (pref buf 2))
         3.0))))

(println (my_mean))
```

```
Compiled:  my_mean >>> [double]*
5.233333
```

`(zalloc 3)` reserves room for three `double`s and returns a pointer to the
first one; `pfill!` writes the three values; the body reads them back with
`pref`. All standard stuff.

## Aggregate types

xtlang has three kinds of compound type: tuples (`<...>`), arrays (`|n,type|`),
and vectors (`/n,type/`, which use SIMD). Tuples are the ones you'll reach for
most often---they behave like C structs, except the fields are indexed rather
than named.

And because type signatures can get long, `bind-type` lets you give them a name:

```xtlang
(bind-type point2d <double,double>)

(bind-func make_point
  (lambda (x:double y:double)
    (let ((p:point2d* (zalloc)))
      (tset! p 0 x)
      (tset! p 1 y)
      p)))

(bind-func distance_from_origin
  (lambda (p:point2d*)
    (sqrt (+ (* (tref p 0) (tref p 0))
             (* (tref p 1) (tref p 1))))))

(println (distance_from_origin (make_point 3.0 4.0)))
```

```
DataType:  point2d >>> <double,double>
Compiled:  make_point >>> [point2d*,double,double]*
Compiled:  distance_from_origin >>> [double,point2d*]*
5.000000
```

`tref` and `tset!` are to tuples what `pref` and `pset!` are to pointers. The
equivalents for arrays are `aref` / `aset!` / `afill!`, and for vectors `vref` /
`vset!` / `vfill!`.

Notice that `make_point` returns a `point2d*`---a pointer---not a tuple by
value. Almost all aggregate data in xtlang is passed around by pointer. The full
range of type signatures and their meanings is in the
[types reference](/reference/types/).

## Closures

xtlang closures are the same idea as Scheme closures: a function plus the
variables it captures from its surrounding scope. The type signature
`[ret,arg1,arg2]*` says "pointer to a closure that returns `ret` and takes these
arguments".

A classic example: a function that returns another function.

```xtlang
(bind-func make_adder
  (lambda (a:i64)
    (lambda (b:i64)
      (+ a b))))

(bind-func try_adder
  (lambda ()
    (let ((add5 (make_adder 5)))
      (add5 7))))

(println (try_adder))
```

```
Compiled:  make_adder >>> [[i64,i64]*,i64]*
Compiled:  try_adder >>> [i64]*
12
```

Read `make_adder`'s signature carefully: `[[i64,i64]*,i64]*` means "a closure
that takes one `i64` and returns a pointer to a closure that takes one `i64` and
returns an `i64`". The inner `lambda` _closes over_ `a`---that's where the `5`
comes from when `add5` is called with `7`.

### Closures that hold state

Every top-level `bind-func` gets its own **zone** (an 8KB region of memory by
default). If you put a `let` _outside_ the `lambda`, anything you allocate there
lives in that zone---and it survives across calls to the closure.

```xtlang
(bind-func counter_closure
  (let ((count:i64* (zalloc)))
    (pset! count 0 0)
    (lambda ()
      (pset! count 0 (+ (pref count 0) 1))
      (pref count 0))))

(println (counter_closure))
(println (counter_closure))
(println (counter_closure))
```

```
Compiled:  counter_closure >>> [i64]*
1
2
3
```

The `zalloc` runs once, when `counter_closure` is compiled, and gives us a
pointer into the closure's zone. Each call to the inner lambda reads and writes
through that same pointer. When you recompile `counter_closure`, the old zone is
freed and a fresh one takes its place---so redefining resets the counter to 0.

This is the standard pattern for any closure that needs persistent state: audio
buffers, oscillator phases, event counters.

## Memory zones

So far we've used `zalloc` without really explaining zones. A **zone** is a
region of memory that you can free all at once. There are three everyday
flavours:

- **The default zone** --- 1 MB, always available. If you haven't pushed any
  zones of your own, `zalloc` allocates from here.
- **Closure zones** --- 8 KB by default, one per `bind-func`. Allocations in the
  outer `let` (as in `counter_closure` above) come from this zone.
- **User zones** --- created with `memzone` for larger or shorter-lived work.

`memzone` takes a size in bytes and a body of expressions. Anything `zalloc`'d
inside the body comes out of the new zone, and the whole zone is freed when the
`memzone` form exits:

```xtlang
(bind-func fill_and_sum
  (lambda (n:i64)
    (memzone 1000000
             (let ((buf:i64* (zalloc n))
                   (acc:i64 0)
                   (i:i64 0))
               (dotimes (i n)
                 (pset! buf i i))
               (dotimes (i n)
                 (set! acc (+ acc (pref buf i))))
               acc))))

(println (fill_and_sum 100))
```

```
Compiled:  fill_and_sum >>> [i64,i64]*
4950
```

We allocate a 1 MB zone, fill a buffer with 0..n-1, sum them, and return the
result. When the `memzone` body exits, the whole MB is reclaimed in one go. No
`free`, no garbage collection.

### Why `salloc` is dangerous for return values

Here's a trap that catches everyone eventually. What if we use `salloc` (stack
allocation) for a value we want to return?

```xtlang
(bind-func make_pair_bad
  (lambda (a:i64)
    (let ((p:<i64,i64>* (salloc)))
      (tfill! p a (* 2 a))
      p)))

(bind-func clobber_demo
  (lambda ()
    (let ((p (make_pair_bad 6)))
      (make_pair_bad 2)
      (printf "p = <%lld,%lld>\n" (tref p 0) (tref p 1)))))

(clobber_demo)
```

```
Compiled:  make_pair_bad >>> [<i64,i64>*,i64]*
Compiled:  clobber_demo >>> [i32]*
p = <2,127154307723709>
```

We expected `p = <6,12>`---but the second call to `make_pair_bad` overwrote the
stack slot that `p` was still pointing to. Stack memory is reused as soon as the
function returns; any pointer into it is a time bomb.

The fix is to allocate in memory that _outlives_ the function. `halloc` is the
safest choice (heap, lives until explicitly freed):

```xtlang
(bind-func make_pair
  (lambda (a:i64)
    (let ((p:<i64,i64>* (halloc)))
      (tfill! p a (* 2 a))
      p)))

(bind-func stable_demo
  (lambda ()
    (let ((p (make_pair 6)))
      (make_pair 2)
      (printf "p = <%lld,%lld>\n" (tref p 0) (tref p 1)))))

(stable_demo)
```

```
Compiled:  make_pair >>> [<i64,i64>*,i64]*
Compiled:  stable_demo >>> [i32]*
p = <6,12>
```

Now `p` points to heap memory that isn't going anywhere.

### Which allocator should I use?

| Allocator | Lifetime                             | Use when                                |
| --------- | ------------------------------------ | --------------------------------------- |
| `salloc`  | until the enclosing function returns | purely local scratch space              |
| `zalloc`  | until the current zone ends          | default; closure state, per-frame audio |
| `halloc`  | forever (until you call `free`)      | global state, cross-closure handles     |

The general advice: reach for `zalloc` first, `salloc` for tiny locals, and
`halloc` only when you genuinely need heap-lifetime memory.

## Where to next

You've now seen the core of the language:

- `bind-func` and type signatures
- pointers, tuples, and named types
- closures with captured state
- the three allocators and `memzone`

From here:

- [types](/reference/types/) --- full tour of the type system, including arrays,
  vectors, and generics
- [memory-management](/reference/memory-management/) --- more depth on zones,
  `memzone`, and the audio zone
- [type inference](/reference/types/#type-inferencing) --- when you need
  annotations and when you don't
- [error messages](/reference/error-messages/) --- common compiler errors and
  how to read them
- [testing](/reference/testing/) --- the `xtmtest` harness if you want to write
  tests

For full working programs, the `examples/core/` directory has plenty of small,
focused `.xtm` files that show these ideas in use.
