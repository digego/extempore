---
title: Error messages
---

A field guide to common xtlang compiler and JIT error messages: what they mean,
the snippet that produces them, and how to fix them.

:::note[All errors here are reproduced]

Each entry was produced by running the minimal snippet shown through
`./extempore --batch`. The error blocks are the real output, except that
internal names whose numbers vary run to run (an `_adhoc_N` suffix, a solver
`(var N)`) are written with `N` rather than a specific value. If you see a
genuinely different message from the same code, please
[open an issue](https://github.com/digego/extempore/issues).

:::

## Type errors

:::note[How type errors are reported]

In the v0.10.0 compiler, inference runs through a single constraint solver, and
when the constraints can't be satisfied most type errors surface the same way: a
`Type Error:` line saying what couldn't be reconciled, followed by

```
Type Error couldn't resolve type: NAME_adhoc_N
```

where `NAME` is the function being compiled and `_adhoc_N` is an internal suffix
whose number varies run to run. The first line is the useful one---it either
names the clashing types directly
(`type mismatch --- T is not compatible with U`) or, for a call, the argument
types and the candidate signatures that didn't match
(`no overload matches the argument types [...] --- candidates are [...]`). The
second line just says which definition failed. A handful of errors (literal
mismatches, unbound names, parse errors) have their own specific messages,
covered below.

:::

### `couldn't resolve type` (unconstrained) {#could-not-resolve-types}

**Snippet**

```xtlang
(bind-func unresolved_example
  (lambda (f)
    (f)))
```

**Error**

```
Type Error couldn't resolve type: unresolved_example_adhoc_8
```

**What it means.** The inferencer couldn't pin down a type. Here `f` is called
but never given a concrete type, so there's nothing to infer from. Note that
fully-unconstrained _numeric_ code does **not** error---the inferencer defaults
an unconstrained number to `double`---so this only bites when there is genuinely
no information to go on.

**Fix.** Annotate the argument so the compiler knows its type:

```xtlang
(bind-func unresolved_example
  (lambda (f:[i64]*)
    (f)))
```

See [type inference](/reference/types/#type-inferencing) for when annotations
are necessary and when they're redundant.

### `couldn't resolve type` (a conflict) {#type-conflict}

Passing the wrong type to a function, mixing up pointer types, or calling with
the wrong number of arguments all produce a `Type Error:` line saying what
clashed and then `couldn't resolve type`.

**Wrong argument type** --- here `takes_i64` wants an `i64` but gets the float
literal `3.0` (xtlang never silently coerces):

```xtlang
(bind-func takes_i64
  (lambda (x:i64)
    (+ x 1)))

(bind-func caller_bad_numeric
  (lambda ()
    (takes_i64 3.0)))
```

```
Compiled:  takes_i64 >>> [i64,i64]*
Type Error: no overload matches the argument types [double] --- candidates are [i64,i64]
Type Error couldn't resolve type: caller_bad_numeric_adhoc_N
```

No overload of the called function matched the argument types you supplied: you
passed a `double`, but the only candidate is `[i64,i64]` (an `i64` argument
returning `i64`). Fix: pass `3` instead of `3.0`, or convert with `dtoi64`.

**Wrong pointer type** --- passing a `coord2d*` where a `coord3d*` is wanted.
Note that even though `coord2d` is structurally `<double,double>`, the compiler
treats named types as distinct:

```xtlang
(bind-type coord3d <double,double,double>)
(bind-type coord2d <double,double>)

(bind-func needs_coord3d
  (lambda (c:coord3d*)
    (tref c 0)))

(bind-func pass_wrong_type
  (lambda ()
    (let ((c:coord2d* (zalloc)))
      (needs_coord3d c))))
```

```
Type Error: no overload matches the argument types [coord2d*] --- candidates are [double,coord3d*]
Type Error couldn't resolve type: pass_wrong_type_adhoc_N
```

The message names both sides: the argument is a `coord2d*`, while the only
candidate, `[double,coord3d*]`, wants a `coord3d*`.

**Dereferencing a non-pointer** --- `pref`/`pset!` and friends expect a pointer
(`double*`, `i64*`, `<...>*`); given a plain value you get a type mismatch
between the value's type and the pointer the solver needed:

```xtlang
(bind-func pref_non_pointer
  (lambda ()
    (let ((x:i64 5))
      (pref x 0))))
```

```
Type Error: type mismatch --- i64 is not compatible with !infer_N*
Type Error couldn't resolve type: pref_non_pointer_adhoc_N
```

Here `x`'s type (`i64`) clashes with the pointer the dereference required (the
`!infer_N*` is the solver's name for "a pointer to something").

**Fix.** Read the `Type Error:` line for the clashing types, then make them
agree: pass the right type, allocate the right pointer type, or `zalloc` a
pointer before dereferencing it.

### `bad numeric value` (literal vs annotation)

A literal that disagrees with an explicit annotation in a `let` binding still
gets its own specific message:

```xtlang
(bind-func let_wrong_type
  (lambda ()
    (let ((x:i64 3.5))
      x)))
```

```
Type Error bad numeric value 3.500000, should be i64
```

**Fix.** Change the literal (`3`, not `3.5`), change the annotation (`:double`
instead of `:i64`), or convert explicitly with `dtoi64` / `i64tod`.

## Undefined and unbound symbols

### `cannot find symbol NAME`

**Snippet**

```xtlang
(bind-func uses_undefined
  (lambda ()
    (undefined_function 3)))
```

**Error**

```
Compiler Error cannot find symbol undefined_function
```

**What it means.** The compiler can't find a `bind-func`, `bind-val`, or
`bind-type` with that name. Usually one of:

- typo in the name
- forgetting to load the library that defines it
- **forward reference**---xtlang compiles definitions in order, so a function
  has to be defined before anything that calls it

The forward-reference case is a classic trap:

```xtlang
(bind-func calls_later
  (lambda ()
    (defined_later 3)))      ;; error: defined_later doesn't exist yet

(bind-func defined_later
  (lambda (x:i64)
    (* x 2)))
```

```
Compiler Error cannot find symbol defined_later
```

The first `bind-func` fails because `defined_later` doesn't exist yet. Reorder
them so dependencies come first, or define a stub first.

**Fix.** Check the name; make sure any `(sys:load ...)` happens before the call
site; reorder `bind-func` forms so dependencies come first.

### `cannot find variable NAME`

**Snippet**

```xtlang
(bind-func set_undef
  (lambda ()
    (set! nope 5)))
```

**Error**

```
Compiler Error cannot find symbol nope
```

**What it means.** `set!` tried to mutate a variable that isn't in scope. In
xtlang, `set!` only works on variables introduced by `let`, `bind-val`, or
function arguments---it won't create a new binding.

**Fix.** Introduce the variable with `let` (or a global `bind-val`) first, then
`set!` it.

## Arity mismatches

### Wrong number of arguments

Calling a function with too few or too many arguments is, like a type mismatch,
reported through the overload solver: a `CONFLICT-OVERLOAD` line (whose `cands`
clause shows the arities the compiler knows about) followed by
`couldn't resolve type`.

**Too few arguments:**

```xtlang
(bind-func needs_two
  (lambda (a:i64 b:i64)
    (+ a b)))

(bind-func call_too_few
  (lambda ()
    (needs_two 3)))
```

```
Type Error: no overload matches the argument types [i64] --- candidates are [i64,i64,i64]
Type Error couldn't resolve type: call_too_few_adhoc_N
```

The `candidates are` clause shows the signature you missed: `[i64,i64,i64]` is a
two-argument function (return type first, then the two `i64` parameters), but
you supplied one argument. **Too many arguments** produces the same shape of
error.

**What it means.** `bind-func` is polymorphic---you can define several versions
with the same name and different signatures---so when a call matches none of
them by arity or type, the solver reports it as an unmatched overload rather
than a specific "wrong arity" message.

**Fix.** Check the function's type signature (the `Compiled:  NAME >>> ...` log
line) and match the argument count and types, or define another `bind-func`
variant with the signature you want.

## Aggregate indexing

### Tuple index out of bounds

```xtlang
(bind-type small_tuple <i64,i64>)

(bind-func bad_tref
  (lambda ()
    (let ((t:small_tuple* (zalloc)))
      (tref t 5))))
```

`small_tuple` has two fields (indices `0` and `1`), so index `5` is out of
bounds:

```
Syntax Error tuple index out of bounds: 5
```

The index must be a literal (you can't `tref` a tuple with a runtime variable),
so the compiler checks it against the tuple's field count and reports a bad one
directly. A negative literal index is caught the same way.

**Fix.** Use a valid index, or reach for an array (`|n,type|*`) if you need
indexed access with a variable. Note that array indexing via `aref` with an
out-of-bounds index is _not_ caught by the compiler---you'll get memory
corruption or a segfault at runtime.

## Parse errors

### `missing close paren`

**Snippet**

```xtlang
(bind-func paren_mismatch
  (lambda (a:i64)
    (+ a 1))
```

**Error**

```
;read-error ("missing close paren:   (lambda (a:i64)\n    (+ a 1))\n(quit 0)\n")
```

**What it means.** The reader hit end-of-input before all parentheses were
balanced. This is a Scheme-level read error, not an xtlang type error--- you'll
see it whenever the forms you send don't parse as complete s-expressions.

**Fix.** Close the paren. In an interactive session your editor's paren-matching
will usually catch this; in `--batch` or file loads it's easier to miss.

:::caution[Batch mode hangs on parse errors]

When `--batch` encounters a read error it prints the message but does _not_
exit. Use `timeout` (e.g. `timeout 10 ./extempore --batch "..."`) when running
untested scripts.

:::

## Memory surprises (not errors, but worth knowing)

Some of the nastiest bugs in xtlang don't produce any error message at all---
the compiler accepts your code, it runs, and it just returns wrong answers. The
[tutorial](/reference/tutorial/#why-salloc-is-dangerous-for-return-values) walks
through one: a `salloc`'d pointer returned from a function points into stack
memory that gets reused as soon as the function returns, so subsequent calls
overwrite it.

A similar trap waits outside `memzone`: a pointer allocated inside the zone is
invalidated when the zone exits, but nothing stops you from holding on to it.

**Rule of thumb.** If a pointer needs to outlive the function that created it,
don't use `salloc`. Use `zalloc` from a zone that you know lives long enough
(often the enclosing closure's zone), or `halloc` for heap memory. See
[memory management](/reference/memory-management/) for the full picture.

## Can't find your error here?

- Check the [tutorial](/reference/tutorial/) for the concepts, especially the
  sections on types and allocators.
- Search the `tests/` and `examples/` directories for similar constructs that
  compile cleanly---a diff against working code is often the fastest way to
  isolate an error.
- If you think the error message itself is unhelpful or wrong, file an issue.
