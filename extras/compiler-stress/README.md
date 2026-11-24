# Compiler stress tests

`stress.py` measures how the xtlang compiler's time grows with the size of its
input. Each family is synthetic xtlang that grows along one dimension
(arithmetic statements, captured variables, nesting depth, overloads and so on;
`stress.py list` describes them all). A sweep compiles each family at several
sizes and prints, per compiler phase, the seconds taken and a growth exponent
between the two largest sizes: about 1 is linear, 2 quadratic, 3 cubic.

```bash
extras/compiler-stress/stress.py sweep                        # every family
extras/compiler-stress/stress.py sweep arith nest_let --sizes 200,400,800
extras/compiler-stress/stress.py profile closure_vars 800 \
    --wrap xtc:codegen:compile:make-closure                   # time more functions
extras/compiler-stress/stress.py gen chain_back 5             # show the code
```

It needs a built `build/extempore`. `profile.xtm` does the timing inside
Extempore, wrapping the compiler's functions with timers. A sweep compiles
against a copy of `runtime/` (`--no-snapshot` turns that off), because the
compiler loads from disk at start-up and an edit would otherwise reach the
compiles still to come.

A total much larger than the phases is usually LLVM: the JIT compiles lazily,
after the phases this times.

## Checking a compiler change

A change meant only to make the compiler faster should leave its output alone.
To check:

1. Before the change, `cmake --build build --target clean_aot`, then
   `cmake --build build`, and copy `libs/aot-cache/`.
2. Dump the JIT-compiled IR of the studio instruments: set
   `*xtc:globals:print-raw-llvm*` to `#t`, load each instrument with
   `studio:load-instrument`, and save the output.
3. After the change, repeat both and diff. Ignore the lines reporting load
   times.

If the IR is meant to change, render the songs in `examples/studio/songs` before
and after with `extras/studio/render` and compare the WAVs. The renders are
bit-identical only with the random number generator seeded to a constant
(`sRandGen` in `src/EXTLLVM.cpp` is seeded from the clock), and `funk.xtm`
varies even then.
