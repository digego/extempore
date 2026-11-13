---
name: extempore-debugging
description:
  Debugging and development guide for Extempore. Use when debugging JIT
  compilation issues, understanding symbol tracking, or testing compilation in
  different modes (batch, eval, interactive).
---

# Extempore debugging skill

## Compilation paths

### Normal (interactive) compilation

```
llvm:compile-ir
  -> llvm:jit-compile-ir-string (Scheme FFI)
    -> jitCompile() in src/SchemeFFI.cpp
      -> initializeTemplateModule() parses runtime/bitcode.ll once
      -> parseAssemblyInto() of (type defs + user IR)
      -> EXTLLVM::addTrackedModule() (ORC JIT)
      -> EXTLLVM::addModule() (metadata tracking)
```

### AOT compilation

When `*impc:aot:current-output-port*` is set:

```
llvm:compile-ir
  -> impc:compiler:queue-ir-for-compilation
    -> appends to *impc:compiler:queued-llvm-ir-string*

impc:compiler:flush-jit-compilation-queue
  -> llvm:jit-compile-ir-string with accumulated IR
```

## Startup sequence

1. C++ `main()` in Extempore.cpp
2. SchemeProcess ctor loads `runtime/init.xtm`
3. SchemeProcess task loads `runtime/scheme.xtm`, `runtime/llvmti.xtm`,
   `runtime/llvmir.xtm`
4. Primary process compiles `runtime/init.ll` via `sys:compile-init-ll`
5. If `EXT_LOADBASE` is true (default), loads `libs/base/base.xtm`
6. `base.xtm` triggers AOT cache loading via
   `impc:aot:insert-header`/`impc:aot:import-ll`
7. AOT cache files (e.g. `libs/aot-cache/base.xtm`) call `llvm:compile-ir` with
   `.ll` files

## Key flags

- `--nobase`: Skip loading base library (useful for debugging JIT in isolation)
- `--noaudio`: Disable audio (required for headless/CI testing)
- `--batch "expr"`: Batch mode (no server, single process); `AGENTS.md` covers
  exiting and `timeout`. Implies `--noaudio` **unless `--audio-outfile` is also
  given** — in that case the offline file driver handles audio output.
- `--eval "expr"`: Evaluate expression at startup but keep full server running
  (utility + primary processes, TCP server on port 7099).
- `--audio-outfile <path>`: render DSP output to a float32 WAV file via an
  offline driver (see below). Combines with `--batch` for headless CI runs.
- `--duration <seconds>`: cap on `--audio-outfile` render length. When reached,
  the driver finalizes the WAV and self-exits with status 0. 0/unset = render
  until `(quit)`.

### --batch vs --eval differences

`--batch` creates a single SchemeProcess with no TCP server. `--eval` starts the
full two-process setup (utility + primary) with TCP server, then evaluates the
expression as a `LOCAL_PROCESS_STRING` task.

**Bugs may reproduce in one mode but not the other.** The xtlang compiler's type
inference state can differ because `--eval` has a utility process that also
loads the base library into shared C++ statics (`sTypeDefinitions`,
`sGlobalMap`). Always try both modes when investigating user-reported bugs,
since users typically work interactively (equivalent to `--eval` / TCP eval).

## Symbol tracking

`EXTLLVM::addModule()` populates `sGlobalMap` with function/global pointers:

- Key: symbol name (string)
- Value: pointer to `llvm::GlobalValue` in the metadata module clone

`EXTLLVM::getFunction()` / `EXTLLVM::getGlobalValue()` look up symbols in this
map.

## sTypeDefinitions accumulator

`jitCompile()` maintains a static string `sTypeDefinitions` (~400KB after full
library loading). It accumulates LLVM IR declarations (struct types, function
declarations, external globals) from every successful compilation so that
subsequent modules can reference earlier symbols. It is prepended to every user
IR string before parsing:

```
fullIR = sTypeDefinitions + userIR
```

This is parsed via `parseAssemblyInto()` into a cloned template module (from
`bitcode.ll`). If `sTypeDefinitions` contains a declaration that conflicts with
something already defined in the template module, the parse fails silently
(stderr is /dev/null) and the Scheme layer sees `#f` from
`llvm:jit-compile-ir-string`, producing "FLUSH FAILED".

## Adhoc polymorphism names

The xtlang compiler generates specialised function names for ad-hoc polymorphism
using the pattern:

```
<basename>_adhoc_<counter>_<base64-encoded-type-signature>
```

For example: `xtm_play_adhoc_492_W05vdGVEYXRhKi...`. The base64 portion
(`cname-encode`/`cname-decode` in `runtime/llvmir.xtm`) encodes the full type
signature. These names can be extremely long (hundreds of characters).

## Common issues

### Type definitions

AOT-compiled `.ll` files reference types like `%mzone`, `%clsvar` defined in
`runtime/bitcode.ll`. These must be available when parsing user IR.

### Windows CRLF

Regex-based IR parsing fails on Windows due to CRLF line endings. Use
line-by-line parsing with explicit CR stripping.

### Symbol not found after compilation

Check that:

1. Module was added to ORC JIT successfully
2. `EXTLLVM::addModule()` was called with the metadata clone
3. Symbol name matches exactly (including mangling like `_adhoc_`, `_poly_`)

### Bug doesn't reproduce in --batch mode

Compiler bugs can appear in one mode but not the other (see "--batch vs --eval
differences" above). For a bug reported from interactive use, try `--batch`, then
`--eval` with the same expression, then a normal start with the expression sent
via TCP (`printf '(expr)\r\n' | nc -w 10 localhost 7099`). For redefinition or
accumulated-state bugs, send several expressions sequentially over TCP.

## Scheme ↔ xtlang interop gotchas

### Calling xtlang functions from Scheme

A top-level `bind-func` makes the function name callable from the Scheme
interaction environment. `(bind-func foo ...)` → `(foo arg1 arg2)` works. Return
values flow back (i64 → Scheme integer, double → real, etc.).

### `--batch` evaluates a single expression

`--batch "<expr>"` reads one Scheme form. Multiple top-level expressions won't
all run — wrap them in `(begin ...)`:

```bash
# WRONG: only the first form runs
--batch '(sys:load "foo.xtm") (foo_test)'

# RIGHT
--batch '(begin (sys:load "foo.xtm") (foo_test))'
```

### `(quit rc)` and stdio flushing

`exit_extempore` in `src/ffi/utility.cpp` calls `std::_Exit(rc)`, which bypasses
destructors **and discards stdio buffers**. It explicitly `fflush(stdout)`
before `_Exit` so xtlang `printf` output isn't lost. If you add another pre-exit
cleanup step there, keep it short — `_Exit` runs right after.

### xtlang `set!` returns the assigned value

Unlike Scheme where `set!` is effectively void, xtlang `set!` returns the new
value. This breaks the common pattern `(if cond (set! x v))` with no else branch
— the type inferencer sees `then: (type of v)` vs. implicit `else: void` and
errors. Fixes: add explicit `void` branches, or wrap in `begin`:

```scheme
;; WRONG: type error "float vs void"
(if (> v peak) (set! peak v))

;; RIGHT
(if (> v peak) (begin (set! peak v) void) void)
```

## Debugging commands

```scheme
;; List all modules
(llvm:list-modules)

;; Print all modules
(llvm:print)

;; Check if function exists
(llvm:get-function "function_name")

;; Print specific function
(llvm:print-function "prefix")
```

## Sending expressions via TCP

Extempore's TCP protocol requires `\r\n` (CRLF) termination. Expressions are
read until `\r\n` is found (`src/SchemeProcess.cpp:541`). Without CRLF, the
expression is buffered but never evaluated.

```bash
# Start extempore with a specific port
./build/extempore --noaudio --port 17099 > /tmp/xtm_output.log 2>&1 &
sleep 8  # wait for base library to load

# Send an expression (printf for CRLF, nc for TCP)
printf '(println 42)\r\n' | nc -w 5 localhost 17099 > /dev/null

# Check output
tail /tmp/xtm_output.log
```

Compilation output goes to extempore's stdout, not back through the TCP socket.
The socket only returns `"Welcome to extempore!"` on connect and (optionally)
the result of `ipc:` calls.

TCP eval dispatches expressions as `SchemeTask::Type::REPL` tasks, which is the
closest to how editors (VS Code, Emacs) send code interactively. This can
produce different results from `--batch` because the evaluation context and
process topology differ.

## Monkey-patching Scheme compiler functions

To debug the xtlang compiler (type inference, callback handling, etc.), you can
redefine Scheme functions at runtime via TCP to inject logging. This avoids
rebuilding and lets you inspect internal state.

```bash
# Redefine a compiler function to add debug output
# (use define, not set! --- set! gets mangled by some code paths)
printf '(define impc:ti:callback-check
  (let ((old impc:ti:callback-check))
    (lambda (ast vars kts request?)
      (println (quote DEBUG) (quote ast:) ast)
      (old ast vars kts request?))))\r\n' | nc -w 10 localhost 17099 > /dev/null

# Now trigger the code path you want to debug
printf '(bind-func my_test (lambda (x:i64) x))\r\n' | nc -w 10 localhost 17099 > /dev/null

# Check the debug output
tail /tmp/xtm_output.log
```

Key runtime functions to instrument:

- `impc:ti:callback-check` (`llvmti.xtm`): callback arity/type checking
- `impc:ti:first-transform` (`llvmti.xtm`): AST transformation (macro expand)
- `impc:ir:compiler:callback` (`llvmir.xtm`): callback IR generation
- `impc:ti:get-closure-arg-types` (`llvmti.xtm`): closure type lookup

## Testing in isolation

```bash
# Skip base library to test JIT directly
./extempore --nobase --batch "(begin (llvm:jit-compile-ir-string \"define i64 @test() { ret i64 42 }\") (println (llvm:get-function \"test\")) (quit 0))"

# Test AOT cache loading
./extempore --nobase --batch "(begin (llvm:compile-ir (sys:slurp-file \"libs/aot-cache/xtmbase.ll\")) (quit 0))"
```

## C++ debug output

**stderr is unconditionally redirected to /dev/null** at startup
(`src/Extempore.cpp:174`: `freopen("/dev/null", "w", stderr)`). Neither
`std::cerr`, `fprintf(stderr, ...)`, nor any amount of flushing will produce
visible output. Options:

- Write to a file:
  `FILE* f = fopen("/tmp/xtm_debug.log", "a"); fprintf(f, ...); fflush(f);`
- Write to stdout: `printf(...); fflush(stdout);` (mixes with Scheme output)
- Temporarily comment out the `freopen` line for a debug build

## Building and testing

The build and the core test labels are in `AGENTS.md`.

```bash
# run audio example tests (need audio libs built, each test has 300s timeout)
cd build && ctest -L examples-audio --output-on-failure

# quick smoke test of a specific file
timeout 120 ./build/extempore --noaudio --batch \
  '(sys:load-then-quit "examples/core/fmsynth.xtm" 10)'
```

Test labels: `libs-core`, `libs-external`, `examples-audio`, `examples-core`,
`examples-graphics`, `audio-offline`. Defined in `extras/cmake/tests.cmake`.

The `audio-offline` label covers two-phase tests that render a DSP file to a WAV
via `--audio-outfile` and then assert on it with `libs/core/audiotest.xtm`.
Register via
`extempore_add_audio_offline_test(name render_xtm duration freq label)`.

### Examples as tests

Examples are registered as tests via the `extempore_add_example_as_test` macro
in `extras/cmake/tests.cmake`. They run with `--batch` (which implies
`--noaudio`) using `sys:load-then-quit`.

Note: because `--batch` implies `--noaudio`, example tests verify that the code
**compiles** but do not exercise the audio callback path. An example can pass as
a test but fail interactively if the issue is audio-specific (e.g. `dsp:set!`
registration, hot-swap). To test with audio enabled, use `--eval` instead of
`--batch`.

## Capturing audio output to a file (headless/SSH/CI)

### Offline rendering with `--audio-outfile` (preferred)

The offline file driver renders DSP output directly to a float32 WAV without any
OS audio subsystem. Works in `--batch` mode, so it's suitable for CI:

```bash
./build/extempore --batch '(sys:load "examples/core/hello-sine.xtm")' \
  --audio-outfile /tmp/out.wav --duration 1.0
```

The driver free-runs faster than realtime (~100× on typical hardware for a
simple sine). Key semantics:

- Counts toward `--duration` only after `dsp:set!` registers a closure, so
  compile-path latency doesn't eat the render window.
- On `--duration` reached, the driver finalizes the WAV header and calls
  `std::_Exit(0)`. User script doesn't need to quit itself.
- On user `(quit rc)`, `exit_extempore` calls `stopFileDriver()` before `_Exit`,
  so the WAV is finalized in either path.
- Not realtime — anything that uses wall-clock (`clock:clock`, MIDI I/O,
  network) will drift because `getRealTime()` is still real time while
  `UNIV::TIME` free-runs. DSP that depends only on the `time` sample counter
  renders identically to realtime.

### Asserting on offline output

`libs/core/audiotest.xtm` provides `audiotest_assert_sine`, `audiotest_rms`,
`audiotest_peak`, `audiotest_goertzel`. Call from Scheme with the xtlang
function name directly; pipe the return through `(quit ...)` so the process exit
code matches the assertion outcome:

```bash
./build/extempore --batch '(begin (sys:load "libs/core/audiotest.xtm")
                                  (quit (audiotest_assert_sine "/tmp/out.wav" 440.0)))'
```

Returns 0 on PASS, 1 on FAIL, with a diagnostic line printed to stdout.

`CMake` registration: see `extempore_add_audio_offline_test` in
`extras/cmake/tests.cmake`. The macro chains render + verify via `cmake -P`
running `extras/cmake/run_audio_offline_test.cmake`, so it works on all
platforms CTest supports.

To verify the _realtime_ path rather than the offline one, capture a real audio
output with PipeWire's `pw-record`: see `references/pipewire-capture.md`.
