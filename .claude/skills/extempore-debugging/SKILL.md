---
name: extempore-debugging
description: Debugging and development guide for Extempore. Use when debugging JIT compilation issues, understanding symbol tracking, or testing compilation in different modes (batch, eval, interactive).
---

# Extempore debugging skill

## Architecture overview

Extempore has three main layers:

1. **C++ runtime** (`src/`): Scheme interpreter, LLVM JIT, audio/OSC
2. **Scheme runtime** (`runtime/`): scheme.xtm, llvmir.xtm, llvmti.xtm
3. **xtlang libraries** (`libs/`): user-facing compiled DSL code

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
- `--batch "expr"`: Batch mode (no server, single process, no audio); exits only
  if the expression calls `(quit ...)`. Implies `--noaudio`.
- `--eval "expr"`: Evaluate expression at startup but keep full server running
  (utility + primary processes, TCP server on port 7099).

### --batch vs --eval differences

`--batch` creates a single SchemeProcess with no TCP server. `--eval` starts the
full two-process setup (utility + primary) with TCP server, then evaluates the
expression as a `LOCAL_PROCESS_STRING` task.

**Bugs may reproduce in one mode but not the other.** The xtlang compiler's type
inference state can differ because `--eval` has a utility process that also loads
the base library into shared C++ statics (`sTypeDefinitions`, `sGlobalMap`).
Always try both modes when investigating user-reported bugs, since users
typically work interactively (equivalent to `--eval` / TCP eval).

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

The xtlang compiler generates specialised function names for ad-hoc
polymorphism using the pattern:

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

### --batch mode hangs after errors

When a compilation error occurs in `--batch` mode, the process does not
automatically exit --- it hangs waiting for further input. Use `timeout` when
running batch tests. The `sys:load-then-quit` helper is designed to exit after a
timeout, but compilation errors can prevent it from reaching the quit call.

### Bug doesn't reproduce in --batch mode

`--batch` runs a single process with no TCP server or utility process. `--eval`
and interactive TCP eval run two processes (utility + primary) that share C++
statics like `sTypeDefinitions` and `sGlobalMap`. This means compiler bugs can
appear in one mode but not the other.

When a user reports a bug from interactive use:

1. First try `--batch` --- if it reproduces, great, it's the simplest to debug
2. If not, try `--eval` with the same expression
3. If not, start extempore normally and send the expression via TCP with
   `printf '(expr)\r\n' | nc -w 10 localhost 7099`
4. If the bug is specifically about redefinition or accumulated state, send
   multiple expressions sequentially via TCP to simulate an interactive session

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

| Function                       | File              | Purpose                            |
| ------------------------------ | ----------------- | ---------------------------------- |
| `impc:ti:callback-check`      | `llvmti.xtm:7583` | callback arity/type checking      |
| `impc:ti:first-transform`     | `llvmti.xtm`      | AST transformation (macro expand) |
| `impc:ir:compiler:callback`   | `llvmir.xtm:4029`  | callback IR generation            |
| `impc:ti:get-closure-arg-types`| `llvmti.xtm`     | closure type lookup               |

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

- Write to a file: `FILE* f = fopen("/tmp/xtm_debug.log", "a"); fprintf(f, ...); fflush(f);`
- Write to stdout: `printf(...); fflush(stdout);` (mixes with Scheme output)
- Temporarily comment out the `freopen` line for a debug build

## Building and testing

```bash
# configure (fetches LLVM ~30s, full configure ~30s)
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release -DEXTERNAL_SHLIBS_GRAPHICS=OFF

# build (LLVM is the bulk of the build time)
cmake --build build --target extempore -- -j$(nproc)

# run core tests (no audio needed, ~150s total)
cd build && ctest -L libs-core --output-on-failure

# run audio example tests (need audio libs built, each test has 300s timeout)
cd build && ctest -L examples-audio --output-on-failure

# quick smoke test of a specific file
timeout 120 ./build/extempore --noaudio --batch \
  '(sys:load-then-quit "examples/core/fmsynth.xtm" 10)'
```

Test labels: `libs-core`, `libs-external`, `examples-audio`, `examples-core`,
`examples-graphics`. Defined in `extras/cmake/tests.cmake`.

### Examples as tests

Examples are registered as tests via the `extempore_add_example_as_test` macro in
`extras/cmake/tests.cmake`. They run with `--batch` (which implies `--noaudio`)
using `sys:load-then-quit`:

```cmake
extempore_add_example_as_test(examples/core/audio_101.xtm 10 examples-audio)
```

This translates to:

```bash
extempore --batch "(sys:load-then-quit \"examples/core/audio_101.xtm\" 10)"
```

Note: because `--batch` implies `--noaudio`, example tests verify that the code
**compiles** but do not exercise the audio callback path. An example can pass as
a test but fail interactively if the issue is audio-specific (e.g. `dsp:set!`
registration, hot-swap). To test with audio enabled, use `--eval` instead of
`--batch`.

## Capturing audio output to a file (headless/SSH)

`--batch` implies `--noaudio`, so you can't capture real audio output that way.
Instead, use `--eval` (which keeps audio enabled) with PipeWire's `pw-record` to
capture from the sink monitor.

### Prerequisites

- PipeWire running (check with `wpctl status`)
- `pw-record` available (from `pipewire` package)
- A PipeWire sink to capture from (the default "Dummy Output" works over SSH)

### Find the sink ID

```bash
wpctl status  # look for the sink ID under "Audio > Sinks"
```

### Capture audio

```bash
# 1. start recording from sink (e.g. sink ID 33)
pw-record --target 33 --rate 44100 --format f32 /tmp/output.wav &
PW_PID=$!
sleep 0.5

# 2. run extempore with audio enabled
timeout 20s ./build/extempore \
  --eval '(sys:load-then-quit "my_dsp_script.xtm" 15)'

# 3. stop recording
kill $PW_PID
wait $PW_PID 2>/dev/null
```

### Analyse the output

```bash
ffprobe /tmp/output.wav 2>&1 | grep -E "Duration|Stream"
ffmpeg -i /tmp/output.wav -af "volumedetect" -f null /dev/null 2>&1 \
  | grep -E "mean_volume|max_volume"
```

### Notes

- `pw-record --target <id>` captures from a specific sink's monitor port
- `--rate 44100 --format f32` matches Extempore's native format (avoids
  resampling); omit these to use PipeWire's default (48000 Hz, s16)
- the first few seconds of the recording will be silence while Extempore loads
  the base library and compiles DSP code
- the ALSA `file` plugin approach (`type file` in `.asoundrc`) does NOT work
  because its `null` slave provides no timing, causing PortAudio's audio clock
  to race ahead and Extempore's load timeouts to expire almost instantly

## Key files

| File                   | Purpose                                             |
| ---------------------- | --------------------------------------------------- |
| `src/SchemeFFI.cpp`    | `jitCompile()` - main JIT entry point               |
| `src/EXTLLVM.cpp`      | `addModule()`, `getGlobalValue()` - symbol tracking |
| `src/ffi/llvm.inc`     | Scheme FFI bindings for LLVM functions              |
| `runtime/llvmir.xtm`   | `llvm:compile-ir`, compilation queue                |
| `runtime/llvmti.xtm`   | Type inference, AOT compilation                     |
| `runtime/bitcode.ll`   | Base type definitions (`%mzone`, `%clsvar`)         |
| `libs/aot-cache/*.ll`  | Pre-compiled LLVM IR                                |
| `libs/aot-cache/*.xtm` | Scheme stubs that load `.ll` files                  |
