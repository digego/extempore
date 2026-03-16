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
