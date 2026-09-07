# Extempore

Live coding environment for music, audio, and graphics. Scheme interpreter with
xtlang---a statically-typed lisp that compiles to LLVM IR at runtime.

## Build

```bash
cmake --preset default
cmake --build build
```

`CMakePresets.json` holds the configurations CI uses: `default`/`ci` (Ninja,
Release, tests on), `release` (assets, no tests, `EXT_SHARE_DIR=.`) and
`sanitize-asan`/`sanitize-ubsan`/`sanitize-tsan`/`sanitize-asan-ubsan`. All of
them build into `build/`.

Ninja is not optional in the presets: it keeps all cores busy far better than
Make on the LLVM build and makes incremental rebuilds near-instant. The AOT
steps run in a Ninja job pool capped at 2, so build at full parallelism.

Key options: `-DASSETS=ON` (download multimedia assets), `-DBUILD_TESTS=ON`
(default).

### LLVM dependency

LLVM is fetched and built automatically via CMake's `FetchContent`. The version
is pinned in `CMakeLists.txt` (currently 22.x). On first build, CMake downloads
the LLVM source tarball and builds only the required components (OrcJIT, target
codegen, AsmParser, Passes, MCDisassembler, IRPrinter).

After configuration, LLVM sources and build artifacts are in:

- `build/_deps/llvm-src/` --- LLVM source tree
- `build/_deps/llvm-build/` --- LLVM build outputs (libraries, generated
  headers)

The LLVM headers used by extempore come from two locations:

- `build/_deps/llvm-src/llvm/include/` --- static headers from source
- `build/_deps/llvm-build/include/` --- generated headers (e.g. `llvm/Config/`)

The first LLVM build takes significant time (~10-30 min depending on hardware).
Subsequent builds reuse the cached LLVM artifacts. The GitHub Actions workflow
caches `build/_deps/` to speed up CI builds.

## Test

```bash
ctest --test-dir build -L libs-core -j4      # core library tests
ctest --test-dir build -L libs-external -j4  # external library tests
```

Tests are `.xtm` files in `tests/`. They run in `--batch` mode (which implies
`--noaudio`), except for `system.xtm` which uses `--eval` for IPC testing.

In addition, building the `aot_external_audio` target (the default) is a pretty
good sign that things are working.

NOTE: this project uses GitHub Actions (in particular the
`.github/workflows/build-and-test.yml` workflow) to build and test on Linux
(x64), macOS (arm64), and Windows (x64).

## Structure

| Directory         | Purpose                                               |
| ----------------- | ----------------------------------------------------- |
| `src/`            | C++ runtime (Scheme interpreter, LLVM JIT, audio/OSC) |
| `include/`        | C++ headers                                           |
| `runtime/`        | Bootstrap files (scheme.xtm, LLVM IR bitcode)         |
| `libs/core/`      | Core xtlang standard library                          |
| `libs/external/`  | Bindings to external libs (WebGPU, audio codecs, FFT) |
| `libs/aot-cache/` | AOT-compiled bytecode (auto-generated, don't edit)    |
| `tests/`          | Test files (.xtm)                                     |
| `examples/`       | Example programs                                      |

## Languages

- **C++20**: runtime in `src/` (SchemeS7.cpp, EXTLLVM.cpp, AudioDevice.cpp)
- **Scheme**: user-facing interpreted language
- **xtlang**: compiled DSL, files use `.xtm` extension, compiles to LLVM IR

## Key files

- `src/Extempore.cpp` --- main entry point
- `src/SchemeS7.cpp` --- s7 Scheme interpreter adapter
- `src/EXTLLVM.cpp` --- LLVM JIT compilation
- `runtime/scheme.xtm` --- Scheme runtime bootstrap
- `libs/core/test.xtm` --- test harness (`xtmtest-run-tests`, `is?` macro)

## Common tasks

```bash
cmake --build build --target aot_core            # AOT compile the core stdlib
cmake --build build --target aot_external_audio  # ... and the audio libs (default)
cmake --build build --target clean_aot           # delete libs/aot-cache
cmake --build build --target xtmdoc              # generate docs
build/extempore --noaudio                        # run without audio
build/extempore --repl                           # interactive REPL (Linux/macOS only)
```

## Evaluating extempore code

For users, extempore is designed to be run interactively (start it, then connect
an editor to port 7099 and send s-expressions for evaluation).

There is also a "batch mode" useful for debugging/testing, e.g.

```bash
./extempore --batch "(begin (println 'hello) (quit 0))"
```

Note: `--batch` implies `--noaudio`, so there's no need to specify both.

If the final `quit` isn't present, then extempore won't exit. And if the eval'ed
code throws an error, extempore won't exit either (it will print the scheme
stacktrace and then await further instructions). For this reason, for scripted
debugging it's often helpful to use `timeout` (combined with `--batch`) with a
short value (e.g. 10s) to ensure that whatever happens the script will exit.
