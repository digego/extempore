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
`sanitize-asan`/`sanitize-ubsan`/`sanitize-tsan`/`sanitize-asan-ubsan`. The
sanitizer presets build into `build-asan/` and so on, the rest into `build/`.
`cmake --workflow --preset default` configures, builds and runs the CI tests.

The presets use Ninja. The AOT steps run in a Ninja job pool capped at 2, so
build at full parallelism.

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
ctest --preset default                       # the suites CI runs
ctest --test-dir build -L libs-core -j4      # core library tests
ctest --test-dir build -L libs-studio -j4    # the studio (libs/studio)
ctest --test-dir build -L audio-offline -j4  # render audio offline, check the WAVs
ctest --test-dir build -L cli               # the command line: output, exit statuses
ctest --test-dir build -L nrepl -j2         # the nREPL server and its clients
ctest --test-dir build -L lsp               # the language server (extempore lsp)
```

Tests are `.xtm` files in `tests/`; `libs/core/test.xtm` is the harness
(`xtmtest-result`, `is?` macro). `build/extempore test tests/core/adt.xtm` runs
one (or several, each in its own process). `tests/cli` tests the command line
itself, and `tests/core/system.xtm` runs in a session, since it tests IPC.

In addition, building the `aot_core` target (part of the default build) is a
pretty good sign that things are working.

NOTE: this project uses GitHub Actions (in particular the
`.github/workflows/build-and-test.yml` workflow) to build and test on Linux
(x64), macOS (arm64), and Windows (x64).

## Common tasks

The build writes its AOT cache and the shared libraries xtlang binds into
`build/libs/` (`aot-cache/`, `platform-shlibs/`), never the source tree; the
runtime finds them beside the binary (`sys:build-dir`). Don't edit them.

```bash
cmake --build build --target aot_core            # AOT compile the core stdlib
cmake --build build --target clean_aot           # delete build/libs/aot-cache
cmake --build build --target xtmdoc              # generate docs
cmake --build build --target tidy                # clang-tidy; CI fails on any finding
build/extempore --no-audio                       # run without audio
build/extempore --repl                           # interactive REPL (Linux/macOS only)
build/extempore fmt libs examples tests runtime extras  # format the .xtm sources (CI checks)
```

The formatter's table of special forms (`src/Formatter.cpp`) has copies in
extempore-emacs-mode and tree-sitter-extempore's `queries/indents.scm`: change
all three together.

`extras/compiler-stress/stress.py` measures how compile time grows with input
size. Its README describes how to check that a compiler change leaves the
generated IR alone.

## The studio

`libs/studio` is the instrument, mixer and pattern-language library. Read
`libs/studio/README.md` before writing or changing an instrument: it has the
instrument interface and the xtlang traps. `extras/studio/render` renders an
`.xtm` score to a WAV offline and prints an analysis of it, which is how to
check a sound without listening to it.

Its `engine`, `dsp`, `fx`, `hall` and `mixer` files are AOT-compiled (target
`aot_studio`). Unlike a core library's, a cache whose sources have changed is
skipped (`libs/studio/aot.xtm`), so an edit to one of them takes effect without
a rebuild, compiled from source.

## Docs site

The docs are an Astro/Starlight site in `docs/` (run commands there, with
`mise exec --`). `python3 verify-examples.py` runs the doc xtlang snippets
against `build/extempore` (local only).

## Evaluating extempore code

For users, extempore is designed to be run interactively (start it, then connect
an editor to port 7099 and send s-expressions for evaluation).

To run code and exit, for debugging or testing, use `exec`:

```bash
build/extempore exec -e "(println 'hello)"
build/extempore exec script.xtm -- --name=value   # the script reads (sys:cmdarg "name")
```

It starts no audio and no servers, and exits when the code has finished and
nothing is left scheduled: status 0, or 1 if an error was raised or logged. Its
stdout is what the code prints, and stderr has the warnings and errors; `-v`
adds the load and `Compiled:` messages, `-vv` debug ones (`EXTEMPORE_LOG`
filters by component, see `extempore exec --help`). Code which schedules itself
forever (a temporal recursion) runs until it calls `(quit)`, so give untested
code a `timeout`.

`build/extempore render FILE -o OUT.wav --duration SECONDS` renders a file's
audio offline, and renders the same samples every time.
`build/extempore --help` lists the subcommands.

To iterate against a running instance instead, start `build/extempore --no-audio
--nrepl-port 0` from a scratch directory and send code from there with
`build/extempore send '...'` (or `--file F`, or `--file F --line N` for one
form): it prints values and output, exits 1 on an error, and libraries stay
loaded between evals. `build/extempore status` says whether it is reachable.
`docs/src/content/docs/guides/nrepl.md` describes the server.
