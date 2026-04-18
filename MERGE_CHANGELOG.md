# aarch64 branch merge

Squashes ~185 commits on the `aarch64` branch into `master`. The changes fall
into a handful of major themes plus a lot of smaller fixes and refactors.

## Platforms

- **Linux aarch64**: first-class support, added to the CI matrix alongside
  macOS arm64, Linux x64 and Windows x64.
- **Apple Silicon (macOS arm64)**: now the default for macOS builds.
- **64-bit target detection** in `include/UNIV.h` now includes aarch64.

## LLVM

- Upgraded from LLVM 17 → **LLVM 22.1.1** (pinned in `CMakeLists.txt`).
- Dropped `ExternalProject` in favour of `FetchContent` for the LLVM
  dependency, so LLVM builds in-tree under `build/_deps/llvm-*` and caches
  across CI runs.
- LLVM is built with only the components Extempore actually needs (OrcJIT,
  target codegen, AsmParser, Passes, MCDisassembler, IRPrinter).
- Migrated from the legacy JIT to **ORC JIT**, including DSP hot-swap, module
  tracking, and adhoc alias handling.
- Fixed deprecation warnings for `Triple`-accepting overloads.
- Added `__hash_memory` shim for macOS builds.

## Scheme interpreter

- Replaced **TinyScheme with s7** (`src/SchemeS7.cpp`, `src/s7.c`). All
  extempore-authored Scheme code now runs through s7.
- Added compatibility shims for TinyScheme APIs (`closure?`, `cptr?`,
  `foldr`, `list*`, `make-string` emit-buffers, `set-input-port`, etc.).
- Fixed ratio literals, `gensym`, `file-exists?`, `open-input-file` for
  s7 semantics.
- Converted TinyScheme-style `(macro (name args) body)` to `(define-macro ...)`
  in `runtime/scheme.xtm` and `libs/core/instruments-scm.xtm` (s7 reserves
  `macro` as a core syntax form).

## xtlang compiler

- Major refactor for performance and maintainability:
  - Split `llvmti.xtm` into separate compiler modules
    (`llvmti-bind.xtm`, `llvmti-caches.xtm`, `llvmti-aot.xtm`, etc.).
  - Refactored `first-transform` into sub-dispatchers with AST accessors.
  - Replaced regex with string ops in hot paths.
  - Cached symbol conversions.
  - Converted generic and vars caches to hash tables.
  - Replaced the `sTypeDefinitions` string accumulator with structured maps.
  - Filtered preamble against user IR and captured declarations from all
    compilations.
- Added structured compiler error handling with shared primitives.
- Added adhoc alias map for `get_native_fptr` lookup.
- Added missing native function declarations to the template bitcode.
- Fixed callback bad-arity error when `bind-func` has a type annotation.
- Added compiler unit tests: `tests/compiler/{transforms,typeunify,typecheck,pipeline,constraints}.xtm`.

## Graphics: OpenGL → WebGPU

- Replaced the OpenGL graphics stack with **WebGPU via wgpu-native**.
- New `libs/external/webgpu.xtm` provides WebGPU bindings.
- New `libs/external/shadertoy.xtm` adds a live-coding Shadertoy-style
  fragment-shader environment with `iTime`, `iResolution`, `iMouse` uniforms.
- New `extras/webgpu/xtmwebgpu.{c,h}` C helper library wraps the parts of
  wgpu-native that xtlang's FFI can't call directly (struct-by-value,
  async callbacks, buffer/bind-group/pipeline construction).
- Two WebGPU examples: `examples/external/webgpu-triangle.xtm` and
  `examples/external/shadertoy.xtm`, both gated on
  `EXTERNAL_SHLIBS_GRAPHICS=ON` and tested in CI.
- Build: `external_shlibs_graphics` target builds glfw, stb_image,
  wgpu-native and libxtmwebgpu with correct rpaths.
- macOS Retina crash fixed in `xtmwebgpu.c`.
- Legacy OpenGL examples, libraries and shader tutorials removed from
  `examples/contrib/` and `libs/contrib/`.

## REPL

- New `--repl` flag starts an interactive linenoise-based REPL
  (`src/LinenoiseREPL.cpp`). Linux/macOS only.

## Build system

- Bumped minimum CMake version to 3.19.
- Switched to the Ninja generator with `EXCLUDE_FROM_ALL` for LLVM to fix
  Windows CI caching.
- Cache `.ninja_deps` / `.ninja_log` so LLVM isn't rebuilt on cache hits.
- AOT targets: `aot_core`, `aot_external_audio`, `clean_aot`.
- macOS arm64 minimum deployment target bumped to 11.0 (Big Sur).
- Apply `-march=native` tuning on ARM64.
- Restored `_aligned_malloc`/`_aligned_free` on Windows (MSVC lacks
  `std::aligned_alloc`).

## Runtime hygiene (C++)

- Replaced platform-specific ifdefs with C++17 stdlib equivalents.
- Replaced C standard headers with their C++ equivalents.
- Replaced `NULL` with `nullptr` in extempore-authored source.
- Replaced `malloc`/`free` with `std::vector` in OSC send paths.
- Used value-type vectors in the OSC TCP server data map.
- Used `std::string_view` for read-only string parameters in EXTLLVM.
- Removed dead `init()`/`destroy()` methods on mutex/condition wrappers.
- Removed unused nanotime/rdtsc timing infrastructure.
- Enforced LF line endings for `.xtm` and `.scm` files.
- Fixed Windows scheduler time advancing without sleep.
- Fixed batch-mode scheduler by initialising time before thread start.
- Fixed `--run` flag to load the base library.

## Test infrastructure

- New `extempore_add_example_as_test` macro registers examples as ctest
  tests, loaded via `sys:load-then-quit` in `--batch` mode.
- Added compiler unit test suite (label `compiler-unit`).
- Unique per-test TCP port allocation so tests can run in parallel.
- CI matrix (`build-and-test.yml`) now covers Linux x64, Linux aarch64,
  macOS arm64, Windows x64.

## Examples cleanup

Kept only examples that compile cleanly in CI:

- `examples/core/` (19 files): audio_101, audio_streams, covers,
  expr_problem, extempore_lang, fmsynth, midi_synth, mtaudio, native_app,
  native_app_with_xtm, native_dll, nbody_lang_shootout, osc_101, petri-net,
  scheduler, synth, topclock_metro, typeclasses, xthread.
- `examples/external/` (9 files): audio_player, convolution_reverb,
  electrofunk, portmidi, portmidi-output, sampler, sing_a_song,
  webgpu-triangle, shadertoy.
- `examples/sharedsystem/` unchanged.

Removed:

- All of `examples/contrib/` (legacy OpenGL demos, unmaintained externals).
- `examples/core/{fasta_lang_shootout, godot_test1, extempore_lang_ipc}.xtm`
  (`fdopen` undeclared, Godot dep, IPC needs `--eval` not `--batch`).
- `examples/external/{granulator, sqlite, ulfius}.xtm` (missing assets or
  unbundled shared libraries).

Fixes to retained examples:

- `examples/core/midi_synth.xtm`: now loads `libs/external/midi.xtm`
  (the old `midi_input.xtm` is gone).
- `examples/core/covers.xtm`: `set_lfo` calls updated to the 6-arg
  signature.

Correspondingly removed legacy graphics/unused libraries from
`libs/external/`: `bullet.xtm`, `horde3d.xtm`, `graphics-pipeline-scm.xtm`,
`particles.xtm`, `particles_vbo.xtm`, `fluid2d.xtm`, `cef.xtm`, `cef/`,
`libulfius.xtm`, `sqlite.xtm`. All of `libs/contrib/` also removed.

## Documentation

- New VitePress site under `docs/` (built via `.github/workflows/docs.yml`).
- `extras/tree-sitter-extempore/` added as a submodule for editor tooling.
- README: removed the "experimental aarch64 branch" note (aarch64 is now
  supported on master), updated copyright to 2025.
- CLAUDE.md updated: LLVM version note, debugging/agent workflow details.

## Bug fixes worth calling out

- Fixed DSP hot-swap after the ORC JIT migration.
- Fixed `get_native_fptr` symbol lookup.
- Fixed MSVC build: `#include <array>` and `/bigobj`.
- Fixed `stdbool.h` missing in `xtmwebgpu.c`.
- Fixed WebGPU rendering crashes on macOS Retina displays.
- Fixed test failures for portmidi, webgpu-triangle, granulator.

## Tooling

- `mise.toml` pins `node = "24"` for docs builds.
- `backlog/` directory tracks outstanding tasks (LLVM 22 ORC JIT features,
  macOS arm64 DSP type error, clang-tidy pass, Windows AOT hang, adhoc alias
  improvements).
