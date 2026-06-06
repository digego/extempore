# Extempore changelog

First, a confession: the Extempore maintainers (i.e. Andrew & Ben) have been
really bad at keeping a changelog. But hopefully we'll be better in the future.

## v0.9.4

Patch release restoring the `put`/`get` symbol property-list functions, broken
since the s7 swap in v0.9.0.

- **Scheme `put`/`get`**: `(put 'a 'b 10)` and `(get 'a 'b)` --- TinyScheme's
  symbol property lists --- raised `Unbound variable put`/`get` since v0.9.0.
  They were TinyScheme builtins (its optional `USE_PLIST` feature, a
  MacLisp/Common-Lisp-heritage extension that no RnRS defines, enabled in our
  vendored copy) with no s7 equivalent, so the interpreter swap dropped them.
  Restored in `runtime/init.xtm` --- the file that already re-provides dropped
  TinyScheme builtins --- backed by a hash table keyed by `(sym . key)`: a
  re-`put` overwrites in place, and `get` returns `#f` when a property is
  absent. New `tests/core/scheme-compat.xtm` guards them. Reported on the
  mailing list by Minoru.

- **Build**: removed the long-broken `EXT_DYLIB` ("build as a dynamic library")
  CMake option. A rename from `DYLIB` to `EXT_DYLIB` left seven of its eight
  source guards as dead `#ifdef DYLIB` blocks, so the embedded-resource loading
  it promised never compiled. The option and its dead code are gone, with no
  change to the normal executable build.

## v0.9.3

Patch release fixing the pattern language, broken since the s7 swap in v0.9.0.

- **Pattern language**: the `:>` (start/modify) and `:|` (stop) pattern
  operators stopped working in v0.9.0 --- evaluating e.g. `(:> A 4 0 (play syn1
  @1 80 dur) (list 60 63 67 72))` raised `Unbound variable A`, as did every
  `examples/sharedsystem/pattern_basics.xtm` line. s7 (unlike the old
  TinyScheme reader) treats any symbol that begins or ends with a colon as a
  self-evaluating keyword constant, and a keyword can't be bound with
  `define-macro`, so the operator definitions silently failed and `(:> ...)`
  was read as an ordinary application. Fixed by narrowing s7's keyword rule (in
  our vendored `src/s7.c`) so a colon only marks a keyword when the character
  adjacent to it is a letter: genuine keywords (`:readable`, `foo:`, `define*`
  keyword args --- all alphabetic) are unaffected, while operator-style names
  like `:>` and `:|` stay ordinary, macro-bindable symbols. This is a
  deliberate extempore-local divergence from stock s7 --- we can't rename the
  operators without breaking pattern-language code already out in the wild, so
  we adapt the reader instead. New `tests/core/pattern-language.xtm` guards it.
  Reported on the mailing list by George.

## v0.9.2

Patch release fixing a multi-form REPL eval regression introduced with the s7
swap in v0.9.0.

- **Multi-form eval**: selecting and evaluating multiple top-level
  s-expressions in one go (over the eval socket or via `--batch`/`--eval`)
  previously ran only the first form and silently discarded the rest. The
  s7-backed `eval_with_error_trap` was using `s7_eval_c_string` (single-form
  read-and-eval) instead of `s7_load_c_string` (the `load` equivalent that
  iterates every top-level form). Reported on the mailing list by George.

## v0.9.1

Patch release fixing a critical packaging bug in the v0.9.0 binary release.

- **Release packaging**: the v0.9.0 binary release had `EXT_SHARE_DIR`
  hardcoded to the GitHub Actions runner's ephemeral source path, so
  downloaded binaries aborted with `could not locate file init.xtm` on every
  user's machine. The release workflow now passes `-DEXT_SHARE_DIR=.` so the
  binary locates `runtime/`, `libs/` etc. relative to the current working
  directory --- matching what the README already tells users to do (run
  `./extempore` from inside the unzipped folder).

  Workaround for anyone stuck on a v0.9.0 download: run
  `./extempore --sharedir .` from inside the extempore folder.

## v0.9.0

A major release. See `MERGE_CHANGELOG.md` for the full breakdown.

### Platforms

- Linux aarch64 and macOS arm64 (Apple Silicon) are now first-class targets in
  the CI matrix alongside Linux x86_64 and Windows x86_64.

### Toolchain

- LLVM upgraded from 17 to **22.1.1**. JIT migrated from the legacy JIT to
  **ORC JIT**.
- LLVM is now fetched and built in-tree via CMake's `FetchContent` (replacing
  `ExternalProject`); only the components Extempore uses are built.
- Minimum CMake 3.28. Builds use the Ninja generator.

### Scheme interpreter

- **TinyScheme replaced with s7**. All Scheme/xtlang code now runs through
  `src/SchemeS7.cpp` with compatibility shims for TinyScheme APIs.

### Graphics

- OpenGL graphics stack replaced with **WebGPU via wgpu-native**. New
  `libs/external/webgpu.xtm`, Shadertoy-style live-coding lib
  (`libs/external/shadertoy.xtm`), and two WebGPU examples
  (`webgpu-triangle.xtm`, `shadertoy.xtm`).
- Legacy OpenGL examples, shader tutorials, and graphics libraries removed.

### Compiler

- Major refactor of `runtime/llvmti.xtm` into separate modules.
- Converted caches to hashtables, replaced regex with string ops, added
  structured error handling. `sTypeDefinitions` string accumulator replaced
  with structured maps.
- New compiler unit test suite (`tests/compiler/`).

### Runtime

- New `--repl` flag: interactive linenoise-based REPL (Linux/macOS).
- DSP hot-swap fixed after the ORC JIT migration.
- Many C++ modernisations (`nullptr`, `std::vector`, `std::string_view`,
  C++17 stdlib replacing platform ifdefs).

### Docs

- New VitePress documentation site under `docs/`.
- `extras/tree-sitter-extempore/` added as a submodule for editor tooling.

### Assets

- The assets tarball (`ASSETS=ON`) now comes from a GitHub release asset on
  `extemporelang/extempore-assets` (tag `v0.9.0`), ~250 MB of audio samples
  and impulse responses. Unused fonts, images, 3D models and the duplicate
  Christmas Carol sample have been trimmed out.

### Examples

- `examples/contrib/` removed (legacy graphics, unmaintained externals).
- Core/external example set slimmed to only those that load cleanly in CI.
- Full working set registered in `extras/cmake/tests.cmake` and tested on
  every push.

### Stability

Post-beta hardening before cutting the stable release:

- **Concurrency**: atomic `UNIV::TIME`/`DEVICE_TIME` and `m_libsLoaded`; race
  fixes in the s7 FFI table; `gethostbyname` replaced with thread-safe
  `getaddrinfo`; MT dispatcher spin-sleep swapped for a counting semaphore;
  audio threads converted to RAII with per-thread PRNG; legacy
  `EXTMutex`/`EXTMonitor`/`EXTCondition` wrappers removed.
- **Buffer/UB fixes**: `sys_slurp_file` off-by-one and unchecked `fread`,
  `rsplit` buffer overrun, `cname_decode` leak.
- **Tooling**: `-Wall -Wextra` with bug-finding warnings promoted to errors;
  new `EXTEMPORE_SANITIZE` build option for ASan/UBSan/TSan; `.clang-format`
  and `.clang-tidy` configs (LLVM-based, no mass reformat).
- **Headless audio**: new `--audio-outfile` offline file driver renders DSP to
  WAV without an OS audio device, enabling audio-assertion tests in CI.
- **Docs**: xtlang tutorial and error-message glossary; new-user onboarding
  overhaul.

## v0.8.9

- bugfix for `@rpath/libportaudio.dylib` bug (introduced in 0.8.8)

## v0.8.8

- upgrade PortAudio (to fix a build error on macOS Big Sur)
- refactoring of LLVM stuff (in preparation for the LLVM 11 upgrade from nicdonaldson)
- improvements to the GitHub actinos CI test matrix

## v0.8.7

### Added

- more detailed build instructions (including `BUILDING.md`)

### Removed

- removed support for ogg/flac in libsndfile (since the current binary release
  is broken on platforms without e.g. libvorbis). In the future we should
  provide a CMake option to allow users to control this behaviour, but it's
  turned off in 0.8.7 because it's a pretty bad experience for novice users to
  have it Extempore crash inscrutably anytime libsndfile is loaded

### Changed

- some changes to the testing infrastructure (thanks @pcawley)

## v0.8.4

### Added

### Removed

### Changed

- A bunch of refactoring in `libs/core/instruments-scm.xtm`, mostly around
  naming consistency but also in the way that the optional "parser" function
  argument to `load-sampler` (which maps filenames to sample slots). In
  particular, the parser function used to have to map a list of filenames to a
  list of "parsed" slots/banks, but now just maps a single filename to a single
  slot/bank (see [the
  docs](https://extemporelang.github.io/docs/guides/sampler/), which have been
  updated to reflect the new behaviour). In general if you had some existing
  sample loading code (pre v0.8.4) and it's now broken then have a look to see
  if that's the problem.

## v0.8

### Added

- Extempore pattern language
- analogue synth
- ability to build/export an extempore "app" as a standalone binary

### Removed

- out of date guff, including long-deprecated and non-working AOT
  compilation scripts, the old `extempore.el` (just use MELPA)

### Changed

- changed the way that AOT-compilation works - no longer generating the LLVM IR
  then shelling out to `llc` to compile them at build time to produce a dll, but
  instead writing the LLVM IR out to `*.ll` files in `libs/aot-cache/` and
  loading them on demand

- assets bundle updated, moved to GH (rather than just being a tarball on
  <https://moso.com.au>)

## v0.60

### Major changes

- add xtlang "compiler cache" (see runtime/llvmti.xtm)
- add MCJIT support (-DEXT_MCJIT), on by default
- update LLVM to 3.7
- remove OpenGL bindings from extempore binary (use glfw for context management
  instead)
- refactor graphics pipeline - gl.xtm and gl-compatibility.xtm replace
  opengl.xtm, graphics-pipeline.xtm replaces shaders.xtm
- add cmake build/install option
- replace --runtime option with --sharedir, which should point to the top-level
  extempore directory (--runtime is kept in as an alias for --sharedir, although
  it points one level higher than it did previously)

### Minor changes

- rename code.ir to init.ll
- add docstrings for bind-{val,poly,type,alias}
- improved inline documentation: rich docstrings
- change bind-val for number types to automatically wrap 'value' expressions in
  a call-as-xtlang
- change AOT-compilation artefact location to libs/aot-cache
- add portmidi lib bindings
- add portaudio lib bindings
- add nanovg lib bindings for hardware-accelerated 2D graphics, deprecate
  shivavg
- change auto-compiled type dataconstructors: now, the default zalloc
  constructor is TypeName_z, which is bind-poly'd to TypeName
- add bind-dylib macro, which automatically sets the variable
  *impc:aot:current-load-dylib-info*
- bring PCRE in-tree, remove libpcre.a as a static dependency
