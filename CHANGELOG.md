# Extempore changelog

First, a confession: the Extempore maintainers (i.e. Andrew & Ben) have been
really bad at keeping a changelog. But hopefully we'll be better in the future.

## v0.10.2

A one-bug patch release. Error messages piled up across a working session: each
new error redisplayed fragments of the earlier ones, so a session with a few
mistakes in it grew into a wall of text --- and the s7 stack quietly grew with
it. The cause was that neither the interpreter's top-level loader
(`s7_load_c_string`, which runs editor evals) nor `s7_call` (which runs a
scheduled callback) unwinds the s7 stack after an uncaught error: the error
long-jump skips the stack-reset step, so every erroring eval or callback left a
handful of frames behind. s7 builds the stacktrace it appends to an error report
by walking that stack, so each new error dragged along the leftovers from all
the earlier ones. On the callback path --- a temporal-recursion loop with a bug
in it, firing every beat --- it grew fastest of all.

Both paths now go through a small Scheme wrapper (`sys:eval-string` for editor
input, `sys:apply-top-level` for callbacks) that runs each form or callback
inside s7's own catch. The catch unwinds the stack cleanly and reports the error
itself --- the same terse s7-style "message, offending form, location" report,
minus the accumulating stacktrace. Loading a file is unchanged: it keeps the C
loader, so an error there still carries the line number within the file.
Reported on the mailing list by George.

## v0.10.1

A one-fix patch release. The editor's documentation popup --- the `xtmdoc`
machinery that answers a hover or doc lookup --- crashed whenever the cursor
landed on a Scheme macro whose template was wrapped in a `let`, which covers
`cosr`, `dotill` and most user-defined macros of that shape. The handler still
pulled the macro's argument list out the way TinyScheme laid out a macro's
source; under the s7 reader (in since 0.9.4) `procedure-source` returns
`(macro <arglist> body...)`, so the old walk dived into the body and errored. It
now reads the parameter list directly. Macros with a bare quasiquote body never
hit the crash but were silently handed the wrong argument list; those are
correct now too. Reported on the mailing list by Minoru.

## v0.10.0

Two large strands of work land together: the xtlang compiler now drives every
compilation through a single constraint-based type-inference engine, and the C++
runtime gets a broad modernisation. None of it changes what Extempore does ---
every test suite stays green on Linux (x64 and arm64), macOS (arm64) and Windows
--- but the runtime now needs a C++20 compiler (GCC 13+, a recent Clang, or MSVC
from Visual Studio 2022), and a good deal of dead code, platform `#ifdef`s and
latent bugs went out with the tide. There are also two xtlang-visible renames
(see below); one keeps backwards-compatible aliases for this release, the other
only affects code that reached into compiler internals.

### Compiler and type system

- **Unified type inference**: xtlang had two type unifiers --- the old
  candidate-list traversal with its enumeration retry loop, and the newer
  canonical type-term core --- and they could disagree. Inference now runs
  entirely through the canonical core (`xtc:typecheck:` over the `xtc:solve:`
  constraint solver): it produces every function's types, and the old
  candidate-list checker and its retry machinery have been deleted. The change
  is behaviour-preserving --- the whole test suite stays green across all four
  platforms --- but it is the largest internal change in the release, so it is
  worth knowing where new type errors would be coming from if you hit one.
  Overload resolution is now near-linear rather than re-enumerating candidates,
  which removes a compile-time blow-up on heavily overloaded call sites, and
  conflicting constraints now report a clearer diagnostic.

- **Generic type variables (#315)**: binding a generic type variable to a tuple
  field lost its pointer depth, so a `i8**` field could be reified as `i8*` and
  miscompile. Pointer depth is now carried structurally through type-variable
  reification, both for function type variables and for data-constructor return
  types.

- **Void-return codegen**: a `void`-returning function whose body ended in a
  value-yielding tail expression emitted invalid LLVM IR; it now correctly emits
  `ret void`.

### xtlang API changes

- **`imp_rand*` RNG builtins renamed to `xtc_rand*`**: the random-number
  builtins (`imp_randf`/`imp_randd`, `imp_rand1_*`/`imp_rand2_*` over
  `f`/`d`/`i32`/`i64`) lose their Impromptu-era prefix. They cross the C++/ABI
  boundary and are a public xtlang API, so the old `imp_rand*` spellings keep
  working as **deprecated aliases** for this release (resolved at desugar time)
  and will be removed in the next one --- existing code that calls them directly
  still compiles, but should be updated.

- **Compiler internals re-namespaced `impc:*` to `xtc:*`**: the compiler's own
  symbols moved from the Impromptu-era `impc:` prefix to `xtc:` (extempore
  compiler), regrouped by compilation phase (`xtc:codegen:`, `xtc:desugar:`,
  `xtc:typecheck:`, `xtc:bind:`, `xtc:driver:`, `xtc:type:`, `xtc:aot:`,
  `xtc:diagnostics:`, `xtc:cache:`, `xtc:globals:`). This is
  behaviour-preserving and affects only code that referenced compiler internals
  directly --- ordinary xtlang and Scheme user code is unchanged.

### Networking

- **OSC parser replaced with oscpp**: the hand-rolled OSC receive parser was
  memory-unsafe on malformed input --- fuzzing found single-packet remote
  crashes (a wild bundle size-jump, an out-of-range abort on a malformed string)
  and a family of over-reads, all reachable from one UDP datagram. Incoming
  packets are now parsed with the vendored, bounds-checked `oscpp` reader (Boost
  licence) inside a single try/catch; a malformed or hostile packet is dropped
  rather than crashing. The xtlang `io:osc` API is unchanged --- addresses, the
  `i/f/d/s/h/t` arg types, `[`/`]` lists, the native-handler path and the NTP
  timetag conversion are all preserved. A libFuzzer harness finds zero crashes
  across the 1713 inputs that crashed the old parser.

### Toolchain and build

- **LLVM 22.1.6**: bumped from 22.1.1, verified green on all CI platforms.
- **PortAudio** updated to a current master snapshot.
- **ccache** is auto-detected and used as the compiler launcher when present,
  making repeat builds much faster.
- **Versioning**: the git tag is now the single source of truth for the version.
  CMake derives it via `git describe` at configure time (CI passes it explicitly
  for its shallow checkout; git-less source builds fall back to `unknown`). A
  running Extempore can finally report itself --- a new `--version` flag and the
  version in the startup banner.
- **`release.sh`**: a new script to cut a release --- it validates the branch,
  working tree and CHANGELOG section, then creates the annotated tag (it
  deliberately does not push; pushing the tag is what publishes the release).

### Testing

- **One test framework**: the harness was duplicated in `runtime/scheme.xtm` and
  `libs/core/test.xtm` and the copies had drifted; `test.xtm` is now the single
  source of truth, rewritten around a flat, append-only result list with a clear
  outcome vocabulary (pass / fail / runtime-error / compile-error / compile-ok /
  fixture-error). The retired `xtmtest` idiom is gone and the suite uses
  single-idiom assertion macros.
- **Auto-discovered tests**: CMake now globs `tests/core` and `tests/compiler`
  rather than carrying a hand-maintained list, so new tests are picked up
  automatically; each run emits a JUnit XML report for per-assertion failures in
  ctest and CI, and the known-broken quarantine (`failing.xtm`) runs as an
  expected failure so it stays green until a fix flips it.

### C++ runtime

A broad modernisation of the C++ runtime; none of it changes what Extempore
does.

- **C++20**: the required standard moves from C++17 to C++20, which lets a few
  hand-rolled stand-ins retire to the standard library. `MtSemaphore` --- whose
  own comment admitted it was a placeholder for `std::counting_semaphore` ---
  becomes the real thing; the eight OSC byte-swap helpers shed ~130 lines of
  `unsigned char*` aliasing for `std::endian` and `std::bit_cast`, and are now
  correct on a big-endian host rather than merely lucky on a little-endian one;
  and an 80-bit-float-to-`double` union pun becomes a `std::bit_cast`. The bump
  also smoked out a genuine latent bug --- `llvm_destroy_zone_after_delay` was
  declared and defined with inconsistent dll-linkage, which MSVC waved through
  under C++17 and rejects under C++20.

- **Correctness**: `getRealTime` on Windows had been reading a boot-relative
  clock (`high_resolution_clock`) and handing it to the cross-machine clock-sync
  code as though it were wall-clock time; it now comes from
  `std::chrono::system_clock` on every platform. Memory zones on Windows were
  quietly under-aligned --- plain `malloc` rather than the promised 32 bytes,
  because an earlier `_aligned_malloc` had been paired with a plain `free` ---
  and are now aligned and released correctly. Several fields shared across
  threads with no synchronisation at all (the scheduler's clock, the run-state
  flags) became `std::atomic`, and a clutch of quieter bugs --- an inverted
  `strcmp`, a `do/while` that dereferenced before its null check, a zone size
  truncated to `int`, a GC-unprotected temporary --- are fixed.

- **Cleanup**: out went a pile of dead weight --- unreachable
  conditional-compilation branches, orphaned helpers, leftover TinyScheme-era
  shims, and the whole vestigial "run a scheduler on thread 0" mechanism
  (`SUBSUME_PRIMARY`, `XTMMainCallback`, the `sched_main` macro), none of it
  reachable since the primary process moved onto thread 0. `thread_kill`'s
  `pthread_cancel` --- undefined behaviour against C++ RAII, and a silent no-op
  on Windows --- gives way to a cooperative stop flag.

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
  operators stopped working in v0.9.0 --- evaluating e.g.
  `(:> A 4 0 (play syn1 @1 80 dur) (list 60 63 67 72))` raised
  `Unbound variable A`, as did every `examples/sharedsystem/pattern_basics.xtm`
  line. s7 (unlike the old TinyScheme reader) treats any symbol that begins or
  ends with a colon as a self-evaluating keyword constant, and a keyword can't
  be bound with `define-macro`, so the operator definitions silently failed and
  `(:> ...)` was read as an ordinary application. Fixed by narrowing s7's
  keyword rule (in our vendored `src/s7.c`) so a colon only marks a keyword when
  the character adjacent to it is a letter: genuine keywords (`:readable`,
  `foo:`, `define*` keyword args --- all alphabetic) are unaffected, while
  operator-style names like `:>` and `:|` stay ordinary, macro-bindable symbols.
  This is a deliberate extempore-local divergence from stock s7 --- we can't
  rename the operators without breaking pattern-language code already out in the
  wild, so we adapt the reader instead. New `tests/core/pattern-language.xtm`
  guards it. Reported on the mailing list by George.

## v0.9.2

Patch release fixing a multi-form REPL eval regression introduced with the s7
swap in v0.9.0.

- **Multi-form eval**: selecting and evaluating multiple top-level s-expressions
  in one go (over the eval socket or via `--batch`/`--eval`) previously ran only
  the first form and silently discarded the rest. The s7-backed
  `eval_with_error_trap` was using `s7_eval_c_string` (single-form
  read-and-eval) instead of `s7_load_c_string` (the `load` equivalent that
  iterates every top-level form). Reported on the mailing list by George.

## v0.9.1

Patch release fixing a critical packaging bug in the v0.9.0 binary release.

- **Release packaging**: the v0.9.0 binary release had `EXT_SHARE_DIR` hardcoded
  to the GitHub Actions runner's ephemeral source path, so downloaded binaries
  aborted with `could not locate file init.xtm` on every user's machine. The
  release workflow now passes `-DEXT_SHARE_DIR=.` so the binary locates
  `runtime/`, `libs/` etc. relative to the current working directory ---
  matching what the README already tells users to do (run `./extempore` from
  inside the unzipped folder).

  Workaround for anyone stuck on a v0.9.0 download: run
  `./extempore --sharedir .` from inside the extempore folder.

## v0.9.0

A major release --- the long-running `aarch64` branch (~185 commits) merged back
into master.

### Platforms

- Linux aarch64 and macOS arm64 (Apple Silicon) are now first-class targets in
  the CI matrix alongside Linux x86_64 and Windows x86_64.

### Toolchain

- LLVM upgraded from 17 to **22.1.1**. JIT migrated from the legacy JIT to **ORC
  JIT**.
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
  structured error handling. `sTypeDefinitions` string accumulator replaced with
  structured maps.
- New compiler unit test suite (`tests/compiler/`).

### Runtime

- New `--repl` flag: interactive linenoise-based REPL (Linux/macOS).
- DSP hot-swap fixed after the ORC JIT migration.
- Many C++ modernisations (`nullptr`, `std::vector`, `std::string_view`, C++17
  stdlib replacing platform ifdefs).

### Docs

- New VitePress documentation site under `docs/`.
- `extras/tree-sitter-extempore/` added as a submodule for editor tooling.

### Assets

- The assets tarball (`ASSETS=ON`) now comes from a GitHub release asset on
  `extemporelang/extempore-assets` (tag `v0.9.0`), ~250 MB of audio samples and
  impulse responses. Unused fonts, images, 3D models and the duplicate Christmas
  Carol sample have been trimmed out.

### Examples

- `examples/contrib/` removed (legacy graphics, unmaintained externals).
- Core/external example set slimmed to only those that load cleanly in CI.
- Full working set registered in `extras/cmake/tests.cmake` and tested on every
  push.

### Stability

Post-beta hardening before cutting the stable release:

- **Concurrency**: atomic `UNIV::TIME`/`DEVICE_TIME` and `m_libsLoaded`; race
  fixes in the s7 FFI table; `gethostbyname` replaced with thread-safe
  `getaddrinfo`; MT dispatcher spin-sleep swapped for a counting semaphore;
  audio threads converted to RAII with per-thread PRNG; legacy
  `EXTMutex`/`EXTMonitor`/`EXTCondition` wrappers removed.
- **Buffer/UB fixes**: `sys_slurp_file` off-by-one and unchecked `fread`,
  `rsplit` buffer overrun, `cname_decode` leak.
- **Tooling**: `-Wall -Wextra` with bug-finding warnings promoted to errors; new
  `EXTEMPORE_SANITIZE` build option for ASan/UBSan/TSan; `.clang-format` and
  `.clang-tidy` configs (LLVM-based, no mass reformat).
- **Headless audio**: new `--audio-outfile` offline file driver renders DSP to
  WAV without an OS audio device, enabling audio-assertion tests in CI.
- **Docs**: xtlang tutorial and error-message glossary; new-user onboarding
  overhaul.

## v0.8.9

- bugfix for `@rpath/libportaudio.dylib` bug (introduced in 0.8.8)

## v0.8.8

- upgrade PortAudio (to fix a build error on macOS Big Sur)
- refactoring of LLVM stuff (in preparation for the LLVM 11 upgrade from
  nicdonaldson)
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
  slot/bank (see
  [the docs](https://extemporelang.github.io/docs/guides/sampler/), which have
  been updated to reflect the new behaviour). In general if you had some
  existing sample loading code (pre v0.8.4) and it's now broken then have a look
  to see if that's the problem.

## v0.8

### Added

- Extempore pattern language
- analogue synth
- ability to build/export an extempore "app" as a standalone binary

### Removed

- out of date guff, including long-deprecated and non-working AOT compilation
  scripts, the old `extempore.el` (just use MELPA)

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
  _impc:aot:current-load-dylib-info_
- bring PCRE in-tree, remove libpcre.a as a static dependency
