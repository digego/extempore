# Extempore changelog

First, a confession: the Extempore maintainers (i.e. Andrew & Ben) have been
really bad at keeping a changelog. But hopefully we'll be better in the future.

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
