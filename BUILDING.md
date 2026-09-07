# Build-from-source notes

If you run into problems with the one-line build commands listed in the
`README.md`, or if just want to understand how Extempore's build process works
in more depth, here's some more information.

## Build-time deps

- a C++20 compiler (recent `clang` or `gcc`; MSVC from Visual Studio 2022)
- Git
- CMake >= 3.28
- Ninja (every preset uses it, on all three platforms)
- Python >= 3.8 (for LLVM)

## Presets

`CMakePresets.json` carries the configurations CI uses, so you can build the
same thing locally:

    cmake --preset default          # Ninja, Release, tests on --- builds into build/
    cmake --build build

| Preset                                                                    | What it configures                                              |
| ------------------------------------------------------------------------- | --------------------------------------------------------------- |
| `default`, `ci`                                                           | Ninja, `Release`, `BUILD_TESTS=ON`                              |
| `release`                                                                 | as above plus `ASSETS=ON`, `BUILD_TESTS=OFF`, `EXT_SHARE_DIR=.` |
| `sanitize-asan`, `sanitize-ubsan`, `sanitize-tsan`, `sanitize-asan-ubsan` | as `default` plus the matching `EXTEMPORE_SANITIZE` value       |

Presets are a starting point, not a straitjacket --- add `-D` options to the
`cmake --preset` line as usual.

## Build options

See the top of `CMakeLists.txt` for all the available options. The ones most
users will care about:

- `ASSETS` (default `OFF`) --- download the multimedia assets (audio samples,
  impulse responses) needed to run many of the examples. ~250MB download. If you
  don't set `-DASSETS=ON` at configure time, CMake still creates an `assets`
  target you can build afterwards.
- `BUILD_TESTS` (default `ON`) --- build the test targets (including examples
  registered as ctest tests).
- `EXTERNAL_SHLIBS_AUDIO` (default `ON`) --- build the audio dependencies
  (portaudio, portmidi, sndfile, kissfft).
- `EXTERNAL_SHLIBS_GRAPHICS` (default `OFF`) --- build the WebGPU graphics stack
  (glfw, wgpu-native, stb_image). Required for the WebGPU examples.
- `JACK` (default `OFF`) --- use the Jack PortAudio backend on Linux instead of
  ALSA.
- `EXTEMPORE_CCACHE` (default `ON`) --- route compilation through `ccache` when
  it's installed. The sub-builds (LLVM, portaudio, sndfile, ...) inherit it, so
  a second build directory costs minutes rather than half an hour.
- `EXTEMPORE_SANITIZE` (default empty) --- build Extempore's own objects with a
  sanitizer: `asan`, `ubsan`, `tsan` or `asan+ubsan`. Anything else is a
  configure error. Unix only.
- `EXT_SHARE_DIR` (default: the source tree) --- where the binary looks for
  `runtime/`, `libs/` and `examples/`, baked in at compile time. The release
  builds set `.`, i.e. the working directory. `--sharedir` overrides it at
  runtime.
- `EXTEMPORE_VERSION` (default: `git describe`) --- the version the binary
  reports. CI passes the tag explicitly because its checkout is shallow.

## LLVM

Extempore links against a specific version of LLVM (currently 22.1.6, pinned in
`CMakeLists.txt`). LLVM is fetched and built in-tree via CMake's `FetchContent`
--- no system LLVM required.

The first build takes ~10-30 min because LLVM is compiled from source.
Subsequent builds reuse the cached artifacts under `build/_deps/llvm-*`. Only
the components Extempore needs are built (OrcJIT, target codegen, AsmParser,
Passes, MCDisassembler, IRPrinter).

CI caches `build/_deps/` across runs --- see
`.github/workflows/build-and-test.yml`.

## Targets

The default target builds Extempore, all the dependencies, and AOT-compiles the
standard library (for faster startup). Other targets worth knowing about:

- `aot_core` --- AOT-compile just the core standard library (pure-xtlang
  libraries with no external C library dependencies).
- `aot_external_audio` --- AOT-compile the external audio libraries (portmidi,
  sndfile, fft, etc). This is the default AOT target.
- `clean_aot` --- remove all AOT-compiled files.
- `assets` --- download and unpack the assets tarball.

## Installing

`cmake --install build --prefix <dir>` lays out a self-contained tree in
`<dir>`: the `extempore` binary with `runtime/`, `libs/` (AOT cache and platform
shared libraries included), `examples/` and, if they were downloaded, `assets/`
beside it. That's the layout of the binary release archives, so a build
configured with the `release` preset (which sets `EXT_SHARE_DIR=.`) runs from
the install directory as-is. A binary built with the default share dir needs
`--sharedir <dir>` to use an installed tree.

## Running Extempore

The `extempore` binary locates its share directory (`runtime/`, `libs/`,
`examples/`) relative to the source tree at build time. Run it from the build
directory:

    ./extempore                 # audio + Scheme interpreter, listens on port 7099
    ./extempore --noaudio       # same, without audio
    ./extempore --repl          # interactive linenoise REPL (Linux/macOS only)
    ./extempore --batch "(begin (println 'hello) (quit 0))"

If you've moved the `extempore` binary away from the source tree (for example,
out of a downloaded release archive), point it at the share directory
explicitly:

    ./extempore --sharedir /path/to/extempore

## Platform-specific notes

### macOS

Extempore's macOS builds target arm64 (Apple Silicon) with a minimum deployment
target of macOS 11.0 Big Sur.

Apple requires distributed binaries to be signed & notarised, and the Extempore
core team haven't got an Apple Developer account set up for that. If Gatekeeper
refuses to run the `extempore` binary from a downloaded release, strip the
quarantine attribute and try again:

    xattr -dr com.apple.quarantine /path/to/extempore

If that doesn't help, reach out on the
[mailing list](mailto:extemporelang@googlegroups.com).

### Linux

Extempore is built & tested on Ubuntu 24.04 (x86_64 and aarch64) in CI.

You'll need the ALSA dev headers for PortAudio. On Ubuntu:

    sudo apt-get install libasound2-dev

For the WebGPU graphics build (`-DEXTERNAL_SHLIBS_GRAPHICS=ON`) you'll also
need:

    sudo apt-get install xorg-dev libglu1-mesa-dev

### Windows

Extempore is built & tested on Windows Server 2022 with Visual Studio 2022. If
you don't already have VS installed, download the free
[Visual Studio Community](https://visualstudio.microsoft.com/vs/community/).

CI builds Windows with Ninja from a Visual Studio developer prompt, which is
what the presets assume. To use the VS generator instead, skip the presets and
configure with `-G "Visual Studio 17 2022" -A x64`.

#### Missing `VCRUNTIME140_1.dll`

If you see _VCRUNTIME140_1.dll was not found_, install the x64
`vc_redist.x64.exe` from the
[official Microsoft page](https://learn.microsoft.com/en-us/cpp/windows/latest-supported-vc-redist).
