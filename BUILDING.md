# Build-from-source notes

If you run into problems with the one-line build commands listed in the
`README.md`, or if just want to understand how Extempore's build process works
in more depth, here's some more information.

## Build-time deps

- a C++20 compiler (recent `clang` or `gcc`; MSVC from Visual Studio 2022)
- Git
- CMake >= 3.28
- Ninja (recommended on Linux/macOS)
- Python >= 3.8 (for LLVM)

## Build options

See the top of `CMakeLists.txt` for all the available options. The ones most
users will care about:

- `ASSETS` (default `OFF`) --- download the multimedia assets (audio samples,
  impulse responses) needed to run many of the examples. ~250MB download. If
  you don't set `-DASSETS=ON` at configure time, CMake still creates an
  `assets` target you can build afterwards.
- `BUILD_TESTS` (default `ON`) --- build the test targets (including examples
  registered as ctest tests).
- `EXTERNAL_SHLIBS_AUDIO` (default `ON`) --- build the audio dependencies
  (portaudio, portmidi, sndfile, kissfft).
- `EXTERNAL_SHLIBS_GRAPHICS` (default `OFF`) --- build the WebGPU graphics
  stack (glfw, wgpu-native, stb_image). Required for the WebGPU examples.
- `JACK` (default `OFF`) --- use the Jack PortAudio backend on Linux instead
  of ALSA.

## LLVM

Extempore links against a specific version of LLVM (currently 22.1.6, pinned in
`CMakeLists.txt`). LLVM is fetched and built in-tree via CMake's `FetchContent`
--- no system LLVM required.

The first build takes ~10-30 min because LLVM is compiled from source.
Subsequent builds reuse the cached artifacts under `build/_deps/llvm-*`. Only
the components Extempore needs are built (OrcJIT, target codegen, AsmParser,
Passes, MCDisassembler, IRPrinter).

CI caches `build/_deps/` across runs --- see `.github/workflows/build-and-test.yml`.

## Targets

The default target builds Extempore, all the dependencies, and AOT-compiles the
standard library (for faster startup). Other targets worth knowing about:

- `aot_core` --- AOT-compile just the core standard library (pure-xtlang
  libraries with no external C library dependencies).
- `aot_external_audio` --- AOT-compile the external audio libraries (portmidi,
  sndfile, fft, etc). This is the default AOT target.
- `clean_aot` --- remove all AOT-compiled files.
- `assets` --- download and unpack the assets tarball.

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

For the WebGPU graphics build (`-DEXTERNAL_SHLIBS_GRAPHICS=ON`) you'll also need:

    sudo apt-get install xorg-dev libglu1-mesa-dev

### Windows

Extempore is built & tested on Windows Server 2022 with Visual Studio 2022. If
you don't already have VS installed, download the free
[Visual Studio Community](https://visualstudio.microsoft.com/vs/community/).

The CMake generator for VS2022 is `-G "Visual Studio 17 2022" -A x64`.

#### Missing `VCRUNTIME140_1.dll`

If you see _VCRUNTIME140_1.dll was not found_, install the x64
`vc_redist.x64.exe` from the
[official Microsoft page](https://learn.microsoft.com/en-us/cpp/windows/latest-supported-vc-redist).
