# Build-from-source notes

If you run into problems with the one-line build commands listed in the
`README.md`, or if just want to understand how Extempore's build process works
in more depth, here's some more information.

## Build-time deps

- a C++23 compiler (GCC 13 or later, a recent Clang or Apple Clang, or MSVC from
  Visual Studio 2022 or later)
- Git
- CMake >= 4.0
- Ninja (every preset uses it, on all three platforms)
- Python >= 3.8 (for LLVM)

`mise.toml` pins CMake, Ninja and Python; `mise install` fetches them.

## Presets

`CMakePresets.json` carries the configurations CI uses, so you can build the
same thing locally:

    cmake --preset default          # Ninja, Release, tests on --- builds into build/
    cmake --build build
    ctest --preset default          # the test suites CI runs

or all three at once with `cmake --workflow --preset default`.

| Preset                                                                    | What it configures                                                                            |
| ------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------- |
| `default`, `ci`                                                           | Ninja, `Release`, `BUILD_TESTS=ON`                                                            |
| `release`                                                                 | as above plus `ASSETS=ON`, `BUILD_TESTS=OFF`, `EXT_SHARE_DIR=.`                               |
| `sanitize-asan`, `sanitize-ubsan`, `sanitize-tsan`, `sanitize-asan-ubsan` | as `default` plus the matching `EXTEMPORE_SANITIZE` value, built into `build-asan/` and so on |

Each configure preset has a build and test preset of the same name; the
sanitizer test presets run the suites that fit the instrumentation's slowdown,
with the sanitizer options CI uses. Add `-D` options to the `cmake --preset`
line as usual.

## Build options

See the top of `CMakeLists.txt` for all the available options. The ones most
users will care about:

- `ASSETS` (default `OFF`) --- download the multimedia assets (audio samples,
  impulse responses) needed to run many of the examples. ~250MB download. If you
  don't set `-DASSETS=ON` at configure time, CMake still creates an `assets`
  target you can build afterwards.
- `BUILD_TESTS` (default `ON`) --- build the test targets (including examples
  registered as ctest tests).
- `EXTERNAL_SHLIBS_GRAPHICS` (default `OFF`) --- build the WebGPU graphics stack
  (glfw, wgpu-native). Required for the WebGPU examples.
- `JACK` (default `OFF`) --- use the Jack PortAudio backend on Linux instead of
  ALSA.
- `EXTEMPORE_CCACHE` (default `ON`) --- route compilation through `ccache` when
  it's installed. The sub-builds (LLVM, portaudio, ...) inherit it, so a second
  build directory costs minutes rather than half an hour.
- `EXTEMPORE_SANITIZE` (default empty) --- build Extempore's own objects with a
  sanitizer: `asan`, `ubsan`, `tsan` or `asan+ubsan`. Anything else is a
  configure error. Unix only.
- `EXT_SHARE_DIR` (default: the source tree) --- the location of `runtime/`,
  `libs/` and `examples/`, baked in at compile time. It is consulted only when
  the binary isn't in an installed tree (see [Installing](#installing)); the
  release builds set `.`.
- `EXTEMPORE_VERSION` (default: `git describe`) --- the version the binary
  reports. CI passes the tag explicitly because its checkout is shallow.

## LLVM

Extempore links against a specific version of LLVM (currently 23.1.6, pinned in
`CMakeLists.txt`). LLVM is fetched and built in-tree via CMake's
`FetchContent` --- no system LLVM required.

The first build takes ~10-30 min because LLVM is compiled from source.
Subsequent builds reuse the cached artifacts under `build/_deps/llvm-*`. Only
the components Extempore needs are built (OrcJIT, target codegen, AsmParser,
Passes, MCDisassembler, IRPrinter).

CI caches `build/_deps/` across runs --- see
`.github/workflows/build-and-test.yml`.

## PCRE2

The `regex:*` builtins are backed by PCRE2 (currently 10.49, pinned in
`CMakeLists.txt`), fetched and built in-tree via `FetchContent` alongside
LLVM --- no system PCRE required. Only the static 8-bit library is built and
it's linked into the binary, so there's nothing extra to ship.

The xtlang compiler's type-string grammars use recursive subpatterns and named
groups, so a PCRE-compatible engine is a hard requirement rather than a
convenience.

## Targets

The default target builds Extempore, all the dependencies, and AOT-compiles the
standard library (for faster startup). Other targets worth knowing about:

- `aot_core` --- AOT-compile just the core standard library (pure-xtlang
  libraries with no external C library dependencies).
- `clean_aot` --- remove all AOT-compiled files.
- `assets` --- download and unpack the assets tarball.

## Installing

`cmake --install build --prefix <dir>` lays out an installed tree, the same as
the release archives hold:

    <dir>/bin/extempore
    <dir>/share/extempore/      runtime/, libs/, examples/ and (if downloaded) assets/
    <dir>/lib/extempore/        the AOT cache and the shared libraries xtlang binds
    <dir>/share/doc/extempore/  README.md, CHANGELOG.md

So `--prefix ~/.local` puts `extempore` on the usual per-user `PATH`, and
`--prefix /usr/local` installs it for everyone. The tree can be moved anywhere:
the binary finds the rest from its own path, after resolving symlinks, so a link
to `bin/extempore` from anywhere on your `PATH` works too.

An installed tree needn't be writable. AOT compilation writes to a per-user
cache directory instead, versioned so each release keeps its own:
`$XDG_CACHE_HOME/extempore/<version>` (or `~/.cache/...`) on Linux,
`~/Library/Caches/extempore/<version>` on macOS and
`%LOCALAPPDATA%\extempore\<version>` on Windows. Set `EXTEMPORE_CACHE_DIR` to
use another. What's compiled there shadows the installed AOT cache.

## Running Extempore

An in-tree build runs against the source tree it was built from, so run it in
place:

    build/extempore                 # audio + Scheme interpreter, listens on port 7099
    build/extempore --no-audio      # same, without audio
    build/extempore --repl          # interactive linenoise REPL (Linux/macOS only)
    build/extempore exec -e "(println 'hello)"   # run code and exit

What the build generates --- the AOT cache (`aot-cache/`) and the shared
libraries xtlang binds (`platform-shlibs/`) --- goes in `build/libs/`, which is
both the lib directory and the cache directory of an in-tree build. In xtlang,
`(sys:share-dir)`, `(sys:lib-dir)` and `(sys:cache-dir)` name the three, and
`(sys:aot-cache-path name)` finds a file in the AOT cache.

To use a share directory other than the one the binary finds --- a build run
against an installed tree's sources, say --- name it explicitly:

    build/extempore --share-dir /path/to/share/extempore

## Platform-specific notes

### macOS

Extempore's macOS builds target arm64 (Apple Silicon) with a minimum deployment
target of macOS 13.3 Ventura. CI builds and tests on macOS 15 and macOS 26.

Apple requires distributed binaries to be signed & notarised, and the Extempore
core team haven't got an Apple Developer account set up for that. The install
script and the VS Code extension download releases without the quarantine flag
that browsers set, so Gatekeeper doesn't stop them. If it refuses to run an
archive you downloaded with a browser, strip the flag and try again:

    xattr -dr com.apple.quarantine /path/to/the/unzipped/folder

If that doesn't help, reach out on the
[mailing list](mailto:extemporelang@googlegroups.com).

### Linux

Extempore is built & tested on Ubuntu 24.04 (x86_64 and aarch64) in CI.

You'll need the ALSA dev headers for PortAudio. On Ubuntu:

    sudo apt-get install libasound2-dev

For the WebGPU graphics build (`-DEXTERNAL_SHLIBS_GRAPHICS=ON`) you'll also
need:

    sudo apt-get install xorg-dev

### Windows

Extempore is built & tested on Windows Server 2025 with Visual Studio. If you
don't already have VS installed, download the free
[Visual Studio Community](https://visualstudio.microsoft.com/vs/community/).

CI builds Windows with Ninja from a Visual Studio developer prompt, which is
what the presets assume. To use the VS generator instead, skip the presets and
configure with `-G "Visual Studio 17 2022" -A x64`.
