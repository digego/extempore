# Extempore

[![Build & test](https://github.com/digego/extempore/actions/workflows/build-and-test.yml/badge.svg?branch=master)](https://github.com/digego/extempore/actions/workflows/build-and-test.yml)
[![Sanitizers](https://github.com/digego/extempore/actions/workflows/sanitizers.yml/badge.svg?branch=master)](https://github.com/digego/extempore/actions/workflows/sanitizers.yml)
[![Release](https://github.com/digego/extempore/actions/workflows/release-binary.yml/badge.svg)](https://github.com/digego/extempore/actions/workflows/release-binary.yml)

Extempore is a live-coding environment for music, audio and graphics. It pairs
a Scheme interpreter with _xtlang_---a statically-typed lisp that compiles to
LLVM IR at runtime---so you can reshape a running program while it keeps
making sound. Runs on Linux (x86_64 and aarch64), macOS (Apple Silicon) and
Windows (x86_64).

## What's new in v0.9.0

v0.9.0 is a large release. The highlights:

- **LLVM 22** --- bumped from LLVM 17, fetched and built in-tree via CMake's
  `FetchContent` (no separate LLVM install required).
- **ORC JIT** --- replaces the legacy MCJIT, including DSP hot-swap and module
  tracking.
- **s7 Scheme** --- replaces TinyScheme as the Scheme interpreter.
- **WebGPU graphics** --- the OpenGL stack has been replaced with WebGPU via
  `wgpu-native`. Enable with `-DEXTERNAL_SHLIBS_GRAPHICS=ON`.
- **Linux aarch64** --- first-class support, alongside macOS arm64, Linux x64
  and Windows x64. All four platforms are tested in CI.
- **Interactive REPL** --- pass `--repl` for a linenoise-based REPL (Linux and
  macOS only).

v0.9.0 reworks a lot under the hood, so expect some rough edges. The old
OpenGL-based graphics stack will not work, and isn't likely to anytime soon ---
although a couple of new minimal WebGPU examples may suggest a way forward. If
you hit core audio breakage, please
[file an issue](https://github.com/digego/extempore/issues) with a minimal
reproducible example where possible.

## Getting started

### The easy way

Download the latest
[binary release](https://github.com/digego/extempore/releases) for your
platform, unzip it and run `extempore` (`extempore.exe` on Windows) from inside
the `extempore` folder.

Then,
[set up your text editor of choice](https://extemporelang.github.io/docs/guides/editor-support/)
and away you go.

_Note_: the VSCode extension used to offer an _Extempore: Download binary_
command for one-click setup. It hasn't been updated for v0.9.0 and may not work
--- downloading the release manually is the safest option for now.

_Note on macOS first launch_: the binaries aren't signed with an Apple
Developer ID, so on first launch macOS will refuse to run them ("apple could
not verify ... is free of malware"). Despite the wording, this means Apple
hasn't _checked_ the binary, not that anything malicious has been _detected_
--- the release is built in public from the source in this repository by the
[Release workflow](https://github.com/digego/extempore/actions/workflows/release-binary.yml),
and you can rebuild it yourself from source if you'd prefer (see below).
Signing with a Developer ID requires a paid Apple Developer account, which
Extempore (as a research project) doesn't currently maintain.

To clear the quarantine flag on the whole unzipped folder in one go, run:

    xattr -dr com.apple.quarantine /path/to/extempore

(replace the path with wherever you unzipped it, e.g. `~/Downloads/extempore`).
After that, `extempore` will launch normally.

### Build from source

**For more information**, check out [BUILDING.md](./BUILDING.md).

Extempore's CMake build process downloads and builds all the dependencies you
need (including LLVM). So, if you've got a C++ compiler, git, CMake >= 3.28 and
Ninja, here are some one-liner build commands.

On **Linux/macOS**:

    git clone https://github.com/digego/extempore && cmake -S extempore -B extempore/build -G Ninja -DASSETS=ON && cmake --build extempore/build -j$(nproc)

On **Windows** (adjust the generator for your VS version):

    git clone https://github.com/digego/extempore && cmake -S extempore -B extempore/build -G "Visual Studio 17 2022" -A x64 -DASSETS=ON && cmake --build extempore/build --config Release

_Note on build time_: the first build takes ~10-30 minutes because LLVM is
compiled from source. Subsequent builds reuse the cached LLVM artifacts under
`build/_deps/`.

_Note on ASSETS_: the `ASSETS` build-time option (boolean, default `OFF`) is set
to `ON` above. This will download the Extempore binary assets --- required for
many of the examples, but adds a ~250MB download to the build process. If you'd
rather not do that, and are happy with some of the examples not working, then
set `-DASSETS=OFF` instead.

_Note on running_: the `extempore` binary locates its runtime files
(`runtime/`, `libs/`, `examples/`) relative to the source tree at build time.
Run it from the build directory (`./extempore`) rather than installing it to a
system location.

### Hear your first sine wave

With Extempore running and an editor connected to port 7099, evaluate this
and you should hear a 440 Hz tone:

```scheme
(bind-func dsp:DSP
  (lambda (in time chan data)
    (* 0.5 (sin (* STWOPI 440.0 (/ (i64tof time) SRf))))))
(dsp:set! dsp)
```

The full example lives at
[`examples/core/hello-sine.xtm`](./examples/core/hello-sine.xtm).

## See Extempore in action

Check out these videos:

- [The Concert Programmer](https://www.youtube.com/watch?v=yY1FSsUV-8c)
- [Interactive, distributed, physics simulation](https://vimeo.com/126577281)
- [Programming in Time](https://www.youtube.com/watch?v=Sg2BjFQnr9s)
- [The Physics Playroom - interactive installation](https://vimeo.com/58239256)
- [An _old_ Graphics Demo](https://vimeo.com/37293927)
- [A Programmer's Guide to Western Music](https://www.youtube.com/watch?v=xpSYWd_aIiI)
- [Ben's livecoding gig videos](https://benswift.me/livecoding/)

## Contributors

The Extempore core team is [Andrew Sorensen](https://github.com/digego) &
[Ben Swift](https://github.com/benswift). [Jim Kuhn](https://github.com/JimKuhn)
contributed significant performance improvements, which are not reflected in the
commit logs, but for which we are extremely grateful. Many others have
contributed to Extempore's development
([see the full list](https://github.com/digego/extempore/graphs/contributors)).

[Ben Swift](https://github.com/benswift) is the current maintainer --- please
direct issues, questions and emails his way.

## Docs & Community

Extempore documentation can be found at <https://extemporelang.github.io/docs/>

You can also join the Extempore community:

- [Extempore Google group/mailing list](http://groups.google.com/group/extemporelang)
- [#extempore on chat.toplap.org](https://chat.toplap.org/home) (although not as
  well-monitored as the mailing list)

## Cite Extempore

- [Extempore: The design, implementation and application of a cyber-physical programming language](https://openresearch-repository.anu.edu.au/handle/1885/144603)
- [Systems level liveness with extempore](https://dl.acm.org/citation.cfm?id=3133858)

## Licence

Copyright (c) 2011-2026, Andrew Sorensen

All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

Neither the name of the authors nor other contributors may be used to endorse or
promote products derived from this software without specific prior written
permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
