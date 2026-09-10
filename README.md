# Extempore

[![Build & test](https://github.com/digego/extempore/actions/workflows/build-and-test.yml/badge.svg?branch=master)](https://github.com/digego/extempore/actions/workflows/build-and-test.yml)
[![Sanitizers](https://github.com/digego/extempore/actions/workflows/sanitizers.yml/badge.svg?branch=master)](https://github.com/digego/extempore/actions/workflows/sanitizers.yml)
[![Release](https://github.com/digego/extempore/actions/workflows/release-binary.yml/badge.svg)](https://github.com/digego/extempore/actions/workflows/release-binary.yml)

Extempore is a live-coding environment for music and audio. It pairs a Scheme
interpreter with _xtlang_---a statically-typed lisp that compiles to LLVM IR at
runtime---so you can reshape a running program while it keeps making sound. Runs
on Linux (x86_64 and aarch64), macOS (Apple Silicon) and Windows (x86_64).

## What's new in v0.11.0

v0.11.0 is a robustness release driven by a full-codebase review---nothing
changes about the language, but a lot changes underneath it.
[CHANGELOG.md](./CHANGELOG.md) has the detail; the headlines:

- **A security fix worth upgrading for** --- a running Extempore turned each
  incoming OSC message into Scheme source text with its address interpolated
  unescaped, so any host that could reach the OSC server could evaluate
  arbitrary code in the session.
- **Errors stop disappearing** --- `sys:load` used to swallow every error in a
  file it loaded, so a broken library could AOT-compile "successfully". It now
  reports each failing form and returns `#f`, which surfaced (and fixed) five
  examples that had quietly stopped working.
- **Memory safety through the C++ runtime** --- every Scheme FFI primitive is
  registered with its real name and arity, so a wrong call is a Scheme error
  rather than a segfault, and the zone allocator, OSC send path, IFF chunk
  scanner and a dozen fixed buffers no longer write past their ends.
- **Relocatable installs** --- the binary looks for `runtime/`, `libs/` and
  `examples/` beside itself, so an unzipped release runs from any working
  directory and can be moved wherever you like.
- **Less vendored code** --- the 2019 Networking TS snapshot and PCRE 8.38 are
  gone; sockets go through one small shim on every platform and regular
  expressions use PCRE2 10.48.
- **Behaviour worth knowing about before you upgrade** --- `pcg32_boundedrand`
  now matches reference PCG32, so seeded streams differ;
  `io:osc:set-integer-64bit?` and `io:osc:send-from-server-socket?` are removed;
  and `topclock` no longer starts its network loop at load when
  `*topclock-autostart*` is `#f`.

Extempore's current stack came together over v0.9.0 and v0.10.0: LLVM 22.1.6
with the ORC JIT (in place of the legacy MCJIT), s7 Scheme (in place of
TinyScheme), a single constraint-based type inference engine in the xtlang
compiler, a C++20 runtime, first-class Linux aarch64, and an interactive
`--repl` (Linux and macOS only). Graphics is now a lean set of WebGPU bindings
(via `wgpu-native`, enabled with `-DEXTERNAL_SHLIBS_GRAPHICS=ON`); the old
OpenGL stack has been retired. If you hit any breakage, please
[file an issue](https://github.com/digego/extempore/issues) with a minimal
reproducible example where you can.

## Getting started

### The easy way

Download the latest
[binary release](https://github.com/digego/extempore/releases) for your platform
and unzip it. The folder is self-contained --- keep it together, but put it
wherever you like --- and `extempore` (`extempore.exe` on Windows) runs from any
working directory.

Then,
[set up your text editor of choice](https://extemporelang.github.io/guides/editor-support/)
and away you go.

_Note_: the VSCode extension used to offer an _Extempore: Download binary_
command for one-click setup. It hasn't been updated for the current releases and
may not work --- downloading the release manually is the safest option for now.

_Note on macOS first launch_: the binaries aren't signed with an Apple Developer
ID, so on first launch macOS will refuse to run them ("apple could not verify
... is free of malware"). Despite the wording, this means Apple hasn't _checked_
the binary, not that anything malicious has been _detected_ --- the release is
built in public from the source in this repository by the
[Release workflow](https://github.com/digego/extempore/actions/workflows/release-binary.yml),
and you can rebuild it yourself from source if you'd prefer (see below). Signing
with a Developer ID requires a paid Apple Developer account, which Extempore (as
a research project) doesn't currently maintain.

To clear the quarantine flag on the whole unzipped folder in one go, run:

    xattr -dr com.apple.quarantine /path/to/extempore

(replace the path with wherever you unzipped it, e.g. `~/Downloads/extempore`).
After that, `extempore` will launch normally.

### Build from source

**For more information**, check out [BUILDING.md](./BUILDING.md).

Extempore's CMake build process downloads and builds all the dependencies you
need (including LLVM). So, if you've got a C++ compiler, git, CMake >= 3.28 and
Ninja, here are some one-liner build commands.

On **Linux/macOS** (and on **Windows** from a Visual Studio developer prompt):

    git clone https://github.com/digego/extempore && cd extempore && cmake --preset default -DASSETS=ON && cmake --build build

`--preset default` is a Ninja + Release build with the tests enabled; see
`CMakePresets.json` (and [BUILDING.md](./BUILDING.md)) for the others.

_Note on build time_: the first build takes ~10-30 minutes because LLVM is
compiled from source. Subsequent builds reuse the cached LLVM artifacts under
`build/_deps/`.

_Note on ASSETS_: the `ASSETS` build-time option (boolean, default `OFF`) is set
to `ON` above. This will download the Extempore binary assets --- required for
many of the examples, but adds a ~250MB download to the build process. If you'd
rather not do that, and are happy with some of the examples not working, then
set `-DASSETS=OFF` instead.

_Note on running_: an in-tree build keeps its runtime files (`runtime/`,
`libs/`, `examples/`) in the source tree, so run it in place
(`build/extempore`). If you'd rather have a self-contained tree somewhere else,
`cmake --install build --prefix <dir>` copies the binary and its runtime files
into `<dir>`; that tree can be moved anywhere and run from any working directory
--- see [BUILDING.md](./BUILDING.md).

### Hear your first sine wave

With Extempore running and an editor connected to port 7099, evaluate this and
you should hear a 440 Hz tone:

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
[Ben Swift](https://github.com/benswift). Andrew created Extempore and remains
its BDFL; Ben is the current maintainer. For issues and questions the
[mailing list](http://groups.google.com/group/extemporelang) is the best place
to start, but you can also email Ben directly. Many others have contributed to
Extempore's development
([see the full list](https://github.com/digego/extempore/graphs/contributors))
--- including [Jim Kuhn](https://github.com/JimKuhn), whose significant
performance improvements don't show up in the commit logs but for which we're
very grateful.

## Docs & Community

Extempore documentation can be found at <https://extemporelang.github.io/>

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
