# Extempore ![Build & test](https://github.com/digego/extempore/workflows/Build%20&%20run%20tests/badge.svg?branch=master) ![Release](https://github.com/digego/extempore/workflows/Release/badge.svg)

A programming environment for cyberphysical programming (Linux/macOS/Windows).

## Getting started

### The easy way

Download [VSCode](https://code.visualstudio.com/), install the Extempore
extension and then use the _Extempore: Download binary_ command to do the rest.

For more details, head to the [Quickstart
page](https://extemporelang.github.io/docs/overview/quickstart/) in Extempore's
online docs.

### The _slightly_ harder way (for those who don't want to use VSCode)

Download the latest [binary
release](https://github.com/digego/extempore/releases) for your platform, unzip
it and run `extempore` (`extempore.exe` on Windows) from inside the `extempore`
folder.

Then, [set up your text editor of
choice](https://extemporelang.github.io/docs/guides/editor-support/) and away
you go.

### Build from source

**For more information**, check out [BUILDING.md](./BUILDING.md).

Extempore's CMake build process downloads and build all the dependencies you
need (including LLVM). So, if you've got a C++ compiler, git and CMake, here are
some one-liner build commands:

On **Linux/macOS**:

    git clone https://github.com/digego/extempore && mkdir extempore/build && cd extempore/build && cmake -DASSETS=ON .. && make && sudo make install
    
On **Windows** (if you're using VS2019---adjust as necessary for your VS
version):

    git clone https://github.com/digego/extempore && mkdir extempore/build && cd extempore/build && cmake -G "Visual Studio 16 2019" -A x64 -DASSETS=ON .. && cmake --build . --target INSTALL --config Release

_Note:_ in the above one-liners the `ASSETS` build-time option (boolean, default
`OFF`) is set to `ON`. This will download the Extempore binary assets---required
for many of the examples, but adds a ~300MB download to build process. If you'd
rather not do that, and are happy with some of the examples not working, then
set `-DASSETS=OFF` instead.

## See Extempore in action

Check out these videos:

- [The Concert Programmer](https://www.youtube.com/watch?v=yY1FSsUV-8c)
- [Interactive, distributed, physics simulation](https://vimeo.com/126577281)
- [Programming in Time](https://www.youtube.com/watch?v=Sg2BjFQnr9s)
- [The Physics Playroom - interactive installation](https://vimeo.com/58239256)
- [An *old* Graphics Demo](https://vimeo.com/37293927)
- [A Programmer's Guide to Western Music](https://www.youtube.com/watch?v=xpSYWd_aIiI)
- [Ben's livecoding gig videos](https://benswift.me/livecoding/)

## Contributors

The Extempore core team is [Andrew Sorensen](https://github.com/digego) & [Ben
Swift](https://github.com/benswift), although many others have made
contributions as well ([see the full
list](https://github.com/digego/extempore/graphs/contributors)).

## Docs & Community

Extempore documentation can be found at https://extemporelang.github.io/docs/

You can also join the Extempore community:

- [Extempore google group](http://groups.google.com/group/extemporelang)
- [Extempore mailing list](mailto:extemporelang@googlegroups.com)
- [#extempore on chat.toplap.org](https://chat.toplap.org/home) (although not as
  well-monitored as the mailing list)

## Cite Extempore

- [Extempore: The design, implementation and application of a cyber-physical programming language](https://openresearch-repository.anu.edu.au/handle/1885/144603)
- [Systems level liveness with extempore](https://dl.acm.org/citation.cfm?id=3133858)

## Licence

Copyright (c) 2011-2020, Andrew Sorensen

All rights reserved.

Redistribution and use in source and binary forms, with or without 
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, 
   this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation 
   and/or other materials provided with the distribution.

Neither the name of the authors nor other contributors may be used to endorse
or promote products derived from this software without specific prior written 
permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE 
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
POSSIBILITY OF SUCH DAMAGE.
