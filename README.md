# Extempore [![Build Status](https://travis-ci.org/JimKuhn/extempore.svg?branch=master)](https://travis-ci.org/JimKuhn/extempore)

A programming environment for cyberphysical programming.

# Getting started

To get started, you can either download a binary release or build
Extempore from source yourself.

## Download a pre-built binary

Download a
[binary release](https://github.com/digego/extempore/releases), unzip
it and run `extempore.exe` from inside the `extempore` folder.

## Build from source

This will download and build all the dependencies you need (including
LLVM). So, if you've got a C++ compiler (for `gcc`, version 4.9 or
later is required), git and CMake, here are some one-liner build
commands:

On **Linux/OSX**:

    git clone https://github.com/digego/extempore && mkdir extempore/cmake-build && cd extempore/cmake-build && cmake .. && make && sudo make install
    
On **Linux/OSX with JACK**:

    git clone https://github.com/digego/extempore && mkdir extempore/cmake-build && cd extempore/cmake-build && cmake -DJACK=ON .. && make && sudo make install
    
On **Windows**:

    git clone https://github.com/digego/extempore && mkdir extempore/cmake-build && cd extempore/cmake-build && cmake -G"Visual Studio 14 2015 Win64" .. && cmake --build . --target ALL_BUILD --config Release

# See Extempore in action

Check out these videos:

- [The Concert Programmer](https://www.youtube.com/watch?v=yY1FSsUV-8c)
- [Interactive, distributed, physics simulation](https://vimeo.com/126577281)
- [Programming in Time](https://www.youtube.com/watch?v=Sg2BjFQnr9s)
- [The Physics Playroom - interactive installation](https://vimeo.com/58239256)
- [An *old* Graphics Demo](https://vimeo.com/37293927)
- [A Programmer's Guide to Western Music](https://www.youtube.com/watch?v=xpSYWd_aIiI)

# Docs & Community

Extempore documentation can be found at http://digego.github.io/extempore/index.html

You can also join the Extempore community:

- [Extempore google group](http://groups.google.com/group/extemporelang)
- [Extempore mailing list](mailto:extemporelang@googlegroups.com)

# Licence

Copyright (c) 2011-2017, Andrew Sorensen

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
