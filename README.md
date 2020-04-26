# Extempore

![`master` branch: build & test](https://github.com/digego/extempore/workflows/Build%20&%20run%20tests/badge.svg?branch=master)

![Latest release](https://github.com/digego/extempore/workflows/Release/badge.svg)

A programming environment for cyberphysical programming.

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

### The "hard" way: build from source

This will download and build all the dependencies you need (including LLVM). So,
if you've got a C++ compiler (for `gcc`, version 4.9 or later is required), git
and CMake, here are some one-liner build commands:

On **Linux/macOS**:

    git clone https://github.com/digego/extempore && mkdir extempore/build && cd extempore/build && cmake .. && make && sudo make install
    
On **Windows** (if you're using VS2017---adjust as necessary for your VS
version):

    git clone https://github.com/digego/extempore && mkdir extempore/build && cd extempore/build && cmake -G "Visual Studio 15 2017" -A x64 .. && cmake --build . --target INSTALL --config Release

If you have problems, check out the platform-specific build notes below.

### Build options

If you're using the CMake GUI, then these will appear as checkboxes/drop-down
menus. If you're using CMake from the CLI, then remember that you specify
options using the `-DOPTION_NAME=OPTION_VALUE` syntax.

Here are a few of the more interesting CMake options:

- `ASSETS` (boolean, default `ON`) will download the Extempore binary assets
  (required for many of the examples), so as long as you're ok with a ~300MB
  download during the build process then add the `-DASSETS=ON` CMake option to
  the above build commands

- `BUILD_DEPS` (boolean, default `ON`) controls whether or not the build process
  will also download and build the extended libraries (e.g. glfw, libsndfile).
  If you want to get those things through another package manager (or not use
  them at all) then set this to `OFF`.

- `EXTERNAL_SHLIBS_AUDIO`/`EXTERNAL_SHLIBS_GRAPHICS` (boolean, default `ON` for
  both) are useful if you want to build the supporting libs for graphics/audio
  but not both (so you should set the one you *don't* want to `OFF`)

- `PACKAGE` (boolean, default `OFF`) prepares everything (including all extended
  deps and AOT-compilation) to create a zipfile for binary distribution. The
  main effect of this flag is to disable processor-specific optimisations for
  all the build commands, which gives you the best chance of producing a
  portable binary. This option also adds a `package` target to the
  makefile/solution, which can be used to actually generate the zipfile/dmg/deb.

### Build targets

The default target will build Extempore, all the dependencies, and AOT-compile
the standard library (for faster startup). However, in other situations the
following targets might come in handy:

- on macOS and Linux, the `install` target will move the extempore executable to
  `/usr/local/bin` (or similar) and the rest of the Extempore share directory to
  `/usr/local/share/extempore` (does nothing on Windows)

- the `aot` target will ahead-of-time compile just the core standard library,
  i.e. the pure-xtlang libraries with no external C library dependencies

- the `clean_aot` target will remove all AOT-compiled files

- the `assets` target won't build anything per. se., but it will download the
  assets e.g. sound files, 3D model files which are referenced in the examples
  (it's pretty big, so make sure you're on an internet connection where you
  don't mind downloading a bunch of data)

### Platform-specific notes

#### macOS

On **macOS 10.14** (Mojave) Apple removed some of the system OpenGL headers,
which means that some of Extempore's dependencies (e.g. nanovg) won't build.
[This blog
post](https://silvae86.github.io/sysadmin/mac/osx/mojave/beta/libxml2/2018/07/05/fixing-missing-headers-for-homebrew-in-mac-osx-mojave/)
has more details, but if you just want a one-liner to fix it, it's

    sudo installer -pkg /Library/Developer/CommandLineTools/Packages/macOS_SDK_headers_for_macOS_10.14.pkg -target /

#### Linux

Extempore is built & tested on Ubuntu, but is also known to work with Debian,
Fedora, Arch, and also inside a docker container.

There are a few extra dependencies which you may need to get through your
package manager. For example, on Ubuntu 20.04:

    sudo apt-get install libasound2-dev xorg-dev libglu1-mesa-dev

#### Windows

Extempore is built & tested on Windows 10 with Visual Studio v15 (2017) and
Visual Studio v16 (2019). If you don't already have VS installed, you can
download the free [Visual Studio
Community](https://www.visualstudio.com/en-us/products/visual-studio-community-vs.aspx)---that's
perfectly fine for building Extempore (although the paid versions of VS will
work as well).

If you don't want to use the the command-line described
[above](#build-from-source), note that Visual Studio has pretty good CMake
integration these days.

If you want to use the **ASIO** audio backend on Windows (which _might_ give you
lower-latency audio, or it might not) you need to download the [ASIO
SDK](http://www.steinberg.net/nc/en/company/developer/sdk_download_portal.html)
from Steinberg. You have to create a [third party developer
account](http://www.steinberg.net/nc/en/company/developer/sdk_download_portal/create_3rd_party_developer_account.html),
then you can log in and download the ASIO SDK (make sure you get the right SDK).
You also need to download and install [ASIO4ALL](http://www.asio4all.com/) with
the 'offline setup panel' option enabled. After that, copy the ASIO files into
the `src/portaudio/src/hostapi/asio`, and use the `-DASIO=ON` CMake option.

## See Extempore in action

Check out these videos:

- [The Concert Programmer](https://www.youtube.com/watch?v=yY1FSsUV-8c)
- [Interactive, distributed, physics simulation](https://vimeo.com/126577281)
- [Programming in Time](https://www.youtube.com/watch?v=Sg2BjFQnr9s)
- [The Physics Playroom - interactive installation](https://vimeo.com/58239256)
- [An *old* Graphics Demo](https://vimeo.com/37293927)
- [A Programmer's Guide to Western Music](https://www.youtube.com/watch?v=xpSYWd_aIiI)
- [Ben's livecoding gig videos](https://benswift.me/livecoding/)

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
