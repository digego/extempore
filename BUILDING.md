# Build-from-source notes

## Build-time deps

- a C++ compiler (`clang`, `gcc` >= 4.9, `msvc` >= VS2015)
- Git
- CMake

For platform-specific deps, see "Platform-specific notes" below.

## Options

See the top of `CMakeLists.txt` for all the available build options.

## Targets

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

## Platform-specific notes

### macOS

On **macOS 10.14** (Mojave) Apple removed some of the system OpenGL headers,
which means that some of Extempore's dependencies (e.g. nanovg) won't build.
[This blog
post](https://silvae86.github.io/sysadmin/mac/osx/mojave/beta/libxml2/2018/07/05/fixing-missing-headers-for-homebrew-in-mac-osx-mojave/)
has more details, but if you just want a one-liner to fix it, it's

    sudo installer -pkg /Library/Developer/CommandLineTools/Packages/macOS_SDK_headers_for_macOS_10.14.pkg -target /

### Linux

Extempore is built & tested on Ubuntu, but is also known to work with Debian,
Fedora, Arch, and also inside a docker container.

There are a few extra dependencies which you may need to get through your
package manager. For example, on Ubuntu 20.04:

    sudo apt-get install libasound2-dev xorg-dev libglu1-mesa-dev

### Windows

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
