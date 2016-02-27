# Building Extempore

## Quick install

### OSX

**Through homebrew**

[Homebrew](http://brew.sh/) makes the process pretty simple, although
since it's building everything (including LLVM) from source it may
still take up to 15mins depending on your machine.

Extempore has a "core" library, which includes things like the math
and audio DSP libraries and doesn't depend on any external shared
libraries. This is the option homebrew will use by default - it's
still a fully-fledged xtlang compiler and everything else.

However, there are a few external libraries which you might want to
use to do certain things, like open up OpenGL canvases or load
compressed audio files. We call this the "extended" library, and
there's a `--with-extended` flag to tell homebrew to go and grab those
other packages as well.

To install Extempore through homebrew, first
```
brew tap benswift/extempore && brew tap homebrew/versions
```
then
```
brew install extempore
```
or, if you want the "extended" libs (e.g. graphics)
```
brew install extempore --with-extended
```
The `homebrew/versions` tap is needed for `glfw3` (required to build
the extended stdlib) while the `benswift/extempore` tap is contains
Extempore itself (and a few other deps)

**Caveats**

If you've installed Extempore through homebrew previously
(i.e. if `brew info extempore` shows a version <= 0.59) then you'll
need to remove a couple of things first:
```
brew rm extempore kissfft libnanovg
```
If you're on OSX **10.9** or earlier, there's an incompatibility with
your version of clang and the LLVM 3.7.0 which Extempore uses.  For
the moment the easiest way around this is to download the old `0.5.9`
version of Extempore with
```
brew tap benswift/extempore
brew install extempore059
```
again, if you want the "extended" libs (e.g. graphics)
```
brew install extempore059 --with-extended
```

### Linux

**Build from source** (also works on OSX)

If you've got `git`, `cmake` and a C++ compiler toolchain
installed, then you can build Extempore with:
```
git clone https://github.com/digego/extempore && mkdir extempore/cmake-build && cd extempore/cmake-build && cmake .. && make install && make aot
```

### Windows

Download a
[precompiled binary](http://extempore.moso.com.au/extras/Extempore-0.6.0-win64.zip),
unzip it and run `extempore.exe` from inside the `extempore` folder.

## Get the dependencies

You'll need

a **C++ compiler toolchain**, e.g.

- `sudo apt-get install g++` on Ubuntu/Debian
- `sudo yum install gcc gcc-c++` on Fedora/CentOS/RHEL
- Xcode or the [command line tools](https://developer.apple.com/library/ios/technotes/tn2339/_index.html#//apple_ref/doc/uid/DTS40014588-CH1-WHAT_IS_THE_COMMAND_LINE_TOOLS_PACKAGE_) on OSX
- Visual Studio on Windows (the
  [Community 2015](https://www.visualstudio.com/en-us/products/visual-studio-community-vs.aspx)
  version is now free)

Extempore should build with clang, gcc and MSVC - and possibly other
compilers (but I haven't tried them).

**git**

- `sudo apt-get install git` on Ubuntu/Debian
- `sudo yum install git` on Fedora/CentOS/RHEL
- `brew install git` on OSX with Homebrew
- `choco install git` on Windows with Chocolatey

**CMake** (version 3.1 or greater)

- `sudo yum install cmake` on Fedora/CentOS/RHEL
- `brew install cmake` on OSX with Homebrew
- `choco install cmake` on Windows with Chocolatey

The Ubuntu 15.04 package archive only includes CMake v3.0, but you can
get a more up-to-date version through a package archive
```
sudo apt-get install software-properties-common && sudo add-apt-repository ppa:george-edison55/cmake-3.x && sudo apt-get update && sudo apt-get install cmake
```

**ALSA** (Linux only)

To use the [ALSA](http://www.alsa-project.org/) portaudio backend
(which is probably what you want, unless you have a real reason to go
with something else) you'll need the libasound package at build-time,
e.g. (on Ubuntu)
```
sudo apt-get install libasound2-dev
```

**Jack** (Linux only)

To use the [Jack](http://www.jackaudio.org/) portaudio backend, you'll need
to have Jack installed, and then to set the `JACK` cmake option with `-DJACK=ON`

**Boost** (Windows only)

We still need one component of the **Boost** libs on Windows
(specifically the ASIO component for TCP/UDP handling). If you've got
the NuGet command line client installed, you can probably do
```
nuget install boost-vc140 & nuget install boost_system-vc140 & nuget install boost_regex-vc140 & nuget install boost_date_time-vc140
```
It doesn't matter how you get these deps or where you put them, as
long as you tell Extempore where they are through the `BOOST_DIR`
cmake variable. The `BOOST_DIR` should have two subdirectories
`include` and `lib`, which should contain the boost header directory
and the `libboost*.lib` files respectively.

**LLVM 3.7.0**

As of `21e750a`, downloading and building LLVM 3.7 happens
automatically as part of the Extempore cmake build process. But
instructions are included at the end of this file in case you want to
do it yourself.

## Configure

Extempore uses CMake for configuration. In your `extempore` directory
(i.e. the one this `INSTALL.md` file is in)
```
mkdir cmake-build && cd cmake-build && cmake ..
```
On **Windows**, you'll need to give CMake a few more details about
where Boost is:
```
md cmake-build && cd cmake-build
cmake -G"Visual Studio 14 2015 Win64" -DBOOST_DIR=c:\path\to\extempore\boost ..
```

## Build & Install

On **Linux/OSX** CMake will generate a `Makefile` in `cmake-build`,
with a few useful targets:

- `make` will build Extempore (if you have a multicore machine, you
  can try e.g. `make -j4` to parallelize the `make` step, especially
  since LLVM takes so long to build)
- `make install` will install `extempore` into `/usr/local/bin`
- `make uninstall` will remove the installed files
- `make aot`/`make aot_extended` will ahead-of-time compile the
  core/extended "standard library"

On **Windows**, CMake will generate a Visual Studio solution (`.sln`)
in `cmake-build`. Open it, and build the `extempore` target.

After that, you're done. Time to
[get started](http://benswift.me/2012/09/26/interacting-with-the-extempore-compiler/).

## External shared libs

Extempore is all about being dynamic and adding functionality
on-the-fly. As a result, there are a bunch of helpful libraries (e.g.
for sound file IO, FFTs, graphics) which we use a lot, but which
aren't compiled statically into the `extempore` executable. Instead,
we load this code at runtime through shared libraries (`.dylib` on
OSX, `.so` on Linux and `.dll` on Windows).

This means that you have to have these shared libraries on your system
somewhere where Extempore can find them.

On **OSX** you can get them through [homebrew](http://brew.sh/)
(assuming you've done a `brew tap benswift/extempore`)

```
brew install assimp libsndfile portmidi libkiss-fft glfw3 libstb-image libnanovg
```

On **Debian/Ubuntu** you can use `apt-get`

```
sudo apt-get install libasound2-dev libgl1-mesa-dev libsndfile1-dev libassimp3 libglfw3 libportmidi-dev
```

You'll have to build KissFFT, stb_image and nanovg yourself, using the
instructions below, but `make install` step means that you won't have
to move anything into `libs/platform-shlibs`.

On **Windows**, there isn't a package manager which will do the job so
you'll need to build from source. Since Windows doesn't have a lib
path, all the dlls should go in
`c:/path/to/extempore/libs/platform-shlibs`. So for all these deps,
move the dll in there when it's done.

### Build from source

#### libsndfile

For Windows, you can just grab the Windows 64-bit installer from
(http://www.mega-nerd.com/libsndfile/), and copy `libsndfile-1.dll`
and `libsndfile-1.lib` into `extempore/libs/platform-shlibs`

Or follow the instructions to build from source.

#### Portmidi

```
git clone git@github.com:extemporelang/portmidi
cd portmidi && mkdir cmake-build && cd cmake-build
cmake -G"Visual Studio 14 2015 Win64" ..
```
then install to `libs/platform-shlibs`

#### KissFFT

```
git clone git@github.com:extemporelang/kiss_fft
cd stb && mkdir cmake-build && cd cmake-build
cmake -G"Visual Studio 14 2015 Win64" ..
```
then install to `libs/platform-shlibs`

#### GLFW3

Download from (http://www.glfw.org/), I used v3.1.1.
```
cmake -DBUILD_SHARED_LIBS=ON -DGLFW_BUILD_EXAMPLES=OFF -DGLFW_BUILD_TESTS=OFF ..
```
then install to `libs/platform-shlibs`

#### stb_image

```
git clone git@github.com:extemporelang/stb
cd stb && mkdir cmake-build && cd cmake-build
cmake -G"Visual Studio 14 2015 Win64" ..
```
then install to `libs/platform-shlibs`

#### nanovg

```
git clone git@github.com:extemporelang/nanovg
cd nanovg && mkdir cmake-build && cd cmake-build
cmake -G"Visual Studio 14 2015 Win64" ..
```
then install to `libs/platform-shlibs`

#### Assimp

Get latest source from
(http://assimp.sourceforge.net/main_downloads.html).

```
mkdir cmake-build && cd cmake-build
cmake -G"Visual Studio 14 2015 Win64" ..
```
then install to `libs/platform-shlibs`

#### Glib/Gobject

*Currently not working - suggestions welcome!*

I got a precompiled binary from
(http://ftp.gnome.org/pub/GNOME/binaries/win64/glib/2.26/) although
the latest version I could find was 2.26, which is a few versions
behind the latest (2.45). It seems like they might not be releasing
Windows binaries anymore?

You could also try getting it from
[Winlibs](https://github.com/winlibs/glib) and building it yourself,
but that seems hairy.

## AOT-compiling the Extempore standard library

This step isn't necessary, but it will make some common Extempore
libraries load up much faster.
```
cd extempore/cmake-build # or wherever your Extempore build dir is
make aot
```

If you want the "extended" Extempore standard library, which requires
the aforementioned external shared libs, then you can try `make
aot_extended` instead.

To remove the AOT-compiled files, use the `clean_aot` target in the
makefile or MSVS project.

### LLVM 3.7.0

Grab the
[3.7.0 source tarball](http://llvm.org/releases/download.html#3.7.0),
apply the `extempore-llvm-3.7.0.patch` in `extras/`
```
cd /path/to/llvm-3.7.0.src
patch -p0 < /path/to/extempore/extras/extempore-llvm-3.7.0.patch
```
On **Windows**, the `<` redirection will work with `cmd.exe`, but not
PowerShell.

Then build LLVM, moving the libraries into `/path/to/extempore/llvm`
as part of the `install` step
```
mkdir cmake-build && cd cmake-build
cmake -DCMAKE_BUILD_TYPE=Release -DLLVM_TARGETS_TO_BUILD=X86 -DLLVM_ENABLE_TERMINFO=OFF -DLLVM_ENABLE_ZLIB=OFF -DCMAKE_INSTALL_PREFIX=c:/path/to/extempore/llvm .. && make && make install
```
On **Windows**, you'll also need to specify a 64-bit generator e.g.
`-G"Visual Studio 14 2015 Win64"`

To build, open the `Extempore.sln` file and build the `ALL_BUILD`
target, then the `INSTALL` target. If the install step doesn't work,
you can try directly calling `cmake -P cmake_install.cmake` which
should be in the same directory. On Windows, the LLVM build output
must be installed into an `llvm` subdirectory in the top-level
Extempore directory (since the AOT compilation process will look in
there to find `llc`).

If LLVM complains about not being able to find python, you can specify
a path to your python executable with the PYTHON_EXECUTABLE CMake
variable:

```
cmake -DCMAKE_BUILD_TYPE=Release -DLLVM_TARGETS_TO_BUILD=X86 -DLLVM_ENABLE_TERMINFO=OFF -DLLVM_ENABLE_ZLIB=OFF -DCMAKE_INSTALL_PREFIX=c:/path/to/extempore/llvm -DPYTHON_EXECUTABLE=c:/path/to/python .. && make && make install
```

## Packaging

*Note: this is still experimental - things may not work, but
 patches/suggestions welcome!*

To build a "package" for binary distribution, use the `-DPACKAGE=ON`
cmake option.

## OSX

```
cmake -DPACKAGE=ON .. && make -j8 aot_extended && make package
```

## Windows

```
cmake -G"Visual Studio 14 2015 Win64" -DPACKAGE=ON -DBOOST_DIR=c:/Users/ben/Code/extempore/boost .. && cmake --build . --config Release --target aot_extended && cmake --build . --config Release --target package
```
