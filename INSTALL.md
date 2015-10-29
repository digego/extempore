# Building Extempore

## Quick install

**Through homebrew (OSX)**

First,
```
brew tap benswift/extempore
```
then
```
brew install extempore
```
or, if you want the "extended" libs (e.g. graphics)
```
brew install extempore --with-extended
```
*Note:* If you've installed Extempore through homebrew previously
(i.e. if `brew info extempore` shows a version <= 0.59) then you'll
need to remove a couple of things first:
```
brew rm extempore kissfft
```

**Build from source (Linux/OSX)**

If you've got `git`, `cmake` and a C++ compiler toolchain
installed, then you can build Extempore with:
```
git clone https://github.com/digego/extempore && mkdir extempore/cmake-build && cd extempore/cmake-build && cmake .. && make install && make aot
```

Those are the "quick install" tips. For more detailed instructions,
read on...

## Get the depenencies

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

To use the ALSA portaudio backend (which is probably what you want,
unless you have a real reason to go with something else) you'll need
the libasound package at build-time, e.g. (on Ubuntu)
```
sudo apt-get install libasound2-dev
```
If you really want to use a different backend (e.g. `jack`) then you
can hack the `PA_USE_*` definitions in `CMakeLists.txt`

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
  can try e.g. `make -j4` to parallellize the `make` step, especially
  since LLVM takes so long to build)
- `make install` will install `extempore` into `/usr/local/bin` and
  the rest of the files (the "Extempore share directory") into
  `/usr/local/share/extempore`
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

Get the source from (http://sourceforge.net/projects/portmedia/files/)

If you don't have Java installed, then you'll need to comment out any
reference to a `JNI_*` variable in the `CMakeLists.txt` - a bit messy
I know, but we don't need the Java stuff in Extempore, and this is
better than having to install Java.
```
mkdir cmake-build && cd cmake-build
cmake -G"Visual Studio 14 2015 Win64" ..
```
build the `portmidi-dynamic` target (don't worry about the other
ones) then install to `libs/platform-shlibs`

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

#### GLEW

**Note:** GLEW is only used on Windows, as a build-time dependency for
nanovg.

Download the latest stable version from
(http://glew.sourceforge.net/).
```
mkdir cmake-build && cd cmake-build
cmake -G"Visual Studio 14 2015 Win64" ../build/cmake/
```
then move `libglew32.lib` out of whichever `Release` dir
it's in into just the toplevel `GLEW_DIR/lib`.

#### nanovg

From source (all platforms)

nanovg needs to be told about where GLEW is through the `GLEW_DIR` variable
```
git clone git@github.com:extemporelang/nanovg
cd nanovg && mkdir cmake-build && cd cmake-build
cmake -G"Visual Studio 14 2015 Win64" -DGLEW_DIR=c:/path/to/glew ..
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
cmake -DCMAKE_BUILD_TYPE=Release -DLLVM_ENABLE_TERMINFO=OFF -DLLVM_ENABLE_ZLIB=OFF -DCMAKE_INSTALL_PREFIX=c:/path/to/extempore/llvm .. && make && make install
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
