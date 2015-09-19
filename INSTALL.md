# Building Extempore from Source

## Build depenencies

You'll need

a **C++ compiler toolchain**, e.g.

- `sudo apt-get install g++` on Ubuntu/Debian
- Xcode or the standalone command line tools on OSX
- Visual Studio on Windows (the "Community" version is now free)

**git**

- `sudo apt-get install git` on Ubuntu/Debian
- `brew install git` on OSX with Homebrew
- `choco install git` on Windows with Chocolatey

**CMake** (version 3.1 or greater)

- `brew install cmake` on OSX with Homebrew
- `choco install cmake` on Windows with Chocolatey

The Ubuntu 15.04 package archive only includes CMake v3.0, but you can
get a more up-to-date version through a package archive

```
sudo apt-get install software-properties-common && sudo add-apt-repository ppa:george-edison55/cmake-3.x && sudo apt-get update && sudo apt-get install cmake
```

### Boost (Windows only)

We still need one component of the **Boost** libs on Windows
(specifically the ASIO component for TCP/UDP handling). If you've got
the NuGet command line client installed, you can probably do

```
nuget install boost-vc140 & nuget install boost_system-vc140 & nuget install boost_regex-vc140 & nuget install boost_date_time-vc140
```

It doesn't matter how you get these deps, but the Extempore CMake
build process expects to find them in `libs/win64/lib` (for the
library files) and `libs/win64/include` (for the headers). So you'll
need to (probably manually) move them in there, which is a bit
painful, but it's a one-time process.

### LLVM 3.7

Grab the
[3.7.0 source tarball](http://llvm.org/releases/download.html#3.7.0),
apply the `extempore-llvm-3.7.0.patch` in `extras/`

```
cd /path/to/llvm-3.7.0.src
patch -p0 < /path/to/extempore/extras/extempore-llvm-3.7.0.patch
```

Then build LLVM, moving the libraries into `/path/to/extempore/llvm`
as part of the `install` step

```
mkdir cmake-build && cd cmake-build
cmake -DCMAKE_BUILD_TYPE=Release -DLLVM_ENABLE_TERMINFO=OFF -DLLVM_ENABLE_ZLIB=OFF -DCMAKE_INSTALL_PREFIX=/path/to/extempore/llvm .. && make && make install
```

On **Windows**, you'll also need to specify a 64-bit generator e.g.
`-G"Visual Studio 14 2015 Win64"`

## Building Extempore

### Generate build files with CMake

In your `extempore` directory,

```
mkdir cmake-build && cd cmake-build
cmake -DEXT_LLVM_DIR=/path/to/extempore/llvm ..
```

If you've set the `EXT_LLVM_DIR` environment variable you don't have
to provide it again to CMake.

On **Windows**, you'll need to give CMake a few more details

```
md cmake-build && cd cmake-build
cmake -G"Visual Studio 14 2015 Win64" -DEXT_LLVM_DIR=c:\path\to\extempore\llvm ..
```

### Build Extempore

On **Linux/OSX** CMake will generate a `Makefile` in `cmake-build`,
with a few useful targets:

- `make` will build Extempore
- `make install` will install `extempore` into `/usr/local/bin` and
  the rest of the files (the "Extempore share directory") into
  `/usr/local/share/extempore`
- `make uninstall` will remove the installed files
- `make aod_stdlibs` will ahead-of-time compile the standard library

On **Windows**, CMake will generate a Visual Studio solution (`.sln`)
in `cmake-build`. Open it, and build the `extempore` target.

## Building the Extempore standard library

### Linux/OSX

It's pretty straightforward. You should be able to get most things
through your package manager.

### Windows

Just some notes.  Mostly for my (Ben's) benefit.

#### libsndfile

Just grab the Windows 64-bit installer from
(http://www.mega-nerd.com/libsndfile/), and copy `libsndfile-1.dll`
and `libsndfile-1.lib` into `extempore/libs/Win64/lib`

#### GLFW

```
cmake -DBUILD_SHARED_LIBS=ON -DGLFW_BUILD_EXAMPLES=OFF -DGLFW_BUILD_TESTS=OFF ..
```

#### stb_image

```
git clone git@github.com:benswift/stb
cd stb && mkdir cmake-build && cd cmake-build
cmake -G"Visual Studio 14 2015 Win64" ..
```

#### nanovg

```
git clone git@github.com:benswift/nanovg
cd nanovg && mkdir cmake-build && cd cmake-build
cmake -G"Visual Studio 14 2015 Win64"
```
