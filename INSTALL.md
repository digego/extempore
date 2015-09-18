# Building Extempore from Source

## Get the Extempore source

```
git clone git@github.com:digego/extempore.git
```

## Get the build-time deps

### Deb/Ubuntu

```
sudo apt-get install git g++ portaudio19-dev
```

### OSX

```
brew install portaudio
```

### Windows

The NuGet version of **Portaudio** should be fine, but building it
from source is fine too.

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

LLVM now uses CMake as well. Grab the
[3.7.0 source tarball](http://llvm.org/releases/download.html#3.7.0),
apply the `extempore-llvm-3.7.0.patch` in `extras/`

```
cd /path/to/llvm-3.7.0.src
patch -p0 < /path/to/extempore/extras/extempore-llvm-3.7.0.patch
```

Then build LLVM, moving the libraries into
`/path/to/extempore/llvm` as part of the `install` step

```
mkdir cmake-build && cd cmake-build
cmake -DCMAKE_BUILD_TYPE=Release -DLLVM_ENABLE_TERMINFO=OFF -DLLVM_ENABLE_ZLIB=OFF -DCMAKE_INSTALL_PREFIX=/path/to/extempore/llvm .. && make && make install
```

On **Windows**, you'll probably want to specify a 64-bit generator e.g.
`-G"Visual Studio 14 2015 Win64"`

## Generate build files with CMake

In your `extempore` directory,

```
mkdir cmake-build && cd cmake-build
cmake -DEXT_LLVM_DIR=/path/to/extempore/llvm ..
```

If you've set the `EXT_LLVM_DIR` environment variable you don't have
to provide it again to CMake.

If you want to work "in-tree" (handy for Extempore developers) then
you can set

```
cmake -DIN_TREE=ON ..
```

This will make the `make install` just install `extempore` into
`/usr/local/bin`, and Extempore will keep using the Extempore source
directory (i.e. the cloned repo) for it's `runtime/`, `libs/`, etc.

On **Windows**, you'll need to give CMake a few more details

```
md cmake-build && cd cmake-build
cmake -G"Visual Studio 14 2015 Win64" -DEXT_LLVM_DIR=c:\path\to\extempore\llvm ..
```

## Build Extempore

### Linux/OSX

CMake will generate a `Makefile` in `cmake-build`, with a few useful
targets:

- `make` will build Extempore
- `make install` will install `extempore` into `/usr/local/bin` and
  the rest of the files (the "Extempore share directory") into
  `/usr/local/share/extempore`
- `make uninstall` will remove the installed files
- `make aod_stdlibs` will ahead-of-time compile the standard library

### Windows

CMake will generate a Visual Studio solution (`.sln`) in
`cmake-build`.  Open it, and build the `extempore` target.

## Standard library

## Linux/OSX

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

#### nanovg

remove `"FONS_USE_FREETYPE"` from `premake4.lua`

```
premake4.exe --platform=x64 vs2012
```

then upgrade the `.sln` by opening it in VS2015
