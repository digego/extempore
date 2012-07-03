# Dependencies

Extempore has some library dependencies that
may not be installed on your system.

## OS X
- `LLVM 3.0`
- `pthread` (if not using `boost`) *or* `boost` (if not using `pthread`)
- `pcre` (Perl-compatible Regular Expressions)
- `portaudio`

## Linux
- `LLVM 3.0`
- `pthread` (if not using `boost`) *or* `boost` (if not using `pthread`)
- `pcre`
- `portaudio`
- `mesa GL` (openGL)

## Windows 7
- `LLVM 3.0`
- `pcre`
- `boost`
- `portaudio`

Most of the libraries are pretty standard and will either already be
installed on your system, or can usually be easily installed from
binary packages.

# Unix & OS X

LLVM *must* be version 3.0 and needs to be built from source, because
a small patch to `LLParser.cpp` (an LLVM source file) is required. The
patch file `llparser.patch` is supplied in the `extempore/extras`
directory. You can get the LLVM source from http://www.llvm.org/

To apply the patch just move into the `lib/AsmParser` directory and
then apply the `llparser.patch` file which in can be found in
`extempore/extras`.

For example, if you've downloaded the LLVM source into `~/Code/llvm`
and cloned extempore into `~/Code/extempore`, then patching the file is as
simple as

```shell
$ cd ~/llvm/Code/lib/AsmParser
$ patch < ~/Code/extempore/extras/llparser.patch
```

After that, building LLVM is the usual `./configure; make; make
install` process, with the addition of a `--prefix` location
specifying where everything will be installed into. In this example
this location is a `Build` directory in `~/Code/llvm`.

```shell
$ cd ~/Code/llvm
$ mkdir Build
$ ./configure --prefix=~/Code/llvm/Build --enable-optimized
$ make -j4
$ make install
```

Once all of the libraries required for your platform (see list above)
are installed, you can compile extempore. First, point extempore to
your LLVM build directory by defining the shell variable
`EXT_LLVM_DIR`, for example

```shell
$ export EXT_LLVM_DIR=~/Code/llvm/Build
```

Then you can build extempore using the `all.bash` shell script 

```shell
$ ./all.bash
```

Voila... you should be done.

The dependencies section above mentions that extempore can be built
with *either* boost or pthreads. At this stage the boost library (if
used) is only used for threading, so unless you have a good reason to
it's probably safer to use pthreads. Still, if you wish to build
against the boost library you need to define `EXT_BOOST`. So the build
command for building extempore with boost support is

```shell
./all.bash -TEXT_BOOST
```

# Windows 7

Windows has thus far only been tested on Windows 7 64-bit, but if you
can get it to build/run on other flavours of Windows then do let us
know and we'll update these instructions.  Also, if anyone wants to
improve and streamline the Windows build process, then patches are
welcome :)

Boost is mandatory for Extempore on Windows, and you will need to
build pcre, boost, portaudio and LLVM 3.0 yourself. The extempore
build process is pretty simple once you've built these libraries.

## Preparing dependencies

A few source directories have to be on the 'include path':

- `llvm-3.0`
- `boost_1_47_0`
- `pcre-8.12`
- `portaudio`

Similarly, you'll need to make some directories for the results of
building the above libs

- `my_llvm_build` - use `cmake` to create then build using msvc
- `my_boost_build` - use `bjam` (see below `--build-dir=my_boost_build`)
- `my_pcre_build` - use `cmake` to create then build using msvc
- build `portaudio` using provided msvc project (`portaudio/build/msvc/portaudio.sln`)

You'll also need to patch LLVM. The `extempore/extras/llparser.patch`
file contains the patch, but because GNU `patch` isn't installed by
default on Windows patching this file isn't quite as simple as it was
on Linux/OS X.

## Patching LLVM

LLVM *must* be version 3.0 and needs to be built from source, because
a small patch to `LLParser.cpp` (an LLVM source file) is required. The
patch file `llparser.patch` is supplied in the `extempore/extras`
directory. You can get the LLVM source from http://www.llvm.org/

To apply the patch on Windows there are a couple of options:

1. Get GNU patch in the form of `patch.exe`
([maybe also check out this advice](http://irq5.wordpress.com/2011/06/26/gnu-patch-and-windows-uac/))
and apply the patch in the same way as is described above in the Linux
& OS X section 2. Apply the patch manually - look at the file
`lib/AsmParser/LLparser.cpp` in the LLVM source directory and the
patch file in a text editor and make the required changes by hand. A
bit cumbersome, but you only have to do it to *compile* extempore.

## Building extempore

The extempore directory expects to find itself in the same directory as
all of the directories mentioned above, although you can change all
of this by manually editing `extempore/msvc/extempore.vcxproj`.

If you have now built everything successfully you should be able to 
build extempore from the command line.

First set the shell's vc/build environment vars

```bat
> cd extempore\msvc
> ms_build_vars.bat
```

You'll need a full version of Visual Studio to get the 64bit compiler
(`cl.exe`). Then you can build extempore:
```bat
> cd extempore
> all.bat
```

And you're done.

**EMACS NOTE**: if you choose to use emacs on windows (recommended)
and run extempore in a `shell` or `eshell` then you should
set `--term ansi` as an option when starting extempore.

## Additional Windows Build Notes

- Use CMake to build pcre (`my_pcre_build`)
- Use CMake to Build LLVM (choose 64bit option - `my_llvm_build`)
- Make sure that Debug build mode has `_ITERATOR_DEBUG_LEVEL`=2 set for both 
- `CMAKE_CXX_FLAGS_DEBUG` and `CMAKE_C_FLAGS_DEBUG` (click advanced checkbox in cmake gui) 
- Make sure portaudio msvc general project settings are set to static lib
- boost command line for bjam is:
```bat
jam --build-dir=<dir> toodset=msvc link=static address-model=64
--build-type=complete stage
```
- extempore msvs project settings needs `PCRE_STATIC` to be defined if you want to build against pcre static libs
- extempore msvs project needs `EXT_BOOST` to be defined
- `pcreposix.lib` needs to come before `pcre.lib` in link list

# Running Extempore

You don't need to do anyting special to run extempore:

```shell
$ ./extempore
```

There are some optional command line options that you may want to use
you can get a list by running `./extempore --help`

Once you've started extempore you can connect using either telnet,
emacs, or vim.

## Telnet

If using telnet the default extempore port to connect to is `7099`.

## Emacs

Emacs currently has the most support for extempore programming. After
starting emacs, the steps to get up and running with extempore are:

1. Open a shell buffer `M-x shell`, `cd` to the extempore directory
and start it up with `./extempore`
2. Load `extras/extempore.el` with `M-x load-file` after you've
started emacs
3. Switch to an xtlang file (or create a new one)
4. Enable first `M-x scheme-mode` and then `M-x extempore-mode`
5. You're away. `C-x C-x` will eval an expression and `C-x C-r` will
eval the region
  
This process (particularly having to load *both* `scheme-mode` and
`extempore-mode` for every file) will improve soon as a more
fully-fledged `extempore-mode` is developed.

To avoid manually having to load `extempore.el` every time you start
emacs (i.e. step 2) you'll probably want to add the following to your
`~/.emacs`:

```elisp
(autoload 'extempore-mode "/path/to/extempore.el" "" t)
```

## Vim

If you want to use (g)vim, see the instructions found in
`extras/extempore.vim`.
