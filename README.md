# What is extempore?

Extempore is a systems programming language designed to support the
programming of real-time systems *in real-time*. Extempore promotes
human orchestration as a meta model of real-time man-machine
interaction in an increasingly distributed and environmentally aware
computing context.

Extempore is designed to support a style of programming dubbed
['cyberphysical' programming](http://dl.acm.org/citation.cfm?id=1869526).
Cyberphysical programming supports the notion of a human programmer
operating as an active agent in a real-time distributed network of
environmentally aware systems. The programmer interacts with the
distributed real-time system procedurally by modifying code
on-the-fly. In order to achieve this level of on-the-fly interaction
Extempore is designed from the ground up to support code hot-swapping
across a distributed heterogeneous network, compiler as service,
real-time task scheduling and a first class semantics for time.

Extempore is designed to mix the high-level expressiveness of Lisp
with the low-level expressiveness of C.  Extempore is a statically
typed, type-inferencing language with strong temporal semantics and
a flexible concurrency architecture in a completely hot-swappable
runtime environment.  Extempore makes extensive use of the LLVM
project to provide back-end code generation across a variety of
architectures.

For more detail on what the Extempore project is all about, see
[the Extempore philosophy](http://benswift.me/2012-08-07-extempore-philosophy.html). 

To see Extempore in action, check out these videos:

- [interactive, distributed, physics simulation](http://vimeo.com/52964510)
- [graphics demo](http://vimeo.com/37293927)
- [general introduction](http://vimeo.com/21956071)
- [a more technical intro](http://vimeo.com/20502359)

# Documentation

More Extempore documentation can be found at http://benswift.me/extempore-docs/index.html

You can also join the Extempore community:

- [Extempore google group](http://groups.google.com/group/extemporelang)
- [Extempore mailing list](mailto:extemporelang@googlegroups.com)

# Installation on Linux & OS X

_More detailed instructions can be found_ [here](http://benswift.me/2013-03-20-building-extempore-on-osx-linux.html)

Building Extempore depends on the following libraries (you can
probably get them through your favourite package manager).

- `LLVM 3.2`
- `pthread`
- `pcre` (Perl-compatible Regular Expressions)
- `portaudio v1.9`
- `mesa GL` (Linux only)

LLVM *must* be version 3.2 and needs to be built from source, because
a small patch to `LLParser.cpp` (an LLVM source file) is required. You
can get the LLVM v3.2 source from
http://llvm.org/releases/download.html#3.2. 

The patch file `llparser.patch` is supplied in the `extempore/extras`
directory.

```shell
$ cd /path/to/llvm/lib/AsmParser
$ patch < /path/to/extempore/extras/llparser.patch
```

After patching, building LLVM is the usual `./configure; make; make
install` process, with the addition of a `--prefix` location
specifying where everything will be installed into (such as a `Build`
directory)

```shell
$ cd /path/to/llvm
$ mkdir Build
$ ./configure --prefix=/path/to/llvm/Build --enable-optimized
$ make -j4
$ make install
```

Once all of the libraries required for your platform (see list above)
are installed, you can compile Extempore. First, point to your LLVM
build directory with the shell variable `EXT_LLVM_DIR`.

```shell
$ export EXT_LLVM_DIR=/path/to/llvm/Build
```

Then you can build extempore using the `all.bash` shell script

```shell
$ ./all.bash
```

Voila... you should be done.

# Installation on Windows 7

_More detailed instructions can be found_
[here](http://benswift.me/2013-03-20-building-extempore-on-windows.html)

Extempore has thus far only been tested on Windows 7 64-bit, but if you
can get it to build/run on other flavours of Windows then do let us
know and we'll update these instructions.  Also, if anyone wants to
improve and streamline the Windows build process, then patches are
welcome :)

On Windows 7, building Extempore requires the following libraries:
- `LLVM 3.2`
- `pcre`
- `boost v1.47`
- `portaudio v1.9`

Boost is *mandatory* on Windows, and you will need to
build pcre, boost, portaudio and LLVM 3.2 yourself.

To do this, the source directories will need to be on the 'include path':

- `llvm-3.2`
- `boost_1_47_0`
- `pcre-8.12`
- `portaudio`

Similarly, you'll need to make some directories for the results of
building the above libs

- `my_llvm_build` - use `cmake` to create then build using msvc
- `my_boost_build` - use `bjam` (see below `--build-dir=my_boost_build`)
- `my_pcre_build` - use `cmake` to create then build using msvc
- build `portaudio` using provided msvc project (`portaudio/build/msvc/portaudio.sln`)

LLVM *must* be version 3.2 and needs to be built from source, because
a small patch to `LLParser.cpp` (an LLVM source file) is required. The
patch file `llparser.patch` is supplied in the `extempore/extras`
directory. You can get the LLVM source from http://www.llvm.org/

The `extempore/extras/llparser.patch` file contains the patch, but
because GNU `patch` isn't installed by default on Windows patching
this file isn't quite as simple as it was on Linux/OS X. To apply the
patch on Windows there are a couple of options:

1. Get GNU patch in the form of `patch.exe`
([maybe also check out this advice](http://irq5.wordpress.com/2011/06/26/gnu-patch-and-windows-uac/))
and apply the patch in the same way as is described above in the 'Linux
& OS X section'
2. Apply the patch manually - look at the file
`lib/AsmParser/LLparser.cpp` in the LLVM source directory and the
patch file in a text editor and make the required changes by hand. A
bit cumbersome, but you only have to do it to *compile* extempore.

The extempore build process is pretty simple once you've built the
libraries. The extempore directory expects to find itself in the same
directory as all of the directories mentioned above, although you can
change all of this by manually editing
`extempore/msvc/extempore.vcxproj`.

If you have now built everything successfully you should be able to
build extempore from the command line.

First set the shell's vc/build environment vars

```
> cd extempore\msvc
> ms_build_vars.bat
```

You'll need a full version of Visual Studio to get the 64bit compiler
(`cl.exe`). Then you can build Extempore:
```
> cd extempore
> all.bat
```

*Additional Windows Build Notes*

- Use CMake to build pcre (`my_pcre_build`)
- Use CMake to Build LLVM (choose 64bit option - `my_llvm_build`)
- Make sure that Debug build mode has `_ITERATOR_DEBUG_LEVEL`=2 set for both
- `CMAKE_CXX_FLAGS_DEBUG` and `CMAKE_C_FLAGS_DEBUG` (click advanced checkbox in cmake gui)
- Make sure portaudio msvc general project settings are set to static lib
- boost command line for bjam is:
```
bjam --build-dir=<dir> toolset=msvc link=static address-model=64 variant=release --build-type=complete stage
```
- extempore msvs project settings needs `PCRE_STATIC` to be defined if you want to build against pcre static libs
- extempore msvs project needs `EXT_BOOST` to be defined
- `pcreposix.lib` needs to come before `pcre.lib` in link list
- You might need to add PA_WDMKS_NO_KSGUID_LIB if you compiling portaudio for 64bit.

# Running Extempore

You don't need to do anything special to run extempore, it'll just run
in any terminal (or command prompt on Windows).

```shell
$ ./extempore
```

The running extempore process acts as a server, and you send extempore
code to the server for evaluation (by default on port `7099`).Once
you've started extempore you can connect using either telnet, Emacs,
vim, sublime text, or whatever you like. The Emacs support is probably
the most mature at the moment, but patches are welcome for other
editors.

There are some optional command line options that you may want to use
you can get a list by running `./extempore --help`

For more detail, see this post on
[interacting with the Exempore compiler](http://benswift.me/2012-09-26-interacting-with-the-extempore-compiler.html).

**Telnet**

If using telnet the default extempore port to connect to is `7099`.

**Emacs**

Add this code to your `.emacs`

```elisp
(autoload 'extempore-mode "/path/to/extempore/extras/extempore.el" "" t)
(add-to-list 'auto-mode-alist '("\\.xtm$" . extempore-mode))
```

To start an Extempore session:

1. Open a shell buffer (`M-x shell`), `cd` to the extempore directory
and start it up with `./extempore`
2. Switch to an `.xtm` file (e.g. from the `examples`
subdirectory) or create a new one
3. You're away. `C-x C-x` will eval an expression and `C-x C-r` will
eval the region

*Version note*: the Emacs major mode (`extempore-mode`) only works
 with Emacs 24 because it inherits from `prog-mode`. If for some
 reason you have to stick on Emacs 23, you can use the Extempore minor
 mode (`extras/extempore-minor.el`).

*Windows note*: if you choose to use emacs on windows (recommended)
and run extempore in a `shell` or `eshell` then you should
set `--term ansi` as an option when starting extempore.

**Sublime Text**

You'll need the
[ST2 Extempore plugin](https://github.com/benswift/extempore-sublime),
which provides syntax highlighting and some commands for connecting to
and working with a running Extempore process. To install the plugin,
download the
[plugin files](https://github.com/benswift/extempore-sublime/zipball/master)
(or clone the repo) and unzip them into your
[ST2 packages directory](http://docs.sublimetext.info/en/latest/basic_concepts.html#the-packages-directory).

Then, to to start hacking on Extempore code in ST2:

1. open up your favourite shell (e.g. `Terminal.app` on OS X or
   `cmd.exe` on Windows)
2. start Extempore: `cd` into your Extempore directory and run
   `./extempore`
3. in ST2, open an Extempore file (the Extempore plugin should be
   loaded automatically when ST2 sees the `.xtm` file extension)
4. connect to the running Extempore process (`ctrl+x, ctrl+y`)

Then, to evaluate Extempore code, highlight the code you want to
evaluate and hit `extempore_evaluate` (which by default is mapped to
`ctrl+x ctrl+x`).

To restart the Extempore process, just `ctrl+c` in the shell where
`extempore` is running to kill it, then start it up again.

**Vim**

If you want to use (g)vim, see the instructions found in
`extras/extempore.vim`.

# Licence

Copyright (c) 2011-2013, Andrew Sorensen

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
