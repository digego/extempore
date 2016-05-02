Installing Extempore
====================

When we talk about *installing* extempore, there are few steps to it:

1. building the ``extempore`` executable and putting it somewhere your
   system can find it (e.g. on your ``PATH``)

2. getting a bunch of helpful libraries which provide *extended*
   functionality (e.g. for audiofile I/O, FFTs, graphics)

3. ahead-of-time (AOT) compiling the Extempore standard library, so
   that things load up much quicker when you start Extempore

In general, it's nice to have all of these things, although
technically you really only need step 1 to start writing xtlang code.
Most of these instructions will do all three steps by default, with
comments about there are ways to tailor your install if you want
something different.

Download a pre-built binary
---------------------------

Download a `binary release`_, unzip it and run ``extempore.exe``
from inside the ``extempore`` folder.

.. note:: If you're not planning to make changes to Extempore itself,
          then downloading a binary is probably the best option. You
          can still write your own xtlang code, run the examples, etc.

.. _binary release: https://github.com/digego/extempore/releases

.. _build-from-source-doc:

Build from source
-----------------

The build-from-source workflow will download and build all the
dependencies you need (including LLVM). So, if you've got a C++
compiler, git and CMake (and an :ref:`ALSA backend
<linux-portaudio-backend-instructions>` on Linux) then here are some
one-liner build commands:

On **Linux/OSX**::

    git clone https://github.com/digego/extempore --branch 0.7.0 && mkdir extempore/cmake-build && cd extempore/cmake-build && cmake .. && make install

On **Windows**::

    git clone https://github.com/digego/extempore --branch 0.7.0 && mkdir extempore/cmake-build && cd extempore/cmake-build && cmake -G"Visual Studio 14 2015 Win64" .. && cmake --build . --target ALL_BUILD --config Release

If you have problems, check out the :ref:`platform-specific notes
below <platform-specific-build-docs>`.

Options, Variables and Targets
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Some info about a few of the more interesting CMake options, variables
and build targets:

CMake options
"""""""""""""

BUILD_DEPS:BOOL (default ON)
  controls whether or not the build process will also download and
  build the extended libraries (e.g. glfw, libsndfile). If you want to
  get those things through another package manager (or not use them at
  all) then set this to ``OFF``.

PACKAGE:BOOL (default ON)
  when true, build Extempore (including all extended deps and
  AOT-compilation) for binary distribution, e.g.::

    cmake -DPACKAGE=ON .. && make package

  The main effect of this flag is to disable processor-specific
  optimisations for all the build commands, which gives you the best
  chance of producing a portable binary. This option also adds a
  ``package`` target to the makefile/solution, which can be used to
  actually generate the zipfile/dmg/deb.

Variables
"""""""""

EXT_LLVM_DIR (environment variable)
  in the bad old days, the Extempore build process relied on this
  (system) environment variable a lot, but currently it's only useful
  if you want to build LLVM yourself (rather than having it done
  automatically) - so unless you want to do that you can just ignore
  it.

Targets
"""""""

The default target will build Extempore, all the dependencies, and
AOT-compile the standard library. Still, in other situations the
following targets might come in handy:

install
  On OSX/Linux, move the extempore executable to ``/usr/local/bin``
  (or similar) and the rest of the Extempore share directory to
  ``/usr/local/share/extempore``. Does nothing on Windows.

aot_extended
  AOT-compile the full standard library

aot
  AOT-compile just the core standard library - i.e. the pure-xtlang
  libraries with no external C library dependencies

clean_aot
  remove all AOT-compiled files

extempore
  build just the extempore executable

assets
  download the assets e.g. sound files, 3D model files which are
  referenced in the examples

.. _platform-specific-build-docs:

Platform-specific notes
^^^^^^^^^^^^^^^^^^^^^^^

OSX
"""

Extempore should build with clang or gcc, the easiest way to get these
is through Xcode or the `command line tools`_.

.. _command line tools: https://developer.apple.com/library/ios/technotes/tn2339/_index.html#//apple_ref/doc/uid/DTS40014588-CH1-WHAT_IS_THE_COMMAND_LINE_TOOLS_PACKAGE_

Linux
"""""

Extempore is most tested on Ubuntu, but is also known to work with
Debian, Fedora, Arch, and also inside a docker container.

There are a few extra dependencies which you may need to get through
your package manager. For example, on Ubuntu 16.04 I needed::

  sudo apt-get install libasound2-dev xorg-dev libglu1-mesa-dev zlib1g-dev

.. _linux-portaudio-backend-instructions:

You'll also need to specify an `ALSA`_ backend for portaudio.

ALSA
  To use the asound portaudio backend (the default) you'll need the
  libasound package.

Jack
  To use the `Jack`_ portaudio backend, you'll need to have Jack
  installed, and then to set the ``JACK`` CMake option with ``-DJACK=ON``.
    
.. _ALSA: http://www.alsa-project.org/
.. _Jack: http://www.jackaudio.org/


Windows
"""""""

Extempore has been tested on Windows 7 & Windows 10 with `Visual Studio Community 2015`_

.. _Visual Studio Community 2015: https://www.visualstudio.com/en-us/products/visual-studio-community-vs.aspx

If you don't want to take the command-line described above, note that
CMake generates a Visual Studio solution (``.sln``), so just open that
and build the ``ALL_BUILD`` target.

On Windows, Extempore requires a few components of the **Boost** 1.59
libs for TCP/UDP handling. These will be automatically downloaded for
you if you've got the `nuget command line tool`_ installed, or if you
want to build boost yourself (remember to keep the labyrinthine boost
directory structure intact) just tell the build process where it is
using the ``BOOST_DIR`` CMake variable.

.. _nuget command line tool: choco install nuget.commandline

If you want to use the **ASIO** audio backend on Windows (which might
give you lower-latency audio, but is not essential) you need to
download the `ASIO SDK`_ from Steinberg. You have to create a `third
party developer account`_, then you can log in and download the ASIO
SDK (make sure you get the right SDK). You also need to download and
install `ASIO4ALL`_ with the 'offline setup panel' option enabled.
After that, copy the ASIO files into the
``src/portaudio/src/hostapi/asio``, and use the ``-DASIO=ON`` CMake
option.

.. _third party developer account: http://www.steinberg.net/nc/en/company/developer/sdk_download_portal/create_3rd_party_developer_account.html
.. _ASIO SDK: http://www.steinberg.net/nc/en/company/developer/sdk_download_portal.html
.. _ASIO4ALL: http://www.asio4all.com/

The one caveat to the "extempore will download and build all the
extended dependencies you need" is the **libsndfile** ``.lib`` and
``.dll``. Currentyl, you need to manually get them from here `here`_
and move them into ``libs/platform-shlibs`` (or if anyone can figure
out how to build a 64-bit libsndfile on Windows in a sane way then let
me know).

.. _here: http://www.mega-nerd.com/libsndfile/#Download
.. _install-extended-doc:

Shared library dependencies
---------------------------

The full list of external libraries required for the Extempore
standard library are:

* apr 1.5.2
* assimp 3.2
* expat 2.1.0
* glfw3 3.1.2
* kiss_fft 1.3.0
* nanovg
* portmidi 217
* sndfile 1.0.26
* stb_image

For those who are interested, there are a few reasons that Extempore
builds and maintains its own "world" of shared lib dependencies:

#. there's no lib path on **Windows**, so it's up to each program to
   make sure it ships with the dlls it needs (and knows where to find
   them)

#. library **versioning** is a bit of a mess at the best of times, so
   by building specific versions of e.g. GLFW or assimp, Extempore can
   guarantee that the bindings will work - otherwise you'll get weird
   errors.

#. for **packaging** (as described above in the note on the
   ``PACKAGE`` variable) it's important to turn off all cpu-specific
   optimisations, and set various compiler flags. Packages installed
   through a package manager don't do this, and so when building e.g.
   a dmg for distribution on OSX it's necessary to make sure any
   compiled objects (including dependencies and Extempore itself) have
   been compiled with the right flags.

Still, if you're keen to get these libraries some other way (e.g.
through your system-provided package manager) then that's fine - just
use ``-DBUILD_DEPS=OFF``.

LLVM 3.7.0
----------

If you don't have an ``EXT_LLVM_DIR`` environment variable set on your
system, then Extempore will download, patch and build LLVM 3.7.0 for
you as part of the ``make extempore`` step. However, if you do want to
build it yourself, then here's how.

Grab the `3.7.0 source tarball`_, apply the
``extempore-llvm-3.7.0.patch`` in ``extras/``::

    cd /path/to/llvm-3.7.0.src
    patch -p0 < /path/to/extempore/extras/extempore-llvm-3.7.0.patch

.. _3.7.0 source tarball: http://llvm.org/releases/download.html#3.7.0

On **Windows**, the ``<`` redirection will work with ``cmd.exe``, but
not PowerShell.

Then build LLVM, moving the libraries into ``/path/to/extempore/llvm``
as part of the ``install`` step::

    mkdir cmake-build && cd cmake-build
    cmake -DCMAKE_BUILD_TYPE=Release -DLLVM_TARGETS_TO_BUILD=X86 -DLLVM_ENABLE_TERMINFO=OFF -DLLVM_ENABLE_ZLIB=OFF -DCMAKE_INSTALL_PREFIX=c:/path/to/extempore/llvm .. && make && make install

On **Windows**, you'll also need to specify a 64-bit generator e.g.
``-G"Visual Studio 14 2015 Win64"``

To build, open the ``Extempore.sln`` file and build the ``ALL_BUILD``
target, then the ``INSTALL`` target. If the install step doesn't work,
you can try directly calling ``cmake -P cmake_install.cmake`` which
should be in the same directory. On Windows, the LLVM build output must
be installed into an ``llvm`` subdirectory in the top-level Extempore
directory (since the AOT compilation process will look in there to find
``llc``).

If LLVM complains about not being able to find python, you can specify a
path to your python executable with the PYTHON\_EXECUTABLE CMake
variable::

    cmake -DCMAKE_BUILD_TYPE=Release -DLLVM_TARGETS_TO_BUILD=X86 -DLLVM_ENABLE_TERMINFO=OFF -DLLVM_ENABLE_ZLIB=OFF -DCMAKE_INSTALL_PREFIX=c:/path/to/extempore/llvm -DPYTHON_EXECUTABLE=c:/path/to/python .. && make && make install

If you **do** build your own patched version of LLVM for Extempore,
then make sure you set the ``EXT_LLVM_DIR`` environment variable to
point to that directory (where you installed LLVM) so that the
Extempore build process knows where to find it.

Install through homebrew (OSX-only)
-----------------------------------

.. note:: As of v0.7.0, the homebrew install process is
          deprecated---since it's caught in a weird no-man's land
          between the easy "binary download" and the flexible "build
          from source" approach. Still, the formula is currently still
          in my tap, so these instructions are here for the moment.

`Homebrew`_ makes the process pretty simple, although since it's
building everything (including LLVM) from source it may still take up to
15mins depending on your machine.

.. _Homebrew: http://brew.sh/

To install Extempore through homebrew, first::

    brew tap benswift/extempore && brew tap homebrew/versions

then::

    brew install extempore --with-extended

or, if you want just the core::

    brew install extempore

**Caveats**

If you've installed Extempore through homebrew previously (i.e. if
``brew info extempore`` shows a version <= 0.59) then you'll need to
remove a couple of things first::

    brew rm extempore kissfft libnanovg

If you're on OSX 10.9 or earlier, there's an incompatibility with your
version of clang and the LLVM 3.7.0 which Extempore uses. For the
moment the easiest way around this is to download the old 0.5.9
release of Extempore with::

    brew tap benswift/extempore
    brew install extempore059 --with-extended
