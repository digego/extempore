Installing Extempore
====================

Quick install
-------------

OSX (via homebrew)
^^^^^^^^^^^^^^^^^^

`Homebrew`_ makes the process pretty simple, although since it's
building everything (including LLVM) from source it may still take up to
15mins depending on your machine.

.. _Homebrew: http://brew.sh/

Extempore has a "core" library, which includes things like the math and
audio DSP libraries and doesn't depend on any external shared libraries.
This is the option homebrew will use by default - it's still a
fully-fledged xtlang compiler and everything else.

However, there are a few external libraries which you might want to use
to do certain things, like open up OpenGL canvases or load compressed
audio files. We call this the "extended" library, and there's a
``--with-extended`` flag to tell homebrew to go and grab those other
packages as well.

To install Extempore through homebrew, first::

    brew tap benswift/extempore && brew tap homebrew/versions

then::

    brew install extempore

or, if you want the "extended" libs (e.g. graphics)::

    brew install extempore --with-extended

The ``homebrew/versions`` tap is needed for ``glfw3`` (required to build
the extended stdlib) while the ``benswift/extempore`` tap is contains
Extempore itself (and a few other deps)

**Caveats**

If you've installed Extempore through homebrew previously (i.e. if
``brew info extempore`` shows a version <= 0.59) then you'll need to
remove a couple of things first::

    brew rm extempore kissfft libnanovg

If you're on OSX **10.9** or earlier, there's an incompatibility with
your version of clang and the LLVM 3.7.0 which Extempore uses. For the
moment the easiest way around this is to download the old ``0.5.9``
version of Extempore with::

    brew tap benswift/extempore
    brew install extempore059

again, if you want the "extended" libs (e.g. graphics)::

    brew install extempore059 --with-extended

Build from source (Linux/OSX)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If you've got ``git``, ``cmake`` and a C++ compiler toolchain installed,
then you can build Extempore with::

    git clone https://github.com/digego/extempore && mkdir extempore/cmake-build && cd extempore/cmake-build && cmake .. && make install && make aot

Windows
^^^^^^^

Download a `precompiled binary`_, unzip it and run ``extempore.exe``
from inside the ``extempore`` folder.

.. warning:: The Windows binary is currently a little out-of-date, so
             some of the docs may be wrong. If you've got experience
             doing packaging/distribution on Windows and want to help
             out (please!) `get in touch`_.

.. _precompiled binary: http://extempore.moso.com.au/extras/Extempore-0.6.0-win64.zip
.. _get in touch: mailto:extemporelang@googlegroups.com

Slow install
------------

Here are some more detailed instructions---in case the quick install
doesn't suit your needs or you just want to have a better idea of
what's going on.

You'll need

a **C++ compiler toolchain**, e.g.

-  ``sudo apt-get install g++`` on Ubuntu/Debian
-  ``sudo yum install gcc gcc-c++`` on Fedora/CentOS/RHEL
-  Xcode or the `command line tools`_ on OSX
-  Visual Studio on Windows (the `Community 2015`_ version is now free)

.. _command line tools: https://developer.apple.com/library/ios/technotes/tn2339/_index.html#//apple_ref/doc/uid/DTS40014588-CH1-WHAT_IS_THE_COMMAND_LINE_TOOLS_PACKAGE_   
.. _Community 2015: https://www.visualstudio.com/en-us/products/visual-studio-community-vs.aspx

Extempore should build with clang, gcc and MSVC - and possibly other
compilers (but I haven't tried them).

**git**

-  ``sudo apt-get install git`` on Ubuntu/Debian
-  ``sudo yum install git`` on Fedora/CentOS/RHEL
-  ``brew install git`` on OSX with Homebrew
-  ``choco install git`` on Windows with Chocolatey

**CMake** (version 3.1 or greater)

-  ``sudo yum install cmake`` on Fedora/CentOS/RHEL
-  ``brew install cmake`` on OSX with Homebrew
-  ``choco install cmake`` on Windows with Chocolatey

The Ubuntu 15.04 package archive only includes CMake v3.0, but you can
get a more up-to-date version through a package archive::

    sudo apt-get install software-properties-common && sudo add-apt-repository ppa:george-edison55/cmake-3.x && sudo apt-get update && sudo apt-get install cmake

**ALSA** (Linux only)

To use the `ALSA`_ portaudio backend (which is probably what you want,
unless you have a real reason to go with something else) you'll need the
libasound package at build-time, e.g. (on Ubuntu)::

    sudo apt-get install libasound2-dev

.. _ALSA: http://www.alsa-project.org/

**Jack** (Linux only)

To use the `Jack`_ portaudio backend, you'll need to have Jack
installed, and then to set the ``JACK`` cmake option with ``-DJACK=ON``

.. _Jack: http://www.jackaudio.org/

**Boost** (Windows only)

We still need one component of the **Boost** libs on Windows
(specifically the ASIO component for TCP/UDP handling). If you've got
the NuGet command line client installed, you can probably do::

    nuget install boost-vc140 & nuget install boost_system-vc140 & nuget install boost_regex-vc140 & nuget install boost_date_time-vc140

It doesn't matter how you get these deps or where you put them, as long
as you tell Extempore where they are through the ``BOOST_DIR`` cmake
variable. The ``BOOST_DIR`` should have two subdirectories ``include``
and ``lib``, which should contain the boost header directory and the
``libboost*.lib`` files respectively.

**LLVM 3.7.0**

As of ``21e750a``, downloading and building LLVM 3.7 happens
automatically as part of the Extempore cmake build process. But
instructions are included at the end of this file in case you want to do
it yourself.

.. _install-configure-doc:

Configure
^^^^^^^^^

Extempore uses CMake for configuration. In your ``extempore`` directory
(i.e. the one this ``INSTALL.md`` file is in)::

    mkdir cmake-build && cd cmake-build && cmake ..

On **Windows**, you'll need to give CMake a few more details about where
Boost is::

    md cmake-build && cd cmake-build
    cmake -G"Visual Studio 14 2015 Win64" -DBOOST_DIR=c:\path\to\extempore\boost ..

Make & Install
^^^^^^^^^^^^^^

On **Linux/OSX** CMake will generate a ``Makefile`` in ``cmake-build``,
with a few useful targets:

-  ``make`` will build Extempore (if you have a multicore machine, you
   can try e.g. ``make -j4`` to parallelize the ``make`` step,
   especially since LLVM takes so long to build)
-  ``make install`` will install ``extempore`` into ``/usr/local/bin``
-  ``make uninstall`` will remove the installed files
-  ``make aot``/``make aot_extended`` will ahead-of-time compile the
   core/extended "standard library"

On **Windows**, CMake will generate a Visual Studio solution (``.sln``)
in ``cmake-build``. Open it, and build the ``extempore`` target.

.. _install-extended-doc:

"Extended" shared libs
----------------------

Extempore is all about being dynamic and adding functionality
on-the-fly. As a result, there are a bunch of helpful libraries which
provide *extended* functionality (e.g. for sound file IO, FFTs,
graphics) which we use a lot, but which aren't compiled statically
into the ``extempore`` executable. Instead, we load this code at
runtime through shared libraries (``.dylib`` on OSX, ``.so`` on Linux
and ``.dll`` on Windows).

This means that you have to have these shared libraries on your system
somewhere where Extempore can find them.

On **OSX** you can get them through homebrew (assuming you've done a
``brew tap benswift/extempore``)::

    brew install assimp libsndfile portmidi libkiss-fft glfw3 libstb-image libnanovg

On **Debian/Ubuntu** you can use ``apt-get``::

    sudo apt-get install libasound2-dev libgl1-mesa-dev libsndfile1-dev libassimp3 libglfw3 libportmidi-dev

You'll have to build KissFFT, stb\_image and nanovg yourself, using the
instructions below, but ``make install`` step means that you won't have
to move anything into ``libs/platform-shlibs``.

On **Windows**, there isn't a package manager which will do the job so
you'll need to build from source. Since Windows doesn't have a lib path,
all the dlls should go in ``c:/path/to/extempore/libs/platform-shlibs``.
So for all these deps, move the dll in there when it's done.

Build extended dependencies from source
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If you want to build these extended dependencies from source,
Extempore can do that automatically as part of the build process. In
the :ref:`cmake configure step <install-configure-doc>` specify an
additional ``-DEXTENDED_DEPS=ON`` option, and the build process will
build them all (this will take a while) and put them in the
``extempore/libs/platform-shlibs`` directory.

AOT-compiling the Extempore standard library
--------------------------------------------

This step isn't necessary, but it will make some common Extempore
libraries load up much faster. There are a few ways to do this, but
the easiest way is to use the ``aot`` target generated by the CMake
configure process::

    cd extempore/cmake-build # or wherever your Extempore build dir is
    make aot

If you want the :ref:`extended <install-extended-doc>` Extempore
standard library, then use the ``make aot_extended`` target instead.

To remove the AOT-compiled files, use the ``clean_aot`` target in the
makefile or MSVS project.

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

Packaging
---------

*Note: this is still experimental - things may not work, but
patches/suggestions welcome!*

To build a "package" for binary distribution, use the ``-DPACKAGE=ON``
cmake option.

OSX
^^^

::

    cmake -DPACKAGE=ON .. && make -j8 aot_extended && make package

Windows
^^^^^^^

::

    cmake -G"Visual Studio 14 2015 Win64" -DPACKAGE=ON -DBOOST_DIR=c:/Users/ben/Code/extempore/boost .. && cmake --build . --config Release --target aot_extended && cmake --build . --config Release --target package

Linux
^^^^^

TODO - investigate the CPack Debian package generator.
