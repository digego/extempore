Extempore wishlist
==================

Building a new programming language, runtime and ecosystem is a multifaceted
job. Here are a few projects (some small, some not so small) which would be
really nice---if you think you'd like to contribute, give us a shout out on the
`mailing list`_.

.. _mailing list: mailto:extemporelang@googlegroups.com


Core
----

These projects involve hacking on the Extempore binary itself.

1. upgrade to LLVM 5.0 & ORCJIT

2. port Extempore to 64-bit ARM (``aarch64``)

xtlang
------

These projects (mostly) involve adding/improving libraries for doing cool things in
xtlang.

1. add animation to the graphics pipeline

2. a 2D/3D hardware-accelerated data visualisation library (e.g. a vega-lite for
   Extempore)

3. add DirectX (or perhaps Vulkan) support

Ecosystem
---------

These projects are "ecosystem/tooling" projects.

1. add xtlang support to highlight.js (shouldn't be *too* difficult, you can
   basically copy the parsing regexes from the Atom plugin)

2. a SWIG (or similar) wrapper generator to automatically generate the xtlang
   ``bind-lib`` definitions

3. improve the main Extempore website (http://extempore.moso.com.au),
   potentially rolling this docs website into the main site

4. add an xtlang package manager (e.g. CPAN or cargo for Extempore)

5. set up Jenkins (or CTest, or whatever) build & test servers for Windows,
   macOS and Linux to create nightly builds and run the test suite

6. make the CMake build process aware of the xtlang ahead-of-time compilation
   process, so that ``make aot`` only re-aot-compiles an xtlang library if it
   has changed
