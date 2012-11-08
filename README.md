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

# Documentation

Extempore's documentation index currently resides here:

- http://benswift.me/extempore-docs/index.html

## Installing Extempore

- [OSX, Linux](http://benswift.me/2012-11-06-building-extempore-on-osx-linux.html)
- [Windows](http://benswift.me/2012-11-05-building-extempore-on-windows.html).

## Using Extempore

- [The Extempore philosophy](http://benswift.me/2012-08-07-extempore-philosophy.html)
-
  [Interacting with the Exempore compiler](http://benswift.me/2012-09-26-interacting-with-the-extempore-compiler.html)
- [xtlang type reference](http://benswift.me/2012-08-09-xtlang-type-reference.html)

# Extempore in action

- http://vimeo.com/52964510 - interactive, distributed, physics simulation
- http://vimeo.com/37293927 - graphics demo
- http://vimeo.com/21956071 - general introduction
- http://vimeo.com/20502359 - a more technical intro

# Extempore's community

- [Extempore google group](http://groups.google.com/group/extemporelang)
- [Extempore mailing list](mailto:extemporelang@googlegroups.com)

# Licence

Copyright (c) 2011-2012, Andrew Sorensen

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
