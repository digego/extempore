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

# Installation

*These are the quick-start options---see* [INSTALL.md](./INSTALL.md)
*for details in how to build from source.*

## OSX

If you're a [homebrew](http://brew.sh/) user, it's pretty
straightforward:

```shell
brew tap benswift/extempore
brew install extempore --with-extended
```

If you get stuck, or want to find out more about the installation
instructions, there are more detailed instructions
[here](http://benswift.me/2013/11/12/building-extempore-through-homebrew/).

## Linux

On Linux, you can you can [build from source](./INSTALL.md)

## Windows 7

Windows installation instructions are provided in
[INSTALL.md](./INSTALL.md) as well.

<!-- TODO link to binary -->

# Running Extempore

You don't need to do anything special to run extempore, it'll just run
in any terminal (or command prompt on Windows).

```shell
$ extempore
```

The running extempore process acts as a server, and you send extempore
code to the server for evaluation (by default on port `7099`).Once
you've started extempore you can connect using either telnet,
[Emacs](http://benswift.me/2012/10/10/extempore-emacs-cheat-sheet/),
[vim](http://benswift.me/2014/11/07/hacking-extempore-in-vim/),
[Sublime Text](http://benswift.me/2012/10/23/extempore-st2-cheat-sheet/),
or whatever you like. The Emacs support is probably the most mature at
the moment, but patches are welcome for other editors.

There are some optional command line options that you may want to use
you can get a list by running `extempore --help`

For more detail, see this post on
[interacting with the Extempore compiler](http://benswift.me/2012-09-26-interacting-with-the-extempore-compiler.html).

# Docs & Community

More Extempore documentation can be found at http://benswift.me/extempore-docs/index.html

You can also join the Extempore community:

- [Extempore google group](http://groups.google.com/group/extemporelang)
- [Extempore mailing list](mailto:extemporelang@googlegroups.com)

# Licence

Copyright (c) 2011-2015, Andrew Sorensen

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
