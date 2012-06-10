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

# Building extempore

Extempore currently builds on OSX, Linux and Windows 7. See
`INSTALL.md` for build instructions.

# Current status

Extempore is functional although subject to substantial ongoing design
and development.  In practice this means that the language is stable
enough for experimentation but not yet for large project work.

# Licence

Extempore is released under a BSD style licence.

# Links

http://groups.google.com/group/extemporelang.
http://vimeo.com/37293927 - graphics demo
http://vimeo.com/21956071 - general introduction
http://vimeo.com/20502359 - a more technical intro
