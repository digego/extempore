---
title: Reference
---

This section is the reference material for Extempore's two languages---Scheme
and xtlang---and for the systems that sit between them.

If you're new, start with the [quickstart](/overview/quickstart/) and the
[philosophy](/overview/philosophy/) pages. Once the quickstart is working, the
[tutorial](/reference/tutorial/) walks through xtlang's core ideas with runnable
examples. Come back to the rest of this section when you want to look up how a
particular piece works.

## xtlang language

- [Tutorial](/reference/tutorial/) --- a hands-on walk through `bind-func`,
  types, closures, and memory zones; every snippet is verified to run.
- [Types](/reference/types/) --- the xtlang type system: primitive types,
  tuples, arrays, pointers, named types, and how they compose.
- [Type inferencing](/reference/type-inference/) --- how xtlang infers types
  when you leave them off, and when you need to annotate.
- [Memory management](/reference/memory-management/) --- stack, zone and heap
  allocation, and when to reach for each.
- [Error messages](/reference/error-messages/) --- common compiler and JIT error
  messages, what they mean, and how to fix them.
- [Concurrency](/reference/concurrency/) --- threads, the audio thread, and
  safely sharing state between them.
- [Docstrings](/reference/docstrings/) --- attaching documentation to xtlang
  definitions.

## Interop

- [Scheme-xtlang interop](/reference/scheme-xtlang-interop/) --- how Scheme and
  xtlang code call each other, and where the boundary sits.
- [C-xtlang interop](/reference/c-xtlang-interop/) --- binding to C libraries
  from xtlang.

## Tooling

- [Testing](/reference/testing/) --- the `xtmtest` harness used across the
  standard library and examples.
