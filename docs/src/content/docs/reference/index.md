---
title: Reference
---

This section is the reference material for Extempore's two languages---Scheme
and xtlang---and for the systems that sit between them.

If you're new, start with the [quickstart](../overview/quickstart.md) and the
[philosophy](../overview/philosophy.md) pages. Once the quickstart is working,
the [tutorial](./tutorial.md) walks through xtlang's core ideas with runnable
examples. Come back to the rest of this section when you want to look up how a
particular piece works.

## xtlang language

- [Tutorial](./tutorial.md) --- a hands-on walk through `bind-func`, types,
  closures, and memory zones; every snippet is verified to run.
- [Types](./types.md) --- the xtlang type system: primitive types, tuples,
  arrays, pointers, named types, and how they compose.
- [Type inferencing](./type-inference.md) --- how xtlang infers types when you
  leave them off, and when you need to annotate.
- [Memory management](./memory-management.md) --- stack, zone and heap
  allocation, and when to reach for each.
- [Error messages](./error-messages.md) --- common compiler and JIT error
  messages, what they mean, and how to fix them.
- [Concurrency](./concurrency.md) --- threads, the audio thread, and safely
  sharing state between them.
- [Docstrings](./docstrings.md) --- attaching documentation to xtlang
  definitions.

## Interop

- [Scheme-xtlang interop](./scheme-xtlang-interop.md) --- how Scheme and xtlang
  code call each other, and where the boundary sits.
- [C-xtlang interop](./c-xtlang-interop.md) --- binding to C libraries from
  xtlang.

## Tooling

- [Testing](./testing.md) --- the `xtmtest` harness used across the standard
  library and examples.
