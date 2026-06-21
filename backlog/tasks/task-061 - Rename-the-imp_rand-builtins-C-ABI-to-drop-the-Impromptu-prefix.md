---
id: TASK-061
title: Rename the imp_rand* builtins (C++ ABI) to drop the Impromptu prefix
status: Done
assignee: []
created_date: "2026-06-09 01:35"
updated_date: "2026-06-09 03:29"
labels:
  - builtins
  - abi
  - cleanup
dependencies: []
priority: low
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

Context: imp*rand / imp_randf / imp_randd / imp_rand1*_ / imp*rand2*_ are xtlang
RNG builtins still carrying the Impromptu prefix. Deferred from the impc->xtc
rename because, unlike the impc: Scheme namespace, these cross the C++/ABI
boundary AND are a public xtlang API.

Touch points (rename in lockstep): src/EXTLLVM.cpp (impl), include/EXTLLVM.h
(decl), runtime/bitcode.ll + src/ffi/number.inc (registration),
runtime/xtc-transforms.xtm (xtc:desugar:transform-atom maps randomf->imp_randf),
runtime/xtc-caches.xtm (builtin cache), libs/base/base.xtm (xtlang defs),
tests/core/builtins.xtm. Forces an EXTLLVM recompile (not just runtime reload).

Decide compatibility: hard break to xtc_rand\* vs keep old names as deprecated
aliases for one release (users may call these directly).

<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria

<!-- AC:BEGIN -->

- [x] #1 Compatibility decision (break vs deprecated alias) made and documented
- [x] #2 Renamed in lockstep across EXTLLVM.cpp/.h, bitcode.ll, number.inc,
      transform-atom, caches, base.xtm, tests
- [x] #3 EXTLLVM rebuilt; full suite green
<!-- AC:END -->
