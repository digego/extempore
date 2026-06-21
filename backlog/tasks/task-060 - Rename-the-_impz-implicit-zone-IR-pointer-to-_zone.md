---
id: TASK-060
title: Rename the _impz implicit-zone IR pointer to _zone
status: Done
assignee: []
created_date: "2026-06-09 01:35"
updated_date: "2026-06-09 03:10"
labels:
  - codegen
  - cleanup
dependencies: []
priority: low
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

Context: \_impz / %\_impz / %\_impzPtr is the implicit memory-zone pointer
threaded as the first parameter through every generated closure body — an
Impromptu-era name. Deferred from the impc->xtc rename because it is an LLVM-IR
string token, not a Scheme symbol.

HAZARD (why it was deferred): a naive global \_impz->\_zone self-collides.
runtime/xtc-bind.xtm emits IR like %\_impz = bitcast %mzone* %\_zone to i8*
where %\_zone already exists as a distinct local; the blunt rename yields
%\_zone = ... %\_zone which is invalid LLVM IR. Also present in runtime/init.ll
and the xtc-codegen.xtm emitters.

Do it by hand: pick a non-colliding name (e.g. \_mzone or \_zoneptr) or
carefully rename in init.ll plus the bind/codegen emitter strings, verifying no
%X = ... %X results. Self-contained (no C++/Scheme-namespace dependency).
Regenerate the AOT cache afterwards.

<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria

<!-- AC:BEGIN -->

- [x] #1 No self-referential IR produced (no %X = ... %X)
- [x] #2 \_impz renamed consistently across init.ll and all IR emitters; AOT
      cache regenerated
- [x] #3 Full suite green incl. audio-offline + aot-compilation
<!-- AC:END -->
