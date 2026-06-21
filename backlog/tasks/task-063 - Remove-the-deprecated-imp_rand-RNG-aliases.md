---
id: TASK-063
title: Remove the deprecated imp_rand* RNG aliases
status: To Do
assignee: []
created_date: "2026-06-09 03:29"
updated_date: "2026-06-09 04:07"
labels:
  - builtins
  - cleanup
  - deprecation
dependencies: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

TASK-061 renamed the RNG builtins imp_rand* -> xtc_rand* and kept the imp_rand*
spellings working as deprecated desugar-time aliases for ONE release. Remove
them next release: delete the imp_rand* -> xtc_rand* cases in
xtc:desugar:transform-atom (runtime/xtc-transforms.xtm, just after the
randomf->xtc_randf case) and the test-imp-rand-alias case in
tests/core/math.xtm. The randomf sugar, the xtc_rand* builtins, and the
test-xtc-rand test all stay.

<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria

<!-- AC:BEGIN -->

- [ ] #1 imp_rand\* desugar aliases removed from transform-atom
- [ ] #2 alias tests removed; xtc_rand\* tests stay green
<!-- AC:END -->
