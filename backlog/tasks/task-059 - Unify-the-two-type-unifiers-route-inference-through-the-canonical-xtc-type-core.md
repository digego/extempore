---
id: TASK-059
title: >-
  Unify the two type unifiers: route inference through the canonical xtc:type:
  core
status: To Do
assignee: []
created_date: '2026-06-09 01:35'
labels:
  - compiler
  - types
  - refactor
dependencies: []
priority: medium
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
After the recent type-system modernisation the compiler has TWO coexisting unifiers.

OLD (string/int-code): xtc:desugar:type-unify plus complex-unify / unify-lists / sym-unify / occurs-in-type? / unity? in runtime/xtc-transforms.xtm. ~86 call sites of type-unify alone, woven through the type-checker. Does xtlang-specific union-type simplification/pruning, not just Robinson unification.

NEW (canonical): xtc:type:unify in runtime/xtc-types.xtm — a clean Robinson unifier over canonical type-terms, with from-intcode/to-intcode and from-pretty/to-pretty bridges in xtc-types-bridge.xtm. Built and unit-tested (tests/compiler/typecore,typeunify,typebridge) but wired into the live inference pipeline at essentially zero sites.

Goal: route all unification through the canonical core and delete the old family. This is a type-system migration, NOT a mechanical rename: either convert old-representation types to canonical terms at each call site via the bridges, or extend the canonical core to subsume the union-simplification the old path relies on. Characterise the old behaviour with tests FIRST, then migrate.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] #1 Old behaviour (incl. union-type simplification/pruning) characterised by tests before any change
- [ ] #2 All xtc:desugar:type-unify/complex-unify/unify-lists/sym-unify/occurs-in-type?/unity? sites migrated or removed
- [ ] #3 Live inference unifies via xtc:type:unify + the int-code/pretty bridges
- [ ] #4 No regression in generic/poly inference (esp. the #315 pointer-depth class); full suite green
<!-- AC:END -->
