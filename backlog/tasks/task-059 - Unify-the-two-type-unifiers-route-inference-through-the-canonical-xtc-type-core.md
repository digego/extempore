---
id: TASK-059
title: >-
  Unify the two type unifiers: route inference through the canonical xtc:type:
  core
status: In Progress
assignee: []
created_date: '2026-06-09 01:35'
updated_date: '2026-06-09 03:30'
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
- [x] #1 Old behaviour (incl. union-type simplification/pruning) characterised by tests before any change
- [ ] #2 All xtc:desugar:type-unify/complex-unify/unify-lists/sym-unify/occurs-in-type?/unity? sites migrated or removed
- [ ] #3 Live inference unifies via xtc:type:unify + the int-code/pretty bridges
- [ ] #4 No regression in generic/poly inference (esp. the #315 pointer-depth class); full suite green
<!-- AC:END -->

## Implementation Plan

<!-- SECTION:PLAN:BEGIN -->
Strangler migration in green increments (Ben chose 'full rewrite': delete the candidate-list union-find, number-crunch, and enumeration retry loop wholesale; route inference through the canonical core).

DONE:
- Increment 0 (AC#1, commit 7c62fe30): froze the old candidate-list unifier's pruning contract in tests/compiler/typeunify.xtm.
- Increment 1 (commit e8b8a404): runtime/xtc-solve.xtm --- the constraint-solving engine over the canonical core (xtc:type:unify). Splits the old conflated candidate list into equality (Robinson unify), numeric default (one post-unification rule), overload (deferred choice). Proven in tests/compiler/solve.xtm incl. #315 pointer depth and 'downstream constraint beats default' (subsumes the retry loop). Loaded live via xtc-globals.xtm but inert.

NEXT (the bulk, multi-session):
- Increment 2: wire the ~60 form-checkers in xtc:typecheck:type-check (runtime/xtc-typecheck.xtm:3103) to emit clean eq/default constraints instead of mutating the candidate-list vars store; shadow-validate the new solve against the old one across the suite. Output contract to preserve: run-type-check (xtc-typecheck.xtm:3282) returns a (symbol . int-code) alist; codegen reads only that; vars-store mutation is internal. Non-Robinson semantics to replace principled: numeric defaulting (numeric-check ~351 + lambda-check ~2809 apply-min), overload (nativef-poly-check ~1844), generic instantiation (nativef-generics ~1327 + reify-generic-type --- KEEP the _poly_ reification, codegen depends on it). Dead scaffolding to delete: emit-constraint!/solve-constraints (recorded, never solved).
- Increment 3: flip run-type-check* (xtc-typecheck.xtm:3300) to the new solve.
- Increment 4 (AC#2): delete the six functions (type-unify/complex-unify/unify-lists/sym-unify/occurs-in-type?/unity?) + the retry loop.
<!-- SECTION:PLAN:END -->
