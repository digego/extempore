---
id: TASK-055
title: 'Fix #315: pointer-depth loss binding generic type-variable tuple fields'
status: To Do
assignee: []
created_date: '2026-06-08 06:10'
updated_date: '2026-06-08 08:58'
labels:
  - xtlang
  - compiler
  - bug
dependencies: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
Phase 1 of the task-054 redesign (see backlog/docs/doc-001). Localised fix for #315 and its bug class (depth>=1 type-variable tuple fields absorb a spurious pointer level). Root cause: impc:ti:reverse-set-bangs-from-reified (runtime/llvmti-typecheck.xtm:1011-1043) binds a type variable to the whole concrete tuple field (e.g. i64*) instead of stripping the field's declared pointer depth first; the depth was lost at runtime/llvmir.xtm:591 where a bang-type field collapses to a bare symbol. Fix recovers the declared depth from the maximized generic type's field strings and strips it before binding. Closes the rehomed task-053 AC#4.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] #1 #315 repro pset! into generic tuple type-checks and runs
- [ ] #2 Bug class A (depth-2), B (typevar return type), C (multiple mixed-depth vars) compile correctly
- [ ] #3 Regression test added to the test harness covering the bug class
- [ ] #4 Full libs-core and libs-external suites stay green; stdlib (adt/std) still loads
- [ ] #5 No spurious pointer level: final specialised signature is [void,Test315{i64}*,i64,i64]*
<!-- AC:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
PROGRESS (canonical-core refactor). Built the isolated type core (runtime/llvmti-types.xtm: term algebra + Robinson unifier + occurs check + instantiation; 28 unit tests in tests/compiler/typecore.xtm) and the int-code<->term bridge (runtime/llvmti-types-bridge.xtm; 14 round-trip tests in tests/compiler/typebridge.xtm). Then fixed the generic-FUNCTION path of #315 (commit: typecheck: fix #315 ...): parser keeps the star (llvmir:591), and update-var normalises depth-bearing bare type variables at the single binding chokepoint via impc:type:reduce-ptr-depth (unifier-backed, so depth underflow fails cleanly instead of the -98). Validated: gsig315_use => 42 (was the #315 type error). All suites green (compiler-unit, libs-core, libs-external).

REMAINING: (1) the generic-TYPE path -- a depth-bearing type variable in a tuple field, e.g. bind-type <!a*,i64> -- still fails; my fix moves it past the pset! type error to 'could not resolve types' at the reification site (reverse-set-bangs-from-reified). Needs the same canonical treatment there. (2) task-057 (unrelated void-return codegen bug) blocks the void-pattern characterisation repros from becoming clean passing tests.
<!-- SECTION:NOTES:END -->
