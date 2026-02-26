---
id: TASK-035
title: Add compiler-internal unit tests for xtlang compiler passes
status: Done
assignee: []
created_date: '2026-02-26 09:44'
updated_date: '2026-02-26 09:44'
labels:
  - compiler
  - testing
dependencies:
  - TASK-033
priority: medium
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
Currently all compiler tests are end-to-end (.xtm files that compile and run). Add unit tests for individual compiler passes: first-transform desugaring, type unification, type-check on small expressions, and IR generation for individual constructs. These tests provide a safety net for subsequent refactoring (especially the vars-threading change) and document expected compiler behaviour.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [x] #1 Test file exists for first-transform with at least 10 desugaring cases (and/or/cond, println, n-ary operators, dot notation)
- [x] #2 Test file exists for type unification with at least 8 cases (simple types, closures, tuples, pointers, failure cases)
- [x] #3 Test file exists for type-check on small expressions (literals, let, lambda, if, arithmetic)
- [x] #4 Tests runnable via ctest with a new label (e.g. compiler-unit)
- [x] #5 All new tests pass
<!-- AC:END -->
