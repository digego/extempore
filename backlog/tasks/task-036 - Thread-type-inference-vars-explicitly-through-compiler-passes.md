---
id: TASK-036
title: Thread type inference vars explicitly through compiler passes
status: To Do
assignee: []
created_date: '2026-02-26 09:44'
updated_date: '2026-02-26 09:44'
labels:
  - compiler
  - architecture
dependencies:
  - TASK-035
priority: medium
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
Currently impc:ti:type-check and all *-check functions mutate a shared vars assoc list via set-cdr!. Change to threading vars explicitly through the pipeline --- either returning updated vars from each function or using a clear mutation protocol with an explicit state object. This removes hidden shared mutable state, makes data flow visible, enables future parallelism, and makes it possible to snapshot/rollback type state for speculative typing of overloaded functions. This is the biggest refactoring and should only be attempted after the compiler has unit tests as a safety net.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] #1 Type inference vars are passed explicitly (not accessed via shared mutable global)
- [ ] #2 All *-check functions receive and return (or explicitly mutate) the vars structure
- [ ] #3 impc:ti:run-type-check threads vars through rather than relying on side effects
- [ ] #4 Core library tests pass
- [ ] #5 AOT compilation works
- [ ] #6 Compiler-internal unit tests pass
<!-- AC:END -->
