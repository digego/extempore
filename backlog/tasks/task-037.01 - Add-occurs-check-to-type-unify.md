---
id: TASK-037.01
title: Add occurs check to type-unify
status: To Do
assignee: []
created_date: '2026-02-27 21:43'
labels:
  - compiler
  - type-inference
dependencies: []
parent_task_id: TASK-37
priority: high
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
Add an occurs check to the existing impc:ti:type-unify function in runtime/llvmti-transforms.xtm. This prevents infinite types from being constructed during unification --- currently nothing detects when a type variable appears in its own solution.

Implementation: during type-unify, before resolving a symbol by looking it up in vars, check whether the symbol being resolved appears anywhere in the type being constructed. If it does, signal a type error rather than looping.

This is a small, high-value change that requires no architectural changes to the existing algorithm. It prepares the ground for union-find (stage 2) where occurs check is a standard component.

Key file: runtime/llvmti-transforms.xtm (impc:ti:type-unify, ~line 1809)
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] #1 occurs check detects self-referential type variables during unification
- [ ] #2 type error is raised when an infinite type is detected
- [ ] #3 all existing tests pass unchanged (ctest -L libs-core, libs-external)
- [ ] #4 no change to inference results for well-typed programs
<!-- AC:END -->
