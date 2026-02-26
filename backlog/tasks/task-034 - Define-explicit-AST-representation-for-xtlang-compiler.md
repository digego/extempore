---
id: TASK-034
title: Define explicit AST representation for xtlang compiler
status: To Do
assignee: []
created_date: '2026-02-26 09:44'
updated_date: '2026-02-26 09:44'
labels:
  - compiler
  - architecture
dependencies:
  - TASK-033
priority: medium
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
The xtlang compiler operates on raw s-expressions with car/cdr pattern matching --- there is no explicit AST type. Introduce a tagged AST representation (e.g. vectors with tag fields or tagged lists) with accessor functions. This gives each AST-consuming function an explicit contract and enables validation between passes. Start with the output of first-transform and input to type-check, since that is the most important boundary.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] #1 AST node types defined with constructors and accessors (at minimum: let, lambda, if, call, var, lit, set!)
- [ ] #2 first-transform produces the new AST representation
- [ ] #3 type-check consumes the new AST representation
- [ ] #4 AST validator function exists and runs between passes in debug mode
- [ ] #5 Core library tests pass
<!-- AC:END -->
