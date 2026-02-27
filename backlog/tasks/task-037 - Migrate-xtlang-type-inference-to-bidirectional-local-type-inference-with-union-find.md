---
id: TASK-037
title: >-
  Migrate xtlang type inference to bidirectional local type inference with
  union-find
status: To Do
assignee: []
created_date: '2026-02-27 21:43'
labels:
  - compiler
  - type-inference
dependencies: []
priority: high
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
Replace the current ad-hoc iterative constraint propagation algorithm in the xtlang compiler with a principled bidirectional local type inference algorithm (Pierce & Turner 2000) using union-find unification.

The current algorithm (in runtime/llvmti-typecheck.xtm and runtime/llvmti-transforms.xtm) has these problems:
- Iterative retry loop with no formal convergence guarantee (run-type-check* walks the AST 1-N times)
- No occurs check (infinite types not detected)
- O(n²) list dedup per variable per pass in vars-update
- Full hash table copy (vars-snapshot) for every generic function check
- ~400-line nativef-generics function that's hard to reason about
- Pervasive mutation of the vars hash table during the AST walk

Migration is done in 4 incremental stages (subtasks), each independently testable. Must not change language semantics (monomorphic-by-default, no let-polymorphism). Must preserve bind-poly/bind-func overloading, !bang generics, and type inference / IR generation separation. Numeric coercion defaulting rules must be replicated exactly.

Key files: runtime/llvmti-typecheck.xtm, runtime/llvmti-transforms.xtm, runtime/llvmti-bind.xtm, runtime/llvmti-caches.xtm, runtime/llvmti-globals.xtm, runtime/llvmti-ast.xtm

References: Pierce & Turner (Local Type Inference, 2000), Dunfield & Krishnaswami (Bidirectional Typing, 2021), Conchon & Filliâtre (A Persistent Union-Find Data Structure, 2007)
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] #1 All 4 stages completed as subtasks
- [ ] #2 All existing tests pass (ctest -L libs-core, libs-external, examples)
- [ ] #3 No change to language semantics
- [ ] #4 Compiler performance equal or better than current
<!-- AC:END -->
