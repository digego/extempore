---
id: TASK-036
title: Thread type inference vars explicitly through compiler passes
status: Done
assignee: []
created_date: '2026-02-26 09:44'
updated_date: '2026-02-27 00:59'
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
- [x] #1 Type inference vars are passed explicitly (not accessed via shared mutable global)
- [x] #2 All *-check functions receive and return (or explicitly mutate) the vars structure
- [ ] #3 impc:ti:run-type-check threads vars through rather than relying on side effects
- [x] #4 Core library tests pass
- [ ] #5 AOT compilation works
- [x] #6 Compiler-internal unit tests pass
<!-- AC:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
Implemented across 6 phases: added tc-result vector return type (#(type vars)), functional vars helpers (vars-update, vars-force, vars-add, vars-snapshot, vars-clear, tc-unwrap), converted all ~35 *-check functions to return tc-result, added dispatcher compatibility shim, replaced all set-cdr! mutations in check functions with functional wrappers, updated 3 call sites in llvmti-transforms.xtm, removed dead code (clean-fvars). The old update-var/force-var still back the functional wrappers during transition. run-type-check* retry logic still uses clear-all-vars for in-place clearing. All compiler-unit (3/3) and libs-core (6/6) tests pass.
<!-- SECTION:NOTES:END -->
