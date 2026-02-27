---
id: TASK-037.03
title: Separate constraint generation from solving
status: To Do
assignee: []
created_date: '2026-02-27 21:43'
labels:
  - compiler
  - type-inference
dependencies: []
parent_task_id: TASK-37
priority: medium
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
Extract constraint emission from the type-check dispatch (impc:ti:type-check, ~50 branches in runtime/llvmti-typecheck.xtm) into an explicit constraint data structure, then solve constraints in a separate pass.

Currently, type-check both generates constraints and solves them (via mutation of the union-find / vars structure) in a single interleaved walk. Separating these concerns makes the algorithm easier to reason about, debug, and extend.

Implementation steps:
1. Define a constraint representation: equality constraints (α = τ), overload constraints (x ∈ {f1, f2, ...} given arg types), and coercion constraints (numeric defaulting)
2. Modify type-check dispatch to emit constraints into a list/queue instead of calling union! directly
3. Implement a constraint solver that processes the constraint list:
   - Equality constraints: union! on union-find cells
   - Overload constraints: match against poly/adhoc caches, emit further equality constraints
   - Coercion constraints: apply numeric defaulting rules (replicate current (apply min res) behaviour exactly)
4. Decompose nativef-generics (~400 lines) into constraint emission (small) + the existing specialisation machinery
5. Ensure the solver handles constraint ordering correctly (some constraints depend on others being solved first)

Key files: runtime/llvmti-typecheck.xtm (type-check dispatch, nativef-generics), runtime/llvmti-transforms.xtm (type-unify, unify), runtime/llvmti-bind.xtm (pipeline orchestration)
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] #1 explicit constraint data structure defined (equality, overload, coercion)
- [ ] #2 type-check dispatch emits constraints instead of solving inline
- [ ] #3 separate constraint solver processes all constraints
- [ ] #4 nativef-generics decomposed into constraint emitter + specialisation
- [ ] #5 numeric coercion defaulting produces identical results to current algorithm
- [ ] #6 all existing tests pass (ctest -L libs-core, libs-external, examples)
<!-- AC:END -->
