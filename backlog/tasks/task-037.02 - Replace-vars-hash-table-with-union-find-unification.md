---
id: TASK-037.02
title: Replace vars hash table with union-find unification
status: Done
assignee: []
created_date: '2026-02-27 21:43'
updated_date: '2026-02-28 05:42'
labels:
  - compiler
  - type-inference
dependencies: []
parent_task_id: TASK-37
priority: high
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
Replace the mutable vars hash table (mapping symbols to lists of candidate types) with a union-find data structure. Each type variable becomes a union-find cell. The current impc:ti:vars-update (which appends to a list and deduplicates) becomes a union! operation. The current impc:ti:type-unify (which traverses lists of candidates) becomes find + path compression.

This eliminates the iterative retry loop in run-type-check* (runtime/llvmti-typecheck.xtm, ~line 3479). Currently the algorithm walks the entire AST 1-N times, retrying until types stabilise. With union-find, a single pass suffices because unification eagerly merges equivalence classes.

Implementation steps:
1. Implement union-find in Scheme (make-uf-cell, find!, union!, snapshot)
2. Replace vars hash table creation (impc:ti:find-all-vars) with union-find cell allocation
3. Replace vars-update calls with union! calls
4. Replace vars-snapshot (full hash table copy) with union-find snapshot (for generic function checking)
5. Replace the retry loop in run-type-check* with a single-pass walk
6. Update impc:ti:unify (the final pass) to read from union-find cells

Key files: runtime/llvmti-typecheck.xtm (run-type-check*, vars-update, vars-snapshot), runtime/llvmti-transforms.xtm (type-unify, unify)
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] #1 union-find data structure implemented with find!, union!, and path compression
- [ ] #2 vars hash table replaced with union-find cells throughout type-check dispatch
- [ ] #3 iterative retry loop in run-type-check* eliminated (single AST pass)
- [ ] #4 vars-snapshot for generic checking uses efficient union-find snapshot
- [ ] #5 all existing tests pass (ctest -L libs-core, libs-external, examples)
- [ ] #6 compiler performance equal or better on representative programs
<!-- AC:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
Replaced vars hash table with union-find unification. See commit af42d746.
<!-- SECTION:NOTES:END -->
