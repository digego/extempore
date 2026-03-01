---
id: TASK-037.04
title: Formalise bidirectional checking and synthesis modes
status: Done
assignee: []
created_date: '2026-02-27 21:43'
updated_date: '2026-02-28 05:41'
labels:
  - compiler
  - type-inference
dependencies: []
parent_task_id: TASK-37
priority: medium
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
Make the checking/synthesis distinction explicit in the type-check dispatch. Currently the request? parameter drives bidirectional inference implicitly --- when present, type information flows downward (checking mode); when absent, types are synthesised bottom-up. This stage formalises that distinction.

Implementation steps:
1. Define two explicit modes: check(τ) and synth, replacing the overloaded request? parameter
2. Split the type-check dispatch into check-specific and synth-specific branches where they differ (e.g. lambda-check in checking mode pushes parameter types down from the expected closure type; in synth mode it infers from usage)
3. Add a subsumption rule at mode boundaries: when check(τ) meets a synthesised type σ, verify σ is compatible with τ (currently done ad-hoc in various places)
4. Document the bidirectional flow in the code, making it clear which branches operate in which mode
5. Clean up cases where request? is used inconsistently or ignored

This is primarily a code clarity and correctness improvement. It makes the inference algorithm's behaviour predictable and easier to extend.

Key files: runtime/llvmti-typecheck.xtm (type-check dispatch, all *-check functions), runtime/llvmti-bind.xtm (pipeline orchestration)

References: Dunfield & Krishnaswami, Bidirectional Typing (2021 survey)
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] #1 explicit check(τ) and synth modes replace request? parameter
- [ ] #2 type-check dispatch branches clearly separated by mode where they differ
- [ ] #3 subsumption rule applied consistently at mode boundaries
- [ ] #4 all existing tests pass (ctest -L libs-core, libs-external, examples)
- [ ] #5 no change to inference results for existing programs
<!-- AC:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
Renamed request? to expected across all ~50 check functions (452 occurrences). Added centralised normalise-expected call in type-check dispatch. Removed per-function cleanup from math-check, math-intrinsic-check, nativef-generics. Added mode helpers (synth-mode?, normalise-expected, check-type) and subsumption helper. Changed begin-check to pass #f for non-final expressions. Added 17 unit tests. All test suites pass.
<!-- SECTION:NOTES:END -->
