---
id: TASK-067
title: 'Improve v0.10.0 type-error messages (CONFLICT dumps, tuple-index regression)'
status: Done
assignee: []
created_date: '2026-06-24 00:36'
updated_date: '2026-06-24 03:20'
labels: []
dependencies: []
priority: medium
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
Verifying the docs against the v0.10.0 compiler surfaced two error-quality issues. (1) Most type errors (wrong arg type, wrong pointer type, deref non-pointer, arity mismatch) now surface as a low-level 'CONFLICT-OVERLOAD/EQ: !infer_N args ((var N)) cands (...)' line followed by a generic 'Type Error couldn't resolve type: NAME_adhoc_N' --- the older specific messages ('bad numeric value', 'got T1 expecting T2', 'no valid polymorphic options') are gone, and the CONFLICT line reads like internal solver debug output rather than a user-facing diagnostic. (2) An out-of-bounds literal tuple index (e.g. tref on index 5 of a 2-field tuple) no longer reports 'tuple index out of bounds' --- it throws an internal Scheme error ';car argument, #f, is boolean but should be a pair'. Both are documented as-is in error-messages.md for now; consider restoring clearer messages at the compiler level, then re-run docs/verify-examples.py to update the docs.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] #1 Decide whether the CONFLICT-* solver output should be user-facing; if not, surface a clearer type-error message for arg-type/pointer-type/arity/deref mismatches
- [ ] #2 Out-of-bounds literal tuple index reports a clean compile error again, not an internal Scheme car error
- [ ] #3 error-messages.md is re-verified against the improved messages via docs/verify-examples.py
<!-- AC:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
Both issues fixed in the live-loadable xtlang compiler (no C++ rebuild needed); full libs-core suite stays green (11/11).

AC#1 (CONFLICT dumps): decided the raw solver output should NOT be user-facing. Rewrote xtc:solve:explain-conflict (runtime/xtc-solve.xtm, the error-path-only WHY pass) to emit readable diagnostics:
- 'Type Error: no overload matches the argument types [double] --- candidates are [i64,i64]' (wrong arg type / wrong pointer type / arity)
- 'Type Error: type mismatch --- i64 is not compatible with !infer_N*' (deref of a non-pointer)
Types render with the type pretty-printer in the [ret,arg,...] form closures already print; numeric defaults are applied first so literal args show as double/i64. Safe because the pass only runs after a compile has already failed.

AC#2 (tuple index OOB): an out-of-bounds literal tuple index left the field projection unresolved and crashed the driver with ';car argument, #f, is boolean but should be a pair'. Added xtc:solve:tuple-index-oob? in discharge-projects to detect a ground tuple-like aggregate with no such field and report via print-index-oob-error ('tuple index out of bounds: N'); covers bare tuples, named types and generic instances, plus negative indices. Guarded project-field-term and the tuple-ref codegen as defence in depth.

AC#3: error-messages.md (plus tutorial.md and types.md) re-verified against the new messages via docs/verify-examples.py --- all green.
<!-- SECTION:NOTES:END -->
