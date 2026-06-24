---
id: TASK-067
title: 'Improve v0.10.0 type-error messages (CONFLICT dumps, tuple-index regression)'
status: To Do
assignee: []
created_date: '2026-06-24 00:36'
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
