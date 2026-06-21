---
id: TASK-064
title: Infer a doloop count's type from its use as the loop bound
status: To Do
assignee: []
created_date: "2026-06-19 07:09"
labels:
  - compiler
  - types
dependencies: []
priority: low
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

An untyped argument used only as a doloop bound is not pinned to an integer
type:

    (bind-func f (lambda (nx) (doloop (x nx) x) nx))

The constraint engine reports "couldn't resolve type" rather than inferring
nx:i64 from the symbol's use as the doloop count. The typed-count sibling
(nx:i64) compiles fine.

This is the residual half of the old dotimes_type_quirk pair (TASK-059 era).
Under the old candidate-list checker the symptom differed --- it mis-inferred
the count as double; the constraint engine now declines to resolve it at all.
The passing typed-count case is now a regression test (doloop_typed_count in
tests/core/type-system.xtm); the failing untyped case stays quarantined as
dotimes_type_quirk_1 in tests/failing.xtm.

Likely fix: doloop's count expression should emit an integer constraint on its
operand so a free argument type grounds to an integer type.

<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria

<!-- AC:BEGIN -->

- [ ] #1 An untyped argument used as a doloop bound infers as an integer type
- [ ] #2 dotimes_type_quirk_1 passes and moves from tests/failing.xtm into
    tests/core/type-system.xtm
<!-- AC:END -->
