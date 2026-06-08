---
id: TASK-053
title: Fix pre-v0.9 bugs that still reproduce on v0.9.4
status: Done
assignee:
  - '@claude'
created_date: '2026-06-08 02:53'
updated_date: '2026-06-08 04:53'
labels:
  - bug
  - xtlang
  - compiler
dependencies: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
During the 2026-06 issue-tracker audit, six bug reports that predate the v0.9.0 overhaul were re-run against the live v0.9.4 build and still fail. Kept open on GitHub; tracked here for follow-up.

- #137 (digego): nested let-bound recursive lambda, also bound to another let var, crashes with SIGSEGV on execution.
- #140 (digego): function composition routed through map (let-bound composed lambda) crashes with SIGSEGV on execution.
- #150 (benswift): letz + (list ...) raises a Type Error; identical body with let works fine.
- #315 (cianoc): pset! into a generic memory pointer held in a generic tuple -> 'Type Error in pointer-set!, got i64*, was expecting i64' (a spurious * is inserted).
- #316 (cianoc): storing mzone* in a tuple -> 'cannot find symbol hcopy:[mzone*,mzone*]' (niche).
- #371 (CastixGitHub): a bind-func whose body returns a different tuple-pointer type than its declared signature compiles with no type error (missing soundness check).

Repro files used in the audit are under /tmp/xtm-repro (118.xtm, 137.xtm, ... regenerate as needed).
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [x] #1 #137 nested let-bound recursive lambda no longer crashes
- [x] #2 #140 map-of-composed-lambda no longer crashes
- [x] #3 #150 letz+list compiles and runs like let
- [ ] #4 #315 pset! into generic tuple type-checks correctly
- [x] #5 #316 mzone* stored in a tuple compiles
- [x] #6 #371 signature/return-type mismatch is rejected with a clear error
<!-- AC:END -->

## Implementation Plan

<!-- SECTION:PLAN:BEGIN -->
Verified all six are real compiler bugs (not feature requests): #137/#140 SIGSEGV on combinations of individually-working features; #150 is letz/memzone failing on VOID-returning bodies (list is a red herring -- check-memzone-void? is purely syntactic); #315 generic-only spurious * (concrete works); #316 mzone* is the only pointer type lacking tuple-copy hcopy (all others incl user-struct pointers work); #371 unsound pointer return-type mismatch accepted (scalar caught). Runtime *.xtm files load live from disk -- no rebuild needed for compiler fixes. Order: #371, #316, #150, #315, #137/#140; commit each separately.
<!-- SECTION:PLAN:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
FINAL: 5 of 6 fixed, committed, validated (#137,#140 -> 8cf85e48; #371 -> a200d3b8; #150 -> 3be3052e; #316 -> 501cb512). #315 (AC#4) is NOT fixed -- rehomed to TASK-054 (audit + principled redesign of the generic type compiler) since its fix is in the generic-instantiation core, not a localized patch. Note: the 'dotimes/println' side-finding was a false alarm -- dotimes requires its index variable declared outside the loop (documented; use doloop for auto-binding), so it is correct behaviour, not a bug.
<!-- SECTION:NOTES:END -->
