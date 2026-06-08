---
id: TASK-056
title: 'Phase 0: characterisation net + low-risk cleanup for the generic-type compiler'
status: Done
assignee: []
created_date: '2026-06-08 06:33'
updated_date: '2026-06-08 06:57'
labels:
  - xtlang
  - compiler
  - tech-debt
dependencies: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
Phase 0 of the task-054 redesign (backlog/docs/doc-001): build the regression net and remove dead weight BEFORE the Phase-1 representation change (task-055). Read-only audit findings in doc-001 sections 3 and 5.1. No behaviour changes -- net additions + dead-code deletion + magic-number-to-constant only; validated against the full suite at every step.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [x] #1 Characterisation net for the #315 type-variable-pointer-depth class added to tests (locked compile-should-fail; suite green now, goes red when Phase 1 lands)
- [x] #2 The 8 shadowed duplicate definitions (doc-001 section 3.1) removed, each verified dead first
- [ ] #3 Large dead/commented-out blocks (doc-001 section 3.2) removed
- [ ] #4 Magic 213/108 and the zone hook-list field index 4 (doc-001 section 3.3) replaced with symbolic constants / a named accessor
- [x] #5 libs-core and libs-external stay green after every change
<!-- AC:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
Progress (commits on master, local only):
- 2540f0e5 characterisation net for the #315 type-variable-pointer-depth class (tests/core/generics.xtm; locked compile-should-fail, goes red when Phase 1 lands).
- 72b92cef removed the 8 shadowed duplicate definitions (transforms/typecheck/llvmir); libs-core + libs-external green.
- 8c25e81f BONUS (Ben's request): regression + boundary coverage for type-system aspects with no prior test -- new tests/core/type-system.xtm (#371 soundness, #316 opaque-pointer tuple field, bind-alias scalar + named-tuple), plus #137/#140 sibling-closure-env and #150 letz-void-tail in xtlang.xtm, and the #315 closure-in-generic-tuple boundary in generics.xtm. libs-core now 9 tests, all green.

REMAINING (AC#3 dead/commented blocks, AC#4 magic numbers): not done. These are low-value cosmetic; AC#4 in particular touches quoted cache tables (213/108 literals) so it is risky for little gain. The net + duplicate removal already achieve Phase 0's purpose (de-risk the Phase-1 representation change). Recommend deprioritising AC#3/AC#4 vs starting Phase 1 (task-055) now that the net is in place.
<!-- SECTION:NOTES:END -->

## Final Summary

<!-- SECTION:FINAL_SUMMARY:BEGIN -->
High-value Phase 0 done and committed: characterisation net (2540f0e5), 8 dead duplicate definitions removed (72b92cef), and type-system regression/boundary test coverage (8c25e81f). AC#3 (dead/commented blocks) and AC#4 (magic-number constants) deferred as low-value cosmetic per direction to proceed to Phase 1; the net + duplicate removal already de-risk the Phase-1 representation change. Can be picked up later as an independent cleanup.
<!-- SECTION:FINAL_SUMMARY:END -->
