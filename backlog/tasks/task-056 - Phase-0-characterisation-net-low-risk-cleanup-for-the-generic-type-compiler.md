---
id: TASK-056
title: 'Phase 0: characterisation net + low-risk cleanup for the generic-type compiler'
status: In Progress
assignee: []
created_date: '2026-06-08 06:33'
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
- [ ] #1 Characterisation net for the #315 type-variable-pointer-depth class added to tests (locked compile-should-fail; suite green now, goes red when Phase 1 lands)
- [ ] #2 The 8 shadowed duplicate definitions (doc-001 section 3.1) removed, each verified dead first
- [ ] #3 Large dead/commented-out blocks (doc-001 section 3.2) removed
- [ ] #4 Magic 213/108 and the zone hook-list field index 4 (doc-001 section 3.3) replaced with symbolic constants / a named accessor
- [ ] #5 libs-core and libs-external stay green after every change
<!-- AC:END -->
