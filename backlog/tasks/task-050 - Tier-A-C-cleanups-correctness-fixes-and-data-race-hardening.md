---
id: TASK-050
title: 'Tier A: C++ cleanups, correctness fixes, and data-race hardening'
status: Done
assignee: []
created_date: '2026-06-07 14:24'
updated_date: '2026-06-07 14:24'
labels:
  - cpp
  - refactor
dependencies: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
Apply the low-risk, Linux-verifiable findings from the modernisation audit (TASK-049): remove dead code and conditional compilation, fix include hygiene, replace hand-rolled code with stdlib equivalents, make cross-thread state atomic, and fix latent correctness bugs.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [x] #1 Dead conditional-compilation, orphan helpers, and dead TinyScheme-era shims removed
- [x] #2 Header hygiene fixed: missing include guard added, duplicate includes/prototypes removed, IWYU
- [x] #3 Hand-written SSE clamp replaced with std::clamp; duplicated helper bodies de-duplicated
- [x] #4 Cross-thread clock and run-state fields made std::atomic
- [x] #5 Latent bugs fixed: inverted strcmp, null-deref do/while, int zone-size truncation, GC-protection, resetOutportString, platform() fallthrough
- [x] #6 getRealTime derived from std::chrono::system_clock (fixes the Windows boot-relative-epoch bug)
- [x] #7 Build warning-clean; non-graphics test suites green on all four CI platforms
<!-- AC:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
Landed in commits ae413944..09ba55f9 (10 commits): remove dead conditional-compilation, header hygiene, std::clamp for the SSE clamp, helper dedup, fixed leaked audio buffer, removed dead s7 shims, atomic clock/run-state, six latent-bug fixes, getRealTime via std::chrono, deterministic IR-preamble maps. Net -133 lines; green on all four CI platforms.
<!-- SECTION:NOTES:END -->
