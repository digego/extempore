---
id: TASK-051
title: 'Tier B: cross-platform C++ runtime fixes'
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
Apply the cross-platform findings from the audit (TASK-049) that needed Windows/macOS CI verification.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [x] #1 thread_kill replaced pthread_cancel with a cooperative std::atomic<bool> stop flag, uniform across platforms
- [x] #2 getRealTime macOS branch folded into the portable std::chrono::system_clock path
- [x] #3 Dead main-thread-callback machinery (SUBSUME_PRIMARY / XTMMainCallback / sched_main) removed end-to-end across C++, xtlang, IR and the AOT cache
- [x] #4 Dead OSC message helpers and the unused OSC singleton removed
- [x] #5 Windows zone memory aligned correctly (_aligned_malloc paired with _aligned_free), fixing the under-alignment bug
- [x] #6 Verified green on Linux x64, Linux arm64, macOS arm64 and Windows x64
<!-- AC:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
Landed in commits b7d94ba1, a1738601, 7dd5ff56, af372ace, 6e8a7f20: cooperative thread_kill stop flag, getRealTime macOS fold, end-to-end removal of the dead main-callback machinery, OSC dead-surface removal, Windows zone aligned-alloc fix. Verified green on Linux x64/arm64, macOS arm64 and Windows x64.
<!-- SECTION:NOTES:END -->
