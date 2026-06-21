---
id: TASK-052
title: "Tier C: migrate the C++ runtime to C++20"
status: Done
assignee: []
created_date: "2026-06-07 14:24"
updated_date: "2026-06-07 14:24"
labels:
  - cpp
  - refactor
dependencies: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

Bump the required C++ standard to C++20 and adopt the C++20 idioms the audit
(TASK-049) identified as safe across the CI compiler matrix (GCC 13 / Apple
Clang 16 / MSVC 2022).

<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria

<!-- AC:BEGIN -->

- [x] #1 CMAKE_CXX_STANDARD bumped to 20; the [=] implicit-this deprecation it
      surfaced fixed
- [x] #2 Hand-rolled MtSemaphore replaced with std::counting_semaphore
- [x] #3 fp80-to-double union type-pun replaced with std::bit_cast
- [x] #4 OSC byte-swaps rewritten with std::endian + std::bit_cast, preserving
      the extern C JIT ABI
- [x] #5 MSVC C++20 dll-linkage mismatch (llvm_destroy_zone_after_delay) fixed
- [x] #6 BUILDING.md and CLAUDE.md updated to note the C++20 requirement
- [x] #7 Green on all four CI platforms
<!-- AC:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->

Landed in commits 47ce61be, 53c25e67, 05f5fe49, e0fd1c49, 3315ccb6, 8aa7830c:
C++20 bump, std::counting_semaphore, std::bit_cast for fp80, OSC
std::endian/std::bit_cast rewrite, MSVC dll-linkage fix, doc updates. Green on
all four CI platforms.

<!-- SECTION:NOTES:END -->
