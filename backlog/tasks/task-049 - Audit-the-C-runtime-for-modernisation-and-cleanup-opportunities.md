---
id: TASK-049
title: Audit the C++ runtime for modernisation and cleanup opportunities
status: Done
assignee: []
created_date: "2026-06-07 14:23"
updated_date: "2026-06-07 14:24"
labels:
  - cpp
  - refactor
dependencies: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

Thoroughly review the first-party C++ in src/ and include/ (plus CMake) for ways
to simplify, modernise, remove platform-specific preprocessor macros in favour
of the standard library, and align with current best practice. Produce a
prioritised, decision-ready plan.

<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria

<!-- AC:BEGIN -->

- [x] #1 Every first-party C++ file and the CMake build reviewed for
      cleanup/modernisation opportunities
- [x] #2 Platform #ifdefs catalogued and classified: deletable-via-stdlib vs
      genuinely platform-specific
- [x] #3 A C++20-vs-C++17 standard-bump analysis grounded in the actual CI
      compiler matrix
- [x] #4 Findings consolidated into a prioritised, phased, decision-ready action
    plan
<!-- AC:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->

Done as a multi-agent review of all first-party C++ (16 per-file/holistic
readers + synthesis) plus a C++20 standard-bump analysis grounded in the CI
compiler matrix; produced two decision-ready reports and a phased action plan,
executed in the tier tasks that follow.

<!-- SECTION:NOTES:END -->
