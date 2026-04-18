---
id: TASK-030
title: Add clang-tidy static analysis to the codebase
status: To Do
assignee: []
created_date: '2026-02-22 21:50'
updated_date: '2026-02-22 21:51'
labels:
  - tooling
  - c++
dependencies: []
priority: low
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
Set up clang-tidy for the C++ codebase to catch deprecations, unused code, and modernisation opportunities. The tooling is already available via Homebrew's LLVM (`/opt/homebrew/opt/llvm/bin/clang-tidy`) and `compile_commands.json` is already generated in `build/`.

Steps:
1. Create a `.clang-tidy` config file in the repo root with a curated set of checks (modernize-*, bugprone-*, misc-unused-*, clang-analyzer-deadcode.*, performance-*)
2. Run clang-tidy across `src/*.cpp` and triage the findings
3. Fix or suppress findings as appropriate
4. Optionally add a CMake target or script to make running it convenient (e.g. `cmake --build . --target tidy`)
5. Consider adding to CI as a non-blocking check
<!-- SECTION:DESCRIPTION:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
Important: Extempore dynamically binds many C++ functions at runtime from Scheme/xtlang, so aggressive unused-function checks (e.g. cppcheck's `unusedFunction`) will produce false positives. Be conservative with these checks --- focus on modernisation, deprecations, and bugprone patterns rather than removing code that appears unused.
<!-- SECTION:NOTES:END -->
