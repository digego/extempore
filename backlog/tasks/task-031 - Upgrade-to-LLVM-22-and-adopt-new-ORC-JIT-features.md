---
id: TASK-031
title: Upgrade to LLVM 22 and adopt new ORC JIT features
status: In Progress
assignee: []
created_date: '2026-02-24 08:39'
updated_date: '2026-02-25 21:38'
labels:
  - llvm
  - jit
dependencies: []
priority: medium
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
Upgrade Extempore from LLVM 21.1.7 to LLVM 22 and adopt relevant new ORC JIT features. LLVM 22.1.0 was released on 24 Feb 2026.

Extempore already uses ORC JIT (LLJIT) with DynamicLibrarySearchGenerator for host process symbols and manual absoluteSymbols registration for builtins (see src/EXTLLVM.cpp). The upgrade itself should be straightforward since the ORC migration is complete, but several new features are worth adopting.

### Automatic shared library resolver (PR #148410)

Replace the current DynamicLibrarySearchGenerator + manual registerSymbol() setup with the new LibraryResolver. This automatically resolves unresolved symbols against shared libraries with less boilerplate. Evaluate whether it can replace both the DynamicLibrarySearchGenerator and some of the manual symbol registration.

### JIT backtrace symbolication (PR #175099, #175469)

Add limited symbolication of JIT backtraces. Currently a segfault in JIT'd xtlang gives almost no useful information. This would make debugging xtlang crashes significantly easier --- high value for a live-coding environment where crashes during performance are common.

### cloneToContext / cloneExternalModuleToContext (PR #146852)

APIs for cloning modules into a separate ThreadSafeContext. Evaluate whether this improves concurrent compilation --- compiling new xtlang closures on a background thread while the audio thread runs. Extempore already uses ThreadSafeModule so the integration point exists.

### ReOptimizeLayer (PR #173204, #173334)

Evaluate whether the re-optimisation pipeline could improve hot-swapping of xtlang closures, potentially allowing in-place updates to compiled code rather than the current recompile-and-relink approach.

### ELF deinitialise support (PR #175981)

Fixes missing deinitialisation on ELF platform with execution order by priority. Relevant for Linux builds --- check whether Extempore's JIT teardown is affected.

### WaitingOnGraph dependency tracking (PR #163027)

Internal ORC refactor replacing baked-in dependence tracking. Unlikely to require Extempore changes, but worth checking for API breakage during the upgrade.

### Not relevant

- SystemZ JITLink TLS, COFF dlupdate, PowerPC XCOFF --- Extempore targets x86-64 and AArch64 on macOS/Linux only.
- llvm-autojit plugin --- addresses whole-program front-loading, but Extempore compiles incrementally so this isn't a bottleneck.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [x] #1 CMakeLists.txt DEP_LLVM_VERSION bumped to new release
- [ ] #2 Extempore builds and passes tests on macOS (AArch64) and Linux (x86-64)
- [x] #3 Evaluate LibraryResolver as replacement for current DynamicLibrarySearchGenerator setup
- [x] #4 Evaluate JIT backtrace symbolication for xtlang crash debugging
- [ ] #5 No regressions in xtlang compilation latency (live-coding responsiveness)
<!-- AC:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
Version bumped from 21.1.7 to 22.1.0 in CMakeLists.txt and CI workflow.

Feature evaluations (all assessed as not worth adopting now):
- LibraryResolver: designed for Clang-Repl auto-discovery; Extempore's explicit registration via DynamicLibrarySearchGenerator + absoluteSymbols is cleaner for our use case.
- JIT backtrace symbolication: no confirmed user-facing API in LLVM 22.
- ReOptimizeLayer: no documentation, appears experimental.
- cloneToContext: existing API, not new in 22.

No breaking ORC JIT API changes between 21 and 22. Awaiting CI verification for AC #2 and #5.
<!-- SECTION:NOTES:END -->
