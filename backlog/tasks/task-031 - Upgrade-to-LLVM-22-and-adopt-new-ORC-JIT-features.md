---
id: TASK-031
title: Upgrade to LLVM 22 and adopt new ORC JIT features
status: Done
assignee: []
created_date: '2026-02-24 08:39'
updated_date: '2026-06-19 07:37'
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
- [x] #2 Extempore builds and passes tests on macOS (AArch64) and Linux (x86-64)
- [x] #3 Evaluate LibraryResolver as replacement for current DynamicLibrarySearchGenerator setup
- [x] #4 Evaluate JIT backtrace symbolication for xtlang crash debugging
- [x] #5 No regressions in xtlang compilation latency (live-coding responsiveness)
<!-- AC:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
## Outcome

LLVM 22 is in and verified on all CI platforms. The version on master is now **22.1.6** (the upgrade overshot the 22.1.0 this task was scoped against).

### How the upgrade actually reached master

Master never ran LLVM 21.1.7 --- the premise of this task is stale. The LLVM 22 upgrade landed via the aarch64 branch (squash-merge `77c48dce`, which brought 22.1.1), then bumped 22.1.1 -> 22.1.6 in `c8bdbf74`. `git log -S "21.1.7" -- CMakeLists.txt` returns nothing: there was never a 21->22 transition on master to regress against. The entire current codebase (aarch64 support, new constraint-based type checker, impc->xtc renamespace) was developed on LLVM 22.

### AC #2 --- builds and passes tests (macOS AArch64 + Linux x86-64)

Verified green by CI run **27273467755** (commit `8ff862c8`, LLVM 22.1.6) on all four platforms: macOS aarch64, Windows x86_64, Linux x86_64, Linux aarch64. Re-confirmed locally on 2026-06-19: clean incremental build (including `aot_external_audio`) plus the full suite --- libs-core 11/11 and libs-external 1/1, zero failures.

### AC #5 --- compilation latency

No LLVM 21 baseline exists on master, so a literal 21-vs-22 regression delta is unmeasurable. Absolute latency under 22.1.6 is healthy for live coding: a representative typed closure (loop + double math + convert) compiles end-to-end (IR gen + opt passes + OrcJIT codegen) in **~25ms** (8-closure mean 25.0ms, min 22.8, max 29.8), comfortably under the ~100ms interactive-feel threshold.

### Feature evaluations (unchanged --- none adopted)

- LibraryResolver: designed for Clang-Repl auto-discovery; Extempore's explicit registration via DynamicLibrarySearchGenerator + absoluteSymbols is cleaner for our use case.
- JIT backtrace symbolication: no confirmed user-facing API in LLVM 22.
- ReOptimizeLayer: no documentation, appears experimental.
- cloneToContext: existing API, not new in 22.
- ELF deinitialise / WaitingOnGraph: no Extempore-side changes needed; no ORC JIT API breakage observed between the 22.x points.
<!-- SECTION:NOTES:END -->
