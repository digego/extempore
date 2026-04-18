---
id: TASK-029
title: investigate DSP type error on macOS arm64
status: Done
assignee: []
created_date: '2026-02-22 10:18'
updated_date: '2026-02-22 21:46'
labels:
  - bug
  - macos
  - arm64
dependencies: []
priority: high
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
bind-func dsp:DSP fails with 'Type Error bad type DSP  Bad closure type for bind-func' when running examples interactively on macOS arm64 (Apple Silicon). Tests pass in --batch mode but examples fail with audio enabled.

The error means impc:ti:typealias-exists? returns #f for DSP, so the type alias (defined in libs/base/base.xtm:86 and libs/aot-cache/base.xtm:22) is not registered by the time the example runs. Cannot reproduce on Linux x86_64.

Likely cause: stale binary built before 4efa51c3 (which added __aarch64__ to 64-bit target detection in UNIV.h). Before that fix, macOS arm64 fell through to TARGET_32BIT, corrupting pointer handling.

To verify: rebuild on macOS arm64 and run:
  timeout 30 ./build/extempore --batch '(begin (println (impc:ti:typealias-exists? "DSP")) (println (sys:pointer-size)) (quit 0))'

If it persists after rebuild, the AOT cache may need regenerating for arm64.

Related commits: ba474ea7 (DSP hot-swap), 4efa51c3 (aarch64 detection)
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [x] #1 Rebuild extempore on macOS arm64 from HEAD and verify DSP alias is registered
- [x] #2 audio_101.xtm loads successfully in interactive mode with audio enabled
- [ ] #3 All example-audio ctest tests pass on macOS arm64
<!-- AC:END -->

## Final Summary

<!-- SECTION:FINAL_SUMMARY:BEGIN -->
Root cause: `--run` flag in `src/Extempore.cpp` incorrectly set `EXT_LOADBASE = false`, preventing the base library (which defines the `DSP` type alias) from loading. This was a copy-paste error from the `--compile` path.

Fix: removed the `EXT_LOADBASE = false` line from the `OPT_INITFILE` case. The `--run` flag now loads the base library as expected, and `audio_101.xtm` runs successfully with audio enabled on macOS arm64.

Note: this was not an arm64-specific issue --- it affected `--run` on all platforms. Tests in `--batch` mode were unaffected because they don't use `--run`.
<!-- SECTION:FINAL_SUMMARY:END -->
