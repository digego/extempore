---
id: TASK-028
title: investigate DSP type error on macOS arm64
status: To Do
assignee: []
created_date: '2026-02-22 10:18'
labels:
  - bug
  - macos
  - arm64
dependencies: []
references:
  - 4efa51c3
  - ba474ea7
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
`bind-func dsp:DSP` fails with "Type Error bad type DSP  Bad closure type for bind-func" when running examples interactively on macOS arm64 (Apple Silicon). Tests pass in `--batch` mode but examples fail when run with audio enabled.

The error occurs because `impc:ti:typealias-exists? "DSP"` returns `#f`, meaning the DSP type alias (defined in `libs/base/base.xtm` line 86 and `libs/aot-cache/base.xtm` line 22) is not registered by the time the example runs.

Cannot reproduce on Linux x86_64 --- the same code works correctly there.

**Likely cause**: the `4efa51c3` commit added `__aarch64__` to the 64-bit target detection in `include/UNIV.h`. Before that fix, macOS arm64 (Apple Clang defines `__GNUC__` but not `__x86_64__`) fell through to `TARGET_32BIT`, giving wrong pointer sizes on a 64-bit platform. A stale binary (built before 4efa51c3 but with runtime files from after) would have corrupted pointer handling that could break the Scheme type alias table.

**To verify**: rebuild on macOS arm64 and check:

```bash
timeout 30 ./build/extempore --batch \
  '(begin (println "DSP-exists:" (impc:ti:typealias-exists? "DSP")) (println "ptr-size:" (sys:pointer-size)) (quit 0))'
```

If the issue persists after a clean rebuild, the AOT cache (`libs/aot-cache/xtmbase.ll`) may need to be regenerated for arm64, or there may be a deeper platform-specific issue in the base library loading path.

**Related commits**: ba474ea7 (DSP hot-swap fix), 4efa51c3 (aarch64 target detection)
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] #1 DSP type alias resolves correctly on macOS arm64 (impc:ti:typealias-exists? returns #t)
- [ ] #2 Examples with bind-func dsp:DSP work interactively on macOS arm64 with audio enabled
- [ ] #3 Tests continue to pass in --batch mode on all platforms
<!-- AC:END -->
