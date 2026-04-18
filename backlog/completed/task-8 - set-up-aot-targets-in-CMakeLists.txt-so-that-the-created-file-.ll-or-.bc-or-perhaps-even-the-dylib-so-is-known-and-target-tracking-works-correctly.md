---
id: task-8
title: >-
  set up aot targets in CMakeLists.txt so that the created file (.ll or .bc, or
  perhaps even the dylib/so) is known and target tracking works correctly
status: Done
assignee: []
created_date: '2025-12-16 22:56'
updated_date: '2025-12-17 00:34'
labels: []
dependencies: []
---

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
Fixed the `aotcompile_lib` macro in CMakeLists.txt to properly track AOT output files and their dependencies.

## Changes made

1. **Track both output files**: Each AOT compilation produces two files:
   - `libs/aot-cache/xtm<basename>.ll` (LLVM IR)
   - `libs/aot-cache/<basename>.xtm` (Scheme stubs)
   
   Both are now listed in `add_custom_command OUTPUT` and `add_custom_target DEPENDS`.

2. **Add file-level dependencies**: The macro now builds a list of dependency files (both `.ll` and `.xtm` for each dep) and adds them to `add_custom_command DEPENDS`. This enables proper cascade rebuilds.

3. **Add source file dependency**: The source `.xtm` file is also added to `DEPENDS`, so changes to source files trigger rebuilds.

## Verified behaviour

| Scenario | Files rebuilt |
|----------|---------------|
| Remove `base.xtm` | 6 (base + all dependents) |
| Remove `xtmbase.ll` | 6 (base + all dependents) |
| Remove `audio_dsp.xtm` | 2 (audio_dsp + instruments) |
| Remove `instruments.xtm` | 1 (instruments only) |
| All files present and up-to-date | 0 (no work to do)
<!-- SECTION:NOTES:END -->
