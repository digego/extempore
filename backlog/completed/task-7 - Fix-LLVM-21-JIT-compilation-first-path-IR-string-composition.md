---
id: task-7
title: Fix LLVM 21 JIT compilation first-path IR string composition
status: Done
assignee: []
created_date: "2025-12-16 21:58"
updated_date: "2025-12-16 21:58"
labels:
  - llvm
  - jit
  - aot
  - bug
dependencies: []
priority: high
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

When building Extempore from scratch on Linux x86_64 with LLVM 21, the first JIT
compilation path in jitCompile() fails with 'unbound variable' errors because
the IR string is missing critical components (sInlineString, externalGlobals,
dstream.str()) that are present in the main compilation path. This causes type
definitions like %mzone to be unavailable during LLVM IR parsing.

<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria

<!-- AC:BEGIN -->

- [ ] #1 First compilation path includes all necessary IR components:
      sInlineString + userTypeDefs + externalGlobals + externalLibFunctions +
      dstream.str() + strippedAsmcode
- [ ] #2 stripBuiltinTypeDefs() function correctly removes duplicate type
      definitions from sInlineString
- [ ] #3 Duplicate function declarations are properly stripped from sInlineSyms
- [ ] #4 AOT compilation of individual libraries (audiobuffer.xtm, sndfile.xtm)
      succeeds without errors
- [ ] #5 Loading AOT-compiled libraries works without 'unbound variable' or
      'non-cptr obj #f' errors
- [ ] #6 Clean rebuild on macOS (after ninja clean_aot) works correctly,
      confirming the fix doesn't break existing platforms
- [ ] #7 Clean rebuild on Linux x86_64 completes successfully from scratch
<!-- AC:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->

$## Current Investigation Status\n\nPartial fix has been attempted but AOT
loading still fails:\n\n### What Works:\n- Individual library AOT compilation
(audiobuffer.xtm, sndfile.xtm) succeeds\n- Modified first compilation path to
match main path structure\n\n### What Still Fails:\n- Loading AOT-compiled
libraries produces "unbound variable" or "non-cptr obj #f" errors\n- Suggests
issue may also exist in AOT loading path, not just compilation\n\n## Technical
Context\n\n### Root Cause:\nThe first JIT compilation path (when sInlineBitcode
is empty) was missing components:\n- `sInlineString` - type definitions\n-
`externalGlobals` - global variable declarations \n- `dstream.str()` -
declaration strings\n\nThis caused LLVM IR parsing failures because types like
%mzone were undefined.\n\n### Files Modified:\n- `src/SchemeFFI.cpp` -
jitCompile() function, lines ~560-590\n\n### Code Changes Made:\n1. Updated IR
string composition:
`sInlineString + userTypeDefs + externalGlobals + externalLibFunctions + dstream.str() + strippedAsmcode`\n2.
Added `stripBuiltinTypeDefs()` to remove duplicate type definitions\n3. Stripped
duplicate function declarations from `sInlineSyms`\n\n### Platform
Differences:\n- macOS aarch64: Works without changes (likely had pre-existing
AOT cache)\n- Linux x86_64: Fails on clean build (no AOT cache)\n\n###
Reproduction
Steps:\n`bash\nninja clean_aot # or rm -rf libs/aot-cache\nrm -rf build && mkdir build && cd build\ncmake -G Ninja .. && ninja\n# Observe failure during AOT compilation of audio_dsp.xtm\n`\n\n###
Next Steps:\n1. Verify macOS behavior after `ninja clean_aot` + rebuild\n2.
Debug AOT loading path to find why loading still fails\n3. Compare IR strings
between working (main path) and failing (first path) compilations\n4. Check if
additional components needed for AOT cache coherence

<!-- SECTION:NOTES:END -->
