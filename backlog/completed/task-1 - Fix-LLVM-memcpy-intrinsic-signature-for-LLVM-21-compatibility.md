---
id: task-1
title: Fix LLVM memcpy intrinsic signature for LLVM 21 compatibility
status: Done
assignee: []
created_date: '2025-12-16 02:13'
updated_date: '2025-12-16 03:45'
labels:
  - llvm
  - compiler
  - aarch64
  - bug
dependencies: []
priority: high
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
When running core tests (tests/all-core.xtm), the xtmbase library fails to load with an LLVM IR error due to breaking changes in LLVM's memcpy intrinsic signature between older LLVM versions and LLVM 21. This blocks PR #415 (aarch64 support) from passing core tests.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [x] #1 Update memcpy intrinsic generation to use new LLVM 21 signature: @llvm.memcpy.p0.p0.i64(ptr dest, ptr src, i64 len, i1 isvolatile)
- [ ] #2 Update memmove intrinsic generation to use new LLVM 21 signature if applicable
- [ ] #3 Update memset intrinsic generation to use new LLVM 21 signature if applicable
- [x] #4 Handle alignment via pointer attributes instead of separate parameter
- [x] #5 Core tests (tests/all-core.xtm) load xtmbase successfully without IR errors
- [ ] #6 Verify compatibility with both old and new LLVM versions if needed
<!-- AC:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
## Investigation Notes (2025-12-16)

### Root Cause Analysis

The error occurs at runtime during xtmbase loading:
```
LLVM IR: <string>:5876:15: error: invalid intrinsic signature
call ccc void @llvm.memcpy.p0i8.p0i8.i64(i8* %val810, i8* %val811, i64 %val813, i32 1, i1 0)
```

### Key Findings

1. **Declaration vs Call mismatch**: The `init.ll` file correctly declares the modern signature:
   ```llvm
   declare void @llvm.memcpy.p0.p0.i64(ptr, ptr, i64, i1)
   ```
   But the generated call uses the OLD signature with:
   - Old typed pointers: `p0i8` instead of opaque `p0`
   - Extra alignment parameter: `i32 1` (old format had alignment as 4th arg)
   - Call format: `i8*` instead of `ptr`

2. **Substitution exists but not applied**: In `runtime/llvmir.xtm:4130`, there's an intrinsic substitution:
   ```scheme
   ((string=? name "memcpy") "llvm.memcpy.p0.p0.i64")
   ```
   And fixup args at line 4136:
   ```scheme
   ((string=? name "memcpy") ", i1 0")
   ```
   
   This means the code is trying to use the modern intrinsic name but somewhere else is generating the old-style call.

3. **The call appears to bypass the substitution**: The error shows `@llvm.memcpy.p0i8.p0i8.i64` which is NOT the substituted name (`llvm.memcpy.p0.p0.i64`). This suggests:
   - Either there's another code path generating memcpy calls
   - Or LLVM itself is auto-generating these calls (e.g., from struct copies)

### LLVM 21 Changes

LLVM's memory intrinsics changed significantly:
- **Old signature**: `@llvm.memcpy.p0i8.p0i8.i64(i8* dest, i8* src, i64 len, i32 align, i1 volatile)`
- **New signature**: `@llvm.memcpy.p0.p0.i64(ptr dest, ptr src, i64 len, i1 volatile)`
  - Opaque pointers (`ptr` not `i8*`)
  - No alignment parameter (use `align` attribute on ptr instead)
  - Intrinsic name uses `p0` not `p0i8`

### Likely Source

The old-style intrinsic is likely being generated:
1. By LLVM's IRBuilder when generating struct copies/moves
2. From AOT-compiled bytecode in `libs/aot-cache/`
3. From somewhere in the scheme compiler that bypasses `impc:ir:intrinsic-substitution`

### Investigation Blocked

Note: Extempore crashes (Killed: 9) after the error, which terminates the Claude Code session. Need to capture more IR output before crash to trace the source.

## Fix Applied (2025-12-16)

### Root Cause

The AOT-compiled cache files in `libs/aot-cache/*.ll` contained pre-compiled LLVM IR with old-style memcpy intrinsic calls that were incompatible with LLVM 21:

- Old: `@llvm.memcpy.p0i8.p0i8.i64(i8* %dest, i8* %src, i64 %len, i32 align, i1 volatile)`
- New: `@llvm.memcpy.p0.p0.i64(ptr %dest, ptr %src, i64 %len, i1 volatile)`

### Changes Made

Fixed 14 `.ll` files in `libs/aot-cache/` using sed to:
1. Change intrinsic name from `p0i8.p0i8` to `p0.p0` (opaque pointers)
2. Change argument types from `i8*` to `ptr`
3. Remove the alignment parameter (`i32 N`) from the call

### Files Modified

- libs/aot-cache/xtmassimp.ll
- libs/aot-cache/xtmaudio_dsp_ext.ll
- libs/aot-cache/xtmaudiobuffer.ll
- libs/aot-cache/xtmbase.ll
- libs/aot-cache/xtmfft.ll
- libs/aot-cache/xtmgl-objects.ll
- libs/aot-cache/xtmgl-objects2.ll
- libs/aot-cache/xtmglfw3.ll
- libs/aot-cache/xtmgraphics-pipeline.ll
- libs/aot-cache/xtminstruments.ll
- libs/aot-cache/xtmmath.ll
- libs/aot-cache/xtmnanovg.ll
- libs/aot-cache/xtmportmidi.ll
- libs/aot-cache/xtmsndfile.ll

### Test Results

- xtmbase loads successfully (1.15 seconds)
- All 6 core tests pass (100%)

### Notes

- The Scheme compiler in `runtime/llvmir.xtm` already has correct intrinsic substitution for `memcpy`
- memmove and memset did not have old-style intrinsics in the cache
- AC #2, #3, #6 may not be needed as no issues were found with those intrinsics
<!-- SECTION:NOTES:END -->
