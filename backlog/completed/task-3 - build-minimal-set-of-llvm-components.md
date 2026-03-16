---
id: task-3
title: build minimal set of llvm components
status: Done
assignee: []
created_date: '2025-12-16 03:35'
updated_date: '2025-12-16 04:08'
labels: []
dependencies: []
---

I _think_ that the current llvm build process (via cmake) builds more components
than extempore actually needs to link against.

For build time efficiency, we should building only the necessary components.

## Acceptance Criteria
<!-- AC:BEGIN -->
- [x] #1 LLVM builds only necessary libraries (no tools)
- [x] #2 Extempore compiles and links correctly
- [x] #3 Extempore runtime works (tested with --eval)
<!-- AC:END -->

## Implementation Plan

<!-- SECTION:PLAN:BEGIN -->
## Analysis

### Current state

The LLVM build is configured with:
- `LLVM_TARGETS_TO_BUILD=${LLVM_TARGET_ARCH}` - already minimal (only native arch)
- `LLVM_INCLUDE_TOOLS=ON` and `LLVM_BUILD_TOOLS=ON` - builds 91 command-line tools
- Various other options already disabled (tests, examples, docs, benchmarks)

### Components linked by Extempore

Extempore links against these LLVM components:
- OrcJIT, native, AsmParser, Passes, MCDisassembler, IRPrinter

This translates to 67 LLVM libraries (including transitive dependencies).

### Waste identified

**Unnecessary libraries (37 total, ~10MB):**
- LLVMExegesis*, LLVMLTO, LLVMMCA, LLVMMCJIT, LLVMTableGen*
- LLVMCoverage, LLVMDWARFLinker*, LLVMInterpreter, LLVMXRay, etc.

**Unnecessary tools (91 binaries, ~838MB):**
- llc, lli, opt, llvm-ar, llvm-objdump, bugpoint, dsymutil, etc.
- None of these are used by Extempore at build or runtime

### Proposed changes

1. Set `LLVM_BUILD_TOOLS=OFF` - skip building 91 tools
2. Set `LLVM_INCLUDE_TOOLS=OFF` - exclude tool sources entirely

This should significantly reduce build time (tools are expensive to link) and disk usage (~838MB saved in binaries).

### Risk assessment

- **Low risk**: Extempore uses `find_package(LLVM CONFIG)` and `llvm_map_components_to_libnames()` which are CMake-based, not dependent on `llvm-config` binary
- The fallback library list in CMakeLists.txt is derived from `llvm-config` but only used as documentation/reference, not executed

## Implementation

```cmake
# Change these lines in CMakeLists.txt (around line 277-278):
-DLLVM_INCLUDE_TOOLS=OFF
-DLLVM_BUILD_TOOLS=OFF
```

### Testing required

1. Clean LLVM build (`rm -rf build/llvm llvm`)
2. Full rebuild to verify libraries are created correctly
3. Run `ctest --label-regex libs-core` to verify Extempore works
<!-- SECTION:PLAN:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
Investigation completed 2025-12-16: Found that LLVM_BUILD_TOOLS=ON causes 91 unnecessary tool binaries (~838MB) to be built. Changing to OFF should provide significant build time savings.

Fix committed: c2072e75 - disabled LLVM tools build. Savings: ~838MB disk space, significant build time reduction. Note: a clean LLVM rebuild is required to see the benefits (rm -rf build/llvm llvm).

Verified after clean rebuild: LLVM install reduced from ~800MB+ to 155MB. Only llvm-tblgen (4.1MB) in bin/, vs 91 tools (838MB) previously. Extempore builds and runs correctly.
<!-- SECTION:NOTES:END -->
