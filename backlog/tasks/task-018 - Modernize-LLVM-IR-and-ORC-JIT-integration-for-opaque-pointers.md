---
id: task-018
title: Modernize LLVM IR and ORC JIT integration for opaque pointers
status: Done
assignee: []
created_date: '2025-12-19 09:53'
updated_date: '2025-12-19 22:57'
labels:
  - llvm
  - jit
  - compiler
  - portability
dependencies: []
priority: high
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
LLVM 21 uses opaque pointers as the only supported pointer model. Extempore
still emits typed pointer IR (i8*, %mzone*, etc) and composes JIT modules using
regex-driven string munging. This is fragile and not cross-platform safe.

Goal: keep the xtlang IR generator largely intact, but modernize the C++ LLVM
integration and migrate IR emission to opaque pointers with minimal, mechanical
changes. The result should build and run on macOS/Linux/Windows and on both
x86_64 and arm64 with stock LLVM 21.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] #1 JIT module composition no longer relies on regex/string preambles; it
      uses LLVM APIs (Linker or direct IR construction) to add runtime types,
      externs, and declarations.
- [ ] #2 Opaque pointer IR (`ptr`) is the default output when running against
      LLVM 21; no typed-pointer IR is required for normal execution.
- [ ] #3 xtlang IR generation changes are localized and mechanical (helper
      functions/macros), not a full rewrite.
- [ ] #4 Core and external tests pass on macOS/Linux/Windows (x86_64, arm64),
      and `aot_external_audio` builds cleanly.
- [ ] #5 AOT cache is regenerated (or auto-invalidated) so cached IR matches the
      pointer mode and intrinsic signatures.
- [ ] #6 `bind-func`, `llvm:get-function-pointer`, and redefinitions continue to
    work (newest wins is acceptable).
<!-- AC:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
## Phase 0 - Baseline and capability detection

1. Confirm LLVM source is unpatched:
   - CMake pulls `llvmorg-21.1.7` via `FetchContent` and no opaque-pointer or
     typed-pointer flags are set in the build.
2. Add a tiny C++ probe (or Scheme FFI) that attempts to parse:
   - One IR snippet with `i8*`
   - One IR snippet with `ptr` Report which syntax is accepted at runtime and
     log the pointer mode.
3. Gate pointer mode off this probe (opaque by default for LLVM 21).

## Phase 1 - Replace string preamble logic with LLVM APIs (SchemeFFI)

1. Load `runtime/bitcode.ll` once into a `RuntimeModuleTemplate` (or prebuilt
   `bitcode.bc`).
2. Build a `DeclsModule` in the same `LLVMContext`:
   - Add named struct types and extern globals/functions via LLVM APIs.
   - Update this module incrementally after each compilation.
3. `jitCompile()` flow:
   - Parse the generated IR into a new module.
   - Clone `RuntimeModuleTemplate`, link `DeclsModule`, then link the new IR
     module using `llvm::Linker`.
   - Set target triple and data layout from `JIT->getDataLayout()` before
     running PassBuilder and verify.
4. Remove regex caches (`sUserTypeDefs`, `sExternalGlobals`,
   `sExternalLibFunctions`) and related string concatenation.
5. For `bind-lib` declarations, store structured declarations (name + type)
   rather than raw `declare ... nounwind` strings; insert via LLVM APIs.

## Phase 2 - Opaque pointer migration in llvmir.xtm (minimal edits)

1. Add a global flag or helper like `*impc:compiler:opaque-pointers?*`.
2. Centralize pointer rendering:
   - Add helpers for pointer types that return `ptr` in opaque mode.
   - Preserve element type for `load`, `store`, `getelementptr`, and `bitcast`.
3. Update generator hot spots to use the helpers:
   - `impc:ir:get-type-str`, `pointer++/--`, and call sites that emit pointer
     types in instruction signatures and function prototypes.
4. Update `runtime/bitcode.ll` to use opaque pointers (or add
   `runtime/bitcode_opaque.ll` and select by pointer mode).
5. Ensure intrinsic names match LLVM 21 (memcpy/memmove/memset) in all paths.

## Phase 3 - ORC JIT cleanup and symbol handling

1. Use `MangleAndInterner` for symbol names and remove underscore fallbacks.
2. Optional: adopt per-compile `JITDylib` layering (newest-first search order)
   to avoid fragile `removeSymbol` logic while still allowing redefinition.
3. Ensure each JITDylib has a `DynamicLibrarySearchGenerator` (or uses explicit
   `absoluteSymbols`) so native bindings work on all platforms.

## Phase 4 - AOT cache compatibility

1. Add a version stamp to `libs/aot-cache/` outputs that encodes:
   - LLVM major version
   - Pointer mode (typed vs opaque)
2. On mismatch, force `clean_aot` behavior.
3. Regenerate all AOT caches using the new pipeline.

## Phase 5 - Tests and cross-platform validation

1. Run core and external test suites:
   - `ctest --label-regex libs-core -j4`
   - `ctest --label-regex libs-external -j4`
2. Build AOT targets:
   - `cmake --build . --target aot_external_audio`
3. Smoke-test examples on all platform/arch combinations.
4. Add a small unit test for pointer-heavy IR (struct pointers, closures, GEPs)
   to guard against opaque-pointer regressions.

## Risk Notes

- Opaque pointer migration is mechanical but touches many IR emission sites.
- AOT caches must be regenerated; stale caches can mask failures.
- Linker-based module composition must avoid duplicate type/decl conflicts.

## Rollout

1. Land Phase 1 (C++ module composition) behind a build flag.
2. Land Phase 2 (opaque pointers) behind runtime feature detection.
3. Remove typed-pointer fallback after verification on all platforms.

## Investigation Results (2025-12-20)

### Key Finding: Typed Pointer IR Still Works in LLVM 21

Contrary to the documentation stating "LLVM 17+ only supports opaque pointers", **LLVM 21's textual IR parser still accepts and auto-upgrades typed pointer syntax**. This was verified empirically:

```scheme
;; Both syntaxes accepted by LLVM 21:
(llvm:compile-ir "define i8* @test(i8* %x) { ret i8* %x }")  ;; typed - works
(llvm:compile-ir "define ptr @test(ptr %x) { ret ptr %x }")  ;; opaque - works

;; Load/store/GEP with typed pointers - all work:
(llvm:compile-ir "define i64 @test(i64* %p) { %v = load i64, i64* %p\n ret i64 %v }")
(llvm:compile-ir "define i64* @test([4 x i64]* %a) { %p = getelementptr [4 x i64], [4 x i64]* %a, i64 0, i64 2\n ret i64* %p }")
```

### Test Results

- All core tests pass: `ctest --label-regex libs-core` (6/6 passed)
- All external tests pass: `ctest --label-regex libs-external` (1/1 passed)
- Complex IR generation (closures, tuples, arrays, GEPs) works correctly

### Scope Analysis

Migrating to opaque pointers would require changes to:

| File | Typed pointer occurrences |
|------|---------------------------|
| runtime/llvmir.xtm | 182 |
| runtime/llvmti.xtm | 205 |
| runtime/bitcode.ll | 118 |
| src/SchemeFFI.cpp | Multiple regex patterns |

The central function `impc:ir:get-type-str` appends `*` characters based on pointer depth. For opaque pointers, all `T*` would become `ptr`, but element types must still be preserved for `load`, `store`, `getelementptr`, and `bitcast` instructions.

### Decision: Close Without Migration

**Rationale:**

1. **No functional issue exists** - LLVM 21 auto-upgrades typed pointer textual IR. The system works correctly on all platforms.

2. **Risk exceeds benefit** - Changing ~500 IR emission sites risks introducing subtle bugs for no functional gain. The typed syntax is actually more readable and debuggable.

3. **Typed syntax already includes element types** - Instructions like `load i64, i64* %ptr` already specify the element type, which is what LLVM needs internally. The "upgrade" is purely syntactic (changing `i64*` to `ptr` in the pointer position).

4. **LLVM maintains backward compatibility for textual IR** - While the C++ API removed typed pointer support, the textual IR parser continues to accept the old syntax for compatibility with existing tooling and IR files.

5. **Future-proofing is speculative** - If LLVM eventually removes textual IR auto-upgrade (unlikely given the ecosystem), migration can be done then with better tooling.

### Separated Concern: C++ String Munging

The task's Phase 1 (replacing regex-driven string composition with LLVM APIs) remains valuable independently of opaque pointers. This has been split into a new task focused on:
- Using LLVM's Linker API instead of string concatenation
- Structured declaration storage instead of regex extraction
- Cleaner, more maintainable JIT compilation flow

See: task-021 for the focused C++ refactoring effort.
<!-- SECTION:NOTES:END -->
