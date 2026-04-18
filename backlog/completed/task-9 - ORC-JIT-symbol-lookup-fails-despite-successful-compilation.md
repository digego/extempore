---
id: task-9
title: ORC JIT symbol lookup fails despite successful compilation
status: Done
assignee: []
created_date: '2025-12-17 17:30'
updated_date: '2025-12-17 22:41'
labels:
  - llvm
  - jit
  - bug
  - critical
dependencies: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
After upgrading to LLVM 21 ORC JIT, functions compile and execute correctly but
`llvm:get-function-pointer` returns `#f` (not found). This breaks AOT loading
because it relies on `mk-ff` which calls `llvm:get-function-pointer` to bind
Scheme functions to compiled xtlang code.

The underlying `JIT->lookup(name)` call in `getFunctionAddress()` fails to find
symbols that were just added via `JIT->addIRModule()`.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [x] #1 `llvm:get-function-pointer` returns valid cptr for functions compiled
      via `bind-func`
- [x] #2 `llvm:get-function-pointer` returns valid cptr for functions loaded
      from AOT cache via `llvm:compile-ir`
- [x] #3 Clean build from scratch completes AOT compilation successfully
- [x] #4 Loading AOT-compiled libraries works without 'non-cptr obj #f' errors
<!-- AC:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
## Technical Analysis

### Call Flow

1. `bind-func` generates LLVM IR and calls `jitCompile()` in `SchemeFFI.cpp`
2. `jitCompile()` parses IR and calls `EXTLLVM::addTrackedModule()`
3. `addTrackedModule()` calls `JIT->addIRModule()` - this succeeds
4. Later, `llvm:get-function-pointer` calls `EXTLLVM::getFunctionAddress()`
5. `getFunctionAddress()` calls `JIT->lookup(name)` - this fails!

### Key Code Locations

**Symbol Lookup (src/EXTLLVM.cpp:616-624):**

```cpp
uint64_t getFunctionAddress(const std::string& name) {
    if (!JIT) return 0;
    auto sym = JIT->lookup(name);
    if (!sym) {
        llvm::consumeError(sym.takeError());
        return 0;  // Returns 0 when lookup fails
    }
    return sym->getValue();
}
```

**Module Addition (src/EXTLLVM.cpp:648-658):**

```cpp
llvm::Error addTrackedModule(llvm::orc::ThreadSafeModule TSM, const std::vector<std::string>& symbolNames) {
    if (!JIT) return llvm::make_error<llvm::StringError>("JIT not initialized", llvm::inconvertibleErrorCode());
    // Note: symbolNames parameter is ignored!
    if (auto err = JIT->addIRModule(std::move(TSM))) {
        return err;
    }
    return llvm::Error::success();
}
```

### Hypothesis

The ORC JIT's lazy compilation may not be materializing symbols before lookup,
or there's a symbol visibility/linkage issue. The symbols might need to be
explicitly registered or have different linkage settings.

Possible fixes to investigate:

1. Force materialization of symbols after adding module
2. Check if symbols need explicit export flags
3. Verify JITDylib symbol table contains the symbols
4. Check if there's a name mangling mismatch

### Related LLVM Changes

LLVM 21 ORC JIT has significant API changes from earlier versions. The symbol
resolution strategy may have changed.

## Additional Findings

### Functions Actually Work Despite Lookup Returning #f

Testing shows:

- `bind-func` creates functions that **execute correctly** (return right values)
- `llvm:get-function-pointer` returns `#f` for function names
- `llvm:get-function` (metadata lookup) also returns `#f`
- The adhoc symbols ARE defined as `#<FOREIGN>` and work

### Platform Difference Hypothesis

**Why macOS might work while Linux fails:**

The issue appears to be in the early startup/AOT loading path, not in
`bind-func` itself. The error occurs during `sys:load "libs/base/base.xtm"` when
trying to compile an expression:

```
eval: unbound variable: xtlang_expression_adhoc_1_W2k4Kl0
Trace: xtlang_expression <- impc:ti:get-expression-type <- sys:load
```

This happens BEFORE user code runs. Possible platform differences:

1. **Cached state**: macOS might have AOT cache from older LLVM that still works
2. **Symbol resolution timing**: ORC JIT might materialize symbols differently
   per platform
3. **First compilation path**: The first jitCompile when
   `sInlineBitcode.empty()` might behave differently

### Core Issue Identified

The `impc:ti:get-expression-type` function tries to compile and run an
`xtlang_expression` during type inference. This expression compilation fails
because symbol lookup returns `#f`.

But later `bind-func` calls work because by then the JIT is fully initialized
and symbols are being materialized properly.

### Next Investigation Steps

1. Add debug output to `jitCompile` to see if first compilation path differs
2. Check if symbols are in `sGlobalMap` after addModule()
3. Check if `JIT->lookup()` error messages reveal anything
4. Compare LLVM JIT configuration between platforms

## macOS Verification Test

**Purpose:** Determine if this is a Linux-specific issue or affects all
platforms.

### Test Procedure

Run a completely clean build on macOS:

```bash
cd /path/to/extempore

# IMPORTANT: Remove ALL cached state
rm -rf build
rm -rf libs/aot-cache

# Fresh build
mkdir build && cd build
cmake .. && make -j$(sysctl -n hw.ncpu)
```

### Expected Outcomes

**If macOS build FAILS with the same error:**

```
Loading xtmbase library... eval: unbound variable: xtlang_expression_adhoc_1_W2k4Kl0
```

→ Issue is **platform-agnostic**, related to LLVM 21 ORC JIT initialization. Fix
should focus on the first compilation path in `jitCompile()`.

**If macOS build SUCCEEDS:** → Issue is **Linux-specific**. Investigate:

- Symbol visibility differences (ELF vs Mach-O)
- ORC JIT platform-specific behavior
- Name mangling differences

### What to Report

After running the test, note:

1. Did the build complete successfully?
2. If failed, what was the exact error message?
3. At what percentage/stage did it fail?
4. Can you run
   `./extempore --batch '(bind-func test (lambda () 42)) (println (test))'`
   successfully?

## macOS Verification Result (2025-12-17)

**Result: macOS build SUCCEEDS** — completed at 100% with no errors.

Clean build procedure:

```bash
rm -rf build libs/aot-cache
mkdir build && cd build
cmake .. && cmake --build . -j$(sysctl -n hw.ncpu)
```

All AOT compilation completed successfully. This confirms the issue is
**Linux-specific**.

### Linux-Specific Investigation

Since macOS works but Linux fails, the issue is likely related to:

1. **ELF vs Mach-O symbol visibility** — On ELF (Linux), symbols have no prefix.
   On Mach-O (macOS), symbols have `_` prefix. The
   `DynamicLibrarySearchGenerator` uses `getGlobalPrefix()` which returns `_` on
   macOS and empty string on Linux.

2. **Symbol export flags** — ELF may require explicit visibility attributes that
   Mach-O doesn't need.

3. **ORC JIT platform differences** — The JIT's symbol resolution may behave
   differently on Linux.

Next step: Check if the lookup is failing due to symbol mangling or if symbols
are not being properly added to the JITDylib on Linux.
<!-- SECTION:NOTES:END -->

## Minimal Reproduction

### What Works

```scheme
;; Direct IR compilation works:
(llvm:compile-ir "define i64 @testfn() { ret i64 42 }")
;; Returns: #<CPTR: ...>

;; bind-func compiles and executes:
(bind-func test_simple (lambda () 42))
(test_simple)  ;; Returns: 42
```

### What Fails

```scheme
;; Function pointer lookup fails immediately after bind-func:
(bind-func test_simple (lambda () 42))
(llvm:get-function-pointer "test_simple")
;; Returns: #f  <-- SHOULD return valid cptr

;; This breaks AOT loading which does:
(mk-ff "hermite_interp_local" (llvm:get-function-pointer "hermite_interp_local_scheme"))
;; ^ When get-function-pointer returns #f, mk-ff tries to use it as a cptr, causing:
;; "Attempting to return a cptr from a non-cptr obj #f"
```

### Full Reproduction

```bash
# Clean build
cd /path/to/extempore
rm -rf build && mkdir build && cd build
cmake .. && make -j$(nproc)

# Build succeeds up to 98%, then fails during AOT compilation with:
# Loading xtmaudiobuffer library... Error: evaluating expr: (impc:aot:compile-xtm-file "libs/core/audio_dsp.xtm")
# Attempting to return a cptr from a non-cptr obj #f
```
