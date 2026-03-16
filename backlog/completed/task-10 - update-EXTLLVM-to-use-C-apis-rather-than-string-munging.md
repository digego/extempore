---
id: task-10
title: update EXTLLVM to use C++ apis rather than string munging
status: Done
assignee: []
created_date: '2025-12-17 23:47'
updated_date: '2025-12-18 00:34'
labels: []
dependencies: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
Refactored C++ code in EXTLLVM/SchemeFFI to use LLVM C++ APIs instead of string munging with regex and rsplit.
<!-- SECTION:DESCRIPTION:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
## Changes made

### 1. Replaced `SanitizeType` with `formatLLVMType` (SchemeFFI.cpp)

**Before:** Used regex (`sTypeSuffixRegex`) to strip numeric suffixes from type names after printing to string.

**After:** Uses `StructType::hasName()` and `StructType::getName()` to directly access the struct name, then manually strips numeric suffixes (e.g., `.123`) without regex.

### 2. Refactored `get_function_args` (llvm.inc)

**Before:** Used `rsplit(" = type ", ...)` to extract type names from printed struct types.

**After:** Simply calls `formatLLVMType()` which handles struct types properly via LLVM APIs.

### 3. Refactored `get_named_type` (llvm.inc)

**Before:** Used `rsplit(" = type ", ...)` to extract the struct body from printed output.

**After:** Uses `StructType::elements()` to iterate over struct elements and build the body string directly.

### 4. Removed unused code

- Removed `sTypeSuffixRegex` regex pattern
- Removed `tmp_str_a` and `tmp_str_b` static buffers

## Testing

All tests pass:
- 6/6 core tests passed
- 1/1 external tests passed
<!-- SECTION:NOTES:END -->
