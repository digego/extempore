---
id: TASK-040
title: Audit LLVM_SCHEME_FF_MAP and FFI table thread safety
status: Done
assignee: []
created_date: "2026-04-19 02:05"
updated_date: "2026-04-19 03:40"
labels:
  - cpp
  - concurrency
dependencies: []
priority: high
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

src/EXTLLVM.cpp:137 (LLVM_SCHEME_FF_MAP) and src/SchemeS7.cpp:25-57
(s_ffiCount/s_ffiTable) are acknowledged in comments as not thread safe. Audit
real access patterns and guard with std::shared_mutex or atomic counters where
appropriate.

Follow-up from #419.

## Resolution

- s_ffiTable / s_ffiCount: s_ffiCount is now std::atomic<int> with fetch_add;
  s_ffiTable entries are std::atomic<foreign_func> with release/acquire pairing
  between mk_foreign_func and ffi_trampoline. No mutex on the hot path.
- LLVM_SCHEME_FF_MAP: guarded with std::shared_mutex (shared_lock on get,
unique_lock on set). The getter now uses find() instead of operator[] to avoid
inserting an empty string for unknown ff.
<!-- SECTION:DESCRIPTION:END -->
