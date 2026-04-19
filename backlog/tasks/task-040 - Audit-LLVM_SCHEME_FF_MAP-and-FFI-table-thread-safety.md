---
id: TASK-040
title: Audit LLVM_SCHEME_FF_MAP and FFI table thread safety
status: To Do
assignee: []
created_date: '2026-04-19 02:05'
labels:
  - cpp
  - concurrency
dependencies: []
priority: high
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
src/EXTLLVM.cpp:137 (LLVM_SCHEME_FF_MAP) and src/SchemeS7.cpp:25-57
(s_ffiCount/s_ffiTable) are acknowledged in comments as not thread safe.
Audit real access patterns and guard with std::shared_mutex or atomic
counters where appropriate.

Follow-up from #419.
<!-- SECTION:DESCRIPTION:END -->
