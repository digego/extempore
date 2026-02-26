---
id: TASK-032
title: Replace compiler assoc-list caches with hash tables
status: Done
assignee: []
created_date: '2026-02-26 09:44'
updated_date: '2026-02-26 10:10'
labels:
  - compiler
  - performance
dependencies: []
priority: high
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
The nine xtlang compiler caches in runtime/llvmti.xtm (closure-cache, nativefunc-cache, polyfunc-cache, etc.) all use association lists with assoc-strcmp lookup, giving O(n) per lookup. Replace them with Extempore's built-in hash tables (make-hashtable, hashtable-ref, hashtable-set\!) for O(1) lookup. This is the lowest-risk, highest-impact performance improvement.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [x] #1 All nine caches in llvmti.xtm use hash tables instead of assoc lists
- [x] #2 Cache API functions (register-new-*, *-exists?, get-*-type, set-*-type) updated to use hash table operations
- [x] #3 reset-*-cache and print-*-cache functions work correctly with hash tables
- [x] #4 Core library tests pass (ctest -L libs-core)
- [x] #5 AOT compilation works (build aot_external_audio target)
<!-- AC:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
Implemented C FFI hash table primitives in src/ffi/hashtable.inc (DJB2 hashing, Scheme vector buckets for GC safety). Converted 9 of 11 compiler caches from assoc-lists to hash tables. Left genericfunc-cache and generictype-cache as alists (multimap semantics, symbol keys, complex iteration patterns). All 6 core tests pass, AOT compilation succeeds for both core and external audio.
<!-- SECTION:NOTES:END -->
