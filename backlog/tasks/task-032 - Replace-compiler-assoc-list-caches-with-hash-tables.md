---
id: TASK-032
title: Replace compiler assoc-list caches with hash tables
status: To Do
assignee: []
created_date: '2026-02-26 09:44'
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
- [ ] #1 All nine caches in llvmti.xtm use hash tables instead of assoc lists
- [ ] #2 Cache API functions (register-new-*, *-exists?, get-*-type, set-*-type) updated to use hash table operations
- [ ] #3 reset-*-cache and print-*-cache functions work correctly with hash tables
- [ ] #4 Core library tests pass (ctest -L libs-core)
- [ ] #5 AOT compilation works (build aot_external_audio target)
<!-- AC:END -->
