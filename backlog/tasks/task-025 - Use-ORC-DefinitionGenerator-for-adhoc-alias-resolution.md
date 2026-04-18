---
id: TASK-025
title: Use ORC DefinitionGenerator for adhoc alias resolution
status: To Do
assignee: []
created_date: '2026-02-19 02:23'
labels:
  - llvm
  - jit
  - cleanup
dependencies: []
references:
  - src/EXTLLVM.cpp
priority: low
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
The current adhoc alias map (sAdhocAliases in EXTLLVM.cpp) resolves counter-less adhoc names by maintaining a std::unordered_map in application code. The more LLVM-native approach would be to implement a custom ORC DefinitionGenerator that intercepts failed lookups and resolves the counter-less name to its counter-ful equivalent at the JIT layer.

This would move the alias resolution into the JIT's own symbol resolution pipeline rather than wrapping it in getFunctionAddress().
<!-- SECTION:DESCRIPTION:END -->
