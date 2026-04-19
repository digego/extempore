---
id: TASK-043
title: Consolidate OSC.cpp UDP/TCP + Windows/POSIX duplication
status: To Do
assignee: []
created_date: '2026-04-19 02:05'
labels:
  - cpp
  - refactor
dependencies: []
priority: medium
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
src/OSC.cpp — 1700+ lines with UDP and TCP paths each duplicated
across Windows (networking-TS experimental) and POSIX. Byte-swap helpers
(lines 93-222) reimplement __builtin_bswap*. Std::byteswap (C++23) will
eventually replace them.

Large standalone project — its own PR.
Follow-up from #419.
<!-- SECTION:DESCRIPTION:END -->
