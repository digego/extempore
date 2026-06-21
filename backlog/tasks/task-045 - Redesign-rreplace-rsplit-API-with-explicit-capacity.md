---
id: TASK-045
title: Redesign rreplace/rsplit API with explicit capacity
status: To Do
assignee: []
created_date: "2026-04-19 02:05"
labels:
  - cpp
  - refactor
  - xtlang
dependencies: []
priority: low
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

rreplace and rsplit currently pass fixed-size char\* buffers from xtlang
(libs/core/adt.xtm uses salloc 2048/4096). #419 added a ceiling check that
returns false if the output would overflow, but the proper fix is an API that
passes capacity, or returns std::string.

Requires coordinated changes in runtime/bitcode.ll type signatures and adt.xtm
callers. Scope it as its own PR.

Follow-up from #419.

<!-- SECTION:DESCRIPTION:END -->
