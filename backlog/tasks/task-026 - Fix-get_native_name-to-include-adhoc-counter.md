---
id: TASK-026
title: Fix get_native_name to include adhoc counter
status: To Do
assignee: []
created_date: '2026-02-19 02:23'
labels:
  - xtlang
  - macros
  - cleanup
dependencies: []
references:
  - libs/base/base.xtm
  - src/EXTLLVM.cpp
priority: low
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
The xtlang get_native_name macro (in libs/base/base.xtm) generates adhoc names without the counter (e.g. foo_adhoc_W2k4K_native), but the compiler always includes a counter (e.g. foo_adhoc_9_W2k4K_native). This naming mismatch is the root cause of the get_native_fptr lookup failure that was worked around with the sAdhocAliases map.

Reconciling the naming at the Scheme macro level so get_native_name produces names matching what the compiler emits would eliminate the need for the alias map entirely. This is a deeper change touching the xtlang macro system and needs careful testing across all uses of get_native_fptr, spawn, and syncspawn.
<!-- SECTION:DESCRIPTION:END -->
