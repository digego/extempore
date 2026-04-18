---
id: TASK-2
title: investigate why GH Actions test matrix no longer works
status: Done
assignee: []
created_date: '2025-12-16 03:33'
updated_date: '2026-02-22 21:57'
labels: []
dependencies: []
---

Honestly, it'd be fine to test just these platforms:

- latest ubuntu (x86_64)
- latest macOS (x86_64)
- latest macOS (aarch64)
- latest windows (x86_64)

Keep it simple, and as fast as possible.

## Final Summary

<!-- SECTION:FINAL_SUMMARY:BEGIN -->
CI test matrix is working on the aarch64 branch (Linux x64, macOS arm64, Windows x64). Will carry over when merged to master.
<!-- SECTION:FINAL_SUMMARY:END -->
