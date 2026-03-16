---
id: task-014
title: check cmake/cpack packaging
status: Done
assignee: []
created_date: '2025-12-18 09:19'
updated_date: '2025-12-18 09:48'
labels: []
dependencies: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
Reviewed the CMake/CPack packaging configuration and found it was broken:
- FetchContent integration of LLVM caused all LLVM install targets to be included in packages
- The `cpack` command would hang trying to install LLVM headers, cmake exports, and all library components

**Resolution**: Removed the PACKAGE option and CPack configuration entirely. Created task-015 to implement a GitHub Actions release workflow for binary distribution instead.
<!-- SECTION:DESCRIPTION:END -->
