---
id: task-015
title: Add GitHub Actions release workflow for binary distribution
status: To Do
assignee: []
created_date: '2025-12-18 09:48'
labels:
  - ci
  - packaging
dependencies: []
priority: medium
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
Create a GitHub Actions workflow to build and publish binary releases of Extempore.

## Background

The previous CMake/CPack packaging approach was removed because:
- FetchContent integration of LLVM caused all LLVM install targets to be included in the package
- This made the package enormous and broken (including LLVM headers, cmake files, etc.)
- Binary distribution is better handled via CI/CD

## Requirements

The workflow should:
1. Trigger on tagged releases (e.g. `v0.8.10`)
2. Build for multiple platforms:
   - macOS (arm64 and x86_64)
   - Linux (x86_64, possibly arm64)
   - Windows (x86_64)
3. Create self-contained ZIP archives containing:
   - The `extempore` binary
   - `runtime/` directory
   - `libs/` directory (including platform-shlibs)
   - `examples/` directory
   - `assets/` directory (downloaded during build)
4. Upload archives as GitHub release assets
5. Use appropriate compiler flags for portable binaries (e.g. `-mtune=generic` on x86_64)
6. Set appropriate deployment targets (e.g. macOS 11.0 for arm64, 10.12 for x86_64)

## Notes

- The existing `.github/workflows/build-and-test.yml` can be used as a reference
- Consider using a matrix build strategy
- AOT compilation of stdlib should be included in the release build
- May want to code-sign macOS binaries to avoid "damaged app" warnings
<!-- SECTION:DESCRIPTION:END -->
