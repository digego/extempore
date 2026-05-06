---
id: TASK-020
title: add libs/aot-cache back to the build cache in GH actions
status: Done
assignee: []
created_date: '2025-12-19 10:37'
updated_date: '2026-04-21 11:28'
labels: []
dependencies: []
---

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
Added separate actions/cache pair for libs/aot-cache in .github/workflows/build-and-test.yml. Key: {os}-aot-llvm-{version}-{hashFiles(xtm/ll sources + src/ + include/)}. restore-keys allows partial matches so CMake's incremental build regenerates only what changed. YAML validated locally; CI will confirm on next push.
<!-- SECTION:NOTES:END -->
