---
id: task-9
title: fix GH actions caching issues
status: Done
assignee: []
created_date: '2025-12-17 05:55'
updated_date: '2025-12-17 06:15'
labels: []
dependencies: []
---

Look at recent runs - the LLVM build isn't cached, which costs lots of time.

See this info which might be relevant:
https://github.com/actions/cache/tree/main/save#always-save-cache

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
## Fix applied (2025-12-17)

Replaced deprecated `save-always` option with separate `actions/cache/restore` and `actions/cache/save` steps.

Key changes to `.github/workflows/build-and-test.yml`:
1. Changed `actions/cache@v4` to `actions/cache/restore@v4` for the restore step
2. Added new "Save LLVM cache" step after Build with condition `if: always() && steps.cache-llvm.outputs.cache-hit != 'true'`

This ensures cache is saved even when build/tests fail, preventing loss of expensive LLVM compilation work.

Commit: 924be82f
Run: https://github.com/digego/extempore/actions/runs/20293247026

Waiting for run to complete to verify caches are created for all platforms.

## Verified working (2025-12-17)

All three platform caches now exist:
- ubuntu-24.04-llvm-21.1.7 (414.65 MiB)
- windows-2022-llvm-21.1.7 (376.08 MiB) 
- macos-15-llvm-21.1.7 (451.96 MiB)

Windows cache was saved despite build failure, confirming the `always()` condition works correctly.
<!-- SECTION:NOTES:END -->
