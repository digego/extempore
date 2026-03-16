---
id: task-6
title: see if GH build and test action is caching the LLVM build
status: Done
assignee: []
created_date: '2025-12-16 10:43'
updated_date: '2025-12-16 21:53'
labels: []
dependencies: []
---

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
## Investigation (2025-12-17)

### Current caching configuration

The workflow at `.github/workflows/build-and-test.yml` **does have caching configured**:

```yaml
- name: Cache LLVM build
  id: cache-llvm
  uses: actions/cache@v4
  with:
    path: build/_deps
    key: ${{ matrix.os }}-llvm-${{ env.LLVM_VERSION }}
```

Cache keys are:
- `macos-15-llvm-21.1.7`
- `ubuntu-24.04-llvm-21.1.7`
- `windows-2022-llvm-21.1.7`

### Current cache state

Only **macOS** caches exist (2 entries, ~452 MiB each):
- `macos-15-llvm-21.1.7` created 2025-12-16T11:17:02Z
- `macos-15-llvm-21.1.7` created 2025-12-16T11:08:51Z

**No caches exist for Linux or Windows.**

### Why caches aren't being used

1. **Cache saves require job success**: The `actions/cache` action only saves on successful job completion by default. Linux and Windows builds are failing, so their caches never get saved.

2. **No runs since cache creation**: The macOS cache was created at 11:08-11:17 UTC, but the most recent workflow run was at 10:46 UTC. No subsequent runs have tested whether the macOS cache would be hit.

3. **Duplicate cache entries**: There are two macOS cache entries with the same key, likely from parallel runs racing to save.

### Recent run analysis (run 20265237222)

| Platform | Cache status | Build result | Cache saved |
|----------|--------------|--------------|-------------|
| macOS aarch64 | miss | success | yes |
| Linux x86_64 | miss | failure | no (skipped) |
| Windows x86_64 | miss | failure | no (skipped) |

### Recommendations

1. **Fix Linux/Windows builds first**: Until these succeed, caches won't be saved. This is the primary blocker.

2. **Consider `save-always: true`**: Add this to save caches even on failure, so subsequent runs can benefit:
   ```yaml
   - name: Cache LLVM build
     uses: actions/cache@v4
     with:
       path: build/_deps
       key: ${{ matrix.os }}-llvm-${{ env.LLVM_VERSION }}
       save-always: true
   ```
   This would save ~45+ minutes of LLVM build time per platform on subsequent runs.

3. **Verify cache hit on next macOS run**: The next successful trigger should show "Cache restored" for macOS, confirming caching works.

### Summary

Caching **is configured correctly** but **not working effectively** because:
- Linux/Windows builds fail before cache can be saved
- macOS cache exists but hasn't been tested with a cache hit yet
<!-- SECTION:NOTES:END -->
