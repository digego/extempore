---
id: task-11
title: setup CTest to use --batch mode if possible for easier parallelisation
status: Done
assignee: []
created_date: '2025-12-18 00:11'
updated_date: '2025-12-18 00:21'
labels: []
dependencies: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
Updated CTest configuration to use `--batch` mode for tests, which runs extempore as a single process without spawning a utility process or server. This is more efficient for parallel test execution.

The `system.xtm` test requires IPC functionality (multi-process communication), so it uses a separate `extempore_add_ipc_test` macro that keeps the original `--eval` behaviour.
<!-- SECTION:DESCRIPTION:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
## Changes made

- Modified `extempore_add_test` macro to use `--batch` instead of `--eval`
- Modified `extempore_add_example_as_test` macro to use `--batch` instead of `--eval`
- Added new `extempore_add_ipc_test` macro for tests that require IPC (uses `--eval`)
- Changed `system.xtm` to use `extempore_add_ipc_test` since it tests IPC functionality

## Why --batch is better for parallelisation

In batch mode, extempore:
- Runs as a single process (no utility process spawned)
- Doesn't start a server thread
- Executes the expression and exits immediately

This reduces resource usage and potential port conflicts during parallel test runs.

## Limitation

Tests that use `ipc:new`, `ipc:call`, etc. cannot run in batch mode because these require the server functionality. Currently only `tests/core/system.xtm` uses IPC.
<!-- SECTION:NOTES:END -->
