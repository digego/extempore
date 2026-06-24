---
id: TASK-066
title: Extend the doc-example verifier and wire it into CI
status: To Do
assignee: []
created_date: '2026-06-24 00:36'
labels: []
dependencies: []
priority: medium
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
docs/verify-examples.py runs xtlang from the docs through extempore --batch and checks claimed output. Two gaps remain: (1) accumulate-mode aborts at intentionally-failing blocks, so tutorial.md (which includes deliberate compile-error demos) can't be checked end to end --- needs a way to mark blocks as expected-to-error or to run blocks in guarded isolation; (2) the guides (audio-signal-processing, making-an-instrument, sampler, etc.) load libraries and assume audio/sharedsystem setup, so they aren't covered yet. Once it's robust, wire it into ctest/CI so doc code that drifts from the compiler fails the build (the docs already claim 'every snippet is verified').
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] #1 Accumulate mode handles intentionally-failing blocks (marker or guarded isolation) so tutorial.md verifies cleanly
- [ ] #2 Coverage extended to the guide pages that contain runnable xtlang, with a documented convention for blocks that need setup or can't be auto-run
- [ ] #3 Verifier runs as a ctest target (or CI job) against the built extempore, failing on any claim mismatch
<!-- AC:END -->
