---
id: TASK-066
title: Extend the doc-example verifier and wire it into CI
status: Done
assignee: []
created_date: '2026-06-24 00:36'
updated_date: '2026-06-24 03:20'
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

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
Verifier (docs/verify-examples.py) extended and wired into CI.

AC#1 (intentional-fail blocks): added the '<!-- verify: expect-error -->' directive --- the block runs on top of its predecessors, the verifier asserts it really errors and matches the shown message, and it's kept out of the shared session so it can't poison later blocks. tutorial.md and types.md now verify cleanly. Also added a 'no unexpected compiler error' assertion to accumulate mode.

AC#2 (guide coverage + convention): added the '<!-- verify: skip -->' directive for blocks that can't run headless (audio/sharedsystem) or are illustrative fragments; documented both directives in overview/contributing.md. Covered the audio-file-io guide (in CONFIG, green). The heavier guides (audio-signal-processing, making-an-instrument, note-level-music, sampler, pattern-language) need extensive per-block annotation plus block-aware claim-harvesting --- deferred to TASK-069.

AC#3 (ctest/CI): registered as the 'docs' ctest label (driven with EXTEMPORE_BIN=$<TARGET_FILE:extempore>); ctest -L docs passes (140s). Added 'docs' to the CI label-regex in build-and-test.yml so doc drift fails the build.

Note: partial AC#2 (one guide covered, not all) --- see TASK-069 for the rest.
<!-- SECTION:NOTES:END -->
