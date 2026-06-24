---
id: TASK-069
title: Extend doc-verifier coverage to the remaining guides
status: To Do
assignee: []
created_date: '2026-06-24 03:19'
labels:
  - docs
dependencies: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
Follow-up from TASK-066. The doc-example verifier (docs/verify-examples.py) now supports expect-error/skip directives and covers the reference pages plus the audio-file-io guide. The heavier guides still need work to verify cleanly:

- audio-signal-processing.md, making-an-instrument.md: need per-block 'verify: skip' on illustrative fragments, on blocks that re-bind SAMPLE, and on play-note/sharedsystem blocks that can't run headless; a few blocks reference internals (e.g. _sin) or generic fragments (<a,a>) that don't compile standalone.
- note-level-music.md, sampler.md, pattern-language.md: depend on the sharedsystem / pattern language and audio setup.

Also needed: make Compiled: claim-harvesting block-aware (only harvest a 'Compiled: NAME' claim from an output block that follows a runnable xtlang block) so illustrative example-output blocks in the guides aren't picked up as false claims (osc_c, tuple_maker were). Once a guide verifies cleanly, add it to CONFIG in verify-examples.py.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] #1 Claim-harvesting is block-aware (no false Compiled: claims from illustrative/prose blocks)
- [ ] #2 audio-signal-processing.md and making-an-instrument.md verify cleanly (with documented skip annotations) and are added to CONFIG
- [ ] #3 Decide how to cover the sharedsystem-dependent guides (note-level-music, sampler, pattern-language): either a headless setup shim or skip with a documented reason
<!-- AC:END -->
