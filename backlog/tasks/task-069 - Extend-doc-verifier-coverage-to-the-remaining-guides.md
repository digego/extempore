---
id: TASK-069
title: Extend doc-verifier coverage to the remaining guides
status: Done
assignee: []
created_date: '2026-06-24 03:19'
updated_date: '2026-06-24 05:07'
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
- [x] #1 Claim-harvesting is block-aware (no false Compiled: claims from illustrative/prose blocks)
- [x] #2 audio-signal-processing.md and making-an-instrument.md verify cleanly (with documented skip annotations) and are added to CONFIG
- [x] #3 Decide how to cover the sharedsystem-dependent guides (note-level-music, sampler, pattern-language): either a headless setup shim or skip with a documented reason
<!-- AC:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
AC#1 (block-aware harvesting): claims_in() replaced by claims_in_blocks(bs) in verify-examples.py. A Compiled:/;; prints claim is harvested only when a runnable block emits it --- a comment inside a runnable xtlang block, or the plain output block immediately following one. Claims in prose (the inline 'Compiled: tuple_maker' line) or after skip/expect-error blocks are ignored. This kills the false osc_c/tuple_maker 'not found' failures.

AC#2 (two guides): audio-signal-processing.md and making-an-instrument.md annotated and added to CONFIG; full suite green. Skips: SAMPLE re-bind, illustrative fragments (library osc_c source using _sin/STWOPI, the dotimes loop fragment, the skeleton template that 'won't compile', the preview make-instrument/dsp blocks before organ is defined), and play-note blocks. The deliberate <a,a> tuple block is expect-error (also corrected its message from 'unbound symbol' to the real 'cannot find symbol'). audio-signal-processing verifies 3 claims (osc_c, saw_synth_note, saw_synth_fx); making-an-instrument runs 4 substantive blocks through to make-instrument with no compiler error.

AC#3 (sharedsystem guides --- DECISION: skip with documented reason): note-level-music, sampler, pattern-language and analogue-synth stay out of CONFIG, documented in a comment block in verify-examples.py. audiosetup.xtm does load headless, but (1) the Compiled: lines the guides quote come from a cold compile and never reprint from the AOT cache in a built tree, (2) sample loading fails without a device ('Not a valid SNDFILE* pointer'), and (3) the guides centre on play-note/pattern scheduling that needs the audio clock to advance. A headless setup shim (drive the clock, disable AOT) is left to a future task.

contributing.md updated to describe block-aware harvesting and that guides are now covered. No CMake change needed --- the 'docs' ctest target runs the verifier with no args, so CONFIG additions are picked up automatically.
<!-- SECTION:NOTES:END -->
