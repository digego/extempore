---
id: TASK-065
title: Verify and clean up xtlang docstrings (fill generated API doc TODOs)
status: To Do
assignee: []
created_date: '2026-06-24 00:12'
labels: []
dependencies: []
priority: medium
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
The generated API reference (docs/src/content/docs/api/core.md and api/audio_dsp.md, produced by the xtmdoc target from @param/@return/@example docstrings in libs/core/*.xtm) ships with 43 and 110 TODO placeholders respectively, plus at least one leaked developer note (dsp_randn: 'there may be a bug... feels suboptimal') and a copy-paste name error (fasttanh's example shows 'fasttan'). Go through every docstring TODO and triage it: some may already be documented at source but missed by the generator (cf TASK-048, get-closure-docstring returning #f for bind-func docstrings), some can be filled mechanically without any maintainer decision (obvious examples, verifiable with --batch), and some genuinely need Ben's input. Fill the no-decision-needed ones at the .xtm source, fix the obvious source bugs, regenerate, and produce a shortlist of the rest. Surfaced during the VitePress->Starlight docs migration.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] #1 Every TODO in api/core.md (43) and api/audio_dsp.md (110) is triaged into one of: already-documented-at-source-but-not-regenerated; fillable-without-maintainer-input; or needs-maintainer-decision
- [ ] #2 If the generator is dropping existing docstrings (see TASK-048), that root cause is investigated and the 'already done' TODOs resolve by regenerating rather than rewriting
- [ ] #3 The fillable docstrings are written at the .xtm source, and any code in @example blocks is verified to compile/run via extempore --batch
- [ ] #4 The leaked dsp_randn developer note and the fasttanh/fasttan name error are fixed at source
- [ ] #5 The API reference is regenerated via the xtmdoc target and the docs site builds clean; no stray TODOs remain except those explicitly listed as needing maintainer input
- [ ] #6 A shortlist of docstrings genuinely needing Ben's decision/input is left for follow-up
<!-- AC:END -->
