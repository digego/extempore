---
id: TASK-068
title: 'Fill maintainer-decision API-doc TODOs (DSP knowledge, zone special forms)'
status: To Do
assignee: []
created_date: '2026-06-24 01:28'
labels:
  - docs
dependencies: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
Follow-up from TASK-065. After the mechanical TODOs in api/core.md and api/audio_dsp.md were filled, ~126 remain that genuinely need Ben's input: they are the original author's DSP-knowledge questions and implementation notes, plus the memory-zone special forms. These can't be filled mechanically without either DSP-design decisions or knowledge of the intended public API surface.

IMPORTANT root-cause finding (re TASK-065 AC#2): these api/*.md pages are NOT produced by the xtmdoc target. They were hand-migrated from the old VitePress docs. The xtmdoc target only emits JSON (/tmp/xtmdoc.json); there is no JSON->markdown converter checked in. So 'regenerate' is not currently a step, and the TODOs are original-author notes, not docstrings dropped by the generator (TASK-048 is unrelated -- these pages don't draw from get-closure-docstring). A separate decision is whether to build a JSON->md generator and drive these pages from the .xtm docstrings, or keep maintaining the markdown by hand.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] #1 Decide whether api/*.md should be hand-maintained or generated from xtmdoc JSON (build a converter if generated)
- [ ] #2 core.md: document the zone special forms (Zone, push_zone, pop_zone, reset_zone, destroy_zone, peek_zone) -- they are codegen special forms (xtc-codegen.xtm), not closures, so their type signatures and public-API status need a maintainer call
- [ ] #3 audio_dsp.md: answer the author's DSP questions across oscillators, filters, delays/combs, effects, blep internals, and windows/envelopes (the ~100 remaining TODOs)
- [ ] #4 Fix the duplicate doc sections: lpf_c, hpf_c, and delay_apf_c each appear twice but have a single source definition
- [ ] #5 dsp_randn, integrator_c and fade_c carry author notes about possible bugs; confirm whether the implementations need fixing
<!-- AC:END -->
