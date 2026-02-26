---
id: TASK-033
title: Split llvmti.xtm into separate compiler modules
status: To Do
assignee: []
created_date: '2026-02-26 09:44'
updated_date: '2026-02-26 09:44'
labels:
  - compiler
  - architecture
dependencies:
  - TASK-032
priority: medium
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
runtime/llvmti.xtm is 12,517 lines containing caches, transforms, type inference, closure conversion, generics, AOT support, and bind-form macros. Split it along natural boundaries into separate files: caches.xtm (~2000 lines), transforms.xtm (~300 lines), type-inference.xtm (~4600 lines), closure-convert.xtm (~600 lines), generics.xtm (~1500 lines), aot.xtm (~500 lines), bind-forms.xtm (~2000 lines). Dependencies are mostly linear: caches → transforms → type-inference → closure-convert → IR generation → bind-forms.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] #1 llvmti.xtm split into at least 4 separate files along phase boundaries
- [ ] #2 Load order defined and documented in a top-level loader or scheme.xtm
- [ ] #3 No change in compiler behaviour (core tests pass)
- [ ] #4 AOT compilation works (build aot_external_audio target)
<!-- AC:END -->
