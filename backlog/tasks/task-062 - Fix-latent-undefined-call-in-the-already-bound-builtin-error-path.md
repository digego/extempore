---
id: TASK-062
title: Fix latent undefined-call in the already-bound builtin error path
status: Done
assignee: []
created_date: "2026-06-09 01:35"
updated_date: "2026-06-09 03:04"
labels:
  - compiler
  - bug
dependencies: []
priority: low
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

Found during the impc->xtc rename. runtime/xtc-caches.xtm (the
register-new-builtin already-bound branch, ~line 92) calls
xtc:cache:get-builtin-type, but no such function is defined — only
xtc:cache:get-builtin-type-str exists. If a builtin is ever re-registered, that
error branch throws unbound-variable instead of printing the intended "already
bound" diagnostic. Almost certainly a typo for get-builtin-type-str (or the call
should be dropped). Not hit by the test suite, so it survived. Trivial fix;
verify the intended diagnostic renders.

<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria

<!-- AC:BEGIN -->

- [x] #1 Error path no longer references an undefined symbol
- [x] #2 The already-bound diagnostic renders correctly when triggered
<!-- AC:END -->
