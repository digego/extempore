---
id: TASK-047
title: Draft JIT/type error-message glossary
status: Done
assignee: []
created_date: "2026-04-20 21:19"
updated_date: "2026-04-21 07:11"
labels:
  - docs
  - errors
  - xtlang
dependencies: []
priority: medium
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

Collect common xtlang/JIT compiler error messages (type errors, missing binds,
arity mismatches, zone-related failures) and explain what each one means in
practical terms. Scoped out of TASK-038 as a dedicated docs page under
docs/reference/.

<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria

<!-- AC:BEGIN -->

- [x] #1 Every error message in the glossary is reproduced by actually
      evaluating a minimal snippet in extempore; quoted text matches the real
      compiler output verbatim
- [x] #2 Each entry includes: the triggering snippet, the exact error message, a
      plain-English explanation, and how to fix it
- [x] #3 Glossary covers at minimum: type errors, missing/undefined binds, arity
      mismatches, and zone-related failures
- [x] #4 Page lives under docs/reference/ and is linked from the docs index
<!-- AC:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->

Created docs/reference/error-messages.md covering type errors (unresolved
inference, bad numeric value, pointer type mismatch, non-pointer de-reference),
undefined symbols (missing function, forward reference, set! on unbound var),
arity mismatches (both too-few and too-many -> 'no valid polymorphic options'),
tuple index bounds, and parse/read errors. Each entry has a minimal snippet, the
verbatim error output (captured by running the snippet through ./build/extempore
--batch), a plain-English explanation, and a concrete fix. A 'memory surprises'
section flags the salloc/zalloc pointer-lifetime hazards that don't produce
errors but silently corrupt data, linking to the tutorial's clobber demo. Linked
from docs/reference/index.md and the VitePress sidebar.

<!-- SECTION:NOTES:END -->
