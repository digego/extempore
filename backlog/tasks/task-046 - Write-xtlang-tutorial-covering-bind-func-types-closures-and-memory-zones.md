---
id: TASK-046
title:
  "Write xtlang tutorial covering bind-func, types, closures and memory zones"
status: Done
assignee: []
created_date: "2026-04-20 21:19"
updated_date: "2026-04-21 07:05"
labels:
  - docs
  - tutorial
  - xtlang
dependencies: []
priority: medium
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

Draft a real tutorial to replace docs/reference/new/ (which has typos like
'beging' and undefined ($ ...) syntax). Cover bind-func, the xtlang type system,
closures, and memory zones. Target: newcomer who has worked through quickstart
and wants to go deeper. Scoped out of TASK-038 so the umbrella task can close.

<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria

<!-- AC:BEGIN -->

- [x] #1 All code snippets are evaluated via ./extempore --batch (or equivalent)
      and produce the output shown in the tutorial; evaluation is reproducible
- [x] #2 Tutorial covers bind-func, xtlang type system, closures, and memory
      zones with runnable examples for each
- [x] #3 Old docs/reference/new/ content (with 'beging' typo and undefined ($
      ...) syntax) is replaced or removed
- [x] #4 Tutorial is pitched at a reader who has finished the quickstart and can
    be followed end-to-end without prior xtlang knowledge
<!-- AC:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->

Replaced docs/reference/new/ with a single, end-to-end
docs/reference/tutorial.md. Every snippet was run via ./build/extempore --batch
(snippets live in /tmp/extempore-tutorial/ during authoring); the output blocks
in the tutorial are the verbatim output. Covers bind-func, primitive and
aggregate types, type inference (including deliberate type errors), pointers,
closures, and the three allocators (salloc/zalloc/halloc) with a
salloc-clobbering demo to motivate zones. Linked from docs/reference/index.md
and the VitePress sidebar.

<!-- SECTION:NOTES:END -->
