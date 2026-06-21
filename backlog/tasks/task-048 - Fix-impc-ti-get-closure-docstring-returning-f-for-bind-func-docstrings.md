---
id: TASK-048
title: "Fix impc:ti:get-closure-docstring returning #f for bind-func docstrings"
status: To Do
assignee: []
created_date: "2026-06-04 11:58"
labels:
  - compiler
  - xtlang
  - bug
dependencies: []
references:
  - "https://github.com/digego/extempore/pull/422"
  - runtime/llvmti-caches.xtm
priority: low
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

impc:ti:get-closure-docstring returns #f even for a freshly-compiled bind-func
that has a docstring:

    (bind-func docfoo:[i64]* "my-doc" (lambda () 1))
    (impc:ti:get-closure-docstring "docfoo")   ;; => #f, expected "my-doc"

This is independent of AOT: the global-var (impc:ti:get-globalvar-docstring) and
named-type (impc:ti:get-namedtype-docstring) accessors both return the right
value AND survive AOT compilation, so closure docstrings appear to have
regressed somewhere in the s7 / compiler refactor. The accessor lives in
runtime/llvmti-caches.xtm.

Found while building the AOT round-trip test (tests/core/aot-compilation.xtm),
which deliberately omits a closure-docstring assertion because of this. Closure
docstrings also feed xtmdoc, so doc generation is affected.

<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria

<!-- AC:BEGIN -->

- [ ] #1 impc:ti:get-closure-docstring returns the docstring for a bind-func
      defined with one
- [ ] #2 Returns the empty string (matching the other accessors) for a bind-func
      with no docstring
- [ ] #3 bind-func docstrings survive AOT compilation
- [ ] #4 tests/core/aot-compilation.xtm asserts closure-docstring preservation
    (assertion currently omitted)
<!-- AC:END -->
