---
id: TASK-054
title: Audit and redesign the xtlang generic type compiler
status: To Do
assignee: []
created_date: '2026-06-08 04:51'
labels:
  - xtlang
  - compiler
  - tech-debt
dependencies: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
The xtlang generic type machinery has accumulated structural debt that produces latent bugs (e.g. issue #315). This task is a READ-ONLY audit producing a findings document plus a phased, de-risked redesign proposal --- not a refactor in itself. The actual refactor is scoped later from the audit's evidence.

Why now: #315 (digego/extempore) was diagnosed during task-053 but has no localized fix. Root cause: two occurrences of the same type variable `!a` --- the `!a` inside a generic-type instantiation `Test315{!a}` and the bare `!a` function parameter --- get separate freshened identities and are never unified, so the value parameter acquires a spurious pointer level. This is symptomatic of deeper issues, not a one-off.

Structural debt observed so far (starting points; verify and expand during the audit):
- Heterogeneous type representation: raw int codes (2=i64, 102=i64*), pretty strings ("%mzone*"), and tagged lists ((114 ...) tuple, (213 ...) closure), with constant conversion (get-type-from-str, get-type-from-pretty-str, pretty-print-type, str-list-check). This impedance mismatch is where bugs hide.
- Ad-hoc type-variable freshening: `!`-vars get ##gnum suffixes but inconsistently; there is no single substitution map enforcing variable identity. #315 shows the same var with different identities (!gxa_34 vs !ga_31, and !gxa_34 with two gnums ##4/##7).
- Scattered string/regex-driven unification across overlapping functions: impc:ti:type-unify, type-unify-closure, polytype-match?, generic-types-matchup?, reify-generic-type, reify-generic-type-expand, nativef-generics-check-args, nativef-generics-make-gtypes-unique.
- Cruft: impc:ti:memzone defined twice (first shadowed; llvmti-transforms.xtm ~557 and ~587), impc:ir:compiler:closure-ref defined twice (llvmir.xtm ~1823 and ~1925), large commented-out alternative blocks in llvmti-typecheck.xtm.

Key files: runtime/llvmti-typecheck.xtm, runtime/llvmti-transforms.xtm, runtime/llvmti-caches.xtm, runtime/llvmti-bind.xtm; codegen in runtime/llvmir.xtm.

Workflow note: runtime/*.xtm load live from disk --- compiler edits need no C++ rebuild. Validate with `ctest --label-regex "libs-core|libs-external"` plus a broad sys:load of generic-heavy libs (adt, std). See task-053 notes for the full #315 diagnosis.

#315 repro (still fails on current build):
  (bind-type Test315 <!a*,i64> (printer? . #f))
  (bind-func Test315:[Test315{!a}*,i64]* (lambda (x) (let ((arr:!a* (halloc x))) (Test315_z arr x))))
  (bind-func put315:[void,Test315{!a}*,i64,!a]* (lambda (arr idx val) (pset! (tref arr 0) idx val)))
  ($ (let ((arr:Test315{i64}* (Test315 16))) (put315 arr 6 4) (println "finished")))
  ;; -> Type Error with pointer-set!, got i64*, was expecting i64
The concrete-typed analog (bind-type CTest <i64*,i64>, same put) compiles fine, so the flaw is generic-only.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] #1 Findings doc maps the type representation(s) and every conversion boundary between int-code / pretty-string / tagged-list forms
- [ ] #2 Freshening + unification call graph documented across type-unify, type-unify-closure, polytype-match?, generic-types-matchup?, reify-generic-type(-expand), nativef-generics-check-args, make-gtypes-unique
- [ ] #3 Inventory of special-case hacks, duplicate definitions, and dead/commented code in the generic path
- [ ] #4 #315 root cause documented precisely, plus the class of related latent bugs the same flaw implies, with repro sketches
- [ ] #5 Phased redesign proposed (canonical type representation + a real unifier with a substitution map and consistent fresh-variable identity), with per-phase risk/effort and a characterisation-test strategy
- [ ] #6 No production code changed (read-only audit; redesign is a written proposal)
<!-- AC:END -->
