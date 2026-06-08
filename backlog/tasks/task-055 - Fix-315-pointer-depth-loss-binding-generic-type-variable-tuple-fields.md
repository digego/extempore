---
id: TASK-055
title: 'Fix #315: pointer-depth loss binding generic type-variable tuple fields'
status: To Do
assignee: []
created_date: '2026-06-08 06:10'
updated_date: '2026-06-08 09:43'
labels:
  - xtlang
  - compiler
  - bug
dependencies: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
Phase 1 of the task-054 redesign (see backlog/docs/doc-001). Localised fix for #315 and its bug class (depth>=1 type-variable tuple fields absorb a spurious pointer level). Root cause: impc:ti:reverse-set-bangs-from-reified (runtime/llvmti-typecheck.xtm:1011-1043) binds a type variable to the whole concrete tuple field (e.g. i64*) instead of stripping the field's declared pointer depth first; the depth was lost at runtime/llvmir.xtm:591 where a bang-type field collapses to a bare symbol. Fix recovers the declared depth from the maximized generic type's field strings and strips it before binding. Closes the rehomed task-053 AC#4.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] #1 #315 repro pset! into generic tuple type-checks and runs
- [ ] #2 Bug class A (depth-2), B (typevar return type), C (multiple mixed-depth vars) compile correctly
- [ ] #3 Regression test added to the test harness covering the bug class
- [ ] #4 Full libs-core and libs-external suites stay green; stdlib (adt/std) still loads
- [ ] #5 No spurious pointer level: final specialised signature is [void,Test315{i64}*,i64,i64]*
<!-- AC:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
TYPE-PATH PROGRESS (commit 563c5edf). Fixed three reification sites so a depth-bearing tuple field carries its depth through type RESOLUTION: maximize-generic-type strips spurious depth from bang-var params (was double-counting -> <!a**,...>); reify-generic-type-expand captures+re-applies the template field stars on substitution; update-var only normalises a starred key when the value concretely reduces (a symbolic value under the base key bound !a to !a*, a union-find cycle). All suites green; Box315 advanced from the pointer-set error -> 'could not resolve' -> now a remaining conflict 'i64* with i64 in (tuple-set! obj 0 arg_0)'.

REMAINING -- SITE D: the auto-generated data constructor. The generic struct reifies correctly now (reify-generic-type and the llvmir make-new-type path both yield {i64*,i64}), but the instantiated data constructor's tuple-set! still sees a field/arg depth mismatch (conflict surfaces via print-type-conflict-error from the binary/unify path, typecheck ~623/707, with the tuple-set! ast). Likely the data-constructor's field/arg types come from yet another reification that loses the field depth, OR the namedtype struct store (get-namedtype-type, used by tuple-set-check) differs from the resolved struct.

RECOMMENDATION: this is now clearly a 4-5 site chain all doing the same depth bookkeeping by hand. The clean completion is to route ALL reification (reify-generic-type, the llvmir make-new-type path, and the data-constructor field/arg derivation) through ONE canonical apply-subst (runtime/llvmti-types.xtm) -- parse template to a term, substitute, render -- so every site shares one depth-correct implementation. Prereq: a pretty-string->term parser (compose existing get-type-from-pretty-str with impc:type:from-intcode). The per-site patching works but the chain is long; the rewrite dissolves it.
<!-- SECTION:NOTES:END -->
