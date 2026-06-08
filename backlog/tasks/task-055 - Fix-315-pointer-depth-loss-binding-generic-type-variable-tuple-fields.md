---
id: TASK-055
title: 'Fix #315: pointer-depth loss binding generic type-variable tuple fields'
status: To Do
assignee: []
created_date: '2026-06-08 06:10'
updated_date: '2026-06-08 09:17'
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
TYPE-PATH DEEP MAP (this session). The generic-TYPE path is a multi-site chain in the reification machinery, confirmed via live probes on bind-type Box315 <!a*,i64>:

SITE A -- parameter double-counts depth. The generic-type instance carries its parameter as the FIELD's depth-bearing var, not the bare var: reify sees 'Box315{!ga_31*}*##6' (param !ga_31*, depth 1) which makes the template field '<!ga_31**,i64>' (depth 2) instead of '<!ga_31*,i64>' (depth 1). The param should be the bare !ga_31. This star is NOT from llvmir:591 (refined guard, commit 379ea606, leaves generic instances alone and didn't change it); it comes from the param/candidate derivation in register-new-generictype / maximize-generic-type / nativef-generics-make-gtypes-unique reading the now-depth-bearing field.

SITE B -- reify-generic-type-expand (transforms ~1140) drops stars on substitution: regex replace-all of (base '[*]*') with tl discards the matched stars. Fix: capture them, '(' base ')([*]*)' with replacement (tl ''). Correct but moot until A is fixed and the var is bound.

SITE C -- binding flow / timing. At the first expand call the !ga_31 vars are all unbound; the base binding (!ga_31##6 <= 2) appears at a later reify iteration, and only the BASE var is ever bound -- the depth-bearing forms (!ga_31*, !ga_31**) stay unbound, so a depth-2 template field never resolves -> 'could not resolve types'.

RECOMMENDED FIX: route generic-type reification through the canonical core's apply-subst rather than regex substitution -- parse the template to a term, build the substitution from the base-var bindings, apply-subst (pointer depth derived structurally, so SITE A/B/C dissolve by construction), render back to a type string. Prereq: a pretty-string->term parser (deferred from the bridge; int-code->term exists) and fixing the param derivation to collect BARE vars. This is the proper holistic completion; the function path (done) is the template for the approach.
<!-- SECTION:NOTES:END -->
