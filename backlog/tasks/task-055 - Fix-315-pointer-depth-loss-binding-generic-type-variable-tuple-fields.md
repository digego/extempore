---
id: TASK-055
title: "Fix #315: pointer-depth loss binding generic type-variable tuple fields"
status: To Do
assignee: []
created_date: "2026-06-08 06:10"
updated_date: "2026-06-08 09:58"
labels:
  - xtlang
  - compiler
  - bug
dependencies: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

Phase 1 of the task-054 redesign (see backlog/docs/doc-001). Localised fix for
#315 and its bug class (depth>=1 type-variable tuple fields absorb a spurious
pointer level). Root cause: impc:ti:reverse-set-bangs-from-reified
(runtime/llvmti-typecheck.xtm:1011-1043) binds a type variable to the whole
concrete tuple field (e.g. i64\*) instead of stripping the field's declared
pointer depth first; the depth was lost at runtime/llvmir.xtm:591 where a
bang-type field collapses to a bare symbol. Fix recovers the declared depth from
the maximized generic type's field strings and strips it before binding. Closes
the rehomed task-053 AC#4.

<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria

<!-- AC:BEGIN -->

- [ ] #1 #315 repro pset! into generic tuple type-checks and runs
- [ ] #2 Bug class A (depth-2), B (typevar return type), C (multiple mixed-depth
      vars) compile correctly
- [ ] #3 Regression test added to the test harness covering the bug class
- [ ] #4 Full libs-core and libs-external suites stay green; stdlib (adt/std)
      still loads
- [ ] #5 No spurious pointer level: final specialised signature is
    [void,Test315{i64}*,i64,i64]\*
<!-- AC:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->

FOUNDATION BUILT (commits f0ad6388, 543d0a53). Added the canonical pretty<->term
parser/renderer (impc:type:from-pretty / to-pretty, surface round-trip tested
incl. bang vars) -- the prereq for routing reification through apply-subst.
Fixed the data-constructor return-type parameter (gather-all-gvars was
collecting depth-bearing field vars -> Box{!a\*}; now bare Box{!a}).

KEY REALISATION: the generic-TYPE path of #315 spans THREE subsystems, each with
its OWN hand-rolled depth bookkeeping, not one:

1. Reification (reify-generic-type, maximize-generic-type,
   reify-generic-type-expand) -- FIXED (commit 563c5edf). Routing these through
   canonical apply-subst would consolidate them, but it only covers subsystem 1.
2. Data-constructor generation (compile-type-dataconstructors, llvmti-bind) --
   ctype param FIXED (543d0a53); field-type list 'a' was already correct.
3. Codegen (impc:ir:compiler:tuple-set, llvmir ~3201) -- SITE E, OPEN: in the
   generated constructor body (tuple-set! obj 0 arg_0), element-type (struct
   field 0) reifies to i64* but the value arg_0 codegens to i64. So the
   constructor's field-0 PARAMETER (!ga_29*) loses its depth at
   specialisation/codegen -- the param is resolved as the bare !ga_29 (i64)
   rather than !ga_29* (i64*). Likely the same param-vs-field-identity
   confusion, now in the codegen-time reification of the constructor's argument
   types.

ASSESSMENT: the function path of #315 is fully fixed and validated. The type
path is advancing site-by-site (now ~5 sites, into codegen) but spans
reification + data-constructor-gen + codegen, each independent. The clean
completion needs a representation that distinguishes a generic type's PARAMETER
(a bare variable identity) from its FIELD usage (the variable at depth) -- right
now they share one symbol and every subsystem re-derives the relationship by
stripping/adding stars. That split is the real fix; the canonical apply-subst
handles it for subsystem 1 but 2 and 3 need the same discipline. Recommend
treating the type-path as its own follow-up rather than continuing to grind
sites.

<!-- SECTION:NOTES:END -->
