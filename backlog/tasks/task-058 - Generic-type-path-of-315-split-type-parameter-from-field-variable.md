---
id: TASK-058
title: 'Generic-type path of #315: split type parameter from field variable'
status: To Do
assignee: []
created_date: '2026-06-08 10:00'
updated_date: '2026-06-08 10:00'
labels:
  - xtlang
  - compiler
  - types
  - '315'
dependencies: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
The generic-FUNCTION path of #315 is fixed (task-055, commit 9a16c07f). The generic-TYPE path -- a depth-bearing type variable in a tuple field, e.g. (bind-type Box <!a*,i64>) -- is not. Root cause is one representation flaw, not a chain of bugs: a generic type's PARAMETER (!a, a bare identity) and its FIELD usage (!a*, the variable at pointer depth) share ONE symbol. Three subsystems (reification, data-constructor generation, codegen) each re-derive the parameter<->field relationship by stripping or adding pointer stars, inconsistently. Preserving field depth in the parser (commit 379ea606) surfaced the inconsistency everywhere at once. The clean fix is to distinguish the parameter (a bare variable identity / type-function abstraction T{X} = <X*,i64>) from the field usage, so the relationship is structural (X vs X*) not symbolic (!a vs !a*), and route the depth handling through the canonical core (runtime/llvmti-types.xtm: from-pretty / apply-subst / to-pretty) so it happens once, correctly, instead of per-site.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] #1 A depth-bearing type-variable tuple field compiles and runs correctly: bind-type Box <!a*,i64>, set then read a field at i64 and at double, returning the stored values
- [ ] #2 tests/core/generics.xtm type-path locks (Csn315, CsnDeep, CsnTwo) flip from compile-should-fail to their expected values -- may require task-057 (void-return codegen bug) first, or rewriting those repros non-void as gsig315/Box315 are
- [ ] #3 The per-site depth patches are removed or subsumed by the canonical reification: the maximize-generic-type bang-param strip, the reify-generic-type-expand star-capture, and the compile-type-dataconstructors gather-all-gvars strip
- [ ] #4 compiler-unit, libs-core and libs-external stay green
<!-- AC:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
COLD-START MAP (from the session that fixed the function path and built the foundation).

REPRO (non-void, dodges the unrelated task-057 void bug):
  (bind-type Box315 <!a*,i64> (printer? . #f))
  (bind-func Box315:[Box315{!a}*,i64]* (lambda (x) (let ((arr:!a* (halloc x))) (Box315_z arr x))))
  (bind-func box315_set:[!a,Box315{!a}*,i64,!a]* (lambda (t idx val) (pset! (tref t 0) idx val) val))
  (bind-func box315_get:[!a,Box315{!a}*,i64]* (lambda (t idx) (pref (tref t 0) idx)))
  (bind-func box315_use (lambda () (let ((t:Box315{i64}* (Box315 16))) (box315_set t 6 4) (box315_get t 6))))
  ;; want (box315_use) => 4
Currently dies: 'Type Error conflicting i64* with i64 in (tuple-set! obj 0 arg_0)' during the first concrete use's compilation.

THE THREE SUBSYSTEMS (each re-derives param<->field by hand):
  1. REIFICATION -- reify-generic-type, maximize-generic-type, reify-generic-type-expand (llvmti-transforms). FIXED by per-site patches (commit 563c5edf): maximize strips bang-var param depth; reify-expand captures+re-applies field stars; update-var only normalises a starred key on concrete reduction (else it made an !a<-!a* cycle).
  2. DATA-CONSTRUCTOR GEN -- compile-type-dataconstructors (llvmti-bind ~1107). ctype return-type param FIXED (commit 543d0a53: gather-all-gvars was collecting !ga_29* for the param; now stripped to !ga_29). Field-type list 'a' was already correct (!ga_29*, i64).
  3. CODEGEN -- impc:ir:compiler:tuple-set (llvmir ~3201). OPEN, SITE E. At line ~3213 element-type = field 0 of the obj struct = i64* (correct); at ~3216 val = codegen of arg_0 = i64 (WRONG, lost depth); conflict at ~3228. So the constructor's field-0 PARAMETER (!ga_29*) is resolved as the bare !ga_29 (i64) at specialisation/codegen instead of !ga_29* (i64*).

WHY A REWRITE, NOT MORE PATCHES: routing reification through canonical apply-subst only covers subsystem 1. Subsystems 2 and 3 have independent depth logic apply-subst never touches. The durable fix is the representation: a generic type is a type FUNCTION; T{X} substitutes X (a bare variable) into the template, and the field <!a*,...> becomes <X*,...> structurally. Then no subsystem re-derives stars.

TOOLS ALREADY BUILT (runtime/llvmti-types.xtm + -bridge.xtm; loaded live via end of llvmti-globals.xtm):
  impc:type:from-pretty s intern  -> term (bang vars interned, depth as (ptr ...))
  impc:type:to-pretty term intern -> surface string (vars render as bang names)
  impc:type:unify / apply-subst / instantiate / reduce-ptr-depth
  Round-trip + reify-keeps-depth tested in tests/compiler/typebridge.xtm.
A canonical reify helper to add: reify(template-string, {param -> concrete-term}) = to-pretty(apply-subst(from-pretty(template), subst)).

RELATED: doc-001 (task-054 audit) has the broader redesign; task-057 is the orthogonal void-return codegen bug that also blocks the void-style characterisation repros. Function-path fix and foundation are committed on master (33f9e680, 961f8aae, 9a16c07f, f0ad6388, 563c5edf, 543d0a53).
<!-- SECTION:NOTES:END -->
