---
id: TASK-055
title: 'Fix #315: pointer-depth loss binding generic type-variable tuple fields'
status: To Do
assignee: []
created_date: '2026-06-08 06:10'
updated_date: '2026-06-08 07:11'
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
ATTEMPTED the localised binding-site fix (strip the field's declared pointer depth in impc:ti:reverse-set-bangs-from-reified before binding the typevar). Implemented, tested against build/extempore, then REVERTED (git restore; tree clean, original #315 error reproduces).

RESULT: the binding patch does NOT fix #315 -- it RELOCATES the error. With !a bound to i64 (stripped), the canonical repro's pset! error is replaced by 'Type Error conflicting i64* with i64 in (tuple-set! obj 0 arg_0)' inside the auto-generated tuple constructor. Cause: vars[!a] is overloaded across pointer depths -- the tuple field !a* needs i64*, the bare param !a needs i64; one binding can't serve both because the typevar symbol carries no depth (dropped at llvmir.xtm:591).

Minimal repro proving it's NOT tuple-specific (no tuple at all):
  (bind-func depthtest:[void,!a*,!a]* (lambda (p v) (pset! p 0 v)))
  ($ (let ((p:i64* (halloc))) (depthtest p 5) (println (pref p 0))))
  ;; -> Type Error: bad type i64, Cannot de-reference non-pointer type
This goes through nativef-generics (function path), not reverse-set-bangs (tuple path), so the defect is in the SHARED typevar representation, spanning both paths.

RE-SCOPE: #315 cannot be fixed by a binding-site patch. The fix is the type-variable-carries-pointer-depth slice of the audit's Phase 2 (doc-001 section 4.4 / Phase 1): make a typevar retain declared pointer depth at parse time (llvmir.xtm:591) and have type-unify (~1872) strip+reapply it. Higher risk (touches the ##gnum/freshening string machinery); must land behind the Phase-0 characterisation net. libs-core stayed green (8/8) under the reverted patch.

PAUSED pending direction on whether to take on the representation change now.

PHASE-1 ATTEMPT 2 (representation fix) -- design validated, reverted (tree clean, libs-core 9/9 green). WIP saved at backlog/docs/task-055-phase1-wip.patch.

INVARIANT chosen: a type variable's value is stored depth-0 under its BASE name (!a##gnum); every USE re-applies its own declared pointer depth. So !a* and !a are distinguishable and a single binding serves both.

THREE coordinated changes made (all in the patch), CONFIRMED individually:
1. Parse (llvmir.xtm ~591): bang branch keeps the stars -- (string-append base (make-string ptr-depth #\*)). Verified: get-type-from-pretty-str '<!a*,i64>' -> (14 !a* 2); depth-0 !a unchanged.
2. Resolve (type-unify, transforms.xtm ~1823): new branch -- if t is a bang at ptr-depth>0, look up the base key (strip stars, keep ##gnum) and pointer++ the result by the depth.
3. Bind (reverse-set-bangs-from-reified, typecheck.xtm ~1033): bind under get-base-type(b)##gnum with pointer--(a, depth).

RESULT: runtime loads fine (parens OK); both failures MOVED but not fixed.
- #315 (tuple) now errors in the auto-generated constructor: 'conflicting i64* with i64 in (tuple-set! obj 0 arg_0)' -- the constructor's !a* arg type resolves to i64 (base) without depth re-applied.
- no-tuple [void,!a*,!a]* now mis-codegens: 'LLVM IR: void type only allowed for function results / bitcast i64 to void'.

REMAINING PATHS needing the SAME base-key/depth normalization (the fix is multi-path, as the audit predicted):
- nativef-generics-check-args (typecheck.xtm ~915-930): function-arg binding (binds gt:=tt under the starred key; needs base-key + pointer-- like change 3).
- the constructor / tuple-set! field-type resolution that raises print-type-conflict-error (globals.xtm:407) -- find where the auto-generated constructor's !a* param/field types are resolved and ensure depth is re-applied.
- likely several get-base-type-then-resolve sites (any code that strips stars before resolving loses the depth).

NEXT: apply the patch, then extend the depth-normalization to check-args + the constructor path, validating against the characterisation net (csn315/csndeep/csntwo/csn_notuple flip from compile-should-fail to their expected values; csnd0 + the libs stay green) after each change.
<!-- SECTION:NOTES:END -->
