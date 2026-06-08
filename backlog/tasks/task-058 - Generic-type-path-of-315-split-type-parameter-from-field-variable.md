---
id: TASK-058
title: 'Generic-type path of #315: split type parameter from field variable'
status: Done
assignee:
  - '@ben'
created_date: '2026-06-08 10:00'
updated_date: '2026-06-08 10:53'
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
- [x] #1 A depth-bearing type-variable tuple field compiles and runs correctly: bind-type Box <!a*,i64>, set then read a field at i64 and at double, returning the stored values
- [x] #2 tests/core/generics.xtm type-path locks (Csn315, CsnDeep, CsnTwo) flip from compile-should-fail to their expected values -- may require task-057 (void-return codegen bug) first, or rewriting those repros non-void as gsig315/Box315 are
- [x] #3 The per-site depth patches are removed or subsumed by the canonical reification: the maximize-generic-type bang-param strip, the reify-generic-type-expand star-capture, and the compile-type-dataconstructors gather-all-gvars strip
- [x] #4 compiler-unit, libs-core and libs-external stay green
<!-- AC:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
ROOT CAUSE (pinned by live probing, not the cold-start guess at SITE E): the disease is upstream of codegen, in the symbol-keyed vars store. A type variable's starred occurrence (!a*, a field/parameter at pointer depth) and its bare identity (!a, the type parameter) were two independent keys. impc:ti:update-var redirected a *reducible* starred binding to the base key (!a* <- i64* => !a <- i64), but when the value was shallower than the star depth (reduce-ptr-depth returns 'fail, e.g. !a* handed i64) it fell through and bound the starred key DIRECTLY to that depth-0 value -- conflating !a* with !a. The generic-function specialiser then read !a* => i64 (star dropped), so the data constructor specialised to [Box{i64}*,i64,i64]* instead of [Box{i64}*,i64*,i64]*, and SITE E (tuple-set) saw arg_0:i64 vs field0:i64* and raised the conflict.

FIX (structural; a type variable is one identity at a pointer depth, X vs X*, never two symbols reconciled by star surgery):
  1. update-var (binding chokepoint): on an irreducible starred binding ('fail), DROP it -- the base governs and resolution re-applies the depth -- instead of binding the starred key.
  2. type-unify (resolution chokepoint): a starred occurrence with no binding of its own is rebuilt from its base via the new canonical impc:type:deepen-ptr-depth (so !a* reifies to i64* when !a=i64).
  3. impc:type:deepen-ptr-depth: canonical inverse of reduce-ptr-depth (bridge), wrap-ptr through the core.
  4. data-constructor type-param list via new canonical impc:type:pretty-type-params (free-vars by identity) -> Box{!a}, replacing the gather-all-gvars+strip patch; gather-all-gvars removed (dead).

The three per-site depth patches (maximize bang-param strip, reify-expand star-capture, dataconstructor gather strip) are all REMOVED -- subsumed by the two chokepoints + canonical helpers.

AC#2: the void-style locks also trip the orthogonal void-return codegen bug (task-057), so they are rewritten value-returning (as gsig315/Box315 are) and flipped to expected values 4/5/42/7; boundary locks (CsnD0, Csn315F) still pass.

VERIFIED: Box315 set/read at i64 (=>4) and double (=>2.5); generics locks Csn315/CsnDeep(depth-2)/CsnTwo(two-var); constructors specialise [Csn315{i64}*,i64*,i64]* and [CsnDeep{i64}*,i64**,i64]*; new typebridge tests for deepen-ptr-depth/pretty-type-params/reduce-underflow; full ctest (54/54) green incl adt/typeclasses; clean_aot + aot_external_audio rebuild green.
<!-- SECTION:NOTES:END -->
