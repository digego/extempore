---
id: TASK-055
title: 'Fix #315: pointer-depth loss binding generic type-variable tuple fields'
status: To Do
assignee: []
created_date: '2026-06-08 06:10'
updated_date: '2026-06-08 09:03'
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
TYPE-PATH FINDINGS (generic-TYPE path of #315, still open). The function path is fixed+committed (see prior note). For the type path (a depth-bearing type variable in a tuple field, e.g. bind-type Box <!a*,i64>):
- parse fix + update-var normalisation move it PAST the pset! type error (baseline) to 'could not resolve types' at reification.
- Site 1: impc:ti:reify-generic-type-expand (transforms ~1140-1143) drops pointer stars during template substitution: (regex:replace-all type (string-append base "[*]*") tl) replaces !a* with the base value tl and discards the matched stars. Fix: capture them -- (string-append "(" base ")([*]*)") with replacement (string-append tl "$2"). Correct but insufficient alone.
- Site 2 (the real blocker): at reification time the template's type variable is UNBOUND. Probe of reify-generic-type-expand output: '<!gxa_33*,i64>' -- !gxa_33 is never substituted because !a=i64 is not in vars (or under a mismatched key) when expand runs. This is the reverse-set-bangs-from-reified (typecheck ~1030) + nativef-generics matchup flow.
- Also observed: template field !a* (depth 1) appearing as !ga_31** (depth 2) for the !ga_ type-var family -- an extra pointer++ somewhere.
NEXT PHASE: route the generic-TYPE reification binding (reverse-set-bangs + matchup + reify-generic-type-expand) through the canonical core (impc:type: apply-subst / reduce-ptr-depth), so the type variable is bound depth-correctly and substituted with stars preserved -- the same approach that fixed the function path, applied to the reification machinery. This is the attempt-2 '-98' territory; the canonical reduce-ptr-depth already handles the underflow that produced -98.
<!-- SECTION:NOTES:END -->
