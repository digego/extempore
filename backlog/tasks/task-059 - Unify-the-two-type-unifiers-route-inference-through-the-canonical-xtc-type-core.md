---
id: TASK-059
title: >-
  Unify the two type unifiers: route inference through the canonical xtc:type:
  core
status: In Progress
assignee: []
created_date: '2026-06-09 01:35'
updated_date: '2026-06-09 07:36'
labels:
  - compiler
  - types
  - refactor
dependencies: []
priority: medium
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
After the recent type-system modernisation the compiler has TWO coexisting unifiers.

OLD (string/int-code): xtc:desugar:type-unify plus complex-unify / unify-lists / sym-unify / occurs-in-type? / unity? in runtime/xtc-transforms.xtm. ~86 call sites of type-unify alone, woven through the type-checker. Does xtlang-specific union-type simplification/pruning, not just Robinson unification.

NEW (canonical): xtc:type:unify in runtime/xtc-types.xtm — a clean Robinson unifier over canonical type-terms, with from-intcode/to-intcode and from-pretty/to-pretty bridges in xtc-types-bridge.xtm. Built and unit-tested (tests/compiler/typecore,typeunify,typebridge) but wired into the live inference pipeline at essentially zero sites.

Goal: route all unification through the canonical core and delete the old family. This is a type-system migration, NOT a mechanical rename: either convert old-representation types to canonical terms at each call site via the bridges, or extend the canonical core to subsume the union-simplification the old path relies on. Characterise the old behaviour with tests FIRST, then migrate.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [x] #1 Old behaviour (incl. union-type simplification/pruning) characterised by tests before any change
- [ ] #2 All xtc:desugar:type-unify/complex-unify/unify-lists/sym-unify/occurs-in-type?/unity? sites migrated or removed
- [ ] #3 Live inference unifies via xtc:type:unify + the int-code/pretty bridges
- [ ] #4 No regression in generic/poly inference (esp. the #315 pointer-depth class); full suite green
<!-- AC:END -->

## Implementation Plan

<!-- SECTION:PLAN:BEGIN -->
Strangler migration in green increments (Ben chose 'full rewrite': delete the candidate-list union-find, number-crunch, and enumeration retry loop wholesale; route inference through the canonical core).

DONE:
- Increment 0 (AC#1, commit 7c62fe30): froze the old candidate-list unifier's pruning contract in tests/compiler/typeunify.xtm.
- Increment 1 (commit e8b8a404): runtime/xtc-solve.xtm --- the constraint-solving engine over the canonical core (xtc:type:unify). Splits the old conflated candidate list into equality (Robinson unify), numeric default (one post-unification rule), overload (deferred choice). Proven in tests/compiler/solve.xtm incl. #315 pointer depth and 'downstream constraint beats default' (subsumes the retry loop). Loaded live via xtc-globals.xtm but inert.

NEXT (the bulk, multi-session):
- Increment 2: wire the ~60 form-checkers in xtc:typecheck:type-check (runtime/xtc-typecheck.xtm:3103) to emit clean eq/default constraints instead of mutating the candidate-list vars store; shadow-validate the new solve against the old one across the suite. Output contract to preserve: run-type-check (xtc-typecheck.xtm:3282) returns a (symbol . int-code) alist; codegen reads only that; vars-store mutation is internal. Non-Robinson semantics to replace principled: numeric defaulting (numeric-check ~351 + lambda-check ~2809 apply-min), overload (nativef-poly-check ~1844), generic instantiation (nativef-generics ~1327 + reify-generic-type --- KEEP the _poly_ reification, codegen depends on it). Dead scaffolding to delete: emit-constraint!/solve-constraints (recorded, never solved).
- Increment 3: flip run-type-check* (xtc-typecheck.xtm:3300) to the new solve.
- Increment 4 (AC#2): delete the six functions (type-unify/complex-unify/unify-lists/sym-unify/occurs-in-type?/unity?) + the retry loop.
<!-- SECTION:PLAN:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
Increment 2 STARTED (commit 0162d3d8, local/unpushed): runtime/xtc-infer.xtm --- the constraint-emitting traversal, the strangler replacement for the candidate-list type-check. Covers a core subset (numeric/bool literals, symbols, let, begin, if, binary arithmetic, comparison ops), errors loudly on unhandled forms. Shadow-validated against the live xtc:bind:get-expression-type over a 22-expression corpus in tests/compiler/infer.xtm (green). Not wired into the live compiler yet.

Remaining increment-2 forms (the substantial part, for dedicated sessions): lambda/closure (closure-tag encoding + arg vars), function application (native-func + closure-call signature lookup -> instantiate-and-unify), the math/compare OVERLOAD specialisation (xtm_addition## etc. for non-numeric operands), and generics (nativef-generics + reify-generic-type --- keep the _poly_ reification). Then increment 3 (flip run-type-check* to collect+solve) and increment 4 (delete the six old functions + retry loop).

Increment 2 CONTINUED (commit 761646d3, local/unpushed): xtc-infer.xtm now also covers lambda and the application of a LOCAL closure (let-/lambda-bound var in call position). A closure key is the closure int-code (213 ret arg...) with nested keys, so lambda and call relate by Robinson unification of two closure terms --- no separate function-type vocabulary. Parameter slots share interned variables with their body uses (pinning a use pins the arg slot); a call's arg types flow in and the return flows out in one equality (instantiate-and-unify). The traversal now threads a lexical 'bound' set to read a symbol-in-call-position as local-closure (handled) vs global (loud error). Shadow-validated over a 30-expr corpus (tests/compiler/infer.xtm, 16 tests green).

Discovered while doing this: a user bind-func is registered as a POLYFUNC (polyfunc-exists? true, get-polyfunc-candidate-types returns its one closure candidate), NOT a nativefunc/closure --- so a call to a user function routes through symbol-check's poly resolution / nativef-poly-check, not the plain nativef-check signature lookup. nativef-check is for bind-lib C natives. This means 'function application' splits: local closures (DONE, clean Robinson) vs global user functions (the poly path, deferred with the overload work).

Remaining increment-2 forms: global-function application via the poly path; the math/compare OVERLOAD specialisation (xtm_addition## etc. for non-numeric operands); generics (nativef-generics + reify-generic-type, keep the _poly_ reification). Known boundary deferred to a later generalisation pass: a bare lambda whose body leaves a parameter unconstrained --- new path declines it (#f, genuinely generic) where the old path emits the raw parameter symbol (213 0 0 b); such lambdas are kept out of the shadow corpus. Then increment 3 (flip run-type-check*) and 4 (delete the six old functions + retry loop).

Increment 2 CONTINUED further (commits 6d831d29, ee42fd1d, bc9dd6bc; local/unpushed). Global-function application now fully handled, built on a new solver primitive.

OVERLOAD CONSTRAINT (ee42fd1d): added the deferred-choice third constraint kind to xtc-solve (eq/default were the only two). A call with several candidate signatures must match exactly one; xtc:solve:run is now a fixpoint --- commit unambiguous overloads first (narrowing the subst), then apply one defaulting pass as a tie-breaker, repeat. This subsumes the old retry loop with no search: an unambiguous signature commits first and forces its arg (downstream beats default), a default breaks a tie no signature can. Candidate vars intern through a fresh table per trial, so generic candidates instantiate independently (incl. the #315 [!t,!t*] deref). Zero-viable = honest conflict. 9 isolation tests in solve.xtm.

CLASS-AWARE VIABILITY (bc9dd6bc): a still-free literal arg may only take a type from its own class (the bound the old candidate-list carried, read off the default constraint via xtc:solve:class-map/arg-admits?). This is what makes an int literal select an i32 candidate over an f64 one --- plain defaulting would pin it to i64 and match neither. 4 more solve.xtm tests (22 total).

MULTI-CANDIDATE WIRING (bc9dd6bc): a global call emits one overload over the callee's candidates (xtc:infer:global-candidates/collect-global-call), replacing the earlier single-signature eq path. Monomorphic = one-candidate case, still resolves at once but now class-checked. Shadow corpus is 42 exprs incl. overloaded calls composed with arithmetic/if/let/local-closure; 22 infer tests green.

KEY FINDING: a user bind-func is a polyfunc; single-candidate calls go through symbol-check (which COERCES an int literal to a float param --- (is_pos 5) => i1 in the oracle), multi-candidate through match-ftypes (class membership). The new path is uniformly class-checked, so it declines the int->float coercion: an intended strictness, documented and kept out of the corpus alongside the result-widening case.

REMAINING increment-2 forms: (1) math/compare OVERLOAD specialisation --- (+ a b) on non-numeric/tuple operands rewrites to xtm_addition##N and dispatches poly; does NOT fit the type-agnostic collection cleanly (needs operand types to decide), the trickier of the two. (2) generics (genericfunc, return type REIFIED from args, not selected --- keep _poly_ reification, codegen depends on it). Then increment 3 (flip run-type-check* to collect+solve; forced-types become eq constraints; remember closure:convert + the scope/alpha-rename concern for shadowed let vars) and increment 4 (delete the six old functions + retry loop).

STATUS / RESUME POINT (as of 2026-06-09, session end): all commits through ba42773b are PUSHED to origin/master --- the 'local/unpushed' tags in the stanzas above are historical, not current. HEAD = ba42773b.

The authoritative state is the 'Increment 2 CONTINUED further' stanza and its 'REMAINING increment-2 forms' list directly above (the two earlier 'Remaining' lists are superseded). To resume, start there: next is (1) the math/compare overload specialisation, then (2) generics, then increment 3 (flip run-type-check*) and 4 (delete the old six + retry loop).

xtc-infer.xtm and xtc-solve.xtm both carry up-to-date module headers describing coverage and the deferred/curated boundaries; tests/compiler/{infer,solve}.xtm are the shadow/isolation suites (22 + 22 tests, green). Run via: ctest --label-regex compiler-unit -j4 (from build/), or ./build/extempore --batch '(xtmtest-run-tests "tests/compiler/infer.xtm" #t #t)'. Note xtc-infer.xtm is still loaded only by its test, not live (increment 3 wires it in).

Increment 2 CONTINUED further still (commit d3f84510, local/unpushed): the math/compare OVERLOAD specialisation is DONE. A binary arithmetic or comparison operator is now emitted as an overload constraint (not a bare equality): its candidates are the builtin numeric widths --- (w w w) for arithmetic, (i1 w w) for comparison --- plus any registered xtm_addition / xtm_lessthan / ... overloads for non-numeric operands (xtc:infer:math-candidates / compare-candidates, via xtc:infer:poly-candidates reading the polyfunc cache). The solver picks by operand type with NO solver change --- the candidate shapes are all already exercised in solve.xtm: (+ 1 2) selects a numeric width, (+ c1 c2) on two tuples selects the tuple xtm_addition candidate, exactly as the old xtm_addition## rewrite did. A free literal transiently matches several widths; the literal's default then pins it and the ambiguity collapses (the defaulting interleave already in the solver). The numeric shadow corpus is unchanged; new tuple-operand cases (a minimal two-field tuple infer_pair with xtm_addition / xtm_equal instances, standing in for the unloaded Complexd) agree with the old checker. infer.xtm is 24 tests; all 10 compiler-unit tests green.

CURATED BOUNDARY: a fully-unconstrained (+ a b) --- a generic-lambda body with no operand evidence --- now resolves to a numeric width via the overload tie-break (finish-pending) rather than declining (#f). This is a test-harness artifact (infer-expr wraps a bare lambda); in real compilation operands are concrete or literal-defaulted at check time, and a genuinely generic math body goes through the genericfunc path, so the case does not arise. Kept out of the corpus, as before. If it ever bites, the alternative is a numeric-sentinel candidate guarded to be viable only on numeric evidence (a small solver addition) instead of enumerating widths.

STATUS / RESUME POINT (updated, supersedes the earlier 'REMAINING increment-2 forms' and STATUS/RESUME POINT stanzas above): commits through ba42773b are PUSHED; d3f84510 (this increment) is committed LOCALLY, unpushed. Resume at (2) generics: a genericfunc whose return type is REIFIED from the argument types (not selected from candidates) --- keep the _poly_ reification, codegen depends on it. Then increment 3 (flip run-type-check* to collect+solve; forced-types become eq constraints; remember closure:convert + the scope/alpha-rename concern for shadowed let vars) and increment 4 (delete the six old functions --- type-unify/complex-unify/unify-lists/sym-unify/occurs-in-type?/unity? --- plus the retry loop). xtc-infer.xtm still loaded only by its test, not live (increment 3 wires it in).
<!-- SECTION:NOTES:END -->
