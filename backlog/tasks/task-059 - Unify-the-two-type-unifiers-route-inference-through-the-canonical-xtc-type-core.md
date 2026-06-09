---
id: TASK-059
title: >-
  Unify the two type unifiers: route inference through the canonical xtc:type:
  core
status: In Progress
assignee: []
created_date: '2026-06-09 01:35'
updated_date: '2026-06-09 13:13'
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

Increment 2 CONTINUED, generics part 1 (commit b82b8607, local/unpushed): SIMPLE generics are DONE. A genericfunc call now resolves through the same overload constraint as a poly call --- xtc:infer:generic-candidates reads the arity-matching specialisations from *xtc:cache:genericfunc-cache*, converts each pretty signature (e.g. [!a,!a]*) to an int-code candidate via get-type-from-pretty-str, and feeds them to collect-global-call. The solver freshens each candidate's bang variables per trial and unifies them with the args, so the return type is REIFIED from the arguments (identity (infer_gid 5) -> i64, first-of-two, nesting, composition with math --- all agree with the old checker). NO solver change; the generic-candidate instantiation was already proven in solve.xtm (the (!t !t) / (!t !t*) tests). infer.xtm is 27 tests; all 10 compiler-unit green.

GENERICS BOUNDARY (the substantial remaining piece): generic-candidates deliberately admits only specialisations that are UNCONSTRAINED and FREE OF NAMED GENERIC TYPES (it skips any pretty signature containing '{'). Deferred, as a loud gap kept out of the corpus:
  (1) NAMED generic types --- List{!a}, Point{!a}, Vector{!a} etc. This is where the bulk of real generics live (list/vector/option ops). The blocker: get-type-from-pretty-str AND xtc:desugar:get-generic-type-as-tuple both throw "Bad type in context" on a generic instance like List_t{i64} when called standalone --- the expansion is entangled with the old path's stateful machinery (the xtc:codegen:get-type-expand-poly flag, gnum substitution, the vars store, reify-generic-type / variable-substitution). To lift this into the constraint world, the clean primitive needed is: "generic type instance (concrete OR with bang vars) -> structural int-code with bang vars" (e.g. List_t{!a}* -> (14 !a 2) at ptr depth), so a candidate (!a List_t{!a}*) unifies against a concrete List_t{i64}* and reifies !a. Once that exists, named-type generics ALSO become plain overload candidates --- same mechanism, no solver change.
  (2) CONSTRAINT-guarded specialisations (the 6th cache field, a predicate the old genericfunc-types evaluates).
  (3) FREE return ([!a,!b]* whose return var isn't bound by a param) --- needs expected-type direction (the old path's `expected` handling).

STATUS / RESUME POINT (updated, supersedes all earlier RESUME stanzas): commits through ba42773b PUSHED; d3f84510 (math/compare overload) and b82b8607 (simple generics) committed LOCALLY, unpushed. NOTE named-type generics are a PREREQUISITE for increment 3 (flipping run-type-check* live), not optional --- a huge amount of real code uses List/Vector generics, so the live path can't go through the new solve until (1) above is handled. Resume at: named-generic-type instantiation (build the "generic instance -> structural int-code with bang vars" primitive, then extend generic-candidates to use it and drop the '{' skip). Then increment 3 (flip run-type-check* to collect+solve; forced-types become eq constraints; remember closure:convert + the scope/alpha-rename concern for shadowed let vars) and increment 4 (delete the six old functions + retry loop). xtc-infer.xtm still loaded only by its test, not live.

Increment 2 CONTINUED, generics part 2 --- NON-RECURSIVE named-type generics (commit d62bd8a9, local/unpushed). The crux was representation: a concrete generic instance is stored as an OPAQUE poly c-name %Name_poly_<base64-of-typeargs> (confirmed by adt.xtm: a Pair{i64,double}* arg resolves to "%Pair_poly_...*"), while a candidate's instance pattern (IPair{!a,!b}*) expands STRUCTURALLY via the parser. They could not unify. Fix, in the int-code BRIDGE (xtc:type:expand-poly-cname + from-intcode-string hook): decode the c-name's base64 type-args (xtc:codegen:split-and-decode-poly-adhoc-name, which Ben flagged is just base64) and substitute them into the generictype template, so from-intcode yields the structural tuple. Now the concrete arg and the candidate pattern unify and the generic reifies --- NO solver change; the bridge is exercised only by the inert canonical core, so live behaviour is unchanged (safety margin holds).

Also in generic-candidates: dropped the named-type skip, and added xtc:infer:sig-reifiable? --- for a multi-specialisation generic it filters out a fully-general fallback whose RETURN is free (first's [!a,!b]*, body-reified in the old path) so it does not shadow the specialisation that unifies. Verified end-to-end against REAL adt.xtm Pair first/second (NEW==OLD==i64/double) plus minimal IBox/IPair stand-ins; infer.xtm is 31 tests, all 10 compiler-unit green.

DEFERRED (kept opaque, out of corpus): (1) RECURSIVE named types --- List=<!a,List*>, Vector, etc. expand-poly-cname explicitly bails on a template that names itself (regex base"[*{,>]") and leaves the instance opaque, so car over a real List gives #f. This is the big real-world chunk (List/Vector are everywhere) and is the resume point. The recursion fix is a custom one-level substitution that, unlike get-type-from-pretty-str, does NOT re-expand the self-referential field but leaves it as the bare generic name (List*) so the concrete and the candidate's bare List* match at the same depth (current get-type-from-pretty-str over-expands to (114 2 (114 !ga_N List*)) --- inconsistent depth). Separately, (list 1 2 3) does not type in infer at all (variadic constructor --- an orthogonal coverage gap to fix before List shadow-tests). (2) Constraint-guarded specialisations. (3) Free returns no specialisation reifies (the old "exercise the generic code" body-reification).

INCREMENT-3 NOTE (carry forward): a generic call that RETURNS a generic instance (e.g. mk_ibox:[IBox{!a}*,!a]*) now yields the STRUCTURAL tuple (114 2 i64) where the old path yields the c-name %IBox_poly_..._*. These agree on the final SCALAR (so scalar-reducing corpus cases match) but diverge on intermediate instance representation --- so going live needs the to-intcode reverse direction to RE-ENCODE a structural generic instance back to its _poly_ c-name (codegen depends on the _poly_ reification). This is part of the increment-3 wiring.

STATUS / RESUME POINT (supersedes earlier RESUME stanzas): through ba42773b PUSHED; d3f84510 (math/compare), b82b8607 (simple generics), d62bd8a9 (named-type generics) committed LOCALLY, unpushed. Resume at: RECURSIVE named-type generics (List/Vector) --- custom one-level template substitution that preserves the bare self-ref + fix (list ...)/variadic constructor typing. Then constraint-guarded + free-return/body-reified generics. Then increment 3 (flip run-type-check*; structural<->c-name re-encode for generic-instance results; forced-types -> eq; closure:convert; shadowed-let alpha-rename) and increment 4 (delete the old six + retry loop). xtc-infer.xtm still loaded only by its test, not live.

Increment 2 CONTINUED, generics part 3 --- NOMINAL parametric types (commits 1b2788ec, 60c19e97, local/unpushed). This SUPERSEDES the structural named-generics approach of d62bd8a9 and the "recursive generics next" resume point above.

WHY THE REWORK: the structural decode (expand a generic instance to its underlying tuple) was found UNSOUND on two counts. (1) Nominal collision: List{i64}, Pair{i64,double}, Point{!a} all expand to the same shape (114 _ _), so a List argument structurally unified with a Pair candidate (confirmed: a first-over-List matched the Pair specialisation; it only coincided on the answer because both return field 0). (2) Container-return divergence: a generic that returns a container (cdr/cons/map) yielded a structural tuple where the old path yields the _poly_ c-name. Ben chose to build the sound foundation rather than scope-limit.

THE FIX (nominal parametric types): a new canonical term (napp name arg...) --- a named generic type applied to type arguments. NO unifier change was needed: the generic Robinson unify already compares the head and runs unify-args, so the name rides through as an atom (mismatched names fail --- List{i64} does NOT unify with a same-shaped Stack{i64}) and the type arguments unify pairwise (List{!a} reifies !a from List{i64}). A recursive type needs no expansion --- the name carries the recursion. The int-code bridge decodes BOTH surfaces to napp (a concrete instance's opaque c-name %Name_poly_<base64>, and a candidate's pretty pattern Name{!a,...} via split-namedtype), and to-intcode-open re-encodes a concrete napp back to the exact c-name (cname-encode of the surface "<args>" string) --- so a container-returning generic reads back byte-identical to the old path. On the infer side, generic-candidates keeps a generic-instance component as its pretty string (the bridge makes it napp) instead of expanding it, and the dispatch now prefers a genericfunc's generic signatures over its compiled monomorphic instances (those cover only already-specialised types --- the cause of car-over-List{double} finding no match).

COVERAGE (all shadow-validated vs the old checker, incl. real adt.xtm Pair first/second and List car/cdr/cons): simple bang-var generics; named-type generics, non-recursive AND recursive; multi-specialisation dispatch (the free [!a,!b]* fallback filtered by sig-reifiable?); nominal disambiguation (gpick over same-shaped Gx/Gy --- the case that needs nominal); container-returning generics (the napp result re-encodes to the c-name). NO solver change throughout; from-intcode/to-intcode are reached only by the inert solve, so live behaviour is unchanged (safety margin holds; aot-compilation + typecheck + pipeline tests green). Tests: typecore +3 napp unit tests, typebridge +3 napp round-trip tests, infer.xtm 35 tests; all 10 compiler-unit green.

DEFERRED (a loud gap, kept out of the corpus): constraint-guarded specialisations (the cache's 6th predicate field); and a FREE RETURN that no specialisation reifies --- e.g. (first someList), whose only matching signature is the [!a,!b]* fallback whose return !a is bound only by the body. The old path body-reifies it ("exercise the generic code"); the constraint path would need expected-type direction or a body pass. These two are the remaining generic prerequisites for going fully live.

STATUS / RESUME POINT (supersedes all earlier RESUME stanzas): through ba42773b PUSHED; d3f84510 (math/compare), b82b8607 (simple generics), d62bd8a9 (structural named-generics, now superseded by the nominal rework), 27210008, 1b2788ec (nominal types), 60c19e97 (napp c-name re-encode) committed LOCALLY, unpushed. Resume at: the two deferred generic cases above IF needed for live (free-return body-reification is the substantive one), then increment 3 --- flip run-type-check* to collect+solve (forced-types become eq constraints; remember closure:convert + the scope/alpha-rename concern for shadowed let vars) --- then increment 4 (delete the six old functions type-unify/complex-unify/unify-lists/sym-unify/occurs-in-type?/unity? + the retry loop). xtc-infer.xtm still loaded only by its test, not live.

## Increment 2 -- free-return generics DONE (commit 19986c13)

A genericfunc's fully-general fallback ([!a,!b]*, first/second) whose return is
bound nowhere in its parameters is now reified by body projection: when no
reifiable specialisation matches the resolved argument types,
xtc:infer:collect-generic-call exercises the fallback body --- a field access
(tref a i) --- against the concrete argument (deref, expand the named or napp
tuple via get-namedtype-type / get-generic-type-as-tuple, read field i).  The
structural analogue of the old 'exercise the generic code' path.  It never
shadows a matching specialisation: a genericfunc carrying a fallback routes
through collect-generic-call, which prefers the lazy reifiable overload when a
spec matches (any-spec-matches?) and only projects the fallback otherwise.

NO solver/core/bridge change -- confined to xtc-infer.xtm (inert traversal) +
its test.  Shadow-validated vs the old checker over a named tuple (bare2) and
napp instances (IList i64/f64, Gx), field 0 AND field 1, composed with math/if.
infer.xtm now 40 tests; compiler-unit 10/10 green; aot/typecheck/pipeline green
(live path untouched).

Deferred (loud gaps, out of corpus): constraint-guarded specialisations (cache
predicate field); a free-return fallback over a NON-ground argument (refers to
an outer var) -- its body can't be exercised standalone, awaits increment 3's
expected-type direction.

## Resume: increment 3 -- go live
Flip run-type-check* (xtc-typecheck.xtm ~3300) to collect+solve; forced-types
become eq constraints; remember closure:convert + the scope/alpha-rename concern
for shadowed let vars.  xtc-infer.xtm gets wired live here (the non-ground
free-return case resolves naturally once expected types flow in).  Then
increment 4 (AC#2): delete the six old unifiers + the retry loop.

## Increment 3 stage 3a -- scaffold + function-level shadow harness DONE

The whole-function entry point xtc:infer:check-function (vars forced-types ast)
is built and shadow-validated against the live xtc:typecheck:run-type-check: it
collects constraints over the t4 AST, seeds each forced-type as a c-eq, solves
once (NO retry loop -- the solver fixpoint subsumes it), and reads every var's
resolved type back into the (var . type) alist the driver's later phases consume.
A forced-type's cdr is the int-code for both scalar (x . 2) and compound
(c 115 2 0) entries, so one c-eq serves both.  collect gained a ret-> case
(pass-through: the marked expr's type is the value; collect-lambda already ties a
lambda's return to its body, and the solver propagates the relationship either
way, so no expected-type direction is needed).

New test tests/compiler/inferfn.xtm shadows run-type-check at FUNCTION
granularity (vs infer.xtm's expression granularity): for 9 functions it
replicates the driver up to t4 and asserts check-function agrees with
run-type-check on every var both resolve.  Verified non-vacuous: e.g. (lambda
(x:i64) (+ x 1)) -> both resolve the anon lambda to (213 2 2).  prep adds the
outer function symbol to vars (the live driver has it registered; the shadow
compares both paths on identical inputs).  Registered in extras/cmake/tests.cmake;
compiler-unit now 11/11 green incl aot (live path untouched -- xtc-infer.xtm is
test-only).

Stage 3a covers functions within collect's existing forms + ret->.  NEXT (3b):
build out the form handlers in difficulty order -- ret->/set!/void/null? then the
tuple/array/vector/pointer/alloc families (the projection machinery from the
free-return work already does field access), then dotimes/while, bitcast,
printf-family/zones -- each batch shadow-validated against a growing corpus of
REAL stdlib functions.  Then 3c (shadow MODE in run-type-check* across a full aot
build -> zero divergence), 3d (flip behind a revertible flag), increment 4
(delete the six old unifiers + retry loop + ~45 old *-check handlers).

## Increment 3 stage 3b batch 1+2 -- memory/aggregate forms DONE

collect now covers the trivial forms (void, null?, impc_null, set!) and the
memory/aggregate families: allocation (stack/heap/zone-alloc -> a fresh var the
binding's forced type fixes), tuple/array/vector ref + set! + ref-ptr, make-tuple,
ref, pref, pointer-ref/pdref (deref), pointer-set!.

Two shapes: a DEREF relates pointer<->pointee structurally (operand = E*, result
E, by unification); a PROJECTION (tuple/array/vector ref) cannot be structural (a
tuple's fields are heterogeneous, an array's size isn't known), so it emits a new
deferred 'project' constraint discharged in the solver fixpoint once the
aggregate resolves.  Added xtc:solve:c-project + xtc:solve:discharge-projects
(interleaved with overloads/defaults; a still-pending project at the fixpoint
just leaves its result undetermined).  The field engine xtc:type:project-field-term
lives in the bridge (it needs codegen/cache): derefs a ptr, indexes a tuple,
yields an array/vector's uniform element, or expands a named/napp type (napp only
once its args are concrete) via get-namedtype-type / get-generic-type-as-tuple.
The free-return reifier now shares this engine (removed the infer-local
expand-to-tuple/project-field duplicates).

inferfn.xtm grew to 18 functions incl tuple build+read (anon + named, field 0/1),
heap-alloc, set!, null?, pointer-ref/set!.  Verified non-vacuous: (lambda (x:i64
y:double) (let ((t:<i64,double>* (salloc))) (tset! t 0 x) (tset! t 1 y) (tref t 0)))
-> both paths give _anon (213 2 2 0), t (114 2 0), x 2, y 0.  compiler-unit 11/11
green incl aot (live path untouched).

NEXT (3b cont.): control forms (dotimes, while, proper multi-return ret->),
then bitcast/bitconvert, the printf/scanf format family, zones, math-intrinsics
-- then start pulling REAL libs/core functions into the corpus.  Then 3c (shadow
MODE across a full aot build), 3d (flip), increment 4 (delete old machinery).
<!-- SECTION:NOTES:END -->
