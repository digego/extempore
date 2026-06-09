---
id: TASK-059
title: >-
  Unify the two type unifiers: route inference through the canonical xtc:type:
  core
status: In Progress
assignee: []
created_date: '2026-06-09 01:35'
updated_date: '2026-06-09 21:40'
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
Strangler migration in green increments through the canonical core (Ben chose full rewrite, not a site-by-site port).

DONE: Inc 0 (freeze old pruning contract, typeunify.xtm) - Inc 1 (xtc-solve.xtm constraint engine) - Inc 2 (xtc-infer.xtm constraint-emitting traversal: full form coverage + nominal-napp generics + free-return body projection, shadow-validated in infer.xtm) - Inc 3 stages 3a (check-function whole-function entry + inferfn.xtm) / 3b (full common-surface + long-tail forms + deferred project constraint) / 3c (live shadow hook, zero divergence over real code) / 3d (flip gated behind *xtc:infer:live?*, default off; common surface compiles+runs via the new path).

REMAINING (see Implementation Notes 'START HERE'): (1) reconcile check-function's result-alist shape/keys with codegen+semantic-phase so dotimes/while drive codegen correctly under the flip (codegen sym-name-stack issue at xtc-codegen.xtm:4119, NOT an inference bug). (2) full from-source shadow-clean sweep -> zero SHADOW-DIVERGE/ERROR. (3) flip *xtc:infer:live?* default to #t. (4) Inc 4 (AC#2/#3): delete the six old unifiers + the retry loop + the ~45 old *-check handlers. (2)-(4) gated on full CI (Linux/macOS/Windows).
<!-- SECTION:PLAN:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
══════════════════════════════════════════════════════════════════════════════
CURRENT STATE & RESUME POINT — authoritative as of 2026-06-10.
Supersedes every dated "STATUS / RESUME POINT" stanza in the History section below.
══════════════════════════════════════════════════════════════════════════════

HEAD = 4bcc02d6. All work is committed LOCALLY and GREEN (compiler-unit 11/11,
incl. the real aot compile). Nothing pushed this session — pushing is Ben's
deliberate step. The live compiler is UNCHANGED by default: the new path sits
behind two default-off flags (*xtc:infer:shadow?*, *xtc:infer:live?*).

WHAT THE TASK IS: replace the old string/int-code candidate-list unifier
(xtc:desugar:type-unify family + the enumeration retry loop in run-type-check*)
with the canonical Robinson core (xtc:type:unify), routing live inference through
a clean constraint engine and then deleting the old family.

──────────────────────────── DONE ────────────────────────────
- Inc 0/1: old pruning contract frozen (tests/compiler/typeunify.xtm); xtc-solve.xtm
  built — the constraint engine over the canonical core (eq / default / overload).
- Inc 2: xtc-infer.xtm — the constraint-EMITTING traversal. Full form coverage +
  all generics. Generics use NOMINAL parametric types: a new canonical term
  (napp name arg...) keeps the generic name, so List{i64} != same-shaped Stack{i64}
  and recursive types need no expansion; the bridge decodes both a concrete
  instance's poly c-name and a candidate's Name{!a} pattern to napp, and re-encodes
  a concrete napp back to the exact c-name. Free-return fallbacks (first/second's
  [!a,!b]*) reify by projecting the body's field. Shadow-validated vs the old
  checker (xtc:bind:get-expression-type) in tests/compiler/infer.xtm.
- Inc 3:
  * 3a — xtc:infer:check-function (vars forced-types ast): the whole-function
    analogue of run-type-check. Collect constraints over the t4 AST, seed each
    forced-type as a c-eq, solve once (NO retry loop — the solver fixpoint subsumes
    it), read every var's type into the (var . type) alist the driver consumes.
    tests/compiler/inferfn.xtm shadows run-type-check at function granularity.
  * 3b — collect covers the FULL common surface + long tail: ret->, set!, void,
    null?, impc_null, string literals; alloc (stack/heap/zone), tuple/array/vector
    ref+set!+ref-ptr, make-tuple/array/vector, ref, pref, pointer-ref/pdref,
    pointer-set!, vector-shuffle; while, dotimes; bitcast/bitconvert; printf family;
    math intrinsics (sqrt/sin/pow/...) + the full mathbinaryaritylist (modulo +
    bitwise); zones; num-of-elts/obj-size. Tuple/array/vector indexing needed the
    one solver extension this migration added: a DEFERRED 'project' constraint
    (xtc:solve:c-project + xtc:solve:discharge-projects, interleaved in the fixpoint)
    with the field engine xtc:type:project-field-term in the bridge; the free-return
    reifier shares it.
  * 3c — live shadow: xtc-infer is loaded live (sys:load in xtc-globals.xtm, after
    solve); run-type-check cross-checks check-function when *xtc:infer:shadow?* is on
    (catch-guarded, observational, logs SHADOW-ERROR/CONFLICT/DIVERGE). Validated
    ZERO divergence over extensive real bind-funcs: recursion (fib, list-sum),
    generics over concrete lists (car/cdr/cons/nil/list/length/reverse), closures as
    values + args, while/dotimes with pointer access, tuples, named types, math,
    strings (printf %s, cat).
  * 3d — the flip, GATED behind *xtc:infer:live?* (default #f). When on,
    run-type-check returns check-function's result directly (old traversal + retry
    loop bypassed). check-function's read-back is the UNION of the var table and the
    program vars the constraints referenced (interned by name) so a form-introduced
    var (dotimes counter) is not dropped. With the flip on the common surface
    COMPILES AND RUNS correctly via the new path (verified: arithmetic, tuples,
    alloc, conditionals).

──────────────── THE ONE REMAINING BLOCKER — START HERE ────────────────
dotimes (and probably a few similar forms) fail under the flip in CODEGEN, not in
inference. The new types are CORRECT — shadow shows the dotimes counter i resolves
to i64. But codegen rejects it: xtc:codegen:compiler at xtc-codegen.xtm:4119-4125
errors "cannot find variable i" because i IS in the `types` alist yet NOT on
*xtc:codegen:sym-name-stack*. Codegen's scope tracking depends on a structural
property of the result alist that check-function spells differently from the old
run-type-check (note check-function keys the function as e.g. lv4_adhoc_8, and
returns only resolved vars).

NEXT STEP: reconcile check-function's alist with what codegen + semantic-phase rely
on — the function-symbol key, the entry format, WHICH vars are present, possibly
order. Reproduce: (set! *xtc:infer:live?* #t) then
  (bind-func lv4 (lambda (n:i64) (let ((s:i64 0)) (dotimes (i n) (set! s (+ s i))) s)))
fails; lv1-lv3 (arithmetic/tuple/alloc) succeed. Diff the new alist (dump in the
live branch of run-type-check) against the old typelist for lv4 to find the
structural property codegen needs.

──────────────────────────── THEN ────────────────────────────
1. Full from-source shadow-clean sweep. Force recompilation (the cache otherwise
   skips run-type-check): (set! *xtc:globals:with-cache* #f) + (set! *xtc:infer:shadow?* #t)
   then load/compile real libs; OR run extempore --nobase --batch
   "(xtc:aot:compile-xtm-file \"<lib>\")" to compile a whole lib from source. Grep
   the output for SHADOW-ERROR / SHADOW-CONFLICT / SHADOW-DIVERGE; drive to zero.
2. Flip *xtc:infer:live?* default to #t.
3. Increment 4 (AC#2/#3): delete the six old unifiers (type-unify, complex-unify,
   unify-lists, sym-unify, occurs-in-type?, unity?), the retry loop in
   run-type-check*, and the ~45 old xtc:typecheck:*-check handlers.
4. All of 2-4 gated on full CI (Linux/macOS/Windows) — Ben's push.

──────────────────────── KEY FILES, FLAGS, COMMANDS ────────────────────────
- runtime/xtc-infer.xtm — the traversal; xtc:infer:collect (the form dispatch),
  check-function, shadow-check; flags *xtc:infer:shadow?* / *xtc:infer:live?* /
  counter *xtc:infer:shadow-count*.
- runtime/xtc-solve.xtm — the solver: eq / default / overload / project constraints
  + the resolve fixpoint.
- runtime/xtc-types.xtm + xtc-types-bridge.xtm — canonical core + int-code/pretty
  bridge (napp term, xtc:type:project-field-term).
- runtime/xtc-typecheck.xtm:3282 xtc:typecheck:run-type-check — the flip branch +
  the shadow hook.
- runtime/xtc-globals.xtm — loads xtc-infer live.
- Tests: tests/compiler/{infer,inferfn,solve,typecore,typebridge}.xtm. Run from
  build/: ctest --label-regex compiler-unit -j4. (11/11 green incl aot.)
  Note: inferfn's standalone prep cannot register a self-symbol or a dotimes
  counter the way the live pipeline does, so recursion and dotimes are validated by
  the LIVE shadow, not by inferfn.

══════════════════════════════════════════════════════════════════════════════
HISTORY / RATIONALE (context only; not needed to resume — full detail in git log)
══════════════════════════════════════════════════════════════════════════════
- Ben chose a full rewrite (delete the candidate-list union-find + number-crunch +
  enumeration retry loop) over a site-by-site port.
- The solver splits the old conflated candidate list into three constraint kinds:
  equality (Robinson unify), numeric default (one post-unify rule), overload
  (deferred choice, a fixpoint that commits unambiguous overloads then defaults as
  tie-break — subsumes the retry loop with no search). Class-aware viability keeps
  an int literal from binding to a float param.
- Generics went through a structural decode first; that was found UNSOUND (List,
  Pair, Point all expand to the same shape, so distinct generics cross-unified, and
  container-returning generics diverged from the old c-name). Reworked to NOMINAL
  napp types (above) — the sound foundation; no unifier change needed.
- SAFETY MARGIN that held until 3c: from-intcode/to-intcode and the solver were
  reached only by the inert canonical core, so changes could not alter live
  behaviour. 3c loaded xtc-infer live but kept it inert behind the shadow flag; 3d's
  flip is likewise default-off. So every commit kept the live compiler unchanged.
- The clean unifier is intentionally STRICTER than the old path on implicit numeric
  coercion (a narrower result into a wider param; a bare int literal into a float
  param). Those two coercions are kept out of the shadow corpus by design.
<!-- SECTION:NOTES:END -->
