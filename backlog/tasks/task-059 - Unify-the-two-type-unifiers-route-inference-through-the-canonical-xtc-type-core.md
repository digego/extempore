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
CURRENT STATE & RESUME POINT — authoritative as of 2026-06-10 (session 2).
Supersedes every dated "STATUS / RESUME POINT" stanza in the History section below.
══════════════════════════════════════════════════════════════════════════════

All work committed LOCALLY and GREEN (compiler-unit 11/11 incl. the real aot
compile; libs-core ctest 9/9). Nothing pushed — pushing is Ben's deliberate
step. The live compiler is UNCHANGED by default: the new path sits behind two
default-off flags (*xtc:infer:shadow?*, *xtc:infer:live?*).

SESSION-2 RESOLUTION OF THE OLD "ONE REMAINING BLOCKER": the dotimes repro in
the previous notes was INVALID xtlang (an undeclared counter is a compile
error on the old path too — counters must be let/arg-declared, see the
dotimes docstring; doloop is the auto-binding variant). The real codegen
integration gaps were (a) the read-back leaking non-locals (globals, ## call
sites read by name) into the types alist, which codegen scope-checks against
its sym-name-stack, and (b) missing old-path symbol discipline in collect.
Both fixed; the flip now compiles and runs the full common surface, generics,
poly overloads, closure values, globals, convert/coercions.

FROM-SOURCE SHADOW SWEEP (cache off, *xtc:infer:shadow?* on, per-lib
extempore --nobase): base 106, math 307, adt 147, math_ext 307, rational 219,
audiobuffer 195, audio_dsp 478, instruments 830, scheduler 225,
pattern-language 0 (pure scheme), fft 351, sndfile 188, audio_dsp_ext 440,
portmidi 203, stb_image 107 functions; PLUS the test corpus run under shadow
(tests/core/{xtlang 233, generics 125, adt 254, type-system 41, std 2}) —
ALL ZERO SHADOW-ERROR/CONFLICT/DIVERGE except (a) ONE accepted benign class:
an UNUSED float-literal binding's width (blsaw_c/blsawXAnalogue_c `(t 0.0)`,
never read: old fp32 via the retry loop's accidental ordering, new fp64
default; observationally identical — no reads, alloca width only), and
(b) analogue_fx (instruments) still CONFLICT — the one remaining function,
under investigation.

The sweep drove a dozen fixes (see git log this session), the structural ones:
- xtc:solve:search — backtracking search over pending-overload candidate
  choices (cascade uniques, split on the first ambiguous, prune on conflict,
  candidate order = default order so greedy first-success ≡ defaulting for
  genuinely free ties). This is the honest replacement for the old retry
  loop: real library code (cstring/String str elections; SAMPLE=float filter
  coefficient chains vs libc double natives; Rational/Complex operator
  overloads) NEEDS joint choice — one-step and cascade-lookahead horizons
  both failed on audio_dsp. Replaces lookahead/finish-pending; defaults now
  run only after all overloads resolve.
- to-pretty renders SURFACE spelling (named types unprefixed) — the %-form
  napp re-encode minted poly c-names that don't exist, and walking one hung
  the old typechecker's resolver (the "adt hang").
- global-candidates UNIONS native+closure+poly sources (tan is a double
  native AND carries a float poly overload).
- value-aware int literal classes, (si64 si32) index defaults, vecmath
  structural candidate (V ⊕ V* → V), result-key class admissibility,
  arg-class by canonical variable, one-armed if, '() forms, pointer-ref-ptr,
  closure-ref/set!/refcheck, compound-head application ((pref buf i) x).

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

──────────────────────────── REMAINING — START HERE ────────────────────────────
The flip TRIAL (default #t) is nearly green; the default is back to #f so the
tree stays releasable.  State of the trial:

- compiler-unit 11/11 under the flip.  libs-core 7/9: tests/core/{adt,xtlang}
  fail with SEQUENCE-DEPENDENT failures (~11 across both).  Everything those
  files exercise passes when run in isolation OR re-typed verbatim through the
  same xtmtest macro in a fresh process — including the file's own first three
  tests, copied character-for-character.  In the file, the first divergence is
  test_bit_twiddle_2: it COMPILES, then CALLING it from scheme throws (xtmtest
  label 'compile'), and subsequent results misattribute/cascade
  (no-compile/incorrect).  tests/core/adt.xtm's analogue: a 'Scheme wrapper
  error: check the arg arity and types' on test_map_pair.  NOT the bytecode
  cache (fails with *xtc:globals:with-cache* #f too).  Suspect surface: the
  scheme-stub/wrapper IR for recompiled-name closures under the flip, or
  result-label misattribution in xtmtest-update-test-result keyed by func-sym.
  Reproduce: flip the default to #t, ctest -R "tests/core/xtlang".

- The big flip blockers ARE fixed this session (see commit 8be9f68d): instance
  type registration in the napp re-encode (the type? hang on unknown c-names),
  check-function reentrancy (nested data-constructor compiles mid-read-back),
  ## call-site keys in the alist, and the post-solve xtm_NAME## rewrite for
  tuple/named operand math (Rational works).  adt.xtm compiles from source
  through the flip in ~2s compile time vs ~6s old.

After the flip validates:
1. Full ctest labels + clean_aot + aot_external_audio rebuilt THROUGH the flip
   (expect a benign cache diff: blsaw `t` allocas double vs float).
2. Increment 4 (AC#2/#3): delete the six old unifiers (type-unify, complex-unify,
   unify-lists, sym-unify, occurs-in-type?, unity?), the retry loop in
   run-type-check*, the ~45 old xtc:typecheck:*-check handlers, and the shadow
   scaffolding (shadow-check + both flags). semantic-phase's unity? check is
   replaced by the live branch erroring on unresolved vars.  Delete
   tests/compiler/{constraints,typeunify}.xtm (they test the old machinery) and
   rework infer/inferfn (their oracle IS the new path once the old one is gone).
3. Gated on full CI (Linux/macOS/Windows) — Ben's push.

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
