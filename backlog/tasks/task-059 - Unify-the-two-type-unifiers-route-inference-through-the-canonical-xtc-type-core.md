---
id: TASK-059
title: >-
  Unify the two type unifiers: route inference through the canonical xtc:type:
  core
status: In Progress
assignee: []
created_date: '2026-06-09 01:35'
updated_date: '2026-06-10 09:53'
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
- [x] #2 All xtc:desugar:type-unify/complex-unify/unify-lists/sym-unify/occurs-in-type?/unity? sites migrated or removed
- [x] #3 Live inference unifies via xtc:type:unify + the int-code/pretty bridges
- [x] #4 No regression in generic/poly inference (esp. the #315 pointer-depth class); full suite green
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
CURRENT STATE & RESUME POINT — authoritative as of 2026-06-10 (session 3).
Supersedes every earlier dated stanza.
══════════════════════════════════════════════════════════════════════════════

THE FLIP IS ON: *xtc:infer:live?* defaults to #t (commit c5a7f3fa) — the
constraint engine drives all compilation; the old candidate-list traversal and
its retry loop are bypassed (still present, deletion is increment 4).  All work
committed LOCALLY; nothing pushed — pushing is Ben's deliberate step.

Validated locally, all through the flipped compiler:
- full AOT rebuild: clean_aot + aot_external_audio, exit 0, no errors
- ctest: compiler-unit 11/11, libs-core 9/9 (incl. the previously failing
  tests/core/adt.xtm), libs-external 1/1

SESSION-3 FIXES (commits bfc91815, 9a305942, c5a7f3fa):
1. THE OLD FLIP BLOCKER (recursive-ADT print instance): a free-return generic
   call whose argument is not ground at collect time (tree_value## inside the
   print instance) emitted only the context-directed overload, leaving the
   fallback's return free; the surrounding print## overload greedily picked
   [void,Tree*], and the residual silently DROPPED both sites from the
   read-back (which is also why the shadow sweep never flagged it — missing
   entries don't count as DIVERGE).  Fix: the fallback's field projection now
   rides the deferred branch as a c-project constraint; when the argument
   grounds mid-solve the return pins to its field and the search prunes wrong
   picks.  A call site dropped for a residual type now also warns loudly.
2. SOLVER PERFORMANCE (instruments AOT was >10min, vs old path 7.7s — hit the
   300s AOT timeout): commit-pass replaces commit-one (commits every
   currently-unique overload per pass, threads subst, keeps NARROWED candidate
   lists — pruning is permanent since substs only extend; narrowing is
   functional so search branches can't leak prunes); overload-viable hoists
   all (key,subst)-only work out of the candidate loop (key decodes,
   resolutions, class-table representatives — old arg-class rescanned the
   class table per candidate ARGUMENT).  instruments AOT now ~11s; audio_dsp
   first-100-fn solve 8.6s → 0.4s.  If perf regresses, profile via
   overload-viable: it was ~99% of solve time.
3. CALL-VALUE LENIENCY: dlogue's (if (FM) ...) calls an i1 local; the old
   checker reads (v) over a non-closure as v itself.  New 'callval deferred
   constraint (zero-arg application of a local symbol): once the head grounds
   — closure: its return; closure with params: conflict; else: the value.
4. analogue_fx (the last shadow CONFLICT) compiles clean under the flip —
   resolved by fix 1/2, no separate change needed.

Diagnostics added: xtc:solve:explain-conflict (replays equalities naming the
first failure, or names every overload with no viable candidate) runs on
check-function's error path; unresolved-call-site dropped warning in the
read-back.

──────────────────────────── REMAINING ────────────────────────────
1. CI (Linux/macOS/Windows) on Ben's push — gates everything below.
   (Expect possible benign cache diff: blsaw `t` allocas double vs float —
   an UNUSED binding, observationally identical.)
2. AC#4 check after CI is green.
3. Increment 4 (AC#2): delete the six old unifiers
   (xtc:desugar:type-unify / complex-unify / unify-lists / sym-unify /
   occurs-in-type? / unity?), the retry loop in run-type-check*, the ~45 old
   xtc:typecheck:*-check handlers, the shadow scaffolding (both flags),
   get-expression-type's old-path internals.  semantic-phase's unity? check is
   replaced by the live branch erroring on unresolved vars.  Delete
   tests/compiler/{constraints,typeunify}.xtm and rework infer/inferfn (their
   oracle IS the new path once the old is gone).
4. AFTER this task lands: merge Ben's xtm-test-review worktree (principled
   xtmtest refactor retiring the GC-fragile harness; based on older master,
   do not merge before).

──────────────────────── KEY FILES, FLAGS, COMMANDS ────────────────────────
- runtime/xtc-infer.xtm — traversal (xtc:infer:collect), check-function,
  fallback-recipe/project-fallback (free-return generics), flags
  *xtc:infer:shadow?* / *xtc:infer:live?* (now #t).
- runtime/xtc-solve.xtm — solver: eq / default / overload / project / callval;
  commit-pass + search (backtracking, most-constrained split);
  explain-conflict.
- runtime/xtc-types.xtm + xtc-types-bridge.xtm — canonical core + bridges.
- runtime/xtc-typecheck.xtm:3282 run-type-check — the flip branch.
- Tests from build/: ctest --label-regex "compiler-unit|libs-core|libs-external" -j4.
- AOT through flip: cmake --build . --target clean_aot && cmake --build . --target aot_external_audio -j.
- Old path for comparison: (set! *xtc:infer:live?* #f) at runtime, or the
  define in xtc-infer.xtm.  Per-lib from-source: *xtc:globals:with-cache* #f.

══════════════════════════════════════════════════════════════════════════════
HISTORY / RATIONALE (context only; full detail in git log)
══════════════════════════════════════════════════════════════════════════════
- Ben chose a full rewrite (delete the candidate-list union-find + number-crunch
  + enumeration retry loop) over a site-by-site port.
- Increments 0–3 (sessions 1–2): old pruning contract frozen
  (tests/compiler/typeunify.xtm); xtc-solve.xtm constraint engine; xtc-infer.xtm
  constraint-emitting traversal (full form coverage, nominal napp generics,
  free-return body projection); check-function whole-function entry; live
  shadow hook; from-source shadow sweep over every core/external lib + test
  corpus, all clean except the two items fixed in session 3.
- Session-2 en-route fixes: real GC bugs in FFI list-builders (s7_cons GC'ing
  before storing args — S7_DEBUGGING=1 build of src/s7.c is the decisive tool
  if corruption ever smells again); test.xtm results alist set-cdr! into
  SEMIPERMANENT pairs; driver reentrancy; bare-generic-name decode;
  pattern-typed read-back.
- xtc:solve:search rationale: real library code NEEDS joint overload choice
  (cstring/String str elections; SAMPLE=float coefficient chains vs libc
  double natives; Rational/Complex operators) — one-step and cascade-lookahead
  horizons both failed on audio_dsp.

─────────────── SESSION-4 ADDENDUM (2026-06-10) ───────────────
Two state changes since the session-3 stanza above:
1. The xtm-test-review worktree ALREADY LANDED on local master (commits
   7251cca8..99c7c85e, on top of the flip) — the 'merge after this task'
   ordering in REMAINING item 4 is moot.  The branch worktree now points at
   the same commit as master.
2. The new harness exposed two genuinely-broken typebridge assertions
   (napp-encode, napp-pattern-reifies): encode needs the List generictype
   registered (the c-name choke point runs List{i64} through the old parser),
   never true in the test's bare process.  The OLD harness had silently
   dropped error results (catch branch passed args out of order into
   xtmtest-update-test-result, label position got the call form, matched no
   group) — so these had NEVER passed; the old green was a lie.  Fixed in
   dde9dfb0 by mirroring adt.xtm's (bind-type List <!a,List*>) before the
   encode assertions.
Suite re-validated at dde9dfb0 through the flipped compiler: compiler-unit
11/11, libs-core 9/9, libs-external 1/1 (21/21).  Local master is 43 commits
ahead of origin/master, nothing pushed.  NOTHING BLOCKS push → CI → AC#4 →
increment 4 (the deletion).

CI run 27261868361 green on all four platforms (Linux x86_64/aarch64, macOS aarch64, Windows x86_64) for the full pushed stack incl. the flip — AC#4 satisfied. Only increment 4 (the deletion, AC#2) remains.

─────────────── INCREMENT 4 DONE (2026-06-10, session 4) ───────────────
da69096d deletes the old path: run-type-check is the constraint-engine entry
alone (retry loop + call/cc continuation gone); the six unifiers + helper
family deleted from xtc-transforms; the ~45 *-check handlers + dispatch +
candidate-list machinery deleted from xtc-typecheck (4088→645 lines);
shadow scaffolding (both flags) removed from xtc-infer; semantic-phase's
unity? check dropped (read-back returns only resolved entries; conflicts
error in run-type-check; dropped call sites error at the site).
Tests: typeunify.xtm + constraints.xtm deleted; inferfn.xtm asserts full
concrete resolution via check-function directly; infer.xtm's corpus is now
an entry-point parity check; typecheck.xtm drops the deleted helpers.
Deletion set computed by reachability from externally-referenced roots
(scripts in /tmp/xtc-reach.py, /tmp/xtc-delete.py, session-local).
Validated: full AOT rebuild clean, ctest 21/21. AC#2 checked — all four
ACs now done. Remaining before Done: push + CI green on the deletion
commit (same gate as the flip), then mark Done.
<!-- SECTION:NOTES:END -->
