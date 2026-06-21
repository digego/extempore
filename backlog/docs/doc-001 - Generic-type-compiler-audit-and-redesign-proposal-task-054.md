---
id: doc-001
title: Generic-type compiler audit and redesign proposal (task-054)
type: specification
created_date: "2026-06-08 05:16"
---

# Generic-type compiler audit and redesign proposal

Read-only audit of the xtlang generic-type machinery, motivated by issue #315
(digego/extempore) and rehomed here from task-053. This document maps the type
representations and their conversion boundaries, the freshening and unification
call graph, the special-case/duplicate/dead-code debt, the precise root cause of
#315 plus the class of latent bugs it implies, and a phased, de-risked redesign.
No production code was changed (see
[§6](#6-confirmation-no-production-code-changed)).

All line references are absolute to files under `runtime/`. Evidence was
gathered by reading the source, by structural (`ast-grep`) and text searches,
and by running read-only experiments against `build/extempore` (built
2026-06-08).

## Executive summary

The generic-type compiler works, but it is built on a stringly-typed
representation with no single source of truth for type identity. Five different
type representations coexist and are converted into one another at dozens of
boundaries, much of it by hand-maintained PCRE. Type variables are bare Scheme
symbols carrying their identity, pointer depth, instantiation number, and
generic provenance all encoded as substrings of their printed name. There is no
substitution map proper: "are these two variables the same?" is answered by
scattered string and regex heuristics.

The headline finding is a **correction**. The diagnosis recorded in task-053 and
in the #315 issue --- "the two occurrences of `!a` get separate freshened
identities and are never unified" --- is mechanically wrong. The two `!a`
occurrences in the _function_ signature do share one freshened name; I confirmed
this both statically and by experiment. The real cause is narrower and more
structural: a type variable has no way to carry a pointer level, so a tuple
field `!a*` collapses to the bare symbol `!a` when parsed, and the
reverse-binding step then binds `!a` to the _whole_ concrete field type (`i64*`)
instead of stripping the field's declared pointer depth first. The value
parameter, which correctly shares that variable, inherits the spurious pointer
level. See [§4](#4-315-root-cause-and-related-latent-bugs).

This matters for the redesign: a "unify the two variables / single substitution
map" fix --- the obvious reading of the original diagnosis --- would not fix
#315, because the variables are already the same. A binding-site patch (strip
the field's pointer depth before binding) was implemented and reverted: it
merely relocates the error, because the variable's binding is overloaded across
pointer depths ([§4.4](#44-empirical-confirmation-the-fix-is-non-local)). The
fix has to give type variables a pointer-carrying representation through parsing
and unification --- the Phase 1 slice of the redesign
([§5](#5-phased-redesign-proposal)).

## 1. Type representations and conversion boundaries (AC#1)

### 1.1 The five representations

| #   | representation        | example                                     | role                         |
| --- | --------------------- | ------------------------------------------- | ---------------------------- |
| a   | raw int code          | `2` (i64), `102` (i64\*), `213` (closure)   | internal scalar type algebra |
| b   | tagged int-code list  | `(114 102 2)` tuple, `(213 0 0)` closure    | internal complex types       |
| c   | pretty string         | `"<i64*,i64>*"`, `"mzone*"`, `"[i64,i64]*"` | user-facing; `bind-*` input  |
| d   | LLVM IR string        | `"{i64*, i64}*"`, `"i64"`                   | the C++/JIT boundary         |
| e   | typevar symbol/string | `!a`, `!gxa_34##7`, `%xlist_poly_PGk2...*`  | generics only                |

Representations (a) and (b) are one scheme: a complex type is a list whose head
is the same kind of int code as a scalar, with the structure spelled out in the
tail. The canonical code table is the single source of truth at
`llvmti-globals.xtm:104-142`:

```scheme
(define *impc:ir:void* -1) (define *impc:ir:double* 0) (define *impc:ir:float* 1)
(define *impc:ir:si64* 2)  (define *impc:ir:ui64* 3)   ;; ... si/ui 32/16/8 ...
(define *impc:ir:i1* 10) (define *impc:ir:char* 11) (define *impc:ir:null* 12)
(define *impc:ir:closure* 13) (define *impc:ir:tuple* 14)
(define *impc:ir:array* 15) (define *impc:ir:vector* 16)
(define *impc:ir:other* 1000) (define *impc:ir:pointer* 100)
```

**Pointer depth is encoded as `+100` per level** (`code = base + 100 * depth`),
applied/stripped centrally:

- encode: `impc:ir:pointer++` (`llvmir.xtm:1012-1027`)
- strip: `impc:ir:pointer--` (`llvmir.xtm:994-1009`)
- read depth: `impc:ir:get-ptr-depth` (`llvmir.xtm:156-166`),
  `(floor (/ code 100))`
- read base: `(modulo code 100)`, used by every predicate (`tuple?` `1104`,
  `closure?` `1134`, `pointer?` `1088`).

So `i64`=2, `i64*`=102, `i64**`=202. `*impc:ir:other*`=1000 is a sentinel "above
all pointer levels" that predicates guard against. A **tuple value** is always a
pointer, so its head is `14+100 = 114`; a **closure value** is
pointer-to-`{i8*,i8*,fnptr*}`, depth 2, so its head is `13+200 = 213` --- this
`213` recurs as a bare literal throughout the native-function cache (see
[§3.3](#33-magic-numbers-and-hard-coded-type-layouts)). Negative codes encode
`void`/`notype`/`badtype` and LLVM `\N` recursive back-references (`@` maps to
`-2`).

### 1.2 The conversion functions and their directions

The four functions named in the task brief resolve to these (all `impc:ir:`):

| brief name                 | real name                                    | file:line            | direction              |
| -------------------------- | -------------------------------------------- | -------------------- | ---------------------- |
| `get-type-from-pretty-str` | `impc:ir:get-type-from-pretty-str(-rec)`     | `llvmir.xtm:356-705` | pretty → code/list/sym |
| `get-type-from-str`        | `impc:ir:get-type-from-str`                  | `llvmir.xtm:771-822` | IR string → code/list  |
| `pretty-print-type`        | `impc:ir:pretty-print-type` (alias `pptype`) | `llvmir.xtm:297-332` | code/list → pretty     |
| `str-list-check`           | `impc:ir:str-list-check`                     | `llvmir.xtm:878-901` | any → canonical head   |

The forward emitter to LLVM is `impc:ir:get-type-str` (`llvmir.xtm:825-864`,
code/list → IR string); it is the only thing the JIT consumes (134 call sites in
`llvmir.xtm`). Bulk converters: `convert-types` (`867-875`, string→code) and
`convert-from-pretty-types` (`708-716`, pretty→code, 19 call sites).

`get-type-from-pretty-str-rec` (343 lines) is the entry of every user-written
type into the numeric algebra and the most complex function in the system. It
dispatches on **regex matches of the surface syntax** of the type string
(`"^\[.*\]$"` closure, `"^\<[^{].*[^}]\>$"` tuple, `"^/.*/"` vector, `"^\|.*\|"`
array, `"^[A-Za-z0-9_]*\{"` generic application, etc.) and has heavy side
effects --- it JIT-compiles new LLVM struct types on the fly (`llvm:compile-ir`
at `583`) and schedules data-constructor compilation (`588`).

### 1.3 Where representations cross (the impedance surface)

The C++ side (`src/ffi/llvm.inc`) knows nothing of the int codes; it speaks only
LLVM IR strings (`get_named_type` `667-694`, registered `865`). The xtlang side
then re-parses those strings back to codes. The volume of textual type
manipulation is large: roughly 35 `regex:` calls in `llvmir.xtm`, 64 in
`llvmti-typecheck.xtm`, 79 in `llvmti-transforms.xtm`, plus ~91
split/contains/replace calls in transforms alone --- almost all operating on
something that is semantically a type.

Two hand-maintained recursive PCRE grammars do the structural parsing as text:
`impc:ir:regex-tc-or-a` (`llvmir.xtm:175-180`, pretty types) and
`impc:ir:regex-structs-or-atoms` (`721-728`, IR types), with
`impc:ir:get-type-joiner` (`184-195`) re-gluing tokens the regex wrongly split.
Pointer depth is read off by counting `*` (`get-ptr-depth`, regex
`"([*]*)($|#)"`). Round-trips that exist purely as backward-compat overhead:
`impc:ti:get-named-type` (`llvmti-caches.xtm:1800-1809`) converts a cached
int-code _list_ back to an IR _string_ (with a comment explaining it mimics the
pre-MCJIT `llvm:get-named-type`), forcing callers to re-parse; and
`llvmti-typecheck.xtm:946-948` calls the C++ `llvm:get-named-type` then
immediately re-parses the string twice. Each list→string→list hop is a place for
representation drift.

The generic encodings (representation e) never enter the numeric scheme; they
ride as symbols and are manipulated by string surgery: `##gnum` add/strip (~30
sites in `llvmti-typecheck.xtm`), `_poly_`/`_adhoc_` name build/decode via
`cname-encode`/`cname-decode` (`llvmir.xtm:346-354, 485-547`), and `bang-type?`
= a bare `(string-contains? ... "!")` (`llvmti-transforms.xtm:1140-1142`).

## 2. Freshening and unification call graph (AC#2)

### 2.1 There is no substitution map

The only typevar store is `vars`, a hashtable of union-find cells
(`make-uf-cell`/`uf-find!`/`uf-union!`, `llvmti-typecheck.xtm:1-45`), accessed
through the `vars-ref`/`vars-update`/... API (`72-304`). The sole other
`make-hashtable` calls just snapshot or clear `vars` (`291`, `301`). Two
variables become "the same" only by `uf-union!` on the _identical symbol key_
(suffix, prefix and all). There is no map that canonicalises a source variable
across its freshened forms.

Cross-variable reconciliation is done instead by scattered heuristics:

- `type-unify` (`llvmti-transforms.xtm:1855-1977`) resolves a variable purely by
  exact-symbol lookup: `(if (and (symbol? t) (impc:ti:vars-ref vars t)) ...)`
  (`1872`).
- `descending-generic-type-match` (`llvmti-typecheck.xtm:823-841`) treats
  **any** `!`-prefixed atom as a wildcard --- two _different_ bang vars "match"
  as long as both start with `!` (`827-835`). It never checks they are the same
  variable.
- `check-bang-against-reified` (`llvmti-transforms.xtm:2003-2024`) decides
  identity by **regex on the printed name**: both must match
  `^!g(.*)_.*##([0-9]*)$` with equal capture groups (`2015-2021`).
- `sym-unify` (`llvmti-transforms.xtm:2028-2061`) bridges a bang to a reified
  type by **gnum equality** (`(= (cadr gtd) (cadr gtd2))`, `2043`).
- `type-unify-closure` (`llvmti-transforms.xtm:1816-1838`) computes a unified
  result then discards it (`(set! p4 '())`, `1837`) --- effectively a no-op in
  its current state.

### 2.2 Two-stage freshening, with an inconsistent prefix

Freshening happens in two distinct stages, and conflating them is the source of
the structural fragility:

**Stage 1 --- definition-time renaming**, baked into the cache. When a generic
entity is registered, its user-written `!a` is rewritten to a `!g`-family name
carrying the global counter `*impc:ti:generic-count*`
(`llvmti-caches.xtm:1767`):

- generic **functions**: `!a → !gxa_<N>` (prefix `!gx`,
  `register-new-genericfunc` `llvmti-caches.xtm:1260-1348`, literal at `1292`;
  counter bumped at `1264`).
- generic **types**: `!a → !ga_<N>` (prefix `!g`, `register-new-generictype`
  `llvmti-caches.xtm:1769-1792`, literal at `1782`; counter bumped at `1773`).

Both share one counter but use different prefixes (`!gx` vs `!g`) and different
"already-`!g`" collapse rules. The downstream identity regex in
`check-bang-against-reified` was written for one shape and does not normalise
the difference.

Critically, `register-new-genericfunc` de-duplicates the `!`-syms
(`cl:remove-duplicates`, `1286`) and _then_ replaces every occurrence
(`regex:replace-everything`, `1302`), so **all occurrences of a variable within
one signature get the same fresh name**. This is the fact that refutes the
original #315 diagnosis ([§4](#4-315-root-cause-and-related-latent-bugs)).

**Stage 2 --- call-time `##gnum` suffixing.** At each call, a per-call `gnum` is
parsed from the call-site symbol `fname##NNN` (`llvmti-typecheck.xtm:1401-1403`)
and appended to every bang/generic name by `nativef-generics-make-gtypes-unique`
(`981-1004`), `variable-substitution` (`1223-1268`), and `inject-missing-vars`
(`1303-1328`); read back by the same `##gnum` string in
`reify-generic-type-expand` (`llvmti-transforms.xtm:1174-1196`) and
`reverse-set-bangs-from-reified` (`llvmti-typecheck.xtm:1011-1043`).

### 2.3 Call graph

```
DEFINITION-TIME freshening (mints !g / !gx names into caches)
├─ register-new-genericfunc   caches.xtm:1260   !a → !gxa_<N>  (dedup 1286, replace-all 1302)
└─ register-new-generictype   caches.xtm:1769   !a → !ga_<N>
     └─ both bump *impc:ti:generic-count*  caches.xtm:1767/1264/1773

CALL-TIME entry: type-check dispatch (typecheck.xtm:3478, 3853, 4019)
└─ nativef-generics           typecheck.xtm:1387   gnum from call-site ##NNN (1401-1403)
   ├─ nativef-generics-make-gtypes-unique   981   append ##gnum
   ├─ nativef-generics-inject-missing-vars  1303  seed vars[…##gnum]
   ├─ variable-substitution                 1223  set vars[!bang##gnum]
   ├─ type-unify                 transforms.xtm:1855   identity = exact symbol (1872)
   │   ├─ reify-generic-type          transforms.xtm:1481  (skips bangs, 1494)
   │   │   └─ reify-generic-type-expand transforms.xtm:1174  match bangs by ##gnum string
   │   ├─ type-unify-closure          transforms.xtm:1816  (computes then discards, 1837)
   │   └─ check-to-update-generic-vars transforms.xtm:1792
   ├─ nativef-generics-check-args     typecheck.xtm:891
   │   └─ generic-types-matchup?      typecheck.xtm:846
   │       └─ descending-generic-type-match  823   !x matches ANY !y
   └─ nativef-generics-check-return-type typecheck.xtm:1095

WRITE-BACK / reconciliation (string/gnum-driven, not a map)
update-var / force-var (typecheck.xtm:130 / 187)
└─ reverse-set-bangs-from-reified   typecheck.xtm:1011   set vars[!b##gnum] (1037)
unify whole-table pass (transforms.xtm:2071)
└─ sym-unify                  transforms.xtm:2028   match by gnum equality (2043)
    └─ check-bang-against-reified   transforms.xtm:2003   identity = regex !g…##num

Substitution store (the only one):
vars hashtable  symbol → union-find cell   (typecheck.xtm:1-45, API 72-304)
```

`polytype-match?` (`llvmti-caches.xtm:1722-1738`) is column-wise non-empty
intersection used for overload selection, adjacent to but not part of the
freshening cluster.

## 3. Special-case hacks, duplicates, and dead code (AC#3)

### 3.1 Duplicate top-level definitions

A structural pass found **8 duplicate `define` names** and zero duplicate
`bind-*`/macro names. Scheme uses last-definition-wins, so in every case the
**second** copy is live and the **first** is dead --- and in every case the
bodies differ, so the dead copy is actively misleading.

| name                           | dead / live                   | what differs                                                                            | risk |
| ------------------------------ | ----------------------------- | --------------------------------------------------------------------------------------- | ---- |
| `impc:ti:memzone`              | `transforms.xtm:586` / `616`  | live copy walks the zone cleanup-hook linked list before destroy; dead copy drops hooks | high |
| `impc:ir:compiler:closure-ref` | `llvmir.xtm:1825` / `1927`    | live adds `hint?` arg, deref, runtime type-check + phi                                  | high |
| `impc:ir:compiler:closure-set` | `llvmir.xtm:1873` / `2016`    | live adds deref, `check_address_type`, null handling                                    | high |
| `impc:ti:fptrcall-check`       | `typecheck.xtm:3141` / `3177` | live guards null fptr                                                                   | med  |
| `impc:ti:allocate-var?`        | `typecheck.xtm:4108` / `4117` | **different return type**: bool vs int 0-3 classifier                                   | high |
| `impc:ti:cond`                 | `transforms.xtm:326` / `335`  | live adds `else`, empty-clause errors                                                   | med  |
| `impc:ti:binary-arity`         | `transforms.xtm:482` / `492`  | live is left-assoc + arity check                                                        | med  |
| `impc:ir:compiler:loop`        | `llvmir.xtm:2382` / `2435`    | live adds start-value + type checks                                                     | med  |

`allocate-var?` (different _contract_ between the two copies) and `memzone`
(silent loss of cleanup hooks if the dead copy were revived) are the dangerous
ones.

### 3.2 Dead / commented-out code

`llvmti-typecheck.xtm` carries an entire abandoned generation of the
generics/poly engine inline next to the live versions --- the single biggest
readability hazard in the generic path:

- `366-452` commented `symbol-check` ("IS NEW"), `1610-1790` dead
  `nativef-generics` + helpers, `1909-1948` dead
  `nativef-poly-check-valid-args`, `2019-2122` dead `nativef-poly-check`,
  `3311-3332` dead `closure-in-first-position`, plus smaller fragments.

Elsewhere: `llvmti-caches.xtm:688-790` is a ~103-line commented-out table of
`llvm.*` intrinsic entries; `transforms.xtm` and `llvmir.xtm` carry assorted
smaller commented IR-emission alternatives (full ranges in the appendix).

### 3.3 Magic numbers and hard-coded type layouts

- **`213` / `108` as bare literals** for closure / `i8*` return markers
  throughout the native-function cache (e.g. `caches.xtm:812`), while the
  typechecker builds the same value symbolically as
  `(+ *impc:ir:closure* (* 2 *impc:ir:pointer*))`
  (`typecheck.xtm:432, 475, 522`). The same concept is spelled two incompatible
  ways; `213` breaks silently if any of the three constants change.
- **Hard-coded `mzone`/`clsvar` struct layouts** with magic field codes at
  `caches.xtm:142-143`; the `clsvar` doc string literally contains
  `TODO_whatisthis` for an unknown field.
- **Hard-coded zone hook-list field index `4`** (`tref ... 4`) duplicated across
  the `mzone` layout and `memzone`/`letz` (`transforms.xtm:623, 649, 707, 711`).
- **`"%mzone*"` string literal** threaded through typecheck, IR emission, and
  bind (`typecheck.xtm:2190+`, `llvmir.xtm:1265+`, `bind.xtm:402+`).
- **Integer-literal width guess by magnitude** (`typecheck.xtm:357-363`):
  candidate types chosen by whether the value is 0/1 or `< 256` --- a
  value-based heuristic with an arbitrary threshold, not a type rule.
- **Void-return detection by substring** (`transforms.xtm:551-555`):
  `(substring ts 0 5)` = `"[void"` then a char peek at index 5.

### 3.4 Author-flagged uncertainty

- `llvmir.xtm:4127` --- `;; this is (possibly?) broken for poly funcs` (directly
  in scope).
- `transforms.xtm:194` ---
  `;; this currently doesn't work for multiple "replace" instances`.
- `caches.xtm:143` --- `TODO_whatisthis` (above).

## 4. #315 root cause and related latent bugs (AC#4)

### 4.1 Correction to the recorded diagnosis

The task-053 / issue diagnosis states that the `!a` inside `Test315{!a}` and the
bare `!a` parameter "get separate freshened identities ... and are never
unified". **This is not what happens.** Both occurrences live in the _function_
signature `[void,Test315{!a}*,i64,!a]*`, and `register-new-genericfunc`
de-duplicates then replaces every occurrence (`caches.xtm:1286, 1302`), so both
freshen to the _same_ name `!gxa_34`. Confirmed by experiment: instrumenting the
reverse-binding on the live repro prints

```
RSBFR poly: Test315{!gxa_34}*##7  namedtype: (114 102 2)  gpolytype: (114 !gxa_34 2)
```

--- one shared name, `!gxa_34`, not two. The `!ga_29` name that also appears
belongs to the _type definition_ `Test315 <!a*,i64>` (minted by
`register-new-generictype`), a separate registration reconciled through
reification; it is not the divergence the diagnosis describes. Were the bare
parameter genuinely a _different_ variable, binding the tuple-field variable
would not affect it, and the parameter would stay `i64` --- the opposite of the
observed symptom.

### 4.2 The actual mechanism

Type variables have **no pointer-level representation**. A variable is a bare
symbol; the `+100`-per-level convention has no base number to attach to. So:

1. **The pointer suffix is dropped when a typevar field is parsed.** In
   `impc:ir:get-type-from-pretty-str-rec` (`llvmir.xtm:363-591`), the function
   computes `ptr-depth` (`369`) but the bang-type branch discards it:

   ```scheme
   591  ((impc:ti:bang-type? base) (string->symbol base))
   ```

   So `!gxa_34*` and `!gxa_34` both return the bare symbol `!gxa_34`. Confirmed:
   `(impc:ir:get-type-from-pretty-tuple "<!gxa_33*,i64>")` → `(!gxa_33 2)` ---
   identical to the no-star version.

2. **The reverse-binding then keeps the pointer level on the concrete side.** In
   `impc:ti:reverse-set-bangs-from-reified` (`llvmti-typecheck.xtm:1011-1043`),
   `namedtype` is the concrete struct `(114 102 2)` (field 0 = `102` = `i64*`)
   and `gpolytype` is the generic struct `(114 !gxa_34 2)` (field 0 = bare
   symbol, `*` already lost). The zip pairs them and, because the generic field
   is a `!`-symbol, executes:

   ```scheme
   1036  (set! vars (impc:ti:vars-update
   1037   (string->symbol (string-append (symbol->string b) "##" (number->string gnum)))
   1038   vars '() a))     ;; a = 102 = i64*
   ```

   binding `!gxa_34 := i64*` instead of `i64`. The field's declared pointer
   depth should have been stripped from the concrete type before binding; it is
   not.

3. **The value parameter inherits it.** `val:!a` correctly shares `!gxa_34`, so
   it resolves to `i64*`. The decoded final specialisation is
   `[void,Test315{i64}*,i64,i64*]*` --- the last argument is `i64*`, not `i64`.

4. **`pset!` rejects it.** `impc:ti:pointer-set-check` (`typecheck.xtm:2560`)
   checks the value against the element type `(impc:ir:pointer-- (car a))`; with
   `val = i64*` ≠ `i64` it raises `got i64*, was expecting i64`. Reproduced
   first-hand on `build/extempore`:
   ```
   Type Error with pointer-set!, got i64*, was expecting i64
   ```

The flaw is generic-only because the concrete analog contains no type variables,
so `reverse-set-bangs-from-reified` never runs (verified:
`bind-type CTest <i64*,i64>` + concrete `put` compiles). The invariant violated:
a generic field `!a` at pointer depth _n_ corresponds to a concrete field
`T*^n`, so the correct binding is `!a := pointer--^n (concrete field)`. The code
strips depth from the generic side (losing it) but keeps it on the concrete
side.

### 4.3 Class of related latent bugs

The same flaw --- a bare type-variable tuple field at pointer depth ≥ 1
absorbing the field's pointer level --- generalises. Status below is from
running each sketch on `build/extempore` unless marked.

| #   | shape                                     | sketch                                     | observed                                                                    |
| --- | ----------------------------------------- | ------------------------------------------ | --------------------------------------------------------------------------- |
| A   | typevar field at depth ≥ 2                | `<!a**,i64>` + `setDeep ... !a`            | `got i64**, was expecting i64` (absorbs all levels) --- confirmed           |
| B   | corrupted typevar as bare **return** type | `<!a*,i64>`, `getRet:[!a,TRet{!a}*]*`      | `LLVM IR: invalid cast i64→ptr` --- confirmed; not confined to `pset!`      |
| C   | multiple typevars, mixed depths           | `<!a*,!b>` + put on each                   | depth-1 `!a` fails, depth-0 `!b` works --- confirmed per-variable           |
| D   | nested generic tuples                     | `Inner <!a,i64>`, `Outer <Inner{!a}*,i64>` | **silent** failure to specialise (no clean error) --- confirmed; worst mode |
| E   | array of pointer elements                 | `\|4,!a*\|` with bare `!a`                 | reasoned-only; depth-0 array element `\|4,!a\|` confirmed _safe_            |
| F   | generic closure in generic tuple          | `<[!a,!a]*,i64>`                           | confirmed **not** a bug (typevars sit at depth 0 inside the closure)        |

The boundary is sharp: **only a type variable that is itself a direct tuple
field carrying a pointer suffix (`!a*`, `!a**`) is affected.\*\* Type variables
nested inside a structured field (closure, array with non-pointer element, or a
depth-0 tuple field) are matched structurally and are correct. Case D (silent
miscompilation of nested generic tuples) is the most dangerous, since it
produces no diagnostic.

### 4.4 Empirical confirmation: the fix is non-local

A localised binding-site fix was implemented and tested against the live build,
then reverted (tree left clean). It does **not** work --- it relocates the error
rather than removing it, which is exactly what pins down why #315 needs the
representation change.

The patch made `reverse-set-bangs-from-reified` recover each field's declared
pointer depth (from the maximised generic tuple string, where the stars still
survive) and strip it from the concrete type before binding, so `!a` binds to
`i64` rather than `i64*`. On the canonical repro the `pset!` error disappears,
but a new one surfaces in the auto-generated tuple constructor:
`Type Error conflicting i64* with i64 in (tuple-set! obj 0 arg_0)`. The reason
is that `vars[!a]` is **overloaded across pointer depths**: the tuple field
`!a*` needs it to mean `i64*`, while the bare parameter `!a` needs it to mean
`i64`. A single binding cannot satisfy both unless each _use_ re-applies its own
declared depth --- and uses can't, because the typevar symbol carries no depth
(dropped at `llvmir.xtm:591`). The old code stored the field-typed value
(`i64*`): right for the field, wrong for the bare use (#315). Stripping makes it
right for the bare use and wrong for the field. The bug is conserved, not
removed.

Crucially the defect is **not tuple-specific**. The minimal reproduction has no
tuple at all:

```
(bind-func depthtest:[void,!a*,!a]* (lambda (p v) (pset! p 0 v)))
($ (let ((p:i64* (halloc))) (depthtest p 5) (println (pref p 0))))
;; -> Type Error: bad type i64, Cannot de-reference non-pointer type
```

A type variable used at two pointer depths in one signature is miscompiled
regardless of tuples; #315 is one instance. This case is a generic _function_
(specialised through `nativef-generics`, not the `reverse-set-bangs` tuple
path), so the defect spans both specialisation paths and lives in the shared
type-variable representation. libs-core stayed green (8/8) under the reverted
patch, confirming the binding change is non-destructive but insufficient. The
consequence: the smallest correct fix for #315 is the
type-variable-carries-pointer-depth slice of the canonical representation,
coordinated across parsing (`llvmir.xtm:591`) and resolution (`type-unify`).
There is no behaviour-preserving binding-site patch.

## 5. Phased redesign proposal (AC#5)

The generic path is central --- the whole stdlib compiles through it --- and
loads live from disk, so the redesign must be incremental and
behaviour-preserving, with a regression net in place before any change. The
guiding shape is strangler-fig: introduce a canonical core behind the existing
accessor API, migrate callers, then delete the old text-driven machinery.

### Phase 0 --- safety net and low-risk cleanup

Build the characterisation harness ([§5.1](#51-characterisation-test-strategy))
_before_ touching logic, and delete the dead weight that obscures the code: the
8 shadowed duplicate definitions ([§3.1](#31-duplicate-top-level-definitions),
each verified dead first) and the large commented-out blocks
([§3.2](#32-dead--commented-out-code)). Replace bare `213`/`108`/field-index `4`
with the symbolic constants and a single named zone-field accessor. None of this
changes behaviour; all of it shrinks the surface for later phases.

_Risk: low. Effort: medium._

### Phase 1 --- type-variable pointer depth (the #315 fix)

The original hope --- a localised binding patch in
`reverse-set-bangs-from-reified` --- was implemented, tested, and reverted: it
relocates the error rather than fixing it
([§4.4](#44-empirical-confirmation-the-fix-is-non-local)). The smallest
_correct_ fix is the type-variable-carries-pointer-depth slice of the Phase 2
representation, taken first: make a typevar retain its declared pointer depth at
parse time (`llvmir.xtm:591`, which currently returns the bare symbol) and have
resolution (`type-unify`, ~line 1872) strip and re-apply it, so `!a*` and `!a`
are distinguishable and `vars[!a]` always holds the base type. Add the
[§4.3](#43-class-of-related-latent-bugs) matrix plus the no-tuple `depthtest`
case ([§4.4](#44-empirical-confirmation-the-fix-is-non-local)) as regression
tests. Closes the rehomed task-053 AC#4.

_Risk: high --- it touches the shared typevar representation and the
string/`##gnum` freshening machinery (§1.3, §2.2), so it must land behind the
Phase 0 net and be validated against libs-core + libs-external + a broad stdlib
load. Effort: medium. This is not a "localised" patch; it is the first, smallest
cut of the representation redesign._

### Phase 2 --- canonical type representation

Introduce one structured type datatype to replace the int-code/list/string trio
internally: an explicit record of `{kind, pointer-depth, components}`, with
**type variables as a first-class node carrying name + pointer depth** --- which
makes the #315 class unrepresentable rather than merely fixed. Keep the existing
accessors (`get-type-str`, `get-type-from-pretty-str`, `pretty-print-type`, the
predicates) as the public API, re-implemented over the canonical form, so
callers do not change yet. The int-code form is derived only at the
LLVM-IR-string boundary. Pretty-string-in and IR-string-out remain the only
genuine string boundaries.

_Risk: high (central), mitigated by stable external API + the net. Effort:
large._

### Phase 3 --- a real unifier with an explicit substitution map

Replace the scattered identity heuristics
([§2.1](#21-there-is-no-substitution-map)) with one Hindley--Milner-style
unifier over the canonical representation: a single substitution map (formalise
the existing `vars` union-find), fresh variables minted by **one** function with
**one** naming scheme (kill the `!gx`/`!g` split and the `##gnum` string
suffixing), a proper occurs-check, and pointer depth carried as data. Retire
`sym-unify`, `check-bang-against-reified`, `descending-generic-type-match`'s
wildcard, `reify-generic-type-expand`'s string substitution, and the
`_poly_`/`_adhoc_` name surgery where the structured form makes them redundant.
This subsumes the consistency problems flagged in
[§2.2](#22-two-stage-freshening-with-an-inconsistent-prefix).

_Risk: high. Effort: large._

### Phase 4 --- decommission the string/regex type algebra

With representation and unifier canonical, delete the two PCRE grammars and the
text-munging helpers that parsed types as strings, plus the backward-compat
round-trips (`get-named-type` list→string→list, the double re-parse at
`typecheck.xtm:946-948`). The five representations collapse to: pretty-in,
canonical-internal, IR-out.

_Risk: medium (mostly deletion, gated by the net). Effort: medium._

| phase | deliverable                     | risk   | effort        |
| ----- | ------------------------------- | ------ | ------------- |
| 0     | net + cleanup                   | low    | medium        |
| 1     | #315 + class fixed              | medium | small--medium |
| 2     | canonical representation        | high   | large         |
| 3     | real unifier + substitution map | high   | large         |
| 4     | remove string algebra           | medium | medium        |

### 5.1 Characterisation test strategy

The net must pin current behaviour broadly enough that a representation swap is
verifiable:

- **Corpus golden-master.** Compile the generic-heavy stdlib (`adt`, `std`, and
  a broad `sys:load` sweep) and snapshot, per construct: success/failure, exact
  error text, and the final specialised signature (decoded via the
  `cname-decode` trick used in [§4.2](#42-the-actual-mechanism)). Diff against
  baseline after every change.
- **Generic matrix.** Turn the [§4.3](#43-class-of-related-latent-bugs) taxonomy
  into positive/negative cases: typevar at depth 0/1/2, multiple vars of mixed
  depth, nested generic tuples, closures-in-tuples, arrays of scalar vs pointer
  elements.
- **Round-trip properties.** For a set of types, assert
  `pretty → canonical → pretty` and `canonical → IR → canonical` are identities.
- **Regression locks.** #315 and bugs A--D enter as xfail and must pass from
  Phase 1; bugs E--F lock in the established boundary so a future change cannot
  silently widen it.

Run via the existing harness (`ctest --label-regex "libs-core|libs-external"`)
plus a dedicated generics test file. Pristine output (zero failures, errors,
warnings, backtraces) is the gate for every commit.

### 5.2 Recommended follow-up tasks

The audit recommends, in order: the Phase-0 cleanup/net, then Phase 1
(type-variable pointer depth = the #315 fix, filed as task-055), then the
remaining Phase 2--4 representation/unifier work. task-055 was originally scoped
to the binding patch; after the
[§4.4](#44-empirical-confirmation-the-fix-is-non-local) finding it is re-scoped
to the Phase-1 representation slice above. The Phase-0 net is a prerequisite for
Phase 1, not optional.

## 6. Confirmation: no production code changed (AC#6)

The audit proper (AC#1--5) was read-only: source reading, structural/text
search, and read-only experiments against the prebuilt `build/extempore`
(REPL-only redefinitions that touched no file, plus throwaway repros under
`/tmp/`). The [§4.4](#44-empirical-confirmation-the-fix-is-non-local) experiment
went one step further: it edited `runtime/llvmti-typecheck.xtm`, ran the suite,
then reverted the file (`git restore`), so the working tree is unchanged --- the
original #315 error reproduces exactly as before. The only persisted artefacts
are this document and the backlog tasks.

## Appendix: file/line index

- Type codes: `llvmti-globals.xtm:104-142`. Pointer ±: `llvmir.xtm:994-1027`,
  depth `156-166`.
- Conversions: `get-type-from-pretty-str(-rec)` `llvmir.xtm:356-705`;
  `get-type-from-str` `771-822`; `get-type-str` `825-864`; `pretty-print-type`
  `297-332`; `str-list-check` `878-901`; PCRE grammars `175-180`, `721-728`.
- C++ boundary: `src/ffi/llvm.inc:667-694` (registered `865`).
- Freshening: `register-new-genericfunc` `llvmti-caches.xtm:1260-1348` (prefix
  `1292`, dedup `1286`, replace `1302`); `register-new-generictype` `1769-1792`
  (prefix `1782`); counter `1767`.
- Unification: `type-unify` `transforms.xtm:1855-1977`; `type-unify-closure`
  `1816-1838`; `reify-generic-type` `1481-1593`; `reify-generic-type-expand`
  `1174-1196`; `sym-unify` `2028-2061`; `check-bang-against-reified`
  `2003-2024`; `generic-types-matchup?` `typecheck.xtm:846-887`;
  `descending-generic-type-match` `823-841`; `vars` core `1-45`, API `72-304`.
- #315: drop site `llvmir.xtm:591`; reverse-binding `typecheck.xtm:1011-1043`
  (binding `1036-1038`), call sites `171-174`/`205-208`; error
  `pointer-set-check` `typecheck.xtm:2560-2597`.
- Duplicates: `transforms.xtm:586/616`, `llvmir.xtm:1825/1927`,
  `llvmir.xtm:1873/2016`, `typecheck.xtm:3141/3177`, `typecheck.xtm:4108/4117`,
  `transforms.xtm:326/335`, `transforms.xtm:482/492`, `llvmir.xtm:2382/2435`.
- Dead blocks: `typecheck.xtm:366-452, 1610-1790, 1909-2122, 3311-3332`;
  `caches.xtm:688-790`.
