---
id: TASK-030
title: Investigate callback "bad arity" error in interactive sessions
status: Done
assignee: []
created_date: '2026-02-23 09:57'
updated_date: '2026-04-21 09:49'
labels:
  - bug
  - compiler
dependencies: []
references:
  - 'runtime/llvmti.xtm:7583-7616'
  - 'runtime/llvmti.xtm:4034-4044'
  - 'runtime/scheme.xtm:251'
  - tests/failing.xtm
priority: medium
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
Toby Gifford reported (2026-02-23) that xtlang temporal recursion via `callback` produces a "bad arity" compiler error when the `bind-func` has an explicit type annotation:

```xtlang
(bind-func test_loop:[i64,i64]*
  (lambda (t:i64)
    (println t)
    (callback (+ t 44100) test_loop (+ t 44100))
    t))
```

Error: `Compiler Error  bad arity in call ast: (llvm_callback (+ t 44100) test_loop_adhoc_13 znew (zcopy##18 (+ t 44100) zold znew))`

**Without** the `:[i64,i64]*` type annotation, the same code compiles fine.

## Root cause (confirmed via debug logging)

In `impc:ti:callback-check` (`runtime/llvmti.xtm:7583`):

1. `impc:ti:get-closure-arg-types` returns **NIL** for the adhoc-specialised function name (e.g. `test_loop_adhoc_8`) during self-referential compilation --- the adhoc name hasn't been registered yet
2. When `ftypeA` is NIL, `ftype` falls through to `cbType` (from `impc:ti:type-check`)
3. When a type annotation is present, `cbType` returns a **nested list** `((213 2 2))` (closure type wrapped in a list), so `ftype = ((213 2 2))` with length 1
4. The arity check at line 7603 computes `(+ 2 1) = 3` which doesn't match `(length ast) = 5`
5. When there's NO type annotation, `cbType` is also NIL, so `ftype` is NIL, and the code takes the `(null? ftype)` branch at line 7592 which **skips the arity check entirely**

The fix should either:
- Unwrap the nested list from `cbType` before using it in the arity check
- Or ensure `impc:ti:get-closure-arg-types` can resolve adhoc names during self-referential compilation

A failing test exists in `tests/failing.xtm` (`callback_single_arg_recursion`).
<!-- SECTION:DESCRIPTION:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
Fixed: direct reproduction from the task description now compiles cleanly. Verified both cases:
- (bind-func test_loop:[i64,i64]* (lambda (t:i64) ... (callback (+ t 44100) test_loop (+ t 44100)) t)) → 'Compiled: test_loop >>> [i64,i64]*'
- (bind-func twoarg_annotated:[i64,i64,i64]* (lambda (a:i64 b:i64) ... (callback (+ a 44100) twoarg_annotated (+ a 44100) b) a)) → 'Compiled: twoarg_annotated >>> [i64,i64,i64]*'
Likely addressed by the type-inference rewrites in TASK-037 / TASK-032-036. tests/failing.xtm does not contain callback_single_arg_recursion.
<!-- SECTION:NOTES:END -->
