---
id: TASK-057
title:
  "Codegen: void-returning function with value-yielding tail emits invalid IR"
status: Done
assignee:
  - "@ben"
created_date: "2026-06-08 08:57"
updated_date: "2026-06-08 11:20"
labels:
  - xtlang
  - compiler
  - codegen
dependencies: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

A bind-func whose return type is void but whose body's tail expression yields a
value (e.g. (pset! p 0 v), which returns the set value) generates invalid LLVM
IR: 'bitcast <val> to void' instead of discarding the value and emitting 'ret
void'. Repro (non-generic, so unrelated to #315): (bind-func f:[void,i64*,i64]\*
(lambda (p v) (pset! p 0 v))) -> 'LLVM IR error: void type only allowed for
function results'. Discovered while fixing #315 (task-055): the generic
characterisation repros in tests/core/generics.xtm (csn_notuple, csn315_use,
csndeep_use, csntwo_use) all use this void+value-tail pattern, so they cannot
become clean passing repros until this is fixed. The fix is in the
closure/return codegen: when the declared return type is void, discard the body
value rather than coercing it.

<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria

<!-- AC:BEGIN -->

- [x] #1 void-returning function with a value-yielding tail compiles and returns
      void (ret void, no bitcast)
- [x] #2 the four void-pattern #315 repros in generics.xtm can be unlocked to
    passing once combined with the type-path fix
<!-- AC:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->

Root cause: impc:ir:compiler:ret (runtime/llvmir.xtm) chose 'ret void' only when
the body tail's VALUE type was void. A void-declared function whose tail yields
a value (pset! returns the stored value) fell through to the coercion path and
emitted 'bitcast <val> to void' -- invalid IR ('void type only allowed for
function results').

Fix: when the declared return type (hinttype?) is void, emit 'ret void' and
discard the tail value (its side-effecting IR is already emitted). The new
condition is guarded by the existing outer-let marker (cadr ast)=(caddr ast):
the closure \_maker reuses this compiler to return the closure POINTER, where
hinttype? is the closure's (void) return type but the value is the pointer --
there we must keep returning the pointer, not ret void.

AC#1: verified -- f057:[void,i64*,i64]\* (lambda (p v) (pset! p 0 v)) now
compiles; raw IR shows 'ret void' with no bitcast; round-trip returns the stored
value. Non-generic characterisation added to tests/core/xtlang.xtm. AC#2: the
four void+value-tail #315 repros (csn315/csn_notuple/csndeep/csntwo) are
restored to generics.xtm as void-returning companions alongside the
value-returning forms from task-058; they need both fixes (type path + this
codegen fix) and all pass.

Verified: full ctest 54/54 green; clean_aot + aot_external_audio rebuild green.

<!-- SECTION:NOTES:END -->
