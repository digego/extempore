---
id: TASK-057
title: 'Codegen: void-returning function with value-yielding tail emits invalid IR'
status: To Do
assignee: []
created_date: '2026-06-08 08:57'
labels:
  - xtlang
  - compiler
  - codegen
dependencies: []
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
A bind-func whose return type is void but whose body's tail expression yields a value (e.g. (pset! p 0 v), which returns the set value) generates invalid LLVM IR: 'bitcast <val> to void' instead of discarding the value and emitting 'ret void'. Repro (non-generic, so unrelated to #315): (bind-func f:[void,i64*,i64]* (lambda (p v) (pset! p 0 v))) -> 'LLVM IR error: void type only allowed for function results'. Discovered while fixing #315 (task-055): the generic characterisation repros in tests/core/generics.xtm (csn_notuple, csn315_use, csndeep_use, csntwo_use) all use this void+value-tail pattern, so they cannot become clean passing repros until this is fixed. The fix is in the closure/return codegen: when the declared return type is void, discard the body value rather than coercing it.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] #1 void-returning function with a value-yielding tail compiles and returns void (ret void, no bitcast)
- [ ] #2 the four void-pattern #315 repros in generics.xtm can be unlocked to passing once combined with the type-path fix
<!-- AC:END -->
