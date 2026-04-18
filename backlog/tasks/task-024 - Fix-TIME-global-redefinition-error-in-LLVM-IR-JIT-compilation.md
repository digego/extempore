---
id: TASK-024
title: Fix @TIME global redefinition error in LLVM IR JIT compilation
status: To Do
assignee: []
created_date: '2026-02-17 04:05'
labels:
  - bug
  - llvm
  - jit
dependencies: []
priority: high
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
## Summary

The xtm_play_adhoc function (and all functions compiled after it) fails to JIT compile with 'redefinition of global @TIME'. This causes all audio examples (fmsynth.xtm, scheduler.xtm, topclock_metro.xtm, etc.) to fail.

## Root cause

In src/SchemeFFI.cpp, jitCompile() works as follows:
1. Clones a template module (from bitcode.ll) which already defines globals like @TIME
2. Prepends sTypeDefinitions (a string accumulating declarations from all prior compilations) to the user IR
3. Calls parseAssemblyInto() to merge the combined IR into the cloned template module

The problem: sTypeDefinitions accumulates '@TIME = external global i64' from a prior compilation (around module ~2694). When this gets prepended to user IR and parsed into the template module clone (which already has @TIME defined from bitcode.ll), LLVM rejects it as a redefinition.

## Key details

- The error is 'LLVM IR: <user>:5599:1: error: redefinition of global @TIME'
- The conflict is between sTypeDefinitions containing '@TIME = external global i64' and the template module clone already having @TIME defined
- The user IR itself does NOT contain @TIME --- the conflict is sTypeDefinitions vs the base module
- stderr is redirected to /dev/null at startup (src/Extempore.cpp:174), which hid all C++ error messages
- The FLUSH FAILED message comes from runtime/llvmir.xtm:63 when llvm:jit-compile-ir-string returns #f
- sTypeDefinitions is ~398KB by the time the error occurs (module 2694 of ~2695)

## Fix approach

The fix should be in jitCompile() in src/SchemeFFI.cpp. When building fullIR = sTypeDefinitions + irString, we need to strip any external declarations from sTypeDefinitions for globals that already exist in the cloned template module. The filtering code started (collecting names from irString) was wrong --- it needs to collect names from the template module instead.

Specifically, after cloning the template module (step 2, ~line 389), collect all global names from the clone, then when building fullIR (step 3, ~line 417), filter sTypeDefinitions to remove external declarations for any globals already in the clone.

## Files involved

- src/SchemeFFI.cpp - jitCompile() function (~line 366), sTypeDefinitions accumulation (~line 546-668)
- src/Extempore.cpp:174 - stderr redirect to /dev/null (makes debugging hard)
- runtime/llvmir.xtm:53-66 - flush-jit-compilation-queue (Scheme layer that reports the error)

## Verification

- Core tests (libs-core) all pass: system, adt, math, std, xtlang, generics
- The audio examples fail: fmsynth, scheduler, topclock_metro, and all external audio examples
- To reproduce: ./build/extempore --noaudio --batch '(sys:load-then-quit "examples/core/fmsynth.xtm" 10)'
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] #1 sTypeDefinitions does not contain external declarations for globals already defined in the template module
- [ ] #2 fmsynth.xtm loads successfully with --noaudio --batch mode
- [ ] #3 All 6 core tests (libs-core) continue to pass
- [ ] #4 Audio examples (scheduler.xtm, topclock_metro.xtm) load without FLUSH FAILED error
<!-- AC:END -->
