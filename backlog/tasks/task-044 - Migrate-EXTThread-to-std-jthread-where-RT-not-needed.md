---
id: TASK-044
title: 'Migrate EXTThread to std::jthread where RT not needed'
status: To Do
assignee: []
created_date: '2026-04-19 02:05'
updated_date: '2026-04-19 03:40'
labels:
  - cpp
  - refactor
dependencies: []
priority: medium
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
EXTThread was kept in #419 because it provides platform-specific
realtime scheduling (SCHED_RR on Linux, THREAD_TIME_CONSTRAINT_POLICY on
macOS) and a subsume-current-thread mode that std::thread doesn't
natively offer.

Most call sites don't need the RT features, so they can use std::jthread
directly. Audit and migrate where safe; keep a small rt_thread helper
just for the audio callback threads.

Follow-up from #419.

## Progress

Migrated in the #419 follow-up PR:

- [x] src/Extempore.cpp — the primary delayed-connect thread and the
      linenoise REPL thread, both fire-and-forget, now use
      std::thread + detach.

Must keep EXTThread (verified):

- AudioDevice::m_threads[] — RT priority (SCHED_RR / THREAD_TIME_CONSTRAINT_POLICY)
- SchemeProcess::m_threadTask — uses setSubsume() + setPriority()
- SchemeProcess::m_threadServer — uses setPriority()
- EXTLLVM::thread_fork / thread_join / thread_kill — xtlang FFI exposes
  EXTThread* as opaque void*; migration breaks ABI.

Remaining candidates (safe by inspection, not yet touched):

- [ ] TaskScheduler::m_queueThread — singleton, never joined, no RT.
      Could be std::jthread. Defer until there's a clear destruction
      story for the singleton.
- [ ] OSC::threadOSC — needs reworking of the `getThread().start()` /
      `getThread().start(fn, arg)` API shape to match std::thread's
      construct-with-callable idiom; touches OSC::startSender /
      startReceiver call sites.
<!-- SECTION:DESCRIPTION:END -->
