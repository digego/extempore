---
id: TASK-044
title: 'Migrate EXTThread to std::jthread where RT not needed'
status: To Do
assignee: []
created_date: '2026-04-19 02:05'
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
<!-- SECTION:DESCRIPTION:END -->
