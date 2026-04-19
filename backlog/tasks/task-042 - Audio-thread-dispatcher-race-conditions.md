---
id: TASK-042
title: Audio thread dispatcher race conditions
status: To Do
assignee: []
created_date: '2026-04-19 02:05'
labels:
  - cpp
  - rt-audio
  - concurrency
dependencies: []
priority: high
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
src/AudioDevice.cpp:169-170, 391 — multi-thread dispatch uses
spin-sleep on atomics and an unsynchronised m_numThreads read
(source comment at line 391 reads 'this is a race :('). Replace with
std::counting_semaphore or condition variable. TSan run required.

Follow-up from #419.
<!-- SECTION:DESCRIPTION:END -->
