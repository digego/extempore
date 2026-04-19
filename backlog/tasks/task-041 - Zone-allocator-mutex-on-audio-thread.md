---
id: TASK-041
title: Zone allocator mutex on audio thread
status: To Do
assignee: []
created_date: '2026-04-19 02:05'
labels:
  - cpp
  - rt-audio
dependencies: []
priority: high
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
src/EXTZones.cpp:69-73 takes a global std::recursive_mutex on every
allocation in the audio-callback path. This is an RT-audio anti-pattern
(priority inversion). Investigate lock-free zone design or per-thread
zones.

Follow-up from #419.
<!-- SECTION:DESCRIPTION:END -->
