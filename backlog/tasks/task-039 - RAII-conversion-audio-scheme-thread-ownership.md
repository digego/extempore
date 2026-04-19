---
id: TASK-039
title: 'RAII conversion: audio/scheme thread ownership'
status: To Do
assignee: []
created_date: '2026-04-19 02:04'
labels:
  - cpp
  - refactor
dependencies: []
priority: medium
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
Convert remaining owning raw pointers to unique_ptr/value types. Follow-up
from the 2026-04-19 C++ modernisation PR (#419):

- AudioDevice::m_threads[MAX_RT_AUDIO_THREADS] → std::array<std::unique_ptr<EXTThread>, N>
- SchemeProcess/SchemeREPL singletons → Meyers singletons
- SchemeTask::m_ptr → std::variant<std::string*, SchemeObj*, pointer>
- Per-thread thread_local PRNGs (EXTLLVM.cpp:392,397) → values not pointers

See docs/superpowers/plans/2026-04-19-cpp-modernisation.md §5 for context.
<!-- SECTION:DESCRIPTION:END -->
