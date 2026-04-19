---
id: TASK-039
title: 'RAII conversion: audio/scheme thread ownership'
status: To Do
assignee: []
created_date: '2026-04-19 02:04'
updated_date: '2026-04-19 03:40'
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

Done in the #419 follow-up PR:

- [x] AudioDevice::m_threads[MAX_RT_AUDIO_THREADS] → std::array<std::unique_ptr<EXTThread>, N>
- [x] Per-thread thread_local PRNGs (EXTLLVM.cpp:392,397) → values not pointers

Still to do:

- [ ] SchemeProcess/SchemeREPL registries → Meyers singletons.
      SchemeREPL::sm_repls is a static std::unordered_map<std::string, SchemeREPL*>
      and SchemeProcess::sm_current is a thread_local SchemeProcess*.
      Straightforward refactor but needs audit of lifetime expectations.
- [ ] SchemeTask::m_ptr → std::variant<std::string*, SchemeObj*, pointer>.
      Hot path in task dispatch, touched by every case branch in
      SchemeProcess::taskImpl; migration touches many call sites and the
      variant alternative per case differs.

See docs/superpowers/plans/2026-04-19-cpp-modernisation.md §5 for context.
<!-- SECTION:DESCRIPTION:END -->
