---
id: DRAFT-001
title: Switch REPL from linenoise to isocline for cross-platform support
status: Draft
assignee: []
created_date: '2026-02-23 10:35'
labels:
  - repl
  - cross-platform
dependencies: []
priority: low
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
The current interactive REPL (--repl flag) uses linenoise, which is POSIX only (guarded by #ifndef _WIN32). isocline (https://github.com/daanx/isocline) is a pure C library (~8k lines, MIT licence, 2 files to vendor) that would give us Windows support plus several features linenoise lacks: full UTF-8, syntax highlighting, 24-bit colour, bracketed paste, and brace matching. This would enable syntax highlighting for Scheme/xtlang in the REPL.

isocline was chosen over replxx (https://github.com/AmokHuginnsson/replxx) because replxx is unmaintained (maintainer deceased Dec 2021), has a ConvertUTF licensing issue blocking distro packaging, and requires ~25 files to vendor vs isocline's 2.

The isocline API differs from linenoise, so src/LinenoiseREPL.cpp would need rewriting rather than a drop-in swap. The vendoring and CMake integration would be similarly simple (replace src/linenoise/ with src/isocline/, same static library pattern).

Low priority --- the current linenoise REPL works well on Linux/macOS.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] #1 Replace src/linenoise/ vendor directory with isocline.c, isocline.h, and LICENSE from daanx/isocline
- [ ] #2 Rewrite src/LinenoiseREPL.cpp to use the isocline API (ic_readline, ic_set_history, ic_set_default_completer etc.)
- [ ] #3 Update CMakeLists.txt to build isocline instead of linenoise, removing the if(UNIX) gate so it builds on all platforms
- [ ] #4 Remove #ifndef _WIN32 guards from LinenoiseREPL.cpp and the --repl flag handling in Extempore.cpp
- [ ] #5 REPL works on Linux, macOS, and Windows
- [ ] #6 Syntax highlighting callback implemented for Scheme/xtlang (parentheses, strings, comments, keywords)
<!-- AC:END -->
