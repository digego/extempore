---
id: TASK-038
title: Improve new-user onboarding experience
status: Done
assignee: []
created_date: '2026-04-19 01:07'
updated_date: '2026-04-20 21:23'
labels:
  - docs
  - onboarding
  - ux
dependencies: []
priority: medium
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
Address newcomer friction found in the 2026-04-19 onboarding audit. A determined self-starter can reach 'hello sine' in about an hour, but casual users bounce at dead links, draft pages that were accidentally published, and a stale contributing guide. This task bundles the quick wins and deeper gaps into one track so they can be prioritised and split out when picked up.

**Audit summary (2026-04-19)**

Top 5 quick wins:
1. README tagline: replace 'A programming environment for cyberphysical programming' with a plain-English version (CLAUDE.md has a good one). Add a 3-line paste-and-hear-sine snippet under Getting Started.
2. docs/reference/index.md currently shows a literal org-mode outline and the string 'Use Cian's stuff here, e.g.'. Rewrite as a real landing page linking the existing types.md/memory-management.md/concurrency.md.
3. Resolve the VSCode 'Download binary' disagreement: README says the command is unmaintained for v0.9.0; docs/overview/quickstart.md:51 still recommends it.
4. Rewrite docs/overview/contributing.md: it still documents the Jekyll flow with 'bundle install'; repo moved to VitePress. Also delete wishlist items already shipped in v0.9.0 (LLVM+ORC upgrade, aarch64).
5. Add examples/hello_world.xtm (println + gentle 2s 440Hz sine, block-by-block comments). Link from README and quickstart.

Deeper gaps:
- A real xtlang tutorial (docs/reference/new/ is a draft with typos like 'beging' and undefined syntax like `($ ...)`)
- A JIT/type error-message glossary
- A one-line 'listening on :7099' startup pointer in src/Extempore.cpp
- Publish the xtmdoc-generated API reference on the docs site
- Pick a canonical GitHub org (digego/extempore vs extemporelang/extempore) and normalise all links
- Modernise the community surface (GitHub Discussions, curated talks list)

Small inconsistencies worth fixing in passing:
- ASSETS size disagreement: CMakeLists.txt:7 says ~500MB, README.md:61 and BUILDING.md:20 say ~250MB
- BUILDING.md:77 mentions Gatekeeper but not the actual fix (xattr -dr com.apple.quarantine)
- --sharedir is warned about in README.md:65 but never shown in use
- Startup banner year in src/Extempore.cpp is 2010-2025; README licence says 2011-2026
- examples/ has no explanation of core/ vs external/ vs sharedsystem/ or which require optional CMake flags
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [x] #1 README tagline replaced and 'paste this to hear sine' block added under Getting Started
- [x] #2 docs/reference/index.md is a real landing page with links, no org-mode draft content visible
- [x] #3 VSCode binary download instructions are consistent between README and quickstart
- [x] #4 docs/overview/contributing.md documents the current VitePress workflow and omits shipped wishlist items
- [x] #5 examples/hello_world.xtm exists and is linked from README and quickstart
- [x] #6 xtlang tutorial covering bind-func, types, closures, and memory zones is drafted or scoped as its own follow-up task
- [x] #7 JIT/type error-message glossary is drafted or scoped as its own follow-up task
- [x] #8 Extempore prints a 'listening on :7099' pointer on startup
- [x] #9 Canonical GitHub org is chosen and all README/docs links normalised
- [x] #10 ASSETS size, macOS quarantine fix, --sharedir, and banner year inconsistencies are reconciled
<!-- AC:END -->
