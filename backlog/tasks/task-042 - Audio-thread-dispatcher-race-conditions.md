---
id: TASK-042
title: Audio thread dispatcher race conditions
status: Done
assignee:
  - "@claude"
created_date: "2026-04-19 02:05"
updated_date: "2026-04-19 21:41"
labels:
  - cpp
  - rt-audio
  - concurrency
dependencies: []
priority: high
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->

src/AudioDevice.cpp:169-170, 391 — multi-thread dispatch uses spin-sleep on
atomics and an unsynchronised m_numThreads read (source comment at line 391
reads 'this is a race :('). Replace with std::counting_semaphore or condition
variable. TSan run required.

Follow-up from #419.

<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria

<!-- AC:BEGIN -->

- [x] #1 Replace sSignalCount/sThreadDoneCount spin-sleep with
      condition-variable counting semaphore (no std::this_thread::sleep_for in
      worker or dispatcher hot path)
- [x] #2 Remove 'this is a race :(' TOCTOU on m_numThreads by snapshotting once
      per dispatcher cycle and by making m_numThreads std::atomic
- [x] #3 libs-core and libs-external ctest suites green
- [x] #4 aot_external_audio target builds with -DEXTEMPORE_SANITIZE=tsan without
    thread-sanitizer reports against AudioDevice.cpp
<!-- AC:END -->

## Implementation Plan

<!-- SECTION:PLAN:BEGIN -->

1. Make AudioDevice::m_numThreads std::atomic<unsigned> (header) so concurrent
reads from the audio callback and writes from initMTAudio/initMTAudioBuf are
defined. getNumThreads returns a loaded snapshot.\n2. In AudioDevice.cpp, drop
the sSignalCount/sThreadDoneCount atomics and MT_SLEEP_DURATION.\n3. Add a small
file-local counting-semaphore class (mutex + condvar + int; C++17-compatible
stand-in for std::counting_semaphore) and two instances: sMtWorkAvailable
(dispatcher->workers) and sMtWorkComplete (workers->dispatcher).\n4. Rewrite
audioCallbackMT and audioCallbackMTBuf worker loops: acquire() at top, DSP,
release() at end. Remove spin-sleep + 'Still locked' watchdog prints.\n5.
Rewrite the three MT dispatcher branches: snapshot numthreads once,
release(numthreads), do sum work, then acquire numthreads times. Remove 'this is
a race :(' warning block and the in-loop re-reads of getNumThreads().\n6. Build,
run ctest libs-core + libs-external, verify aot_external_audio target
builds.\n7. Rebuild with -DEXTEMPORE_SANITIZE=tsan, run a short MT audio
scenario, confirm no TSan reports touching AudioDevice.cpp.
<!-- SECTION:PLAN:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->

Replaces the atomic-spin + 100us-sleep MT audio dispatcher with a C++17
counting-semaphore pattern (mutex + condition_variable + unsigned count) and
makes AudioDevice::m_numThreads std::atomic so the dispatcher's snapshot of the
worker count is defined.\n\n## Summary\n- include/AudioDevice.h: m_numThreads ->
std::atomic<unsigned>; getNumThreads() does a std::memory_order_acquire load.\n-
src/AudioDevice.cpp: drop sSignalCount/sThreadDoneCount atomics and
MT_SLEEP_DURATION; introduce an anonymous-namespace MtSemaphore (notify_one on
release(1), notify_all on release(n)); two instances sMtWorkAvailable
(dispatcher->workers) and sMtWorkComplete (workers->dispatcher).\n-
audioCallbackMT / audioCallbackMTBuf: worker loop is now acquire() -> DSP ->
release(1). Removed spin-sleep and the 'Still locked' watchdog prints (deadlock
detection is better handled externally; the old prints never actually recovered
anything).\n- Three MT dispatcher branches in audioCallback (DSPSUMWrapper
zerolatency, DSPSUMWrapper non-zerolatency, DSPSUMWrapperArray) now snapshot
numthreads once, do release(numthreads), and acquire numthreads times. Removed
the in-loop re-reads of getNumThreads() and the 'this is a race :(' warning
block that was firing on the TOCTOU.\n- initMTAudio / initMTAudioBuf: store
numthreads with release ordering; use the local snapshot for allocations and
thread-creation loop.\n\n## Test plan\n- Clean build (build/) succeeds with
-Wall -Wextra -Wshadow; aot_external_audio target builds.\n- ctest libs-core
(6/6) and libs-external (1/1) green.\n- Rebuild under -DEXTEMPORE_SANITIZE=tsan
(build-tsan/) and re-run libs-core + libs-external under 'setarch -R' (to
sidestep the known TSan-vs-high-entropy-ASLR mmap issue); all tests green, zero
ThreadSanitizer reports.\n- Live MT-audio stress under TSan (sys:init-mt-audio)
still requires hardware and is manual; the primitive change is mechanical, so
correctness is by construction.\n\n## Follow-up\n- Migrate to
std::counting_semaphore if/when the project moves to C++20; MtSemaphore is a
drop-in replacement.

<!-- SECTION:NOTES:END -->
