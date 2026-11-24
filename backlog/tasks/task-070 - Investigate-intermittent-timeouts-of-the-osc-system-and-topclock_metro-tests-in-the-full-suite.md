---
id: TASK-070
title: >-
  Investigate intermittent timeouts of the osc, system and topclock_metro tests
  in the full suite
status: To Do
assignee: []
created_date: '2026-11-24T11:16:22Z'
updated_date: '2026-11-24T20:08:52Z'
labels:
  - tests
dependencies: []
priority: medium
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
In a full local run (`ctest --preset default` on Linux x64), three tests sometimes time out together: `tests/core/osc.xtm` and `tests/core/system.xtm` at 120 s, and `examples/core/topclock_metro.xtm` at 300 s. In the same run every other test passes.

What is known (observed 2026-11-24, across seven full runs):

- the full suite failed this way three times and passed four times, on identical code each side of a failure
- the three pass every time they are run on their own, together, or alongside the MIDI tests and examples (including six repeats with `--repeat until-fail`)
- when stuck, `system.xtm` has printed `xtmtest ipc:call` and is waiting for the reply from the process it made with `ipc:new` on port 17599; `osc.xtm` has loaded xtmbase and printed nothing since (it then starts an OSC server on 17755 and waits for a round trip); `topclock_metro` has printed "Nominating myself in topclock election...". All three are waiting on local networking or on Extempore's own scheduler
- no stray extempore process and no other listener on those ports was found afterwards
- the failures first appeared after the commit which lets `examples/external/portmidi*.xtm` find a MIDI device and run in full (84e23713), in a session where `/dev/snd/seq` had been made readable; before that the same recipe passed three times. A causal link is not established: starting the MIDI examples and then the three tests did not reproduce it
- the failing runs were each a rebuild (`cmake --build build`) followed at once by the suite, but that recipe has also passed
- ctest runs a test which failed last time first, and cheap tests last, so a failure changes the next run's order. The failures and passes alternated, which fits the three failing only when they run at the tail of the suite, though one passing run also had them at the tail

Not yet done: a stack trace of a stuck process. `gdb -p <pid> -batch -ex 'thread apply all bt 12'` on one of the three, once it has been running for more than 40 s, would show what it is waiting on. Whether CI shows the same timeouts is also unknown.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] #1 The cause of the timeouts is identified, with a recipe which reproduces them
- [ ] #2 The cause is fixed, or the tests are changed so that they can't interfere, and ten consecutive full local runs pass
<!-- AC:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
Two further runs of the failing recipe (touch libs/core/midi.xtm, rebuild, full suite) with a script waiting to attach gdb both passed, so the tally is three failures in nine full runs and no stack trace yet. The script is simple to rewrite: start the suite, poll ps every 5 s for an osc.xtm or system.xtm process older than 40 s, and run gdb on it.

2026-11-25: fourteen more full runs passed (one with /dev/snd/seq unreadable, eleven with it readable, then two overlapping suites started 20 s apart), so the full-suite hang is still unreproduced; tally three failures in 23 runs. The journal shows no clock step on 2026-11-24, and CI has never shown these timeouts.

The 10:59 failure in the session which filed this had no other extempore running on weddle while the three were at the tail, so a concurrent run there is not established. What the three share is a resume from the SchemeProcess task queue: sys:sleep's callback (osc, topclock_metro, which never printed 'which I won!' 4 s after nominating itself) and the ipc:call reply (system).

Concurrent copies of a test do collide: two system.xtm runs at once hang exactly as the failure did (the second can't bind 17599, ipc:new connects to the first's server, and the reply never comes back), and two osc.xtm runs fail on 17755. Fixed in 1980f76b (ipc:new returns #f when its port is taken) and 8fb4ed82 (the two tests search for a free port). Two topclock_metro runs at once both finish.

Side find, not fixed: SchemeProcess.cpp:362 and :385 pass an end time where scheme_call takes a duration, so callback deadlines are about 2*now + maxDuration (longer than meant, so not a hang).

2026-11-25, later: fixed the callback time limit (7153bc73: callbacks got about three times the current time instead of call_default_time, and s7 abandoned an evaluation at its limit without a word, which would hang a test silently; it now prints 'evaluation stopped at its time limit'). Added a hang report (8b19eb15): ctest sends SIGUSR1 to a test which times out, and extempore writes the scheduler queue, each process's task and time limit, and every thread's backtrace into the log; stdout is now line-buffered so a killed test keeps its last lines (before, the 'last line printed' evidence above could be missing up to 4 KB). Only osc.xtm, system.xtm and topclock_metro resume inside a callback continuation, which is why these three and no others hang. Six more runs of the failing recipe (touch libs/core/midi.xtm, rebuild, full suite, sequencer readable) all passed, as did 96-way stress runs of osc.xtm and system.xtm: 3 failures in 29 full runs, none since. Next occurrence: read the hang report in build/Testing/Temporary/LastTest.log.
<!-- SECTION:NOTES:END -->
