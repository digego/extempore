---
id: doc-003
title: Command line and offline render redesign
type: specification
created_date: "2026-11-25 12:30"
---

# Command line and offline render redesign

Two linked changes: reorganise the `extempore` command line around what the
user is doing, and make offline rendering deterministic. The render is one of
the command-line modes, so they are designed together.

## The command line today

34 options, of which the problems are structural:

- `--run`, `--eval`, `--batch` and `--compile` all set the startup expression,
  and the last one silently wins. `--batch EXPR` is both a mode and its payload.
- `--batch` implies `--noaudio`, unless `--audio-outfile` is given, which is an
  error with `--noaudio`. These rules live only in `src/Extempore.cpp`.
- A batch run continues until something calls `(quit)`, so callers wrap their
  file in `(sys:load-then-quit FILE 0)` (93 uses of `--batch` across 34 files,
  including every ctest command) and agents are told to use `timeout`.
- `--port N` also puts the utility process on N−1; the nREPL server has its
  own options.
- `--arch`, `--cpu` and `--attr` only matter to `--compile`; `--realtime` is
  Windows-only; `--timediv` has no users in the repo; `--runtime` is deprecated.
- Mixed spellings: `--noaudio` and `--nobase` beside `--no-nrepl`.

## Subcommands

```
extempore [FILE]                    live session: audio, eval servers, nREPL
extempore exec FILE | -e EXPR       script: no audio, no servers
extempore render FILE -o OUT.wav    offline render (below)
extempore compile FILE              AOT-compile FILE to a native executable
extempore send [CODE]               evaluate code in a running extempore (nREPL)
extempore repl                      interactive REPL on a running extempore
extempore devices                   list audio devices
extempore version
```

- **session** (no subcommand) is today's default. `FILE` replaces `--run`.
  Session options keep their current names, since editors and users' scripts
  pass them: `--port`, `--device`, `--device-name`, `--indevice`,
  `--indevice-name`, `--samplerate`, `--frames`, `--channels`, `--inchannels`,
  `--latency`, `--nrepl-port`, `--nrepl-bind`, `--no-nrepl`, `--no-audio`.
- **exec** evaluates the file or expression and exits when it finishes: status
  0, or 1 on an uncaught error (as `--batch` does now). Scheduled callbacks
  still pending then are dropped, since a temporal recursion would otherwise
  keep it running forever. `--wait` keeps it running until `(quit)`, which is
  today's `--batch` behaviour, for scripts that schedule work (the scheduler
  and topclock tests). It has to be a flag: callbacks run on the script's own
  task thread, so the script can't block waiting for them. This replaces
  `--batch` and the `sys:load-then-quit` wrapper.
- **render** takes `--duration SECONDS` (required) plus the audio-format
  options (`--samplerate`, `--channels`, `--frames`).
- **compile** owns `--arch`, `--cpu` and `--attr`.
- **send** is the nREPL client, built into the binary with the server's C++
  bencode codec: code from an argument or stdin, or `--file F --line L --column
C` for the top-level form at a position, with `--editor`, `--wait-for-save`
  and the port lookup of today's `extras/nrepl/extempore-nrepl`. It replaces
  that Python script, so editors need nothing but extempore (no Python on
  Windows, no symlink or wrapper), and the client is versioned with the server.
  It dispatches before LLVM, audio or Scheme start, so each call costs a
  process start and nothing more.
- **repl** is an interactive client of a running extempore's nREPL server,
  replacing `--repl` (a line-editing client of the 7099 server started inside
  the session). It shows output, values and errors apart, and Ctrl-C
  interrupts the running eval, which 7099 can't do. Linux and macOS only, as
  now (linenoise).
- **devices** replaces `--print-devices`.

Options for every subcommand: `--sharedir`, `--no-base`, `--opt-level`,
`--term`.

Removed: `--timediv` (no users) and `--runtime` (deprecated already).
`--realtime` stays a session option, documented as Windows-only.

### Deprecation

One release where the old forms still work and print the new spelling once:

| old                                      | new                                    |
| ---------------------------------------- | -------------------------------------- |
| `--run FILE`                             | `extempore FILE`                       |
| `--eval EXPR`                            | `extempore -e EXPR` (session)          |
| `--batch EXPR`                           | `extempore exec -e EXPR`               |
| `--batch "(sys:load-then-quit F 0)"`     | `extempore exec F`                     |
| `--audio-outfile F --duration S --run X` | `extempore render X -o F --duration S` |
| `--compile F`                            | `extempore compile F`                  |
| `--print-devices`                        | `extempore devices`                    |
| `--repl`                                 | `extempore repl` (second terminal)     |
| `extras/nrepl/extempore-nrepl`           | `extempore send`                       |
| `--noaudio`, `--nobase`                  | `--no-audio`, `--no-base`              |

The next release removes them. The repo's own callers (`extras/cmake`,
`extras/studio/render`, `extras/compiler-stress`, the docs, AGENTS.md and the
backlog) move in the release that adds the subcommands, so the warnings only
reach outside users.

### Editor plugins

None is affected. The Emacs mode starts plain `extempore` (plus the user's
`extempore-program-args`, empty by default) and VS Code runs `extempore` with no
arguments in a terminal opened in the share dir; the vim and Sublime plugins
connect to a running extempore without starting one. All four default to
`localhost:7099`. Users who set `extempore-program-args` to old flags get the
deprecation warning.

## Deterministic offline render

`--audio-outfile` (`FileAudioDriver` in `src/AudioDevice.cpp`) renders on a
thread that calls `processFrames` as fast as it can, advancing the audio clock
each block, while scheduled Scheme callbacks run on the process's task thread,
which knows nothing of the render's progress. Sleeps keep them roughly in step:
1 ms per block until a DSP closure is set, then 100 µs per block. Whether a
callback lands before the render passes its time depends on machine load, so a
render isn't repeatable and timing assertions are flaky (`audio-offline/hello-sweep`
failed this way on macOS CI, 2026-11-25).

`extempore render` runs the clock in lockstep with the work instead:

1. The render thread owns the clock; no audio device and no scheduler thread
   run.
2. Before rendering the block `[t, t + frames)`, it runs the scheduler
   synchronously for every task due before `t + frames` (today's `timeSlice`
   body, called directly).
3. It waits until every Scheme process is idle: its task queue empty and no
   task running. A task can schedule another one due inside the same block
   (a zero-delay callback), so steps 2 and 3 repeat until neither the scheduler
   nor any process has work due before `t + frames`.
4. Then it renders the block and writes it.

Loading the file happens at audio time 0 and the clock doesn't move while it
compiles, so a score's top-level code runs at the time it's written for,
however long compilation takes. Output starts at time 0, which drops today's
"start writing at `dsp:set!`" special case.

With the same random seeds, the same file renders to the same samples on any
machine, at whatever speed the work allows.

### What it needs

- `TaskScheduler`: a synchronous `runDue(uint64_t until)` for the render
  thread, sharing the queue logic with `timeSlice`.
- `SchemeProcess`: a way to wait for idle (the task queue empty and
  `m_taskStart == 0`) under `m_guardMutex`, with a condition variable the task
  thread signals when it finishes a task.
- `FileAudioDriver`: the loop above, without sleeps; the existing WAV writing
  and `--duration` handling stay.
- A wall-clock time limit for tasks. The existing limit (`begin_hook` against
  `call_end_time`) counts audio time, which stands still during a task, so a
  callback that loops forever would stall the render instead of being stopped.

### Limits

- Work outside the Scheme task queues is not in lockstep: threads started from
  xtlang (`spawn`), and MIDI or OSC input. They still run, at their own pace.
- A callback that never finishes stalls the render until the wall-clock limit
  stops it.
- `(now)` inside a callback is the start of the block, as it is live.

### Tests

The audio-offline tests move to `extempore render`. A new test renders the same
file twice and compares the WAVs sample for sample, and `hello-sweep`'s switch
point can then be asserted to the block.

## Order of work

1. The lockstep render behind the existing `--audio-outfile` flag, so the
   audio-offline tests stop flaking before any interface changes.
2. The subcommands, including `send` and `repl`, with the old flags as
   deprecated aliases; move the repo's callers over. `extras/nrepl/extempore-nrepl`
   goes, and the bencode client and form finder it holds move into
   `tests/nrepl`: protocol tests need to send what no real client would
   (malformed bencode, a message a byte at a time, replies never read), which
   is easiest from Python. The helix binding in the docs (and on benswift.me)
   changes to `extempore send`.
3. Remove the aliases in the following release.
