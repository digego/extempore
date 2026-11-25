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

Three aims decide the open choices:

- **simple**: one way to do each thing, and one spelling for each option
- **robust**: behaviour that doesn't depend on timing, on what else is running
  or on which editor is in use
- **editor-neutral**: editors get their features from the binary through
  standard protocols (LSP, nREPL), so each needs configuration only. Whatever
  one editor needs beyond that lives in that editor's configuration or plugin,
  never in extempore.

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
- An unknown `--name[=value]` is a warning, and is kept for `sys:cmdarg`, so a
  misspelt option goes unnoticed. `--compile` takes `--link` and `--dll` this
  way, and neither is in `--help`.
- Everything goes to stdout, colour codes included when stdout is a pipe:
  `--batch '(println 3)'` prints 16 lines, and callers pass `--term nocolor`.
- `--arch`, `--cpu` and `--attr` only matter to `--compile`; `--realtime` is
  Windows-only; `--timediv` has no users in the repo; `--runtime` is deprecated.
- Mixed spellings: `--noaudio`, `--nobase` and `--samplerate` beside
  `--no-nrepl` and `--device-name`.

## Subcommands

```
extempore [FILE]                    live session: audio, nREPL, legacy eval server
extempore exec FILE | -e EXPR       script: no audio, no servers
extempore test FILE...              run .xtm test files
extempore render FILE -o OUT.wav    offline render (below)
extempore compile FILE              AOT-compile a library, or --exe an executable

extempore send [CODE]               evaluate code in a running extempore (nREPL)
extempore repl                      interactive REPL on a running extempore
extempore lsp                       language server over stdio (below)
extempore status                    the running extempore's version and processes

extempore devices                   list audio devices
extempore version
```

The first group starts a runtime. The second are clients of a running
extempore's nREPL server: they dispatch before LLVM, audio or Scheme start, so
each costs a process start and nothing more (`--version` returns in under 10 ms
from the 87 MB binary).

### Rules for every subcommand

- A first argument that names a subcommand is one; a file of the same name is
  `./exec`.
- Options are whole words joined by hyphens, as `--x v` or `--x=v`. The short
  forms are `-e`, `-o` and `-h`.
- An unknown option is an error. Arguments after `--` belong to the script:
  each `--name[=value]` there is what `sys:cmdarg` reads.
- Each subcommand has its own option table, which drives both its parser and
  its `--help`.
- Outside a session, stdout carries only what the code prints (for `send`, its
  output and values). There is no banner, and load messages, compiler messages,
  warnings and errors go to stderr. A session prints what it does today.
- `--color auto|always|never`. `auto`, the default, is colour when stdout is a
  terminal and `NO_COLOR` isn't set.

Exit status:

| status | meaning                                                             |
| ------ | ------------------------------------------------------------------- |
| 0      | the work succeeded                                                  |
| 1      | the work failed: an uncaught error, a failed assertion              |
| 2      | it never started: a usage error, a missing file, no extempore found |
| N      | the code called `(quit N)`                                          |

Runtime subcommands take `--share-dir`, `--no-base` and `--opt-level`. Clients
take `--host` and `--port`, the nREPL server's. Without `--port` they use
`EXTEMPORE_NREPL_PORT`, then a `.nrepl-port` file in the directory of the file
they were given (or the working directory) or above it, then 7888.

### The runtime subcommands

- **session** (no subcommand) loads `FILE`, then evaluates `-e EXPR`, if
  given. Its options: `--device` and `--in-device` (an index, or a regex on the
  device's name), `--sample-rate`, `--frames`, `--channels`, `--in-channels`,
  `--latency`, `--realtime` (Windows), `--no-audio`, `--repl`, `--nrepl-port`,
  `--nrepl-bind`, `--no-nrepl`, `--legacy-port` and `--no-legacy-server`.
  `--repl` runs the `repl` client in the session's terminal, connected to the
  session's own server; it is an error if that server didn't start.
- **exec** evaluates the file or expression and exits when the work is done:
  the code has finished, nothing is scheduled and every Scheme process is idle.
  A script that schedules callbacks runs until the last one has run, and a
  temporal recursion runs until it calls `(quit)`, as written. `--wait` keeps
  it running until `(quit)` regardless, for scripts waiting on something
  outside the task queues (OSC, MIDI, IPC, a thread they spawned). Status 1 if
  the code or a callback raised an uncaught error.
- **test** runs each file through `xtmtest-run-tests` (`libs/core/test.xtm`),
  each in its own process, so a result doesn't depend on which files ran
  before. It prints a summary and exits 1 if any assertion failed. `--junit DIR`
  writes the per-file XML reports ctest and CI collect. `tests.cmake` becomes a
  list of files, and rerunning one file is `extempore test tests/core/adt.xtm`.
  The batch tests also stop taking a `--port` each: only the IPC test needs
  ports.
- **render** takes `-o OUT.wav`, `--duration SECONDS` (required), `--seed N`
  and the audio-format options (`--sample-rate`, `--channels`, `--frames`).
  Details below.
- **compile** AOT-compiles a library into the cache (`xtc:aot:compile-xtm-file`,
  today reached only through `--batch` from CMake), or with `--exe` builds an
  executable (`xtc:aot:compile-xtm-exe`, today's `--compile`). It owns
  `--arch`, `--cpu`, `--attr`, `--link` and `--dll`.

There is no docstring-export subcommand. The `xtmdoc` target's 600-character
`--eval` becomes a script in `extras/` that `exec` runs, taking its output path
after `--`.

### The clients

- **send** evaluates code and prints its output and values on stdout and its
  errors on stderr. The code is its argument, or stdin when stdin isn't a
  terminal; `--file F` loads the whole file (nREPL's `load-file`), and
  `--file F --line L [--column C]` evaluates the top-level form at that
  position. `--process NAME` picks the Scheme process (`primary` by default),
  so an agent beside a performance can use `utility`. Status 1 on an eval error,
  2 if there was nothing to send or no server. It shares the server's C++
  bencode codec, so the client is versioned with the server, and editors and
  agents need nothing but extempore (no Python on Windows, no symlink or
  wrapper).
- **repl** shows output, values and errors apart, and Ctrl-C interrupts the
  running eval, which the 7099 server can't do. Linux and macOS only, as now
  (linenoise).
- **status** prints the server's version, processes and ops (nREPL's
  `describe`): the first thing to run when an editor isn't getting through.
- **lsp** is a language server (below).

`send` has no editor-specific options. Today's `extras/nrepl/extempore-nrepl`
has two for helix (`--editor`, `--wait-for-save`); they aren't carried over, and
helix moves to the language server, which needs neither.

### The legacy eval server

The original eval server (7099, with the utility process on the port below) is
the legacy server. It stays on by default while editor plugins speak its
protocol, with `--legacy-port N` to move it and `--no-legacy-server` to leave it
out. nREPL and the language server replace it: it goes off by default once the
Emacs mode and the VS Code extension use them, and is removed in a later
release. The utility process itself stays; only its port goes.

### Compatibility

A command line that uses any of the old mode options (`--run`, `--eval`,
`--batch`, `--compile`, `--audio-outfile`, `--print-devices`) is parsed whole by
today's rules, which are kept as they are: the last startup expression wins,
unknown options are stored for `sys:cmdarg`, and `--audio-outfile` without
`--duration` renders until `(quit)`. It prints the new form once, on stderr.
Old and new forms can't be mixed.

| old                                | new                                      |
| ---------------------------------- | ---------------------------------------- |
| `--run FILE`                       | `extempore FILE`                         |
| `--eval EXPR`                      | `extempore -e EXPR`                      |
| `--batch EXPR`                     | `extempore exec --wait -e EXPR`          |
| `--audio-outfile F [--duration S]` | `extempore render ... -o F --duration S` |
| `--compile F`                      | `extempore compile --exe F`              |
| `--print-devices`                  | `extempore devices`                      |

Renamed options are accepted in the new grammar too, with the same notice:

| old                                  | new                               |
| ------------------------------------ | --------------------------------- |
| `--noaudio`, `--nobase`              | `--no-audio`, `--no-base`         |
| `--samplerate`, `--inchannels`       | `--sample-rate`, `--in-channels`  |
| `--indevice`                         | `--in-device`                     |
| `--device-name`, `--indevice-name`   | `--device`, `--in-device`         |
| `--sharedir`                         | `--share-dir`                     |
| `--port`                             | `--legacy-port`                   |
| `--term nocolor`, `--term` otherwise | `--color never`, `--color always` |

Both are a deprecation, with an end: the old parser and the renamed options
work for two minor releases after the one that introduces the subcommands, and
are then deleted together.

Two options go outright, with an error naming the reason: `--timediv` (no users
in the repo, and no documentation) and `--runtime` (deprecated already, for
what is now `--share-dir`). The nREPL options, port and client have never been in a
release, so they change without aliases.

The repo's own callers (`extras/cmake`, `extras/studio/render`,
`extras/compiler-stress`, `docs/verify-examples.py`, the docs and AGENTS.md)
move to the new forms in the same change, so the notices only reach outside
users.

### Editors

An editor integrates at one of two levels, and extempore has nothing specific
to any editor at either:

- **the language server**, for any editor with a language client: eval,
  diagnostics, outline, completion and hover from one implementation. The
  editor needs a file type for `.xtm` and `command = "extempore", args =
  ["lsp"]`.
- **`extempore send`**, for an editor that can only run a command: pipe the
  selection to it, or give it the file and line.

The existing plugins are unaffected while the legacy server stays. The Emacs
mode starts plain `extempore` (plus the user's `extempore-program-args`, empty
by default) and VS Code runs `extempore` with no arguments in a terminal opened
in the share dir; the vim and Sublime plugins connect to a running extempore
without starting one. All four default to `localhost:7099`. Users who set
`extempore-program-args` to old flags get the deprecation notice.

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

1. The render thread owns the clock; no audio device, no scheduler thread and
   no server run.
2. Before rendering the block `[t, t + frames)`, it runs the scheduler
   synchronously for every task then in the queue and due before `t + frames`
   (today's `timeSlice` body, called directly).
3. It waits until every Scheme process is idle: its task queue empty and no
   task running.
4. Then it renders the block and writes it.

A task that schedules another one due inside the same block gets it in the next
block. A live session does the same when the new task misses the scheduler's
pass, so nothing runs later than it could live, and a callback that reschedules
itself for `(now)` costs one run per block where a repeat-until-quiet rule would
never leave the block.

Loading the file happens at audio time 0 and the clock doesn't move while it
compiles, so a score's top-level code runs at the time it's written for,
however long compilation takes. Output starts at time 0, which drops today's
"start writing at `dsp:set!`" special case.

The render ends exactly at `--duration`, or earlier if the code calls `(quit)`;
either way the WAV is complete. An uncaught error in the file or in a callback
is printed on stderr, the render carries on as a live session would, and the
exit status is 1. With stderr on a terminal it shows progress (audio time
against the duration). So `extras/studio/render` drops its port choice, log
polling, error grep and kill timers and keeps only the analysis (numpy and
scipy, which stay outside the binary).

The same file with the same `--seed` renders to the same samples on the same
build and machine. `--seed` defaults to 0, so two plain renders match. Across
machines the samples can differ in the last bits (libm, and code generated for
the host CPU).

### What it needs

- `TaskScheduler`: a synchronous `runDue(uint64_t until)` for the render
  thread, sharing the queue logic with `timeSlice`.
- `SchemeProcess`: a way to wait for idle (the task queue empty and
  `m_taskStart == 0`) under `m_guardMutex`, with a condition variable the task
  thread signals when it finishes a task. `exec` uses the same test to know its
  work is done.
- `FileAudioDriver`: the loop above, without sleeps; the existing WAV writing
  and `--duration` handling stay.
- Seeding. xtlang's generator is thread-local and seeded from
  `std::random_device` (`sRandGen` in `src/EXTLLVM.cpp`), and s7's default
  random state is seeded from the clock; neither can be set today. Both take
  their seed from `--seed`, each thread's generator from the seed and the order
  the threads were made.
- A wall-clock time limit for callbacks. The existing limit (`begin_hook`
  against `call_end_time`) counts audio time, which stands still during a task,
  so a callback that loops forever would stall the render instead of being
  stopped. The initial load is exempt, since compiling can take minutes.

### Limits

- Work outside the Scheme task queues is not in lockstep: threads started from
  xtlang (`spawn`), and MIDI or OSC input. They still run, at their own pace.
- A callback that never finishes stalls the render until the wall-clock limit
  stops it.
- `(now)` inside a callback is the start of the block, as it is live.
- Code that reads the wall clock (`clock:clock`) gets a different answer each
  run.

### Tests

The audio-offline tests move to `extempore render`. A new test renders the same
file twice, one that uses `random`, and compares the WAVs sample for sample;
`hello-sweep`'s switch point can then be asserted to the block.

## Language server

`extempore lsp` speaks LSP on stdio. It is a client of a running extempore's
nREPL server, sharing `send`'s nREPL client and form finder, and uses the JSON
code in LLVM's Support library (which clangd uses) for the protocol.

It uses only what every language client implements, so no editor needs code of
its own: commands, code actions, diagnostics, `window/showMessage` and
`window/logMessage`. Where one editor can do less, its configuration makes up
the difference.

Without a running extempore it offers what a parser can: syntax errors
(unbalanced parentheses), an outline of top-level definitions (`bind-func`,
`define`, `bind-type` and so on) and folding. Connected, it adds eval,
interrupt, and completion and hover from nREPL's `completions` and `lookup`
(doc-002 phase 3).

### Eval

Three commands, through `workspace/executeCommand`:

- `extempore.eval` takes `{uri, range}`, an LSP `Location`: 0-based, in the
  position encoding agreed at `initialize`. An empty range evaluates the
  top-level form at that position; a non-empty one evaluates the text it covers.
- `extempore.evalBuffer` takes `{uri}`.
- `extempore.interrupt` takes nothing.

The same three are code actions ("Evaluate top-level form" or "Evaluate
selection", "Evaluate buffer"), for which the client supplies the URI and range
itself. An editor binds a key to the command where it can build the argument,
and falls back to the code action where it can't.

The code comes from the editor's buffer as it stands, so nothing is saved
first. An eval reports in three places:

- values and the first line of output go to `window/showMessage`
- the whole output goes to `window/logMessage`
- an error becomes a diagnostic on the first line of its form, the only
  position nREPL reports

A diagnostic belongs to the form's text. It moves with the form as lines above
it change, and goes when the form evaluates cleanly, when its text is edited or
when the document closes.

### Configuration

`initializationOptions` takes `host`, `port` and `process`; without them the
server finds the port as `send` does and uses `primary`. It owns the connection
and reconnects when extempore restarts. A command sent with no extempore
running answers with a message saying where it looked.

### What helix does (tested 2026-11-25)

Tested against a stub server in helix `92ba4c07` and read in upstream
`1a3fdfda`:

- `:lsp-workspace-command NAME ARGS` parses `ARGS` as a stream of JSON values
  after expanding `%{...}`, so one key can send a command with the cursor's
  place. A binding passing `"%{file_path_absolute}" %{cursor_line}
  %{cursor_column}` reached the server as `[path, 18, 3]`, and edits made
  without saving reached it (`didChange`) before the command did.
- helix ignores a command's result, so the server reports through
  `window/showMessage` (the status line, one line: newlines are dropped) and
  `textDocument/publishDiagnostics`, which helix shows inline under the line
  with the multi-line message intact.
- helix declares no inlay-hint refresh, so the server can't put `=> 144` beside
  the form when an eval finishes.
- A code action ("Evaluate top-level form") also works, and carries the
  document's URI, but takes three keys (`space a`, Enter).
- A Windows path in `%{file_path_absolute}` isn't valid JSON (`\U` is a bad
  escape).
- Servers configured in the user's `languages.toml` start without a trust
  prompt (helix's default `workspace-trust` level is `servers`); only a
  project's own `.helix/languages.toml` needs `:workspace-trust`.

helix's variables give a path and a 1-based line where `extempore.eval` takes a
URI and a 0-based one. Converting them is the helix binding's job, not the
server's, and is still to be tested (`%sh{...}` can do the arithmetic). The
code action needs no conversion and is the fallback, including on Windows.

### Other editors

VS Code (`vscode-languageclient`), Neovim (`vim.lsp`) and Emacs (eglot) send
`extempore.eval` with the document's URI and the cursor's LSP position, which
is the argument as specified. Vim and Sublime have language clients as
packages. None is tested yet. The VS Code extension and the Emacs mode
can then drop their own TCP clients, which is what lets the legacy server go.

## Order of work

1. The lockstep render behind the existing `--audio-outfile` flag, so the
   audio-offline tests stop flaking before any interface changes.
2. The new grammar and the runtime subcommands (`exec`, `test`, `render`,
   `compile`, `devices`), with the output and exit-status rules, `--seed`, and
   the old parser kept for old command lines. Move the repo's callers over.
3. `send`, `repl` and `status`, and the legacy server's two options. AGENTS.md
   and the nREPL tests' client calls move to `send`.
4. `extempore lsp` with the parser features and eval. The helix binding (in the
   docs and on benswift.me) moves from the Python client to the language
   server, and `extras/nrepl/extempore-nrepl` goes: its bencode client moves
   into `tests/nrepl`, since protocol tests need to send what no real client
   would (malformed bencode, a message a byte at a time, replies never read),
   which is easiest from Python. Its form finder is deleted; the C++ one is
   tested through `send --file --line`.
5. nREPL `completions` and `lookup` (doc-002 phase 3), then completion and
   hover in the language server.

Steps 2 to 4 land before the next release, so no released `extempore` has the
Python client or an option made for one editor.
