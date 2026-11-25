---
title: The nREPL server
---

Extempore runs an [nREPL](https://nrepl.org) server beside its eval server on
port 7099. Editors, scripts and coding agents send it code and get back each
form's value, the output it printed, and errors with their kind and line.

It starts with extempore on `127.0.0.1:7888`:

```
nREPL server started on port 7888 on host 127.0.0.1 - nrepl://127.0.0.1:7888
```

| option              | effect                                                                 |
| ------------------- | ---------------------------------------------------------------------- |
| `--nrepl-port N`    | listen on port N; `0` picks a free port and writes it to `.nrepl-port` |
| `--nrepl-bind ADDR` | listen on another IPv4 address (anyone who can reach it can run code)  |
| `--no-nrepl`        | don't start the server                                                 |

If 7888 is taken (by a second extempore, say), extempore prints a warning and
runs without the server. A taken `--nrepl-port` is an error.

`--no-legacy-server` starts a session without the eval server on 7099, for when
everything which talks to it speaks nREPL.

## From the command line

The `extempore` binary is its own client:

```sh
extempore send '(+ 1 2)'
echo '(println "hello")' | extempore send
extempore send --file song.xtm                       # load the whole file
extempore send --file song.xtm --line 12 --column 3  # one form of it
extempore send --process utility '(ipc:get-process-name)'
extempore repl
extempore status
```

`send` prints what the code printed and each form's value on stdout, and errors
on stderr. Its exit status is 0, 1 if the evaluation failed, or 2 if there was
nothing to send or no extempore to send it to. `--file F --line L --column C`
evaluates the top-level form enclosing that position, or the nearest one before
it.

`repl` is an interactive REPL (Linux and macOS). Ctrl-C stops the evaluation
which is running and Ctrl-D leaves. `extempore --repl` starts a session with one
in its own terminal.

`status` prints the running extempore's version, its processes and the server's
ops: the first thing to try when an editor isn't getting through.

All three take `--host` and `--port`. Without `--port` the port is
`$EXTEMPORE_NREPL_PORT`, then the one in a `.nrepl-port` file in the working
directory (for `send --file`, the file's directory) or above it, then 7888.

For a script or coding agent, start `extempore --no-audio --nrepl-port 0` in a
scratch directory and send it code with `extempore send` from there. Libraries
stay loaded between evals.

## The protocol

Code is read and evaluated a form at a time, stopping at the first error. A
request gets

- `out` messages with what the code printed, without terminal colour codes (the
  console shows it too)
- a `value` message per form
- on an error, an `err` message with the report, then one with `ex` (e.g.
  `wrong-type-arg` or `xtlang-type-error`), `status: ["eval-error"]` and an
  `extempore/error` dictionary (`kind`, `message`, `form`, `file`, `line`)
- one message with `status: ["done"]`

Send `file` and `line` with an `eval` so errors name your source file and line.
Output printed later, by a callback or a temporal recursion, goes only to the
console.

A session made with `clone` belongs to one Scheme process: `primary`, or the one
named by `extempore/process` (`"utility"`, or one made with `ipc:new`). A
process evaluates one thing at a time, so evals sent to it queue in arrival
order whichever client sent them. A coding agent working beside a performance is
better pointed at the utility process.

`interrupt` stops a session's running eval the next time control is in Scheme
code; compiled xtlang, and LLVM compiling it, run to completion first. Evals
also stop at the process's time limit (`sys:set-default-timeout`, 60 seconds by
default).
