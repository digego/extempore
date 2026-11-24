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

## From the command line

`extras/nrepl/extempore-nrepl` is a client that needs only Python 3. Put it on
your `PATH`, then:

```sh
extempore-nrepl eval '(+ 1 2)'
echo '(println "hello")' | extempore-nrepl eval
extempore-nrepl eval --file song.xtm --line 12 --column 3
extempore-nrepl describe
```

`--file F --line L --column C` evaluates the top-level form enclosing that
position, or the nearest one before it. Values go to stdout and errors to
stderr, with exit status 1 if evaluation failed. `--editor` puts everything on
stdout and always exits 0, for an editor's shell-command integration.

The port comes from `--port`, then `$EXTEMPORE_NREPL_PORT`, then a `.nrepl-port`
file in the file's directory or above it, then 7888.

For a script or coding agent, start `extempore --noaudio --nrepl-port 0` in a
scratch directory and send it code with `extempore-nrepl eval`. Libraries stay
loaded between evals.

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
