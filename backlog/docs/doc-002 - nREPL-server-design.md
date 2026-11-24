---
id: doc-002
title: nREPL server design
type: specification
created_date: "2026-11-25 09:30"
---

# nREPL server design

A second eval server for Extempore that speaks [nREPL](https://docs.nrepl.org),
on its own port, so editors and tools get request ids, per-form values,
structured errors, streamed output, interrupt, completion and lookup. The
existing eval server on 7099/7098 is untouched, so current clients (Emacs
`extempore-mode`, the VS Code extension, `--repl`, `ipc:connect`) keep working.

The motivating client is helix, which has no plugin system and can only reach
outside the editor through shell commands and language servers. The intended
stack is:

```
helix ⇄ (LSP) extempore language server ⇄ (nREPL) extempore
```

The language server is a separate project and out of scope here, beyond the
requirements it puts on the protocol.

## The existing eval server

- `SchemeProcess::serverImpl` (`src/SchemeProcess.cpp:478`) accepts raw text
  terminated by CRLF and queues a `REPL` task. The task thread evaluates it and
  `send()`s one NUL-terminated reply straight to the client socket
  (`src/SchemeProcess.cpp:333`).
- The reply is the printed value, or on error the contents of a 256-byte buffer
  (`SCHEME_OUTPORT_STRING_LENGTH`, `include/SchemeProcess.h:107`), so long
  errors are truncated.
- Replies have no id, there is no way to tell which form of a multi-form send
  produced what, and output printed during the eval goes to the console only.
- It binds `INADDR_ANY` (`src/SchemeProcess.cpp:179`): any host on the network
  can evaluate code. Fixing that is a separate change; the nREPL server binds
  loopback by default.

## Why nREPL

LSP and nREPL work at different layers. LSP connects an editor to tooling about
documents (positions, diagnostics, completion) and has no notion of a running
program. nREPL connects any number of clients to a running program: sessions,
eval, streamed output, interrupt. Clojure tooling uses both side by side
(clojure-lsp for static analysis, nREPL for evaluation), and this design does
the same.

Using nREPL instead of a bespoke JSON-RPC protocol means the protocol is
already designed, including ids, sessions, streaming and capability discovery.
Its wire format (bencode) is small enough to implement without a dependency,
and client libraries exist in Python, JavaScript and Rust. Messages are open
dictionaries, so Extempore-specific data travels as extra namespaced keys
without breaking generic clients. Servers already exist for several
non-Clojure languages (Fennel, Racket, Guile, Janet, Steel Scheme, Common
Lisp), and the docs' [Building Servers](https://docs.nrepl.org/nrepl/building_servers.html)
page and the [proof](https://github.com/nrepl/proof) compatibility suite cover
exactly this case.

## Protocol

### Transport and startup

- bencode over TCP, bound to `127.0.0.1` unless `--nrepl-bind` says otherwise
- off unless `--nrepl-port=N` is given; `N=0` picks a free port
- on startup, print the reference server's banner line
  (`nREPL server started on port N on host 127.0.0.1 - nrepl://127.0.0.1:N`),
  which some clients parse, and write the port to `.nrepl-port` in the working
  directory, removing it on exit
- one server for the whole extempore process, not one per Scheme process
- not started in `--batch` mode, which has no server today

### Sessions and processes

A session belongs to one Scheme process, chosen at `clone` with the key
`extempore/process` (`"primary"` by default, or `"utility"`). Requests without
a session get an ephemeral one on the primary process, as the spec allows.

The spec expects sessions to evaluate concurrently. Extempore cannot: each
Scheme process has one task thread, so evals on the same process queue behind
each other whatever session they come from. `describe` documents this, and
`interrupt` is the escape hatch.

### Ops

| op                         | phase | notes                                                                                                          |
| -------------------------- | ----- | -------------------------------------------------------------------------------------------------------------- |
| `describe`                 | 2     | `ops`, `versions` (`nrepl`, `extempore`), `aux` (process names)                                                |
| `clone`, `close`           | 2     | sessions close when the connection that cloned them closes                                                     |
| `ls-sessions`              | 2     |                                                                                                                |
| `eval`                     | 2     | `code`, optional `file`, `line`, `column`; see below                                                           |
| `load-file`                | 2     | `file` is the contents; `file-path` sets the location                                                          |
| `interrupt`                | 2     | `interrupted` on the interrupted request; `session-idle`, `interrupt-id-mismatch`, `unknown-session` otherwise |
| `completions`              | 3     | Scheme symbols plus xtlang closures, types and aliases                                                         |
| `lookup`                   | 3     | docstring, xtlang type signature, source form where known                                                      |
| `extempore/log-subscribe`  | 4     | streams the process's console output to the connection                                                         |
| anything else, incl. stdin | ---   | `unknown-op`; evaluated code never reads stdin                                                                 |

### Eval

`code` is read form by form, as `sys:eval-string` does today
(`runtime/init.xtm:165`). For each form the server sends either a `value`
message or an error, then a single `status: ["done"]` when the whole request
finishes. After an error, evaluation continues with the next form, matching
current editor behaviour. The spec is inconsistent on this point; check what
`proof` expects before settling it.

`file`, `line` and `column` set the input port's location (s7's
`port-line-number` has a setter), so read errors and s7's own error locations
point at the source instead of line 0 of a string.

An error reply carries:

- `err`: the same text `sys:error-report-string` produces today
  (`runtime/init.xtm:122`), so generic clients show something useful
- `ex`: the error type, e.g. `wrong-type-arg` or `xtlang-type-error`
- `status: ["eval-error"]`
- `extempore/error`: a dictionary with `kind`, `message`, `form` (printed),
  `line`, `file` and, for xtlang diagnostics, `symbol`

Errors are reported against the top-level form. xtlang diagnostics can't point
at a sub-form because the compiler doesn't carry source positions through its
transforms; adding that is a non-goal for now.

### Output

The Scheme `print` and `println` are C functions that `printf` to stdout
(`src/ffi/misc.cpp:254`), bypassing s7's ports. Phase 1 changes them to write to
s7's current output port, whose default writes to stdout, so the console sees no
difference. During an nREPL eval the task thread binds the current output port
to an `s7_open_output_function` port that streams `out` messages tagged with the
request's id and session. `print-with-colors` must not emit ANSI escapes into
that port.

Output that doesn't come from Scheme code running inside a request --- `printf`
from JIT-compiled xtlang, scheduler callbacks, audio-thread messages --- stays
on stdout. Phase 4's log subscription delivers it.

### Interrupt

`begin_hook` (`src/SchemeS7.cpp:103`) already abandons an eval that passes its
time limit. Interrupt adds an atomic flag on the scheme wrapper that the same
hook checks. That covers Scheme code, including the xtlang compiler itself,
which is written in Scheme. It cannot stop JIT-compiled code or LLVM codegen
already under way; the reply arrives when control returns to Scheme. Evals keep
the process's existing time limit (`getMaxDuration`).

## Structured xtlang diagnostics (phase 1)

The 34 `xtc:diagnostics:print-*` functions in `runtime/xtc-globals.xtm` print a
coloured report to stdout and then call `xtc:diagnostics:throw-or-quit`
(`runtime/xtc-globals.xtm:198`), which raises `(error "")`. Whatever catches it
gets an empty error; the information only exists as console text. About 350
call sites use these functions.

Change the functions, not the call sites: keep printing exactly the same
console report, then raise an error whose type names the kind
(`xtlang-syntax-error`, `xtlang-type-error`, `xtlang-compile-error`, ...) and
whose info carries the message, the offending form and the symbol. Audit the
handlers that catch these errors (`catch` in `runtime/init.xtm:96` maps every
error to a value, so most are indifferent) before changing the type.

This is useful before any nREPL work: `xtmtest-compile-fails`
(`libs/core/test.xtm:138`) currently passes on any error, including a typo in
the test, and could assert on the kind and message instead.

## Implementation

- **C++:** a new `NreplServer` (its own source and header) with its own thread
  and a `poll` loop like `serverImpl`'s, a bencode codec, connection and session
  tables, and a bounded outbound queue per connection written only by the
  server thread. Request dispatch for ops that need Scheme goes through a new
  `SchemeTask::Type` carrying the request and a reply sink; the task thread
  pushes responses onto the connection's queue and wakes the server thread.
- **Scheme:** the per-form eval, error capture, completion and lookup, in a new
  `runtime/` file. The data they need (`owlet`, the xtlang caches,
  `xtm-closure-doc` in `runtime/xtc-bind.xtm`) lives on the Scheme side.
- **Process lifetime:** a client that disconnects mid-eval doesn't cancel the
  eval; its responses are dropped. `(quit)` sent over nREPL closes every
  connection cleanly. A client that stops reading has its connection closed
  once its outbound queue is full, so it can never block a task thread.

## Batch mode and tests

`--batch` stays as it is: one process, no server, exits when the script quits.
Its main wart is separate and small: an uncaught error at the top level leaves
the process running, so scripted use needs `timeout`. With structured errors in
place, batch mode can exit non-zero on the first uncaught error instead.

nREPL is a better interface than `--batch` for tools and agents that drive
Extempore: start one `--noaudio --nrepl-port=0` instance, send evals, and get a
deterministic `done` per request, structured errors and `interrupt` instead of a
killed process, with libraries staying loaded between evals.

The `.xtm` test suites keep running one fresh process per file under ctest,
since that isolation is worth more than the startup time a shared instance
would save. A developer can still re-run one test file in a running instance
with `load-file`.

## Testing the server

A ctest label `nrepl`, driven by a Python client using only the standard
library (bencode is a few dozen lines), each test starting its own extempore
on a free port. Run under the `sanitize-asan` and `sanitize-tsan` presets as
well as the default build; the server thread and the outbound queues are where
TSan will earn its keep.

Expected behaviour:

- `describe` lists exactly the implemented ops; an unknown op gets `unknown-op`
- a multi-form eval returns one `value` per form, in order, then one `done`
- `out` from `println` arrives tagged with the right id, before that request's
  `done`
- each error kind returns the documented keys, and the next eval in the same
  session succeeds without a growing stack (as `tests/core/error-reporting.xtm`
  checks today)
- `file`/`line` move read-error and runtime-error locations
- the existing 7099 server behaves as before with the nREPL server running
  (`tests/core/system.xtm` keeps passing)

Off the golden path:

- a client disconnects mid-eval: the eval finishes, the server neither crashes
  nor blocks, and other clients are unaffected
- a client reconnects: its old sessions are gone (`unknown-session`), new ones
  work, and definitions made earlier are still in the process
- several clients at once, and several sessions on one connection: replies and
  `out` go only to the requester, including while evals queue on one process
- primary and utility sessions evaluate concurrently with each other
- `interrupt` on a running eval, on an idle session, with a wrong
  `interrupt-id`, and on an unknown session
- an eval that passes its time limit
- malformed bencode, a frame split across many TCP reads, several frames in one
  read, a message over the size limit, and non-UTF-8 bytes in `code`: the bad
  connection gets an error or is closed, and the server keeps serving others
- a client that never reads its replies while an eval prints heavily: its
  connection is closed, and the task thread keeps going
- a request that arrives before the runtime has finished loading is answered
  once it has
- the requested port is in use: extempore reports it and exits non-zero; port 0
  writes `.nrepl-port` and removes it on exit
- `(quit)` over nREPL with other clients connected

Separately, run `proof` against the server now and then as a compatibility
check against what real clients expect. It needs Clojure tooling, so it isn't
part of CI.

## Phases

1. Structured xtlang diagnostics, `print`/`println` through s7 ports, batch mode
   exiting non-zero on an uncaught error. Useful on their own.
2. The nREPL server with `describe`, sessions, `eval`, `load-file` and
   `interrupt`, its test suite and docs.
3. `completions` and `lookup`.
4. `extempore/log-subscribe`.
5. The language server, as its own project.

## Open questions

- whether eval stops at the first error or continues (current behaviour
  continues)
- the default for `--nrepl-port`: off with an explicit port, as proposed, or on
  by default at a fixed port for live performance
- whether `.nrepl-port` in the working directory is right for Extempore, which
  is often started from somewhere other than the project being edited
- the shape of `lookup`'s `info`, which the spec leaves open; follow what
  cider-nrepl's `info` returns where it makes sense
