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
outside the editor through shell commands and language servers. It gets a
command-line client in this repo now (see [Editor client](#editor-client)), and
later a language server:

```
helix ⇄ (LSP) extempore language server ⇄ (nREPL) extempore
```

The language server is a separate project and out of scope here, beyond the
requirements it puts on the protocol.

## The existing eval server

- `SchemeProcess::serverImpl` (`src/SchemeProcess.cpp`) accepts raw text
  terminated by CRLF and queues a `REPL` task. The task thread evaluates it and
  `send()`s one NUL-terminated reply straight to the client socket.
- The reply is the printed value, or on error the contents of a 256-byte buffer
  (`SCHEME_OUTPORT_STRING_LENGTH`, `include/SchemeProcess.h`), so long errors are
  truncated.
- Replies have no id, there is no way to tell which form of a multi-form send
  produced what, and output printed during the eval goes to the console only.
- Every connect and disconnect is logged on the console, so a client that
  connects per eval (as a shell command from an editor must) fills the console
  with noise. Ben's `xtm-send` keeps a background relay alive to avoid it.
- It binds `INADDR_ANY`: any host on the network can evaluate code. Fixing that
  is a separate change; the nREPL server binds loopback.

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
Lisp), and the docs'
[Building Servers](https://docs.nrepl.org/nrepl/building_servers.html) page and
the [proof](https://github.com/nrepl/proof) compatibility suite cover this case.
Where the draft spec and real clients disagree, this design follows proof's
`doc/spec-changes.md`, which sides with the clients.

## Protocol

### Transport and startup

- bencode over TCP, bound to `127.0.0.1` unless `--nrepl-bind` says otherwise
- on by default at port 7888, so plain `extempore` is ready for an editor.
  `--nrepl-port=N` moves it, `--nrepl-port=0` picks a free port, `--no-nrepl`
  turns it off
- if the default port is taken (usually by a second extempore), print a warning
  and run without the server; if a port given with `--nrepl-port` is taken,
  report it and exit non-zero
- on startup, print the reference server's banner line
  (`nREPL server started on port N on host 127.0.0.1 - nrepl://127.0.0.1:N`),
  which some clients parse
- write `.nrepl-port` to the working directory only for `--nrepl-port=0`, and
  remove it on exit. Extempore is often started from somewhere other than the
  project being edited, and with a fixed default port clients don't need the
  file; a random port is the one case where they do
- one server for the whole extempore process, not one per Scheme process
- not started in `--batch` mode, which has no server today
- nothing is logged per connection

### Sessions and processes

A session belongs to one Scheme process, chosen at `clone` with the key
`extempore/process` (`"primary"` by default; any live process name, so
`"utility"` or one made by `ipc:new`). Requests without a session get an
ephemeral one on the primary process, as the spec allows. Every response
carries the request's `id` and a `session` (the ephemeral id for sessionless
requests), since Calva ignores responses without one.

The spec expects sessions to evaluate concurrently. Extempore cannot: each
Scheme process has one task thread, so evals on the same process queue behind
each other whatever session they come from. Sessions on different processes do
run concurrently. `describe` documents this, and `interrupt` is the escape
hatch.

### Ops

| op                         | phase | notes                                                                                                          |
| -------------------------- | ----- | -------------------------------------------------------------------------------------------------------------- |
| `describe`                 | 2     | `ops` (a map, which CIDER and Calva need), `versions` (`nrepl`, `extempore` with `version-string`), `aux`      |
| `clone`, `close`           | 2     | `new-session`; `close` answers `session-closed`; sessions close when the connection that cloned them closes    |
| `ls-sessions`              | 2     |                                                                                                                |
| `eval`                     | 2     | `code`, optional `file`, `line`, `column`; see below                                                           |
| `load-file`                | 2     | `file` is the contents; `file-path` sets the location; one `value`, the last form's                            |
| `interrupt`                | 2     | `interrupted` on the interrupted request; `session-idle`, `interrupt-id-mismatch`, `unknown-session` otherwise |
| `completions`              | 3     | Scheme symbols plus xtlang closures, types and aliases, in one message with `done`                             |
| `lookup`                   | 3     | docstring, xtlang type signature; `no-info` when there's nothing                                               |
| `extempore/log-subscribe`  | 4     | streams the process's console output to the connection                                                         |
| anything else, incl. stdin | ---   | `status: [error unknown-op done]` with the `op`; evaluated code never reads stdin                              |

A request naming a session that doesn't exist gets `unknown-session`, an eval
without `code` gets `no-code`, and a handler that fails still sends `done`
(with `server-error`). Every request gets exactly one `done`.

### Eval

`code` is read form by form, as `sys:eval-string` does
(`runtime/init.xtm`). Each form that evaluates gets a `value` message, its
result printed with s7's `object->string`. Evaluation stops at the first error,
as the reference server does and proof requires; the 7099 server continues, but
nREPL clients expect a single `eval-error` per request. A single `done` follows
once the request finishes.

`value`, `out` and `err` travel in separate messages, since clients treat them
as mutually exclusive.

An error sends an `err` message with the text `sys:error-report-string`
produces (`runtime/init.xtm`), so generic clients show something useful, then a
message with:

- `ex` and `root-ex`: the error type, e.g. `wrong-type-arg` or
  `xtlang-type-error`
- `status: ["eval-error"]`
- `extempore/error`: a dictionary with `kind`, `message`, `form` (printed,
  truncated), `file` and `line`

`line` moves the reader's starting line (s7's `port-line-number` has a setter),
and `file` names the source, so errors point at the source instead of line 0 of
a string. s7 tracks no columns, so `column` is accepted and ignored. The line
reported for a form is the line it starts on: s7 records no line on pairs read
from a string port, so the eval loop skips whitespace and comments itself
before each `read`. Errors are reported against the top-level form. xtlang
diagnostics can't point at a sub-form because the compiler doesn't carry source
positions through its transforms; adding that is a non-goal for now.

Responses carry no `ns`: Extempore has no namespaces, and the spec doesn't
define it.

### Output

The console stays as it is today: everything an nREPL eval prints, and every
error it raises, still appears there. The requester additionally gets its own
output as `out` messages. A live coder's console is the session log, often on a
projector, and it shouldn't go quiet because the code came over nREPL.

The Scheme `print` and `println` are C functions that `printf` to stdout
(`src/ffi/misc.cpp`), bypassing s7's ports. Phase 1 changes them, and the ANSI
colour codes `print-with-colors` emits, to write to s7's current output port,
whose default (`*stdout*`) writes to stdout, so the console sees no difference.
During an nREPL eval the task thread binds the current output port to a tee
port (`s7_open_output_function`): every character goes to stdout as before, and
a copy with ANSI escapes stripped is buffered and sent as `out` at each newline
and before the request's next `value`, `err` or `done`.

Output that doesn't come from Scheme code running inside a request --- `printf`
from JIT-compiled xtlang, scheduler callbacks, audio-thread messages --- stays
on the console only. Phase 4's log subscription delivers it.

### Interrupt

`begin_hook` (`src/SchemeS7.cpp`) already abandons an eval that passes its time
limit. Interrupt adds an atomic on the scheme wrapper that the same hook checks:
it holds the serial number of the request to stop, and the hook compares it
with the serial of the request running, so an interrupt that arrives as one
request finishes can't stop the next. That covers Scheme code, including the
xtlang compiler itself, which is written in Scheme. It cannot stop JIT-compiled
code or LLVM codegen already under way; the reply arrives when control returns
to Scheme.

An abandoned eval never returns to the Scheme eval loop, so the C++ task
handler owns the end of every request: it sends `interrupted` (or, past the
time limit, an `err` and `eval-error`) and then `done`. Without an
`interrupt-id`, `interrupt` stops the session's running request, which Conjure
relies on. Evals keep the process's existing time limit (`getMaxDuration`).

## Structured xtlang diagnostics (phase 1)

The `xtc:diagnostics:print-*-error` functions in `runtime/xtc-globals.xtm` (27
of them raise; about 280 call sites use them) print a coloured report and then
call `xtc:diagnostics:throw-or-quit`, which raises `(error "")`. Whatever
catches it gets an empty error; the information only exists as console text.
On the console the report is followed by a `;` block that shows the internals of
`throw-or-quit` as the offending form.

Change the functions, not the call sites. Each prints exactly the same console
report and then raises an error whose type names the kind
(`xtlang-syntax-error`, `xtlang-type-error`, `xtlang-generics-error`,
`xtlang-compile-error`, `xtlang-error`), with the report as plain text for its
message. The report goes to the console only, not to the current output port,
so an nREPL client receives it once, as `err`, instead of also as `out`. The
explanation lines the solver prints just before the report (`runtime/xtc-solve.xtm`,
"no overload matches ...") are the most useful part, so they become part of the
message too. The console's `;` block after an xtlang error drops the message it
has already printed.

`catch` in `runtime/init.xtm` maps every error to a value, so most handlers are
indifferent to the type; audit the rest before changing it.

This is useful before any nREPL work: `xtmtest-compile-fails`
(`libs/core/test.xtm`) currently passes on any error, including a typo in the
test, and can assert on the kind instead.

## Implementation

- **C++:** a new `NreplServer` (its own source and header) with its own thread
  and a `poll` loop like `serverImpl`'s, a bencode codec (unit tested in
  `tests/cpp-unit`), and connection and session tables. Ops that need Scheme go
  to the session's process as a new `SchemeTask::Type` carrying the request.
- **Outbound:** each connection has a mutex-guarded outbound buffer and a
  non-blocking socket. A task thread appends a response and writes what the
  socket takes at once; the server thread drains any remainder when `poll`
  reports `POLLOUT`. Replies go out without waiting for the server thread's
  poll timeout, no wake-up pipe is needed, and a task thread never blocks on a
  client. A connection whose buffer passes its limit is closed.
- **Scheme:** the per-form eval loop, error capture, completion and lookup, in
  a new `runtime/nrepl.xtm`. It hands each response to C++ as an association
  list, which C++ encodes. The data completion and lookup need (`rootlet`, the
  xtlang caches, `xtm-closure-doc` in `runtime/xtc-bind.xtm`) lives on the
  Scheme side.
- **Process lifetime:** a client that disconnects mid-eval doesn't cancel the
  eval; its responses are dropped. `(quit)` sent over nREPL closes every
  connection cleanly.

## Editor client

`extras/nrepl/extempore-nrepl` is a command-line nREPL client in Python using
only the standard library. It is what helix (or any editor that can run a shell
command) uses until the language server exists, a scripting interface for tools
and agents, and the client the test suite drives.

- `eval` takes code from an argument or stdin, or `--file F --line L --column C`
  to evaluate the top-level form at that position (the one enclosing it, else
  the nearest before it), so an editor needn't select the form first
- the port comes from `--port`, then `$EXTEMPORE_NREPL_PORT`, then a
  `.nrepl-port` found by walking up from the file's directory, then 7888
- it prints output, then the value or the error report, in a compact form meant
  for an editor popup or status line

In helix, evaluating the form at the cursor without moving it:

```toml
[keys.normal.","]
e = [":write", ":sh extempore-nrepl eval --file %{file_path_absolute} --line %{cursor_line} --column %{cursor_column}"]
```

`:sh` runs asynchronously, so a slow compile doesn't freeze the editor, and
shows the output in a popup. `:pipe-to` would avoid the `:write` but blocks the
editor until the command exits and discards its output.

## Batch mode and tests

`--batch` stays as it is: one process, no server, exits when the script quits.
Its main wart is separate and small: an uncaught error at the top level leaves
the process running, so scripted use needs `timeout`. Batch mode exits non-zero
on an error in its startup expression instead.

nREPL is a better interface than `--batch` for tools and agents that drive
Extempore: start one `--noaudio` instance, send evals, and get a deterministic
`done` per request, structured errors and `interrupt` instead of a killed
process, with libraries staying loaded between evals.

The `.xtm` test suites keep running one fresh process per file under ctest,
since that isolation is worth more than the startup time a shared instance
would save. A developer can still re-run one test file in a running instance
with `load-file`.

## Testing the server

A ctest label `nrepl`, driven by Python tests that use the editor client's
module, each starting its own extempore on a free port. Run under the
`sanitize-asan` and `sanitize-tsan` presets as well as the default build; the
server thread and the outbound buffers are where TSan will earn its keep.

Expected behaviour:

- `describe` lists exactly the implemented ops; an unknown op gets `unknown-op`
- a multi-form eval returns one `value` per form, in order, then one `done`;
  evaluation stops at the first error
- `out` from `println` arrives tagged with the right id, before that request's
  `done`, without ANSI escapes, and the console still shows it
- each error kind returns the documented keys, and the next eval in the same
  session succeeds without a growing stack (as `tests/core/error-reporting.xtm`
  checks today)
- `file`/`line` move read-error and runtime-error locations
- the existing 7099 server behaves as before with the nREPL server running
  (`tests/core/system.xtm` keeps passing)
- the client finds the right top-level form for a file position, including
  positions inside strings, comments and character literals

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
- the default port in use: a warning, and extempore runs on; an explicit port
  in use: extempore reports it and exits non-zero; port 0 writes `.nrepl-port`
  and removes it on exit
- `(quit)` over nREPL with other clients connected

Separately, run `proof` against the server now and then as a compatibility
check against what real clients expect. It needs Clojure tooling, so it isn't
part of CI.

## Phases

1. Structured xtlang diagnostics, `print`/`println` through s7 ports, batch mode
   exiting non-zero on an uncaught error. Useful on their own.
2. The nREPL server with `describe`, sessions, `eval`, `load-file` and
   `interrupt`, the editor client, the test suite and docs.
3. `completions` and `lookup`.
4. `extempore/log-subscribe`.
5. The language server, as its own project.

## Open questions

- the shape of `lookup`'s `info`, which the spec leaves open; follow what
  cider-nrepl's `info` returns where it makes sense (`arglists-str`, `doc`,
  `file`, `line`), plus the xtlang type
