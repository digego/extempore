---
id: doc-002
title: nREPL server design
type: specification
created_date: "2026-11-25 09:30"
---

# nREPL server design

Extempore runs an [nREPL](https://docs.nrepl.org) server beside the original
eval server on 7099/7098, which is unchanged. Clients get request ids, a value
per form, errors with their kind and line, streamed output and interrupt. The
user-facing description is `docs/src/content/docs/guides/nrepl.md`.

## Why nREPL

LSP connects an editor to tooling about documents and has no notion of a
running program; nREPL connects any number of clients to a running program.
Clojure tooling uses both (clojure-lsp for analysis, nREPL for evaluation), and
so does this design. nREPL already defines ids, sessions, streaming and
capability discovery, its bencode wire format needs no dependency, and its
messages are open dictionaries, so Extempore-specific data travels as
namespaced keys. Where the draft spec and real clients disagree, the server
follows [proof](https://github.com/nrepl/proof)'s `doc/spec-changes.md`.

## Clients

- **helix, and editors that can run a shell command:**
  `extras/nrepl/extempore-nrepl` now, a language server later
- **VS Code:** its extension can speak nREPL over TCP directly (as Calva does
  for Clojure) or use the language server
- **coding agents:** `extempore-nrepl` through their shell tool. It prints
  values, output and errors as text and exits 1 on an error. An MCP wrapper
  would only help once agents need sessions or interrupt

The language server is for editors. Testing the helix binding showed the limits
of shell-command integration: the result sits in a popup until Esc, errors
can't be shown in place, and the client reads the form from disk, so the binding
saves first and has to wait for helix's asynchronous `:write`. A language server
has the buffer's contents and gives helix, VS Code, Neovim and Emacs completion,
hover and diagnostics from one implementation, as a client of nREPL.

A human and an agent can share one extempore. Sessions keep replies apart, not
state: definitions are global to a process, and evals on one process queue. An
agent working beside a performance should use the utility process or its own
(`ipc:new`).

## Protocol decisions

- on by default at `127.0.0.1:7888`; a taken default port is a warning, a taken
  `--nrepl-port` is fatal. `.nrepl-port` is written only for `--nrepl-port=0`,
  since extempore is often started away from the project being edited
- one server per extempore process, none in `--batch`, nothing logged per
  connection
- a session belongs to one Scheme process (`extempore/process` at `clone`).
  Evals on a process run one at a time whichever session sent them; sessions
  on different processes run concurrently
- eval stops at the first error, as the reference server does and proof
  requires (the 7099 server carries on)
- `value`, `out` and `err` go in separate messages; every response carries `id`
  and `session`; every request gets exactly one `done`, sent by C++ so that an
  interrupted or timed-out eval still ends cleanly
- `extempore/error` carries `kind`, `message`, `form`, `file` and `line`. `line`
  is where the top-level form starts; xtlang diagnostics can't point at a
  sub-form because the compiler keeps no source positions
- no `ns`: Extempore has no namespaces
- the console shows everything it did before; the requester also gets its own
  output as `out`, without colour codes. Output from callbacks and compiled
  xtlang goes to the console only
- interrupt stops Scheme code, including the xtlang compiler, at s7's
  `begin_hook`; it can't stop compiled xtlang or LLVM codegen

## Phases

1. Typed xtlang diagnostics, `print` through s7 ports, `--batch` exiting 1 on an
   uncaught error. Done.
2. The server (`describe`, sessions, `eval`, `load-file`, `interrupt`), the
   command-line client, tests (`ctest -L nrepl`) and docs. Done.
3. `completions` and `lookup`.
4. `extempore/log-subscribe`: a process's console output, for a client to watch
   what callbacks print.
5. The language server, as `extempore lsp` (doc-003). Its eval needs only
   phase 2, so it comes before phase 3; completion and hover follow phase 3.

Run [proof](https://github.com/nrepl/proof) against the server now and then; it
needs Clojure tooling, so it isn't in CI.

## Open questions

- interrupting from the command line, where each `eval` has an ephemeral
  session: an `interrupt` naming a process instead of a session?
- the shape of `lookup`'s `info`: follow cider-nrepl's (`arglists-str`, `doc`,
  `file`, `line`) plus the xtlang type
