#pragma once

// extempore lsp: a language server on stdin and stdout. Without a running
// extempore it offers what a parser can (reader problems, an outline,
// folding); with one, it evaluates code through that extempore's nREPL server
// and reports back. backlog/docs/doc-003 has the design.

#include "NreplClient.h"

namespace extemp::lsp {

// Runs until the client says exit; returns the process's exit status.
// Defaults says where to find the extempore, unless the client's
// initializationOptions (host, port, process) say otherwise.
int run(const nrepl::client::Target& Defaults);

}  // namespace extemp::lsp
