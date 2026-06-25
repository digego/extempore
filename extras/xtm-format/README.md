# Formatting xtlang / Scheme `.xtm` files

All `.xtm` source in this tree is indented with a single canonical model so that
the three editor integrations we maintain agree on it and never fight a saved
file:

- **Emacs** --- `extempore-mode`'s `extempore-indent-function` (a copy of
  `lisp-indent-function`): a `def`-prefixed head indents like `defun`, the
  explicit `(put 'X 'extempore-indent-function N)` table gives the rest, and
  everything else aligns arguments under the first operand.
- **Helix** (and any tree-sitter editor) --- `queries/indents.scm` in
  [tree-sitter-extempore](https://github.com/extemporelang/tree-sitter-extempore),
  built on the same scmindent model Helix's bundled Scheme query uses.
- **batch / one-off reformat** ---
  [`scmindent`](https://github.com/ds26gte/scmindent) by Dorai Sitaram,
  configured with `lispwords.json` here.

The `extras/tree-sitter-extempore` submodule is pinned here as a tooling
dependency of this workflow: it supplies both the grammar Helix loads for the
query above and the parser used as the non-destructiveness oracle below. (Editor
integrations are otherwise not vendored --- they're linked from the
editor-support guide; this grammar is the exception because the formatter itself
consumes it.)

The keyword set is identical across all three: `lispwords.json` mirrors the
Emacs `put`-table one-for-one, and the `indents.scm` regex is the same set
anchored exact-match (plus the `def` prefix). `set!` is intentionally not in the
set, so it operand-aligns --- matching Emacs.

## Reformatting the tree

`scmindent` is not vendored (it declares no licence). Fetch it, apply the
one-line fix below, and run it with this config:

```bash
# fetch
curl -fsSLO https://raw.githubusercontent.com/ds26gte/scmindent/master/scmindent.js

# scmindent's keyword lookup `if (n = lispKeywords[s])` rejects a value of 0,
# so defun-style forms (bind-func, bind-type, ...) need this fix:
sed -i 's/if (n = lispKeywords\[s\]) {/if ((n = lispKeywords[s]) !== undefined) {/' scmindent.js

# format a file in place
LISPWORDS=extras/xtm-format/lispwords.json node scmindent.js < FILE > FILE.tmp && mv FILE.tmp FILE
```

The reformat is whitespace-only. To prove a run is non-destructive, parse the
file with the tree-sitter grammar before and after and confirm the node tree
(positions stripped) is identical with zero `ERROR`/`MISSING` nodes.
