---
title: Editor support
---

As discussed in the [quickstart](/overview/quickstart/#editor-setup) there are
Extempore plugins for several popular editors. This page shows how to set things
up and lists any editor-specific instructions (e.g. the specific names of the
commands & keybindings).

:::note

If you don't have a favourite text editor, or don't really know what a text
editor is, then that's ok! [VSCode](#vscode) is probably the text editor for
you.

:::

## VSCode {#vscode}

[Visual Studio Code](https://code.visualstudio.com/), which is usually referred
to as just **VSCode**, is a cross-platform text editor from Microsoft. For a
general introduction to VSCode, check out the excellent
[docs](https://code.visualstudio.com/docs).

VSCode has an
[Extempore extension](https://github.com/extemporelang/vscode-extempore), so it
knows how to work with Extempore code. The VSCode setup instructions are
actually listed in the [quickstart](/overview/quickstart/#editor-setup) guide
already, so you should head over there and get started.

## Helix {#helix}

[Helix](https://helix-editor.com) talks to Extempore through its
[language server](#language-server), which is part of the `extempore` binary.
In `~/.config/helix/languages.toml`:

```toml
[language-server.extempore]
command = "extempore"
args = ["lsp"]

[[language]]
name = "extempore"
scope = "source.extempore"
file-types = ["xtm"]
comment-token = ";"
language-servers = ["extempore"]
auto-format = true
```

`auto-format` [formats](#formatting) each file as it's saved. For syntax
highlighting, add the
[tree-sitter grammar](https://github.com/extemporelang/tree-sitter-extempore#helix)
as well. Then <kbd>Space</kbd> <kbd>a</kbd> offers "Evaluate top-level form" and
"Evaluate buffer". For one key each, add this to `config.toml`:

```toml
[keys.normal.","]
e = ':lsp-workspace-command extempore.eval {"uri":"file://%{file_path_absolute}","range":{"start":{"line":%sh{echo $((%{cursor_line}-1))},"character":0},"end":{"line":%sh{echo $((%{cursor_line}-1))},"character":0}}}'
b = ':lsp-workspace-command extempore.evalBuffer {"uri":"file://%{file_path_absolute}"}'
i = ':lsp-workspace-command extempore.interrupt'
```

<kbd>,</kbd> <kbd>e</kbd> evaluates the top-level form the cursor's line is in
(the server counts lines from 0 and Helix from 1, which is what the `%sh{}` is
for), <kbd>,</kbd> <kbd>b</kbd> the whole buffer and <kbd>,</kbd> <kbd>i</kbd>
interrupts an evaluation. The buffer is evaluated as it stands, saved or not.
Values and the first line of output appear in the status line, and an error is
marked on its form until the form is edited or evaluates cleanly. Completion
and hover (<kbd>Space</kbd> <kbd>k</kbd>) work as for any language. The bindings
take over Helix's <kbd>,</kbd> (keep primary selection); pick another prefix if
you use it. On Windows the path in `%{file_path_absolute}` isn't valid inside
the JSON, so use the <kbd>Space</kbd> <kbd>a</kbd> menu there.

## Any editor with a language client {#language-server}

`extempore lsp` is a [language server](https://microsoft.github.io/language-server-protocol/):
point your editor's language client at that command for `.xtm` files. It is a
client of a running extempore's [nREPL server](/guides/nrepl/), which it finds
as `extempore send` does (`.nrepl-port`, or 7888); the `initializationOptions`
`host`, `port` and `process` say otherwise.

Without a running extempore it reports unbalanced parentheses (when a file is
opened or saved), gives an outline of the definitions in a file and folding
ranges, and [formats](#formatting) a document or a range of its lines. With
one, it completes names (Scheme's and xtlang's), shows a function's arguments,
type and docstring on hover, and adds three commands, which are also offered as
code actions:

| command                | argument       | what it does                                                         |
| ---------------------- | -------------- | -------------------------------------------------------------------- |
| `extempore.eval`       | `{uri, range}` | evaluates the selection, or with none the top-level form at the cursor |
| `extempore.evalBuffer` | `{uri}`        | evaluates the whole buffer                                           |
| `extempore.interrupt`  |                | stops the evaluation which is running                                |

The range is an LSP range: lines and characters from 0. Values and the first
line of output come back as `window/showMessage`, the whole output as
`window/logMessage`, and an error as a diagnostic on the first line of its form.

A client which sends `"evalResults": "response"` in its `initializationOptions`
gets each evaluation's result as the answer to its `workspace/executeCommand`
request, sent when the evaluation finishes, in place of the two messages:
`values` (one per form), `out` (what it printed), `interrupted`, and when it
failed `error` (`message`, and `kind` and `line` where extempore gives them).
`range` is where the code came from: the selection or form evaluated, or for
`extempore.evalBuffer` the form which failed. Errors are still marked as
diagnostics. This suits an editor which shows results beside the code, as the
[VSCode extension](#vscode) does.

An editor which can only run a shell command can use
[`extempore send`](/guides/nrepl/#from-the-command-line): pipe the selection to
it, or give it `--file` and `--line`.

## Formatting {#formatting}

`extempore fmt` formats `.xtm` files: `extempore fmt FILE` or `extempore fmt DIR`
rewrites them in place, `--check` lists the ones which aren't formatted (and
exits 1 if there are any), and with no files it formats stdin to stdout. It
changes whitespace only, and indents as Racket and Clojure code is indented. A
call's arguments align with the first one when it's on the head's line, and
with the head when it isn't; a body form's body (`begin`, `cond`, `lambda`,
`let`, `when`, a `bind-func`, and anything named `define…`, `def-…` or
`with-…`) indents two columns; data (a quoted list, a vector, a list not headed
by a symbol) aligns with its first element. Whitespace at the ends of lines and of
the file goes. Line breaks stay where they are, and so do the lines of a string
or a block comment. The Extempore source tree is formatted this way, and CI
checks that it stays so.

The language server formats too, so an editor with a language client can format
on save ([Helix](#helix) with `auto-format = true`); in [Emacs](#emacs) it's
<kbd>M-x</kbd> `extempore-format-buffer`, or set `extempore-format-on-save`. The
editors' own indentation (Emacs's extempore-mode, and Helix's through
tree-sitter-extempore) follows the same rules as you type.

## Emacs {#emacs}

[extempore-emacs-mode](https://github.com/extemporelang/extempore-emacs-mode)
(Emacs 29.1 or later) is on [MELPA](https://melpa.org/): <kbd>M-x</kbd>
`package-install` <kbd>RET</kbd> `extempore-mode` <kbd>RET</kbd>. It starts
Eglot with the [language server](#language-server), so it finds a running
extempore as `extempore send` does, and shows each result at the end of its
form. Completion, eldoc and errors marked as diagnostics come from the server.

| command                                                      | keybinding         |
| ------------------------------------------------------------ | ------------------ |
| `extempore-send-dwim` (the region, or the top-level form)    | <kbd>C-M-x</kbd>   |
| `extempore-send-buffer`                                      | <kbd>C-c C-k</kbd> |
| `extempore-interrupt`                                        | <kbd>C-c C-b</kbd> |
| `switch-to-extempore` (or start Extempore if it isn't running) | <kbd>C-c C-z</kbd> |
| `extempore-repl` (`extempore repl` in a buffer)              | <kbd>C-c C-e</kbd> |
| `extempore-format-buffer` (with `extempore fmt`)             |                    |

For an extempore older than the nREPL server, set `extempore-transport` to
`legacy` and connect to its eval server with <kbd>C-c C-j</kbd>. The mode's
README has the rest.

## Sublime Text {#sublime-text}

Extempore's [Sublime Text](https://www.sublimetext.com) plugin is available from
[GitHub](https://github.com/benswift/extempore-sublime).

The plugin's commands and key bindings are documented in its
[own repo](https://github.com/benswift/extempore-sublime).

### Known issues {#known-issues}

The syntax highlighting currently doesn't cover a few edge cases---so if you end
up tinkering with `Extempore.JSON-tmLanguage` to fix anything then feel free to
submit a pull request.

## Vim {#vim}

Extempore's [vim plugin](https://github.com/timburgess/extempore.vim) is written
by Tim Burgess.

| command                           | keybinding                    |
| --------------------------------- | ----------------------------- |
| open connection to Extempore      | <kbd>LEADER</kbd><kbd>o</kbd> |
| close connection to Extempore     | <kbd>LEADER</kbd><kbd>x</kbd> |
| send enclosing block to Extempore | <kbd>LEADER</kbd><kbd>w</kbd> |
| send selection to Extempore       | <kbd>LEADER</kbd><kbd>s</kbd> |
| send entire file to Extempore     | <kbd>LEADER</kbd><kbd>a</kbd> |

The vim mode doesn't yet support multiple connections or user-specified
host/port, but pull requests are welcome.
