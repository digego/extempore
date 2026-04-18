---
title: Editor support
---

As discussed in the [quickstart](../overview/quickstart.md#editor-setup) there are Extempore plugins for several popular editors. This
page shows how to set things up and lists any editor-specific instructions (e.g.
the specific names of the commands & keybindings).

::: info
If you don't have a favourite text editor, or don't really know what a text
editor is, then that's ok! [VSCode](#vscode) is probably the text editor for
you.
:::

## VSCode {#vscode}

[Visual Studio Code](https://code.visualstudio.com/), which is usually referred
to as just **VSCode**, is a cross-platform text editor from Microsoft. For a
general introduction to VSCode, check out the excellent
[docs](https://code.visualstudio.com/docs).

VSCode has an [Extempore
extension](https://github.com/extemporelang/vscode-extempore), so it knows how
to work with Extempore code. The VSCode setup instructions are actually listed
in the [quickstart](../overview/quickstart.md#editor-setup) guide
already, so you should head over there and get started.

## Emacs {#emacs}

[extempore-emacs-mode](https://github.com/extemporelang/extempore-emacs-mode) is
the Emacs mode for working with Extempore code, and it's available from
[MELPA](http://melpa.org/). If you're using `package.el` (i.e. if you're on a
modern-ish Emacs) then just <kbd>M-x</kbd> `package-install` <kbd>RET</kbd>
`extempore-mode` <kbd>RET</kbd> and you're done.

If you encounter an error with `eldoc-beginning-of-sexp`, put this code in
your init file to bind that name
```
(unless (fboundp 'eldoc-beginning-of-sexp)
  (defalias 'eldoc-beginning-of-sexp 'elisp--beginning-of-sexp))
```

If you don't want to get it from MELPA, just [download the
file](https://github.com/extemporelang/extempore-emacs-mode/blob/master/extempore-mode.el)
and put it in your load path.

[Ben's](https://benswift.me) a Spacemacs user these days, and has created [an
Extempore
layer](https://github.com/benswift/.dotfiles/blob/master/spacemacs-layers/extempore/packages.el)
(although he hasn't got around to getting it accepted in the main spacemacs
layer repo or however that works, so you'll need to do a bit of downloading &
manual setup stuff to get it working).

| command                                                              | keybinding                                                                                  |
|----------------------------------------------------------------------|---------------------------------------------------------------------------------------------|
| `switch-to-extempore` (or start Extermpore if it's not running)      | <kbd>CTRL</kbd>+<kbd>C</kbd> <kbd>CTRL</kbd>+<kbd>Z</kbd> |
| `extempore-connect-or-disconnect`                                    | <kbd>CTRL</kbd>+<kbd>C</kbd> <kbd>CTRL</kbd>+<kbd>J</kbd> |
| `extempore-send-dwim` (to evaluate current top-level form or region) | <kbd>CTRL</kbd>+<kbd>META</kbd>+<kbd>X</kbd>                               |
| `extempore-send-region`                                              | <kbd>CTRL</kbd>+<kbd>C</kbd> <kbd>CTRL</kbd>+<kbd>R</kbd> |
|                                                                      |                                                                                             |

To restart the Extempore process, just <kbd
class="nopretty">CTRL</kbd>+<kbd>C</kbd> <kbd
class="nopretty">CTRL</kbd>+<kbd>C</kbd> in the `*extempore*` buffer where
`extempore` is running to kill it, then start it up again with
`switch-to-extempore`.

## Atom {#atom}

Extempore's [Atom](https://atom.io) package is available from
[GitHub](https://github.com/extempore-lang/extempore-atom).

| command                | keybinding                  |
|------------------------|-----------------------------|
| `Extempore Connect`    | <kbd>ALT</kbd>+<kbd>O</kbd> |
| `Extempore Disconnect` | <kbd>ALT</kbd>+<kbd>X</kbd> |
| `Extempore Evaluate`   | <kbd>ALT</kbd>+<kbd>S</kbd> |

## Sublime Text {#sublime-text}

Extempore's [Sublime Text](https://www.sublimetext.com) plugin is available from
[GitHub](https://github.com/benswift/extempore-sublime).

| command                | keybinding |
|------------------------|------------|
| `extempore_connect`    |            |
| `extempore_disconnect` |            |
| `extempore_evaluate`   |            |

### Known issues {#known-issues}

The syntax highlighting currently doesn't cover a few edge cases---so if you end
up tinkering with `Extempore.JSON-tmLanguage` to fix anything then feel free to
submit a pull request.

## Vim {#vim}

Extempore's [vim plugin](https://github.com/timburgess/extempore.vim) is written
by Tim Burgess.

| command                           | keybinding    |
|-----------------------------------|---------------|
| open connection to Extempore      | <kbd>LEADER</kbd><kbd>o</kbd> |
| close connection to Extempore     | <kbd>LEADER</kbd><kbd>x</kbd> |
| send enclosing block to Extempore | <kbd>LEADER</kbd><kbd>w</kbd> |
| send selection to Extempore       | <kbd>LEADER</kbd><kbd>s</kbd> |
| send entire file to Extempore     | <kbd>LEADER</kbd><kbd>a</kbd> |

The vim mode doesn't yet support multiple connections or user-specified
host/port, but pull requests are welcome.
