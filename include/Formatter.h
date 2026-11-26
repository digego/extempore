#pragma once

// Formatting Extempore source (extempore fmt, and the language server's
// formatting requests). It changes whitespace only: each line's indentation,
// and whitespace at the ends of lines and of the file. Line breaks stay where
// they are, as does every line which begins inside a string or a block
// comment.
//
// The rules are Racket's and Clojure's (racket-mode, clojure-mode, cljfmt): a
// call's arguments align with the first one when it shares the head's line,
// and with the head when it doesn't; a body form's body (begin, cond, lambda,
// let, a definition) indents two columns; data (a quoted list, a vector, a
// list not headed by a symbol) aligns with its first element. Which symbols
// head body forms is the table and the name prefixes in Formatter.cpp;
// extempore-emacs-mode and tree-sitter-extempore's indents.scm follow them.

#include <expected>
#include <string>
#include <string_view>

namespace extemp::format {

// The formatted text, or why the text can't be formatted (it doesn't read).
std::expected<std::string, std::string> format(std::string_view Text);

}  // namespace extemp::format
