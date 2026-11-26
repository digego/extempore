#pragma once

// Formatting Extempore source (extempore fmt, and the language server's
// formatting requests). It changes whitespace only: each line's indentation,
// and whitespace at the ends of lines and of the file. Line breaks stay where
// they are, as does every line which begins inside a string or a block
// comment.
//
// The indentation follows the Lisp convention (Emacs's lisp-indent): a call's
// arguments align with the first one, a special form's body indents two
// columns, and data aligns with its first element. Which symbols are special
// forms is the table in Formatter.cpp; extempore-emacs-mode and
// tree-sitter-extempore's indents.scm keep copies of it.

#include <expected>
#include <string>
#include <string_view>

namespace extemp::format {

// The formatted text, or why the text can't be formatted (it doesn't read).
std::expected<std::string, std::string> format(std::string_view Text);

}  // namespace extemp::format
