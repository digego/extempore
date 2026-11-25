#pragma once

// Finding top-level forms in Extempore source text, for the clients which
// evaluate "the form at the cursor" (extempore send, extempore lsp) and the
// REPL's "is this input complete?". It follows s7's reader for what hides a
// parenthesis: strings, ; comments, #| block comments |# and #\ character
// literals.

#include <cstddef>
#include <optional>
#include <string_view>
#include <vector>

namespace extemp::forms {

struct Form {
    std::size_t begin = 0;  // offset of its opening parenthesis
    std::size_t end = 0;    // one past its closing parenthesis
    std::size_t line = 1;   // the line it starts on, from 1
    bool closed = true;     // false for a last form which runs to the end unterminated
};

// Each top-level list in Text, in order.
std::vector<Form> topLevel(std::string_view Text);

// The offset of a line and column, both from 1, the column counted in
// characters (code points). Out-of-range values are clamped.
std::size_t offsetOf(std::string_view Text, std::size_t Line, std::size_t Column);

// The form at Offset: the one enclosing it, else the nearest one before it,
// else the first one after it. Empty if Text has no forms.
std::optional<Form> at(std::string_view Text, std::size_t Offset);

// False while Text ends inside a list, a string or a block comment: a REPL
// reads another line.
bool complete(std::string_view Text);

// Something a reader would trip on: a closing parenthesis with nothing open,
// or a list, string or block comment which never ends.
struct Problem {
    std::size_t offset = 0;  // where it is (for an unterminated thing, where it starts)
    std::string_view message;
};
std::vector<Problem> problems(std::string_view Text);

// A top-level form which names something: (bind-func name ...), (define name
// ...), (define (name args) ...), (bind-lib library name ...) and the like.
struct Definition {
    Form form;
    std::string_view head;   // "bind-func", "define", ...
    std::string_view name;   // without any :type which follows it
    std::size_t nameOffset = 0;
};
std::vector<Definition> definitions(std::string_view Text);

}  // namespace extemp::forms
