#include "Formatter.h"

#include "FormFinder.h"

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <string>
#include <vector>

namespace extemp::format {

namespace {

// How a list headed by a symbol indents the lines after its first.
constexpr int kCall = -1;        // align with the first argument, or with the head
constexpr int kDefinition = -2;  // the body indents two columns

struct Special {
    std::string_view name;
    int distinguished;  // arguments before the body: on their own lines they indent four
};

// extempore-emacs-mode's `put` list and tree-sitter-extempore's indents.scm
// keep copies of this table.
constexpr auto kSpecial = std::to_array<Special>({
    {"begin", 0},
    {"beginz", 0},
    {"delay", 0},
    {"lambda", 1},
    {"lambdaz", 1},
    {"lambdas", 1},
    {"lambdah", 1},
    {"let", 1},  // a named let distinguishes two
    {"let*", 1},
    {"letrec", 1},
    {"letz", 1},
    {"case", 1},
    {"when", 1},
    {"unless", 1},
    {"dotimes", 1},
    {"doloop", 1},
    {"while", 1},
    {"memzone", 1},
    {"let-syntax", 1},
    {"letrec-syntax", 1},
    {"syntax-rules", 1},
    {"call-with-input-file", 1},
    {"with-input-from-file", 1},
    {"with-input-from-port", 1},
    {"call-with-output-file", 1},
    {"with-output-to-file", 1},
    {"with-output-to-port", 1},
    {"call-with-values", 1},
    {"do", 2},
    {"syntax-case", 2},
    {"dynamic-wind", 3},
    {"bind-func", kDefinition},
    {"bind-macro", kDefinition},
    {"bind-poly", kDefinition},
    {"bind-type", kDefinition},
    {"bind-data", kDefinition},
    {"bind-alias", kDefinition},
    {"bind-val", kDefinition},
    {"bind-ext-val", kDefinition},
    {"bind-lib", kDefinition},
    {"bind-lib-val", kDefinition},
    {"bind-dylib", kDefinition},
    {"bind-instrument", kDefinition},
    {"bind-sampler", kDefinition},
    {"macro", kDefinition},
});

int indentKind(std::string_view Head) {
    for (const auto& special : kSpecial) {
        if (special.name == Head) {
            return special.distinguished;
        }
    }
    // define, define-macro, define-syntax, definec, definec:dsp, ... (but not
    // defined? or default)
    if (Head == "define" || Head.starts_with("define-") || Head.starts_with("definec")) {
        return kDefinition;
    }
    return kCall;
}

bool isLet(std::string_view Head) {
    return Head == "let" || Head == "let*" || Head == "letrec" || Head == "letz";
}

enum class Kind : std::uint8_t {
    Symbol,
    Literal,  // a number, string, character, boolean or quoted atom
    List,
};

struct Element {
    std::size_t line;
    std::size_t column;  // in the formatted text, in characters
    Kind kind;
    std::string_view text;  // for a symbol
};

struct Open {
    std::size_t column;  // of its opening parenthesis, in the formatted text
    bool data;           // quoted, or a vector: its elements align with the first
    std::vector<Element> elements;
};

enum class Prefix : std::uint8_t {
    None,
    Quote,     // '
    Template,  // ` and its unquotes , ,@: code
};

bool isSpace(char C) {
    return C == ' ' || C == '\t' || C == '\f' || C == '\v' || C == '\r';
}

bool isDelimiter(char C) {
    return isSpace(C) || C == '(' || C == ')' || C == ';' || C == '"';
}

bool looksNumeric(std::string_view Atom) {
    std::size_t i = 0;
    if (i < Atom.size() && (Atom[i] == '+' || Atom[i] == '-')) {
        ++i;
    }
    if (i < Atom.size() && Atom[i] == '.') {
        ++i;
    }
    return i < Atom.size() && Atom[i] >= '0' && Atom[i] <= '9';
}

// The indentation of a line which starts with First, inside the innermost
// open list.
std::size_t indentation(const std::vector<Open>& Stack, std::string_view First) {
    if (Stack.empty()) {
        return 0;
    }
    const Open& open = Stack.back();
    const std::size_t paren = open.column;
    const auto& elements = open.elements;
    if (elements.empty()) {
        return paren + 1;
    }
    const Element& head = elements.front();
    if (open.data || head.kind != Kind::Symbol) {
        return head.column;
    }
    int kind = indentKind(head.text);
    if (kind == kDefinition) {
        return paren + 2;
    }
    if (kind == kCall) {
        if (elements.size() > 1 && elements[1].line == head.line) {
            return elements[1].column;
        }
        return head.column;
    }
    if (isLet(head.text) && elements.size() > 1 && elements[1].kind == Kind::Symbol) {
        kind = 2;
    }
    const auto distinguished = std::size_t(kind);
    std::size_t onHeadLine = 0;
    for (std::size_t i = 1; i < elements.size() && elements[i].line == head.line; ++i) {
        ++onHeadLine;
    }
    if (onHeadLine > distinguished) {
        return elements[distinguished + 1].column;
    }
    const bool startsElement = First.front() != ')' && First.front() != ';' &&
                               !First.starts_with("#|");
    if (startsElement && elements.size() <= distinguished) {
        return paren + 4;
    }
    return paren + 2;
}

std::size_t characters(std::string_view Text) {
    std::size_t count = 0;
    for (const char c : Text) {
        if ((static_cast<unsigned char>(c) & 0xC0) != 0x80) {
            ++count;
        }
    }
    return count;
}

std::string where(std::string_view Text, std::size_t Offset) {
    std::size_t line = 1;
    std::size_t lineStart = 0;
    for (std::size_t i = 0; i < Offset && i < Text.size(); ++i) {
        if (Text[i] == '\n') {
            ++line;
            lineStart = i + 1;
        }
    }
    return "line " + std::to_string(line) + ", column " +
           std::to_string(characters(Text.substr(lineStart, Offset - lineStart)) + 1);
}

}  // namespace

Result format(std::string_view Text) {
    if (const auto problems = forms::problems(Text); !problems.empty()) {
        return {"", where(Text, problems.front().offset) + ": " +
                        std::string(problems.front().message)};
    }

    std::vector<std::string_view> lines;
    bool crlf = false;
    for (std::size_t start = 0; start <= Text.size();) {
        auto end = Text.find('\n', start);
        if (end == std::string_view::npos) {
            end = Text.size();
        }
        std::string_view line = Text.substr(start, end - start);
        if (line.ends_with('\r')) {
            crlf = true;
            line.remove_suffix(1);
        }
        lines.push_back(line);
        start = end + 1;
    }

    enum class Inside : std::uint8_t { Code, String, BlockComment };
    Inside inside = Inside::Code;
    std::vector<Open> stack;
    Prefix prefix = Prefix::None;
    std::size_t prefixColumn = 0;
    std::vector<std::string> out;
    out.reserve(lines.size());

    for (std::size_t number = 0; number < lines.size(); ++number) {
        const std::string_view line = lines[number];
        std::string formatted;
        std::size_t pos = 0;
        // the formatted column of line[i] is its character index plus shift
        std::ptrdiff_t shift = 0;
        if (inside != Inside::Code) {
            formatted = line;
        } else {
            while (pos < line.size() && isSpace(line[pos])) {
                ++pos;
            }
            const std::string_view rest = line.substr(pos);
            if (!rest.empty()) {
                const std::size_t indent = indentation(stack, rest);
                formatted = std::string(indent, ' ') + std::string(rest);
                shift = std::ptrdiff_t(indent) - std::ptrdiff_t(pos);
            }
        }
        const auto column = [&](std::size_t At) {
            return std::size_t(std::ptrdiff_t(characters(line.substr(0, At))) + shift);
        };
        const auto add = [&](Kind K, std::size_t Column, std::string_view Atom) {
            if (!stack.empty()) {
                stack.back().elements.push_back(
                    {number, prefix == Prefix::None ? Column : prefixColumn, K, Atom});
            }
            prefix = Prefix::None;
        };

        while (pos < line.size()) {
            if (inside == Inside::String) {
                while (pos < line.size() && line[pos] != '"') {
                    pos += line[pos] == '\\' ? 2 : 1;
                }
                if (pos < line.size()) {
                    inside = Inside::Code;
                    ++pos;
                }
                continue;
            }
            if (inside == Inside::BlockComment) {
                const auto close = line.find("|#", pos);
                if (close == std::string_view::npos) {
                    pos = line.size();
                } else {
                    inside = Inside::Code;
                    pos = close + 2;
                }
                continue;
            }
            const char c = line[pos];
            const char next = pos + 1 < line.size() ? line[pos + 1] : '\0';
            if (isSpace(c)) {
                ++pos;
            } else if (c == ';') {
                pos = line.size();
            } else if (c == '"') {
                add(Kind::Literal, column(pos), {});
                inside = Inside::String;
                ++pos;
            } else if (c == '#' && next == '|') {
                inside = Inside::BlockComment;
                pos += 2;
            } else if (c == '#' && next == '(') {
                add(Kind::List, column(pos), {});
                stack.push_back({column(pos + 1), true, {}});
                pos += 2;
            } else if (c == '\'' || c == '`' || c == ',') {
                if (prefix == Prefix::None) {
                    prefixColumn = column(pos);
                }
                prefix = c == '\'' ? Prefix::Quote : Prefix::Template;
                pos += c == ',' && next == '@' ? 2 : 1;
            } else if (c == '(') {
                const bool data = prefix == Prefix::Quote ||
                                  (prefix == Prefix::None && !stack.empty() && stack.back().data);
                add(Kind::List, column(pos), {});
                stack.push_back({column(pos), data, {}});
                ++pos;
            } else if (c == ')') {
                if (!stack.empty()) {
                    stack.pop_back();
                }
                prefix = Prefix::None;
                ++pos;
            } else {
                const std::size_t begin = pos;
                if (c == '#' && next == '\\') {
                    pos += 3;  // #\ and the character, which may be a delimiter
                    while (pos < line.size() &&
                           (static_cast<unsigned char>(line[pos]) & 0xC0) == 0x80) {
                        ++pos;
                    }
                }
                while (pos < line.size() && !isDelimiter(line[pos])) {
                    ++pos;
                }
                const std::string_view atom = line.substr(begin, std::min(pos, line.size()) - begin);
                // #_name is s7's builtin reference, called like any other symbol
                const bool symbol = prefix != Prefix::Quote && (c != '#' || next == '_') &&
                                    !looksNumeric(atom);
                add(symbol ? Kind::Symbol : Kind::Literal, column(begin), atom);
            }
        }

        if (inside != Inside::String) {
            while (!formatted.empty() && isSpace(formatted.back())) {
                formatted.pop_back();
            }
        }
        out.push_back(std::move(formatted));
    }

    while (!out.empty() && out.back().empty()) {
        out.pop_back();
    }
    std::string result;
    const std::string_view newline = crlf ? "\r\n" : "\n";
    for (const auto& line : out) {
        result += line;
        result += newline;
    }
    return {std::move(result), ""};
}

}  // namespace extemp::format
