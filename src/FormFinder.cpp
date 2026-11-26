#include "FormFinder.h"

#include <algorithm>

namespace extemp::forms {

namespace {

// One pass over the text: the top-level forms, and whether it ends inside a
// string or a block comment.
struct Scan {
    std::vector<Form> forms;
    std::vector<Problem> problems;
    bool insideAtom = false;
};

Scan scan(std::string_view Text) {
    Scan result;
    const std::size_t n = Text.size();
    std::size_t depth = 0;
    std::size_t start = 0;
    std::size_t startLine = 1;
    std::size_t line = 1;
    // a top-level datum after #; is commented out, so it isn't a form
    bool commentNext = false;
    bool startCommented = false;
    for (std::size_t i = 0; i < n; ++i) {
        const char c = Text[i];
        const char next = i + 1 < n ? Text[i + 1] : '\0';
        if (depth == 0 && commentNext && !std::string_view(" \t\r\n;#('`,@").contains(c)) {
            commentNext = false;  // the commented datum is an atom
        }
        if (c == '\n') {
            ++line;
        } else if (c == ';') {
            const auto newline = Text.find('\n', i);
            if (newline == std::string_view::npos) {
                break;
            }
            i = newline - 1;  // the newline is counted on the next pass
        } else if (c == '"') {
            const std::size_t opened = i;
            for (++i; i < n && Text[i] != '"'; ++i) {
                if (Text[i] == '\\') {
                    ++i;
                }
                if (i < n && Text[i] == '\n') {
                    ++line;
                }
            }
            if (i >= n) {
                result.insideAtom = true;
                result.problems.push_back({opened, "this string never ends"});
            }
        } else if (c == '#' && next == ';') {
            commentNext = depth == 0;
            ++i;
        } else if (c == '#' && next == '|') {
            const std::size_t opened = i;
            const auto close = Text.find("|#", i + 2);
            const auto end = close == std::string_view::npos ? n : close + 2;
            line += std::size_t(std::count(Text.begin() + i, Text.begin() + end, '\n'));
            if (close == std::string_view::npos) {
                result.insideAtom = true;
                result.problems.push_back({opened, "this block comment never ends"});
                break;
            }
            i = close + 1;
        } else if (c == '#' && next == '\\') {
            commentNext = false;
            i += 2;  // the character after #\ is never a delimiter
            if (i < n && Text[i] == '\n') {
                ++line;
            }
        } else if (c == '(') {
            if (depth == 0) {
                start = i;
                startLine = line;
                startCommented = commentNext;
                commentNext = false;
            }
            ++depth;
        } else if (c == ')') {
            if (depth == 0) {
                result.problems.push_back({i, "this closing parenthesis has nothing to close"});
            } else if (--depth == 0 && !startCommented) {
                result.forms.push_back({start, i + 1, startLine, true});
            }
        }
    }
    if (depth > 0) {
        if (!startCommented) {
            result.forms.push_back({start, n, startLine, false});
        }
        result.problems.push_back(
            {start, "this form never ends: a closing parenthesis is missing"});
    }
    return result;
}

}  // namespace

std::vector<Form> topLevel(std::string_view Text) {
    return scan(Text).forms;
}

std::size_t offsetOf(std::string_view Text, std::size_t Line, std::size_t Column) {
    std::size_t offset = 0;
    for (std::size_t line = 1; line < Line; ++line) {
        const auto newline = Text.find('\n', offset);
        if (newline == std::string_view::npos) {
            break;  // past the last line: stay on it
        }
        offset = newline + 1;
    }
    for (std::size_t column = 1; column < Column && offset < Text.size() && Text[offset] != '\n';
         ++column) {
        // one character: a lead byte and its continuation bytes
        do {
            ++offset;
        } while (offset < Text.size() && (static_cast<unsigned char>(Text[offset]) & 0xC0) == 0x80);
    }
    return offset;
}

std::optional<Form> at(std::string_view Text, std::size_t Offset) {
    const auto forms = topLevel(Text);
    if (forms.empty()) {
        return std::nullopt;
    }
    const Form* before = nullptr;
    for (const auto& form : forms) {
        if (form.begin > Offset) {
            break;
        }
        if (Offset < form.end) {
            return form;
        }
        before = &form;
    }
    return before ? *before : forms.front();
}

bool complete(std::string_view Text) {
    const auto result = scan(Text);
    return !result.insideAtom && (result.forms.empty() || result.forms.back().closed);
}

std::vector<Problem> problems(std::string_view Text) {
    return scan(Text).problems;
}

std::vector<Definition> definitions(std::string_view Text) {
    const auto isSpace = [](char c) { return c == ' ' || c == '\t' || c == '\n' || c == '\r'; };
    std::vector<Definition> found;
    for (const auto& form : topLevel(Text)) {
        std::size_t i = form.begin + 1;
        // the next token of the form from i: [begin, end)
        const auto token = [&](std::size_t& Begin, std::size_t& End) {
            while (i < form.end && isSpace(Text[i])) {
                ++i;
            }
            Begin = i;
            while (i < form.end && !isSpace(Text[i]) && Text[i] != '(' && Text[i] != ')') {
                ++i;
            }
            End = i;
            return End > Begin;
        };
        std::size_t begin = 0;
        std::size_t end = 0;
        if (!token(begin, end)) {
            continue;
        }
        const std::string_view head = Text.substr(begin, end - begin);
        if (!head.starts_with("bind-") && !head.starts_with("define")) {
            continue;
        }
        // (bind-lib library name ...) and its kin name the library first
        if (head.starts_with("bind-lib") && !token(begin, end)) {
            continue;
        }
        while (i < form.end && isSpace(Text[i])) {
            ++i;
        }
        if (i < form.end && Text[i] == '(') {
            ++i;  // (define (name args) ...)
        }
        if (!token(begin, end)) {
            continue;
        }
        std::string_view name = Text.substr(begin, end - begin);
        name = name.substr(0, name.find(':'));
        if (name.empty() || name.front() == '"') {
            continue;
        }
        found.push_back({form, head, name, begin});
    }
    return found;
}

}  // namespace extemp::forms
