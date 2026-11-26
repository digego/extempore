#include "LanguageServer.h"

#include "Bencode.h"
#include "FormFinder.h"
#include "Formatter.h"

#include "llvm/Support/JSON.h"
#include "llvm/Support/raw_ostream.h"

#include <algorithm>
#include <chrono>
#include <condition_variable>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <mutex>
#include <optional>
#include <string>
#include <thread>
#include <vector>

#ifdef _WIN32
#include <fcntl.h>
#include <io.h>
#endif

namespace extemp::lsp {

namespace {

namespace json = llvm::json;
using nrepl::client::Connection;

// --- text positions --------------------------------------------------------

// A position as LSP gives it: both from 0, the character counted in UTF-16
// code units unless the client agreed to UTF-8.
struct Position {
    std::size_t line = 0;
    std::size_t character = 0;
};

std::size_t offsetAt(std::string_view Text, Position At, bool Utf8) {
    std::size_t offset = 0;
    for (std::size_t line = 0; line < At.line; ++line) {
        const auto newline = Text.find('\n', offset);
        if (newline == std::string_view::npos) {
            return Text.size();
        }
        offset = newline + 1;
    }
    std::size_t units = 0;
    while (units < At.character && offset < Text.size() && Text[offset] != '\n') {
        const auto lead = static_cast<unsigned char>(Text[offset]);
        const std::size_t bytes = lead < 0x80 ? 1 : lead < 0xE0 ? 2 : lead < 0xF0 ? 3 : 4;
        units += Utf8 ? bytes : (bytes == 4 ? 2 : 1);
        offset = std::min(offset + bytes, Text.size());
    }
    return offset;
}

Position positionAt(std::string_view Text, std::size_t Offset, bool Utf8) {
    Offset = std::min(Offset, Text.size());
    Position at;
    std::size_t lineStart = 0;
    for (std::size_t i = 0; i < Offset; ++i) {
        if (Text[i] == '\n') {
            ++at.line;
            lineStart = i + 1;
        }
    }
    for (std::size_t i = lineStart; i < Offset;) {
        const auto lead = static_cast<unsigned char>(Text[i]);
        const std::size_t bytes = lead < 0x80 ? 1 : lead < 0xE0 ? 2 : lead < 0xF0 ? 3 : 4;
        at.character += Utf8 ? bytes : (bytes == 4 ? 2 : 1);
        i += bytes;
    }
    return at;
}

// True if [Begin, End) is a selection to evaluate as it stands. An empty range
// is a cursor, and so is a range of one character: that is what a cursor is
// in an editor whose cursor is a selection (helix, kakoune).
bool isSelection(std::string_view Text, std::size_t Begin, std::size_t End) {
    if (End <= Begin || Begin >= Text.size()) {
        return false;
    }
    const auto lead = static_cast<unsigned char>(Text[Begin]);
    const std::size_t bytes = lead < 0x80 ? 1 : lead < 0xE0 ? 2 : lead < 0xF0 ? 3 : 4;
    return End > Begin + bytes;
}

std::size_t endOfLine(std::string_view Text, std::size_t Offset) {
    const auto newline = Text.find('\n', Offset);
    return newline == std::string_view::npos ? Text.size() : newline;
}

// file:///home/me/a%20song.xtm -> /home/me/a song.xtm (and file:///C:/x -> C:/x)
std::string pathFromUri(std::string_view Uri) {
    if (!Uri.starts_with("file://")) {
        return std::string(Uri);
    }
    Uri.remove_prefix(7);
    std::string path;
    for (std::size_t i = 0; i < Uri.size(); ++i) {
        if (Uri[i] == '%' && i + 2 < Uri.size()) {
            const std::string hex(Uri.substr(i + 1, 2));
            char* end = nullptr;
            const long value = std::strtol(hex.c_str(), &end, 16);
            if (end == hex.c_str() + 2) {
                path.push_back(char(value));
                i += 2;
                continue;
            }
        }
        path.push_back(Uri[i]);
    }
    if (path.size() > 2 && path[0] == '/' && path[2] == ':') {
        path.erase(0, 1);  // a Windows drive
    }
    return path;
}

// A character of a symbol, as far as finding the one at the cursor goes.
bool isSymbolChar(char C) {
    return static_cast<unsigned char>(C) > ' ' && C != '(' && C != ')' && C != '"' && C != ';' &&
           C != '\'' && C != '`' && C != ',';
}

std::string trimmed(std::string_view Text) {
    const auto first = Text.find_first_not_of(" \t\r\n");
    if (first == std::string_view::npos) {
        return {};
    }
    const auto last = Text.find_last_not_of(" \t\r\n");
    return std::string(Text.substr(first, last - first + 1));
}

std::string firstLine(std::string_view Text) {
    const std::string text = trimmed(Text);
    return text.substr(0, text.find('\n'));
}

// --- the server ------------------------------------------------------------

struct Document {
    std::string text;
    // Forms whose last evaluation failed, by their text, with the report. A
    // diagnostic follows its form about the file and goes when the form is
    // edited or evaluates cleanly.
    std::map<std::string, std::string, std::less<>> failed;
    // Reader problems are found when the file is opened or saved. Once shown
    // they follow each edit until they are fixed; a form which is open only
    // because it is being typed isn't called a problem.
    bool hasProblems = false;
};

// An evaluation which has been sent and not yet finished.
struct Pending {
    std::string uri;
    std::string key;     // the form's text, for its diagnostic ("" for none)
    bool whole = false;  // the whole buffer
    std::string out;
    std::string err;
    std::vector<std::string> values;
    // from extempore/error: what went wrong, and the line its form starts on
    std::string message;
    std::int64_t errorLine = 0;
    bool failed = false;
    bool interrupted = false;
};

// A question the main thread is waiting on the answer to (completions, lookup).
struct Query {
    bool answered = false;
    bencode::Dict reply;
};

// The connection to an extempore, and the thread reading from it.
struct Link {
    Connection connection;
    std::string host;
    std::uint16_t port = 0;
    std::string session;
    std::map<std::string, Pending, std::less<>> pending;  // by request id
    std::map<std::string, std::shared_ptr<Query>, std::less<>> queries;
    bool alive = true;
};

enum MessageType : std::uint8_t { Error = 1, Warning = 2, Info = 3, Log = 4 };

class Server {
  public:
    explicit Server(nrepl::client::Target Defaults) : m_target(std::move(Defaults)) {}

    int run() {
        for (;;) {
            std::optional<json::Value> message = readMessage();
            if (!message) {
                return m_shutdown ? 0 : 1;  // stdin closed
            }
            const json::Object* object = message->getAsObject();
            if (!object) {
                continue;
            }
            const auto method = object->getString("method");
            const json::Value* id = object->get("id");
            const json::Object* params = object->getObject("params");
            static const json::Object none;
            if (!method) {
                continue;  // a response to something we asked: nothing is
            }
            if (*method == "exit") {
                return m_shutdown ? 0 : 1;
            }
            std::unique_lock<std::mutex> lock(m_mutex);
            if (id) {
                request(*method, *id, params ? *params : none, lock);
            } else {
                notification(*method, params ? *params : none);
            }
        }
    }

  private:
    // --- transport

    std::optional<json::Value> readMessage() {
        std::size_t length = 0;
        std::string line;
        for (;;) {
            line.clear();
            for (int c = std::fgetc(stdin); c != '\n'; c = std::fgetc(stdin)) {
                if (c == EOF) {
                    return std::nullopt;
                }
                line.push_back(char(c));
            }
            if (!line.empty() && line.back() == '\r') {
                line.pop_back();
            }
            if (line.empty()) {
                break;
            }
            if (line.starts_with("Content-Length:")) {
                length = std::strtoul(line.c_str() + 15, nullptr, 10);
            }
        }
        std::string body(length, '\0');
        if (length && std::fread(body.data(), 1, length, stdin) != length) {
            return std::nullopt;
        }
        auto parsed = json::parse(body);
        if (!parsed) {
            llvm::consumeError(parsed.takeError());
            return json::Value(nullptr);
        }
        return std::move(*parsed);
    }

    void write(json::Object Message) {
        Message["jsonrpc"] = "2.0";
        std::string body;
        llvm::raw_string_ostream stream(body);
        stream << json::Value(std::move(Message));
        stream.flush();
        const std::lock_guard<std::mutex> lock(m_outMutex);
        std::fprintf(stdout, "Content-Length: %zu\r\n\r\n", body.size());
        std::fwrite(body.data(), 1, body.size(), stdout);
        std::fflush(stdout);
    }

    void respond(const json::Value& Id, json::Value Result) {
        write(json::Object{{"id", Id}, {"result", std::move(Result)}});
    }

    void notify(llvm::StringRef Method, json::Value Params) {
        write(json::Object{{"method", Method}, {"params", std::move(Params)}});
    }

    void showMessage(MessageType Type, const std::string& Text) {
        notify("window/showMessage",
               json::Object{{"type", int(Type)}, {"message", json::fixUTF8(Text)}});
    }

    // --- requests and notifications (m_mutex is held)

    void request(llvm::StringRef Method, const json::Value& Id, const json::Object& Params,
                 std::unique_lock<std::mutex>& Lock) {
        if (Method == "initialize") {
            respond(Id, initialize(Params));
        } else if (Method == "shutdown") {
            m_shutdown = true;
            // an editor may keep a stopped server's diagnostics on show
            for (const auto& document : m_documents) {
                notify("textDocument/publishDiagnostics",
                       json::Object{{"uri", document.first}, {"diagnostics", json::Array{}}});
            }
            respond(Id, nullptr);
        } else if (Method == "textDocument/documentSymbol") {
            respond(Id, symbols(uriOf(Params)));
        } else if (Method == "textDocument/foldingRange") {
            respond(Id, folds(uriOf(Params)));
        } else if (Method == "textDocument/formatting") {
            respond(Id, formatting(Params, false));
        } else if (Method == "textDocument/rangeFormatting") {
            respond(Id, formatting(Params, true));
        } else if (Method == "textDocument/codeAction") {
            respond(Id, codeActions(Params));
        } else if (Method == "textDocument/completion") {
            respond(Id, completion(Params, Lock));
        } else if (Method == "textDocument/hover") {
            respond(Id, hover(Params, Lock));
        } else if (Method == "workspace/executeCommand") {
            executeCommand(Params);
            respond(Id, nullptr);
        } else {
            write(json::Object{
                {"id", Id},
                {"error", json::Object{{"code", -32601},
                                       {"message", ("no such method: " + Method).str()}}}});
        }
    }

    void notification(llvm::StringRef Method, const json::Object& Params) {
        const json::Object* document = Params.getObject("textDocument");
        const std::string uri(document ? document->getString("uri").value_or("") : "");
        if (Method == "textDocument/didOpen" && document) {
            Document& doc = m_documents[uri];
            doc.text = std::string(document->getString("text").value_or(""));
            doc.hasProblems = !forms::problems(doc.text).empty();
            publishDiagnostics(uri);
        } else if (Method == "textDocument/didChange") {
            const json::Array* changes = Params.getArray("contentChanges");
            auto found = m_documents.find(uri);
            if (!changes || changes->empty() || found == m_documents.end()) {
                return;
            }
            // full-document sync: the last change is the whole text
            if (const json::Object* change = changes->back().getAsObject()) {
                found->second.text = std::string(change->getString("text").value_or(""));
            }
            changed(uri, found->second, false);
        } else if (Method == "textDocument/didSave") {
            auto found = m_documents.find(uri);
            if (found != m_documents.end()) {
                if (const auto text = Params.getString("text")) {
                    found->second.text = std::string(*text);
                }
                changed(uri, found->second, true);
            }
        } else if (Method == "textDocument/didClose") {
            if (m_documents.erase(uri)) {
                notify("textDocument/publishDiagnostics",
                       json::Object{{"uri", uri}, {"diagnostics", json::Array{}}});
            }
        }
    }

    static std::string uriOf(const json::Object& Params) {
        const json::Object* document = Params.getObject("textDocument");
        return std::string(document ? document->getString("uri").value_or("") : "");
    }

    json::Value initialize(const json::Object& Params) {
        if (const json::Object* options = Params.getObject("initializationOptions")) {
            if (const auto host = options->getString("host")) {
                m_target.host = std::string(*host);
            }
            if (const auto port = options->getInteger("port")) {
                m_target.port = std::uint16_t(*port);
            }
            if (const auto process = options->getString("process")) {
                m_target.process = std::string(*process);
            }
        }
        // UTF-16 positions unless the client offers UTF-8, which needs no
        // conversion
        if (const json::Object* capabilities = Params.getObject("capabilities")) {
            if (const json::Object* general = capabilities->getObject("general")) {
                if (const json::Array* encodings = general->getArray("positionEncodings")) {
                    for (const auto& encoding : *encodings) {
                        if (encoding.getAsString() == llvm::StringRef("utf-8")) {
                            m_utf8 = true;
                        }
                    }
                }
            }
        }
        return json::Object{
            {"capabilities",
             json::Object{
                 {"positionEncoding", m_utf8 ? "utf-8" : "utf-16"},
                 {"textDocumentSync",
                  json::Object{{"openClose", true},
                               {"change", 1},  // the whole document each time
                               {"save", json::Object{{"includeText", false}}}}},
                 {"documentSymbolProvider", true},
                 {"foldingRangeProvider", true},
                 {"documentFormattingProvider", true},
                 {"documentRangeFormattingProvider", true},
                 {"codeActionProvider", true},
                 {"completionProvider", json::Object{}},
                 {"hoverProvider", true},
                 {"executeCommandProvider",
                  json::Object{{"commands", json::Array{"extempore.eval", "extempore.evalBuffer",
                                                        "extempore.interrupt"}}}},
             }},
            {"serverInfo", json::Object{{"name", "extempore"}}},
        };
    }

    // --- what a parser can say

    json::Object range(const Document& Doc, std::size_t Begin, std::size_t End) const {
        const Position begin = positionAt(Doc.text, Begin, m_utf8);
        const Position end = positionAt(Doc.text, End, m_utf8);
        return json::Object{
            {"start", json::Object{{"line", std::int64_t(begin.line)},
                                   {"character", std::int64_t(begin.character)}}},
            {"end", json::Object{{"line", std::int64_t(end.line)},
                                 {"character", std::int64_t(end.character)}}},
        };
    }

    json::Value symbols(const std::string& Uri) {
        json::Array result;
        const auto found = m_documents.find(Uri);
        if (found == m_documents.end()) {
            return result;
        }
        const Document& doc = found->second;
        for (const auto& definition : forms::definitions(doc.text)) {
            // 12 function, 13 variable, 23 struct, 5 class (a type alias)
            int kind = 13;
            if (definition.head == "bind-func" || definition.head.starts_with("define-macro") ||
                definition.head == "bind-macro" || definition.head.starts_with("bind-lib") ||
                definition.head == "bind-poly" ||
                doc.text.substr(definition.nameOffset - 1, 1) == "(") {
                kind = 12;
            } else if (definition.head == "bind-type") {
                kind = 23;
            } else if (definition.head == "bind-alias") {
                kind = 5;
            }
            result.push_back(json::Object{
                {"name", json::fixUTF8(std::string(definition.name))},
                {"detail", std::string(definition.head)},
                {"kind", kind},
                {"range", range(doc, definition.form.begin, definition.form.end)},
                {"selectionRange", range(doc, definition.nameOffset,
                                         definition.nameOffset + definition.name.size())},
            });
        }
        return result;
    }

    json::Value folds(const std::string& Uri) {
        json::Array result;
        const auto found = m_documents.find(Uri);
        if (found == m_documents.end()) {
            return result;
        }
        for (const auto& form : forms::topLevel(found->second.text)) {
            const Position begin = positionAt(found->second.text, form.begin, m_utf8);
            const Position end = positionAt(found->second.text, form.end, m_utf8);
            if (end.line > begin.line) {
                result.push_back(json::Object{{"startLine", std::int64_t(begin.line)},
                                              {"endLine", std::int64_t(end.line)}});
            }
        }
        return result;
    }

    // The edits which format a document (extempore fmt), or the lines of a
    // range of it: none if it doesn't read. Formatting keeps the line breaks,
    // so a line of the formatted text replaces the same line of the document.
    json::Value formatting(const json::Object& Params, bool InRange) {
        json::Array edits;
        const auto found = m_documents.find(uriOf(Params));
        if (found == m_documents.end()) {
            return edits;
        }
        const Document& doc = found->second;
        const auto result = format::format(doc.text);
        if (!result || *result == doc.text) {
            return edits;
        }
        const json::Object* where = InRange ? Params.getObject("range") : nullptr;
        if (!where) {
            edits.push_back(json::Object{{"range", range(doc, 0, doc.text.size())},
                                         {"newText", *result}});
            return edits;
        }
        const std::size_t first = positionAt(doc.text, offsetIn(doc, where->getObject("start")), m_utf8).line;
        const std::size_t last = positionAt(doc.text, offsetIn(doc, where->getObject("end")), m_utf8).line;
        const auto lineStarts = [](std::string_view Text) {
            std::vector<std::size_t> starts{0};
            for (std::size_t i = 0; i < Text.size(); ++i) {
                if (Text[i] == '\n') {
                    starts.push_back(i + 1);
                }
            }
            return starts;
        };
        const auto lineOf = [](std::string_view Text, const std::vector<std::size_t>& Starts,
                               std::size_t Line) {
            const std::size_t begin = Starts[Line];
            const std::size_t end = Line + 1 < Starts.size() ? Starts[Line + 1] - 1 : Text.size();
            return std::pair{begin, end};
        };
        const auto docStarts = lineStarts(doc.text);
        const auto newStarts = lineStarts(*result);
        for (std::size_t line = first; line <= last && line < docStarts.size() &&
                                       line < newStarts.size();
             ++line) {
            const auto [begin, end] = lineOf(doc.text, docStarts, line);
            const auto [newBegin, newEnd] = lineOf(*result, newStarts, line);
            const std::string_view now = std::string_view(doc.text).substr(begin, end - begin);
            const std::string_view then =
                std::string_view(*result).substr(newBegin, newEnd - newBegin);
            if (now != then) {
                edits.push_back(json::Object{{"range", range(doc, begin, end)},
                                             {"newText", std::string(then)}});
            }
        }
        return edits;
    }

    // The offset of an LSP position ({line, character}) in a document.
    std::size_t offsetIn(const Document& Doc, const json::Object* At) const {
        Position position;
        if (At) {
            position.line = std::size_t(At->getInteger("line").value_or(0));
            position.character = std::size_t(At->getInteger("character").value_or(0));
        }
        return offsetAt(Doc.text, position, m_utf8);
    }

    json::Value codeActions(const json::Object& Params) {
        const std::string uri = uriOf(Params);
        const json::Object* where = Params.getObject("range");
        json::Array result;
        if (uri.empty() || !where) {
            return result;
        }
        const auto found = m_documents.find(uri);
        const bool selection =
            found != m_documents.end() &&
            isSelection(found->second.text, offsetIn(found->second, where->getObject("start")),
                        offsetIn(found->second, where->getObject("end")));
        const auto action = [&](const char* Title, const char* Command, json::Value Argument) {
            result.push_back(json::Object{
                {"title", Title},
                {"command", json::Object{{"title", Title},
                                         {"command", Command},
                                         {"arguments", json::Array{std::move(Argument)}}}},
            });
        };
        action(selection ? "Evaluate selection" : "Evaluate top-level form", "extempore.eval",
               json::Object{{"uri", uri}, {"range", json::Object(*where)}});
        action("Evaluate buffer", "extempore.evalBuffer", json::Object{{"uri", uri}});
        return result;
    }

    // --- completion and hover: questions for the extempore, when there is one

    // Asks the extempore and waits a moment for its answer: a process which
    // is busy with an evaluation answers after it, which is too late for a
    // menu. Lock (m_mutex) is released while waiting.
    std::optional<bencode::Dict> ask(const std::string& Uri, bencode::Dict Message,
                                     std::unique_lock<std::mutex>& Lock) {
        if (!connect(pathFromUri(Uri), true)) {
            return std::nullopt;
        }
        const std::shared_ptr<Link> link = m_link;
        Message["session"] = bencode::Value(link->session);
        const std::string id = link->connection.send(std::move(Message));
        if (id.empty()) {
            link->alive = false;
            return std::nullopt;
        }
        const auto query = std::make_shared<Query>();
        link->queries.emplace(id, query);
        m_answered.wait_for(Lock, std::chrono::milliseconds(1500),
                            [&] { return query->answered || !link->alive; });
        link->queries.erase(id);
        if (!query->answered) {
            return std::nullopt;
        }
        return std::move(query->reply);
    }

    // The symbol at (or ending at) a position: [begin, end) in the text.
    static std::pair<std::size_t, std::size_t> symbolAt(std::string_view Text, std::size_t Offset,
                                                        bool Whole) {
        std::size_t begin = std::min(Offset, Text.size());
        std::size_t end = begin;
        while (begin > 0 && isSymbolChar(Text[begin - 1])) {
            --begin;
        }
        while (Whole && end < Text.size() && isSymbolChar(Text[end])) {
            ++end;
        }
        return {begin, end};
    }

    json::Value completion(const json::Object& Params, std::unique_lock<std::mutex>& Lock) {
        json::Array items;
        const std::string uri = uriOf(Params);
        const auto found = m_documents.find(uri);
        if (found == m_documents.end()) {
            return items;
        }
        const std::size_t cursor = offsetIn(found->second, Params.getObject("position"));
        const auto [begin, end] = symbolAt(found->second.text, cursor, false);
        const std::string prefix = found->second.text.substr(begin, end - begin);
        // what is replaced is said outright: editors differ over where a word
        // with a colon or a star in it begins
        const json::Object replaced = range(found->second, begin, end);
        if (prefix.empty()) {
            return items;
        }
        const auto reply = ask(
            uri, bencode::Dict{{"op", bencode::Value("completions")}, {"prefix", bencode::Value(prefix)}},
            Lock);
        const bencode::Value* candidates = reply ? bencode::find(*reply, "completions") : nullptr;
        if (!candidates || !candidates->isList()) {
            return items;
        }
        for (const auto& candidate : candidates->list) {
            if (!candidate.isDict()) {
                continue;
            }
            const std::string name(bencode::findString(candidate.dict, "candidate"));
            const auto type = bencode::findString(candidate.dict, "type");
            // 3 function, 14 keyword, 7 class, 6 variable
            const int kind = type == "function" ? 3
                             : (type == "macro" || type == "special-form") ? 14
                             : type == "class" ? 7
                                               : 6;
            items.push_back(json::Object{
                {"label", json::fixUTF8(name)},
                {"kind", kind},
                {"textEdit", json::Object{{"range", json::Object(replaced)},
                                          {"newText", json::fixUTF8(name)}}},
            });
        }
        return items;
    }

    json::Value hover(const json::Object& Params, std::unique_lock<std::mutex>& Lock) {
        const std::string uri = uriOf(Params);
        const auto found = m_documents.find(uri);
        if (found == m_documents.end()) {
            return nullptr;
        }
        const std::size_t cursor = offsetIn(found->second, Params.getObject("position"));
        const auto [begin, end] = symbolAt(found->second.text, cursor, true);
        if (end == begin) {
            return nullptr;
        }
        const json::Object where = range(found->second, begin, end);
        std::string name = found->second.text.substr(begin, end - begin);
        // osc:[double,double]* is osc with its type; sys:load is one name. Ask
        // for the whole of it first.
        for (int attempt = 0; attempt < 2; ++attempt) {
            const auto reply = ask(
                uri, bencode::Dict{{"op", bencode::Value("lookup")}, {"sym", bencode::Value(name)}},
                Lock);
            const bencode::Value* info = reply ? bencode::find(*reply, "info") : nullptr;
            if (info && info->isDict()) {
                const auto field = [&](const char* Key) {
                    return std::string(bencode::findString(info->dict, Key));
                };
                const std::string args = field("arglists-str");
                std::string text = "```extempore\n";
                if (args.size() > 2) {
                    text += "(" + name + " " + args.substr(1);
                } else if (args == "()") {
                    text += "(" + name + ")";
                } else {
                    text += name;
                }
                if (const std::string type = field("extempore/type"); !type.empty()) {
                    text += "  ; " + type;
                }
                text += "\n```";
                if (const std::string kind = field("extempore/kind"); !kind.empty()) {
                    text += "\n\n" + kind;
                }
                if (const std::string doc = field("doc"); !doc.empty()) {
                    text += "\n\n" + doc;
                }
                return json::Object{
                    {"contents", json::Object{{"kind", "markdown"}, {"value", json::fixUTF8(text)}}},
                    {"range", json::Object(where)},
                };
            }
            const auto colon = name.find(':');
            if (!reply || colon == std::string::npos || colon == 0) {
                break;
            }
            name.erase(colon);
        }
        return nullptr;
    }

    // --- diagnostics

    // The document's text changed (or was saved).
    void changed(const std::string& Uri, Document& Doc, bool Saved) {
        if (Saved || Doc.hasProblems) {
            Doc.hasProblems = !forms::problems(Doc.text).empty();
        }
        // a failed form which has been edited is no longer that form
        if (!Doc.failed.empty()) {
            std::map<std::string, std::string, std::less<>> still;
            for (const auto& form : forms::topLevel(Doc.text)) {
                const auto text = Doc.text.substr(form.begin, form.end - form.begin);
                if (const auto found = Doc.failed.find(text); found != Doc.failed.end()) {
                    still.insert(*found);
                }
            }
            Doc.failed = std::move(still);
        }
        publishDiagnostics(Uri);
    }

    void publishDiagnostics(const std::string& Uri) {
        const auto found = m_documents.find(Uri);
        if (found == m_documents.end()) {
            return;
        }
        const Document& doc = found->second;
        json::Array diagnostics;
        const auto add = [&](std::size_t Begin, std::size_t End, const std::string& Message) {
            diagnostics.push_back(json::Object{
                {"range", range(doc, Begin, End)},
                {"severity", 1},
                {"source", "extempore"},
                {"message", json::fixUTF8(Message)},
            });
        };
        if (doc.hasProblems) {
            for (const auto& problem : forms::problems(doc.text)) {
                add(problem.offset, std::min(problem.offset + 1, doc.text.size()),
                    std::string(problem.message));
            }
        }
        if (!doc.failed.empty()) {
            for (const auto& form : forms::topLevel(doc.text)) {
                const auto found_ =
                    doc.failed.find(doc.text.substr(form.begin, form.end - form.begin));
                if (found_ != doc.failed.end()) {
                    // on the form's first line, the only place nREPL names
                    add(form.begin, std::min(endOfLine(doc.text, form.begin), form.end),
                        found_->second);
                }
            }
        }
        notify("textDocument/publishDiagnostics",
               json::Object{{"uri", Uri}, {"diagnostics", std::move(diagnostics)}});
    }

    // --- evaluation

    void executeCommand(const json::Object& Params) {
        const auto command = Params.getString("command").value_or("");
        const json::Array* arguments = Params.getArray("arguments");
        const json::Object* argument =
            arguments && !arguments->empty() ? arguments->front().getAsObject() : nullptr;
        if (command == "extempore.interrupt") {
            if (m_link && m_link->alive && !m_link->pending.empty()) {
                m_link->connection.send(bencode::Dict{{"op", bencode::Value("interrupt")},
                                                      {"session", bencode::Value(m_link->session)}});
            } else {
                showMessage(Info, "nothing is being evaluated");
            }
            return;
        }
        if (command != "extempore.eval" && command != "extempore.evalBuffer") {
            showMessage(Error, ("extempore: no such command: " + command).str());
            return;
        }
        const std::string uri(argument ? argument->getString("uri").value_or("") : "");
        const auto found = m_documents.find(uri);
        if (found == m_documents.end()) {
            showMessage(Error, std::string(command) +
                                   " takes {uri, range} for a document which is open");
            return;
        }
        const Document& doc = found->second;
        const std::string path = pathFromUri(uri);
        Pending pending;
        pending.uri = uri;
        bencode::Dict message;
        if (command == "extempore.evalBuffer") {
            pending.whole = true;
            message = {{"op", bencode::Value("load-file")},
                       {"file", bencode::Value(doc.text)},
                       {"file-path", bencode::Value(path)}};
        } else {
            const json::Object* where = argument->getObject("range");
            const std::size_t begin = offsetIn(doc, where ? where->getObject("start") : nullptr);
            const std::size_t end = offsetIn(doc, where ? where->getObject("end") : nullptr);
            const auto form = forms::at(doc.text, begin);
            if (form) {
                pending.key = doc.text.substr(form->begin, form->end - form->begin);
            }
            std::string code;
            std::int64_t line = 1;
            if (isSelection(doc.text, begin, end)) {
                code = doc.text.substr(begin, end - begin);
                line = std::int64_t(positionAt(doc.text, begin, m_utf8).line) + 1;
            } else if (form) {
                code = pending.key;
                line = std::int64_t(form->line);
            } else {
                showMessage(Warning, "there is no form here to evaluate");
                return;
            }
            message = {{"op", bencode::Value("eval")},
                       {"code", bencode::Value(std::move(code))},
                       {"file", bencode::Value(path)},
                       {"line", bencode::Value(line)}};
        }
        if (!connect(path, false)) {
            return;
        }
        message["session"] = bencode::Value(m_link->session);
        const std::string id = m_link->connection.send(std::move(message));
        if (id.empty()) {
            m_link->alive = false;
            showMessage(Error, "extempore went away");
            return;
        }
        m_link->pending.emplace(id, std::move(pending));
    }

    // Makes sure there is a connection to the extempore for a file at Path.
    // If there is none to be had, a command says so to the user; a question
    // asked on the user's behalf (Quiet) doesn't, and isn't asked again for a
    // few seconds.
    bool connect(const std::string& Path, bool Quiet) {
        const std::uint16_t port = nrepl::client::findPort(m_target.port, Path);
        if (m_link && m_link->alive && m_link->port == port && m_link->host == m_target.host) {
            return true;
        }
        const auto now = std::chrono::steady_clock::now();
        if (Quiet && now - m_lastFailure < std::chrono::seconds(3)) {
            return false;
        }
        if (m_link) {
            m_link->connection.shutdown();  // its reader thread ends
        }
        auto link = std::make_shared<Link>();
        link->host = m_target.host;
        link->port = port;
        const std::string where = link->host + ":" + std::to_string(port);
        if (const std::string why = link->connection.open(link->host, port); !why.empty()) {
            m_link.reset();
            m_lastFailure = now;
            if (!Quiet) {
                showMessage(Error, "no extempore to talk to: no nREPL server at " + where +
                                       " (" + why + ")");
            }
            return false;
        }
        bencode::Dict clone{{"op", bencode::Value("clone")}};
        if (!m_target.process.empty()) {
            clone["extempore/process"] = bencode::Value(m_target.process);
        }
        bencode::Dict reply;
        if (!link->connection.request(std::move(clone), reply) ||
            bencode::findString(reply, "new-session").empty()) {
            m_link.reset();
            m_lastFailure = now;
            const auto err = bencode::findString(reply, "err");
            if (!Quiet) {
                showMessage(Error, err.empty() ? "the extempore at " + where + " gave no session"
                                               : trimmed(err));
            }
            return false;
        }
        link->session = std::string(bencode::findString(reply, "new-session"));
        m_link = link;
        std::thread([this, link] { readFrom(link); }).detach();
        return true;
    }

    // The reader thread of one connection: every message extempore sends.
    void readFrom(const std::shared_ptr<Link>& Link_) {
        bencode::Dict message;
        while (Link_->connection.read(message) == Connection::Read::Message) {
            const std::lock_guard<std::mutex> lock(m_mutex);
            const auto id = bencode::findString(message, "id");
            if (const auto query = Link_->queries.find(id); query != Link_->queries.end()) {
                query->second->reply = std::move(message);
                query->second->answered = true;
                m_answered.notify_all();
                continue;
            }
            const auto found = Link_->pending.find(id);
            if (found == Link_->pending.end()) {
                continue;
            }
            Pending& pending = found->second;
            pending.out += bencode::findString(message, "out");
            pending.err += bencode::findString(message, "err");
            if (const bencode::Value* value = bencode::find(message, "value");
                value && value->isString()) {
                pending.values.push_back(value->string);
            }
            if (const bencode::Value* error = bencode::find(message, "extempore/error");
                error && error->isDict()) {
                if (const bencode::Value* line = bencode::find(error->dict, "line");
                    line && line->isInteger()) {
                    pending.errorLine = line->integer;
                }
                pending.message = std::string(bencode::findString(error->dict, "message"));
            }
            using nrepl::client::hasStatus;
            pending.failed |= hasStatus(message, "eval-error") || hasStatus(message, "error");
            pending.interrupted |= hasStatus(message, "interrupted");
            if (hasStatus(message, "done")) {
                finished(pending);
                Link_->pending.erase(found);
            }
        }
        const std::lock_guard<std::mutex> lock(m_mutex);
        Link_->alive = false;
        m_answered.notify_all();
        if (!Link_->pending.empty()) {
            Link_->pending.clear();
            showMessage(Error, "extempore went away before the evaluation finished");
        }
    }

    // An evaluation is done: tell the user, and mark or clear its form.
    void finished(Pending& Eval) {
        if (!trimmed(Eval.out).empty()) {
            notify("window/logMessage",
                   json::Object{{"type", int(Log)}, {"message", json::fixUTF8(Eval.out)}});
        }
        const bool failed = Eval.failed || !Eval.err.empty();
        const auto found = m_documents.find(Eval.uri);
        Document* doc = found == m_documents.end() ? nullptr : &found->second;
        if (doc && Eval.whole) {
            doc->failed.clear();
            // the error names the line its form starts on
            if (failed && Eval.errorLine > 0) {
                for (const auto& form : forms::topLevel(doc->text)) {
                    if (std::int64_t(form.line) == Eval.errorLine) {
                        Eval.key = doc->text.substr(form.begin, form.end - form.begin);
                    }
                }
            }
        }
        if (Eval.interrupted) {
            showMessage(Warning, "interrupted");
        } else if (failed) {
            // the message alone where the server gives one: the report around
            // it says where, which the diagnostic's own place already does
            std::string report = trimmed(Eval.message.empty() ? Eval.err : Eval.message);
            if (report.empty()) {
                report = "the evaluation failed";
            }
            showMessage(Error, firstLine(report));
            if (doc && !Eval.key.empty()) {
                doc->failed[Eval.key] = report;
            }
        } else {
            std::string summary = firstLine(Eval.out);
            for (const auto& value : Eval.values) {
                summary += (summary.empty() ? "=> " : "  => ") + value;
            }
            showMessage(Info, summary.empty() ? "evaluated" : summary);
            if (doc && !Eval.key.empty()) {
                doc->failed.erase(Eval.key);
            }
        }
        if (doc) {
            publishDiagnostics(Eval.uri);
        }
    }

    nrepl::client::Target m_target;
    bool m_utf8 = false;
    bool m_shutdown = false;
    // Guards everything below; the reader threads take it for each message.
    std::mutex m_mutex;
    std::condition_variable m_answered;  // a Query has its reply
    std::chrono::steady_clock::time_point m_lastFailure{};
    std::mutex m_outMutex;
    std::map<std::string, Document> m_documents;  // by URI
    std::shared_ptr<Link> m_link;
};

}  // namespace

int run(const nrepl::client::Target& Defaults) {
#ifdef _WIN32
    // no newline translation: the protocol counts bytes
    _setmode(_fileno(stdin), _O_BINARY);
    _setmode(_fileno(stdout), _O_BINARY);
#endif
    return Server(Defaults).run();
}

}  // namespace extemp::lsp
