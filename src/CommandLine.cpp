#include "CommandLine.h"

#include <algorithm>
#include <array>
#include <charconv>
#include <cstdlib>

namespace extemp::cli {

namespace {

enum class Kind : std::uint8_t {
    Flag,   // takes no value
    Text,   // any value
    Whole,  // a non-negative whole number
    Real,   // a number
    Color,  // auto, always or never
};

struct Option {
    std::string_view name;  // without dashes; a one-letter name is a short option
    Kind kind;
    std::string_view value;  // the value's name in --help, empty for a flag
    std::string_view help;
    bool repeats = false;
};

struct Subcommand {
    Command command;
    std::string_view name;      // empty for the session
    std::string_view synopsis;  // after "extempore "
    std::string_view summary;
    std::vector<Option> options;
    std::size_t minFiles;
    std::size_t maxFiles;
};

constexpr std::size_t kAnyNumber = std::size_t(-1);

// Options of every subcommand which starts a runtime.
const std::array<Option, 6> kRuntimeOptions{{
    {"share-dir", Kind::Text, "DIR", "the share directory (runtime/, libs/, examples/)"},
    {"no-base", Kind::Flag, "", "don't load the base library at startup"},
    {"opt-level", Kind::Whole, "N", "LLVM optimisation level, 0 to 3"},
    {"q", Kind::Flag, "", "quiet: only errors"},
    {"v", Kind::Flag, "", "more messages: -v, -vv and -vvv each say more", true},
    {"timestamps", Kind::Color, "WHEN",
     "start each message with the time: auto (a session whose stderr isn't a terminal), "
     "always or never"},
}};

const std::array<Option, 2> kCommonOptions{{
    {"color", Kind::Color, "WHEN",
     "auto (the default: on a terminal, unless NO_COLOR), always or never"},
    {"help", Kind::Flag, "", "print this help"},
}};

// Options of every client of a running extempore's nREPL server.
const std::array<Option, 2> kClientOptions{{
    {"host", Kind::Text, "HOST", "where the extempore is running [127.0.0.1]"},
    {"port", Kind::Whole, "N",
     "its nREPL port [$EXTEMPORE_NREPL_PORT, else a .nrepl-port file here or above, else 7888]"},
}};

const std::array<Option, 3> kFormatOptions{{
    {"sample-rate", Kind::Whole, "HZ", "audio sample rate"},
    {"channels", Kind::Whole, "N", "number of output channels"},
    {"frames", Kind::Whole, "N", "frames per block [1024]"},
}};

std::vector<Option> with(std::vector<Option> Own, bool Runtime) {
    if (Runtime) {
        Own.insert(Own.end(), kRuntimeOptions.begin(), kRuntimeOptions.end());
    }
    Own.insert(Own.end(), kCommonOptions.begin(), kCommonOptions.end());
    return Own;
}

std::vector<Option> client(std::vector<Option> Own) {
    Own.insert(Own.end(), kClientOptions.begin(), kClientOptions.end());
    return with(std::move(Own), false);
}

std::vector<Option> withFormat(std::vector<Option> Own) {
    Own.insert(Own.end(), kFormatOptions.begin(), kFormatOptions.end());
    return with(std::move(Own), true);
}

const std::vector<Subcommand>& subcommands() {
    static const std::vector<Subcommand> table{
        {Command::Session, "", "[FILE] [-e EXPR]",
         "live session: audio, nREPL and the legacy eval server",
         withFormat({
             {"e", Kind::Text, "EXPR", "evaluate EXPR at startup, after FILE"},
             {"device", Kind::Text, "DEVICE",
              "output (or duplex) audio device: an index, or a regex on its name"},
             {"in-device", Kind::Text, "DEVICE",
              "input audio device: an index, or a regex on its name"},
             {"in-channels", Kind::Whole, "N", "number of input channels"},
             {"latency", Kind::Whole, "MS", "audio output latency to ask for"},
             {"realtime", Kind::Flag, "", "realtime process priority (Windows only)"},
             {"no-audio", Kind::Flag, "", "run without an audio device"},
             {"repl", Kind::Flag, "", "an interactive REPL in this terminal (Linux and macOS)"},
             {"nrepl-port", Kind::Whole, "N",
              "port for the nREPL server [7888]; 0 picks one and writes .nrepl-port"},
             {"nrepl-bind", Kind::Text, "ADDR",
              "IPv4 address the nREPL server listens on [127.0.0.1]"},
             {"no-nrepl", Kind::Flag, "", "don't start the nREPL server"},
             {"legacy-port", Kind::Whole, "N",
              "port for the legacy eval server [7099]; the utility process takes N-1"},
             {"no-legacy-server", Kind::Flag, "", "don't start the legacy eval server"},
         }),
         0, 1},
        {Command::Exec, "exec", "exec FILE | -e EXPR [-- --NAME[=VALUE]...]",
         "run a script: no audio, no servers; exits when its work is done",
         with(
             {
                 {"e", Kind::Text, "EXPR", "evaluate EXPR instead of a file"},
                 {"wait", Kind::Flag, "", "keep running until the code calls (quit)"},
             },
             true),
         0, 1},
        {Command::Test, "test", "test FILE... [--junit DIR]",
         "run .xtm test files, each in its own process",
         with(
             {
                 {"junit", Kind::Text, "DIR", "write a JUnit XML report per file into DIR"},
             },
             true),
         1, kAnyNumber},
        {Command::Render, "render", "render FILE -o OUT.wav --duration SECONDS",
         "render FILE's audio to a WAV file, offline and repeatably",
         withFormat({
             {"o", Kind::Text, "OUT.wav", "the file to write (32-bit float WAV)"},
             {"duration", Kind::Real, "SECONDS", "length of audio to render"},
             {"seed", Kind::Whole, "N", "seed for the random number generators [0]"},
         }),
         1, 1},
        {Command::Compile, "compile", "compile FILE [--exe]",
         "compile a library ahead of time into the cache, or --exe an executable",
         with(
             {
                 {"exe", Kind::Flag, "", "build a native executable"},
                 {"name", Kind::Text, "NAME", "name the cache entry NAME, not after the file"},
                 {"dll", Kind::Flag, "", "build a shared library"},
                 {"link", Kind::Text, "LIBS", "with --exe: extra arguments for the linker"},
                 {"arch", Kind::Text, "ARCH", "target architecture [this host's]"},
                 {"cpu", Kind::Text, "CPU", "target CPU [this host's]"},
                 {"attr", Kind::Text, "ATTR", "a target attribute; may be given more than once",
                  true},
             },
             true),
         1, 1},
        {Command::Send, "send", "send [CODE]",
         "evaluate code in a running extempore: CODE, or stdin, or --file",
         client({
             {"file", Kind::Text, "FILE", "load FILE; with --line, evaluate one form of it"},
             {"line", Kind::Whole, "N", "with --file: the top-level form at line N (from 1)"},
             {"column", Kind::Whole, "N", "with --line: the column (from 1) [1]"},
             {"process", Kind::Text, "NAME", "the Scheme process to evaluate in [primary]"},
         }),
         0, 1},
        {Command::Repl, "repl", "repl", "an interactive REPL on a running extempore",
         client({
             {"process", Kind::Text, "NAME", "the Scheme process to evaluate in [primary]"},
         }),
         0, 0},
        {Command::Lsp, "lsp", "lsp",
         "a language server on stdin and stdout, for an editor to start",
         client({
             {"process", Kind::Text, "NAME", "the Scheme process to evaluate in [primary]"},
         }),
         0, 0},
        {Command::Status, "status", "status",
         "show the running extempore: its version, processes and nREPL ops", client({}), 0, 0},
        {Command::Fmt, "fmt", "fmt [FILE|DIR]... [--check]",
         "format .xtm files in place (a directory's, recursively), or stdin to stdout",
         with(
             {
                 {"check", Kind::Flag, "",
                  "change nothing: list the files which aren't formatted, and exit 1 if any"},
             },
             false),
         0, kAnyNumber},
        {Command::Devices, "devices", "devices", "list the audio devices", with({}, false), 0, 0},
        {Command::Version, "version", "version", "print the version", with({}, false), 0, 0},
    };
    return table;
}

const Subcommand& subcommand(Command Cmd) {
    for (const auto& sub : subcommands()) {
        if (sub.command == Cmd) {
            return sub;
        }
    }
    return subcommands().front();
}

const Option* findOption(const Subcommand& Sub, std::string_view Name) {
    for (const auto& option : Sub.options) {
        if (option.name == Name) {
            return &option;
        }
    }
    return nullptr;
}

// An option's old spelling, in the new grammar: accepted with a notice.
struct Renamed {
    std::string_view oldName;
    std::string_view newName;
    std::string_view note = "";  // added to the notice
};
constexpr std::array<Renamed, 9> kRenamed{{
    {"noaudio", "no-audio"},
    {"nobase", "no-base"},
    {"samplerate", "sample-rate"},
    {"inchannels", "in-channels"},
    {"indevice", "in-device"},
    {"device-name", "device"},
    {"indevice-name", "in-device"},
    {"sharedir", "share-dir"},
    // the clients' --port is the nREPL server's
    {"port", "legacy-port", " (the eval server's port; the nREPL server's is --nrepl-port)"},
}};

// Options which are gone, and why.
struct Removed {
    std::string_view name;
    std::string_view reason;
};
constexpr std::array<Removed, 2> kRemoved{{
    {"timediv", "the scheduler's time division is no longer an option"},
    {"runtime", "use --share-dir"},
}};

// The old mode options, and what to type instead of each.
struct OldMode {
    std::string_view option;
    std::string_view now;
    bool takesValue = true;
};
enum : std::uint8_t { kRun, kEval, kBatch, kCompile, kOutfile, kDevices };
constexpr std::array<OldMode, 6> kOldModes{{
    {"--run", "extempore FILE"},
    {"--eval", "extempore -e EXPR"},
    {"--batch", "extempore exec -e EXPR (add --wait if the code ends by calling (quit))"},
    {"--compile", "extempore compile --exe FILE"},
    {"--audio-outfile", "extempore render FILE -o OUT.wav --duration SECONDS"},
    {"--print-devices", "extempore devices", false},
}};

bool isWhole(std::string_view Text) {
    return !Text.empty() && std::ranges::all_of(Text, [](char c) { return c >= '0' && c <= '9'; });
}

bool isReal(std::string_view Text) {
    const std::string copy(Text);
    char* end = nullptr;
    std::strtod(copy.c_str(), &end);
    return !copy.empty() && end == copy.c_str() + copy.size();
}

std::string dashed(std::string_view Name) {
    return (Name.size() == 1 ? "-" : "--") + std::string(Name);
}

// Empty if Value suits Opt, else what is wrong with it.
std::string checkValue(const Option& Opt, std::string_view Value) {
    switch (Opt.kind) {
    case Kind::Whole:
        if (!isWhole(Value)) {
            return dashed(Opt.name) + " expects a whole number, got \"" + std::string(Value) + "\"";
        }
        break;
    case Kind::Real:
        if (!isReal(Value)) {
            return dashed(Opt.name) + " expects a number, got \"" + std::string(Value) + "\"";
        }
        break;
    case Kind::Color:
        if (Value != "auto" && Value != "always" && Value != "never") {
            return dashed(Opt.name) + " expects auto, always or never, got \"" + std::string(Value) +
                   "\"";
        }
        break;
    case Kind::Flag:
    case Kind::Text:
        break;
    }
    return {};
}

}  // namespace

bool Invocation::has(std::string_view Name) const {
    return std::ranges::any_of(options, [&](const auto& o) { return o.first == Name; });
}

std::optional<std::string_view> Invocation::get(std::string_view Name) const {
    std::optional<std::string_view> value;
    for (const auto& [name, v] : options) {
        if (name == Name) {
            value = v;
        }
    }
    return value;
}

std::vector<std::string_view> Invocation::all(std::string_view Name) const {
    std::vector<std::string_view> values;
    for (const auto& [name, v] : options) {
        if (name == Name) {
            values.emplace_back(v);
        }
    }
    return values;
}

namespace {

// A command line which uses the old mode options, as the subcommand which
// replaces them.
struct Rewritten {
    std::vector<std::string> args;
    std::vector<std::string> notices;
    std::string error;
};

// Empty if Args uses none of the old mode options.
std::optional<Rewritten> rewriteOldModes(std::span<const std::string_view> Args) {
    std::array<std::optional<std::string>, kOldModes.size()> given;
    Rewritten result;
    std::vector<std::string> rest;
    std::size_t i = 0;
    for (; i < Args.size() && Args[i] != "--"; ++i) {
        const std::string_view arg = Args[i];
        const auto eq = arg.find('=');
        const auto mode = std::ranges::find(kOldModes, arg.substr(0, eq), &OldMode::option);
        if (mode == kOldModes.end()) {
            rest.emplace_back(arg);
            continue;
        }
        auto& value = given[std::size_t(mode - kOldModes.begin())];
        if (!value) {
            result.notices.push_back(std::string(mode->option) +
                                     " is deprecated and will be removed: use " +
                                     std::string(mode->now));
        }
        if (!mode->takesValue) {
            value = "";
        } else if (eq != std::string_view::npos) {
            value = std::string(arg.substr(eq + 1));
        } else if (i + 1 < Args.size()) {
            value = std::string(Args[++i]);
        } else {
            result.error = std::string(mode->option) + " requires a value";
        }
    }
    if (result.notices.empty()) {
        return std::nullopt;
    }
    if (!result.error.empty()) {
        return result;
    }
    const auto& run = given[kRun];
    const auto& eval = given[kEval];
    const auto& batch = given[kBatch];
    const auto& compile = given[kCompile];
    const auto& outfile = given[kOutfile];
    bool session = false;
    if (int(run.has_value()) + int(eval.has_value()) + int(batch.has_value()) +
            int(compile.has_value()) >
        1) {
        result.error = "give one of --run, --eval, --batch and --compile";
    } else if (given[kDevices]) {
        result.args = {"devices"};
    } else if (compile) {
        result.args = {"compile", "--exe", *compile};
    } else if (outfile && run) {
        result.args = {"render", *run, "-o", *outfile};
    } else if (outfile) {
        result.error = "--audio-outfile needs a file to render (--run FILE)";
    } else if (batch) {
        result.args = {"exec", "--wait", "-e", *batch};
    } else if (run) {
        session = true;
        result.args = {*run};
    } else if (eval) {
        session = true;
        result.args = {"-e", *eval};
    }
    // --batch meant no audio, and command lines often said so as well
    for (auto& arg : rest) {
        if (session || arg != "--noaudio") {
            result.args.push_back(std::move(arg));
        }
    }
    for (; i < Args.size(); ++i) {
        result.args.emplace_back(Args[i]);
    }
    return result;
}

Invocation parseCurrent(std::span<const std::string_view> Args) {
    Invocation inv;
    std::size_t i = 0;
    if (!Args.empty()) {
        for (const auto& sub : subcommands()) {
            if (!sub.name.empty() && sub.name == Args[0]) {
                inv.command = sub.command;
                i = 1;
                break;
            }
        }
    }
    const Subcommand& sub = subcommand(inv.command);
    const auto fail = [&](std::string Message) {
        inv.error = std::move(Message);
        return inv;
    };
    for (; i < Args.size(); ++i) {
        const std::string_view arg = Args[i];
        if (arg == "--") {
            for (++i; i < Args.size(); ++i) {
                const std::string_view script = Args[i];
                if (!script.starts_with("--") || script.size() == 2) {
                    return fail("arguments after -- are --NAME or --NAME=VALUE, got \"" +
                                std::string(script) + "\"");
                }
                const auto eq = script.find('=');
                inv.scriptArgs.emplace_back(
                    script.substr(2, eq == std::string_view::npos ? eq : eq - 2),
                    eq == std::string_view::npos ? std::string_view() : script.substr(eq + 1));
            }
            break;
        }
        if (!arg.starts_with("-") || arg == "-") {
            inv.files.emplace_back(arg);
            continue;
        }
        std::string_view name;
        std::optional<std::string_view> inlineValue;
        if (arg.starts_with("--")) {
            const auto eq = arg.find('=');
            name = arg.substr(2, eq == std::string_view::npos ? eq : eq - 2);
            if (eq != std::string_view::npos) {
                inlineValue = arg.substr(eq + 1);
            }
            if (name.size() < 2) {
                return fail("unknown option " + std::string(arg));
            }
        } else {
            name = arg.substr(1);
            // -vv: a repeating flag, given that many times
            if (name.size() > 1 &&
                std::ranges::all_of(name, [&](char c) { return c == name.front(); })) {
                const Option* flag = findOption(sub, name.substr(0, 1));
                if (flag && flag->repeats && flag->kind == Kind::Flag) {
                    for (std::size_t n = 0; n < name.size(); ++n) {
                        inv.options.emplace_back(flag->name, "");
                    }
                    continue;
                }
            }
            if (name.size() != 1) {
                return fail("unknown option " + std::string(arg) +
                            " (long options take two dashes)");
            }
            if (name == "h") {
                name = "help";
            }
        }
        if (const auto removed = std::ranges::find(kRemoved, name, &Removed::name);
            removed != kRemoved.end()) {
            return fail("--" + std::string(name) + " is gone: " + std::string(removed->reason));
        }
        // --term chose a colour scheme; only "nocolor" still means anything
        if (name == "term") {
            std::string_view term;
            if (inlineValue) {
                term = *inlineValue;
            } else if (i + 1 < Args.size()) {
                term = Args[++i];
            } else {
                return fail("--term requires a value");
            }
            const std::string_view when = term == "nocolor" ? "never" : "always";
            inv.notices.push_back("--term is now --color: use --color " + std::string(when));
            inv.options.emplace_back("color", when);
            continue;
        }
        if (const auto renamed = std::ranges::find(kRenamed, name, &Renamed::oldName);
            renamed != kRenamed.end() && findOption(sub, renamed->newName)) {
            inv.notices.push_back("--" + std::string(name) + " is now --" +
                                  std::string(renamed->newName) + std::string(renamed->note));
            name = renamed->newName;
        }
        const Option* option = findOption(sub, name);
        if (!option) {
            std::string message = "unknown option " + std::string(arg.substr(0, arg.find('=')));
            if (!sub.name.empty()) {
                message += " for extempore " + std::string(sub.name);
            }
            return fail(message);
        }
        std::string_view value;
        if (option->kind == Kind::Flag) {
            if (inlineValue) {
                return fail(dashed(option->name) + " does not take a value");
            }
        } else {
            if (inlineValue) {
                value = *inlineValue;
            } else if (i + 1 < Args.size()) {
                value = Args[++i];
            } else {
                return fail(dashed(option->name) + " requires a value");
            }
            if (auto problem = checkValue(*option, value); !problem.empty()) {
                return fail(std::move(problem));
            }
        }
        if (option->name == "help") {
            inv.help = true;
        }
        if (!option->repeats && inv.has(option->name) && option->kind != Kind::Flag) {
            return fail(dashed(option->name) + " was given more than once");
        }
        inv.options.emplace_back(option->name, value);
    }
    if (inv.help) {
        return inv;
    }
    const std::string who = sub.name.empty() ? "extempore" : "extempore " + std::string(sub.name);
    if (inv.files.size() > sub.maxFiles) {
        if (inv.command == Command::Send) {
            return fail("extempore send takes the code as one argument (quote it), got " +
                        std::to_string(inv.files.size()));
        }
        return fail(sub.maxFiles == 0
                        ? who + " takes no arguments, got \"" + inv.files.front() + "\""
                        : who + " takes one file, got \"" + inv.files[0] + "\" and \"" +
                              inv.files[1] + "\"");
    }
    if (inv.files.size() < sub.minFiles) {
        return fail(who + " needs a file");
    }
    if (!inv.scriptArgs.empty() && inv.command != Command::Session &&
        inv.command != Command::Exec && inv.command != Command::Render) {
        return fail(who + " takes no arguments after --");
    }
    switch (inv.command) {
    case Command::Exec:
        if (inv.files.empty() == !inv.has("e")) {
            return fail("extempore exec takes a file or -e EXPR (one of them)");
        }
        break;
    case Command::Render:
        if (!inv.has("o")) {
            return fail("extempore render needs -o OUT.wav");
        }
        if (!inv.has("duration")) {
            return fail("extempore render needs --duration SECONDS");
        }
        if (std::strtod(std::string(inv.get("duration").value_or("0")).c_str(), nullptr) <= 0.0) {
            return fail("--duration must be more than 0");
        }
        break;
    case Command::Compile:
        if (inv.has("exe") && inv.has("name")) {
            return fail("--name is for a library, not with --exe");
        }
        if (inv.has("link") && !inv.has("exe")) {
            return fail("--link needs --exe");
        }
        break;
    case Command::Send:
        if (inv.has("file") && !inv.files.empty()) {
            return fail("extempore send takes code or --file (one of them)");
        }
        if ((inv.has("line") || inv.has("column")) && !inv.has("file")) {
            return fail("--line and --column need --file");
        }
        if (inv.has("column") && !inv.has("line")) {
            return fail("--column needs --line");
        }
        if (inv.get("line") == "0" || inv.get("column") == "0") {
            return fail("--line and --column count from 1");
        }
        break;
    case Command::Session:
        if (inv.has("repl") && inv.has("no-nrepl")) {
            return fail("--repl talks to the nREPL server, so not with --no-nrepl");
        }
        break;
    case Command::Repl:
    case Command::Lsp:
    case Command::Status:
    case Command::Fmt:
    case Command::Test:
    case Command::Devices:
    case Command::Version:
        break;
    }
    return inv;
}

}  // namespace

Invocation parse(std::span<const std::string_view> Args) {
    auto rewritten = rewriteOldModes(Args);
    if (!rewritten) {
        return parseCurrent(Args);
    }
    Invocation inv;
    if (rewritten->error.empty()) {
        const std::vector<std::string_view> args(rewritten->args.begin(), rewritten->args.end());
        inv = parseCurrent(args);
    } else {
        inv.error = std::move(rewritten->error);
    }
    inv.notices.insert(inv.notices.begin(), rewritten->notices.begin(), rewritten->notices.end());
    return inv;
}

std::vector<std::string> optionArguments(const Invocation& Inv) {
    const Subcommand& sub = subcommand(Inv.command);
    std::vector<std::string> args;
    for (const auto& [name, value] : Inv.options) {
        args.push_back(dashed(name));
        if (findOption(sub, name)->kind != Kind::Flag) {
            args.push_back(value);
        }
    }
    return args;
}

std::string help(Command Cmd) {
    const Subcommand& sub = subcommand(Cmd);
    std::string text = "usage: extempore " + std::string(sub.synopsis) + "\n\n  " +
                       std::string(sub.summary) + "\n";
    if (Cmd == Command::Session) {
        std::size_t width = 0;
        for (const auto& other : subcommands()) {
            width = std::max(width, other.synopsis.size());
        }
        text += "\nsubcommands (extempore SUBCOMMAND --help for each):\n";
        for (const auto& other : subcommands()) {
            if (other.name.empty()) {
                continue;
            }
            text += "  " + std::string(other.synopsis) +
                    std::string(width + 2 - other.synopsis.size(), ' ') +
                    std::string(other.summary) + "\n";
        }
    }
    std::vector<std::string> left;
    std::size_t width = 0;
    for (const auto& option : sub.options) {
        std::string spelling = dashed(option.name);
        if (option.name == "help") {
            spelling = "-h, --help";
        }
        if (!option.value.empty()) {
            spelling += " " + std::string(option.value);
        }
        width = std::max(width, spelling.size());
        left.push_back(std::move(spelling));
    }
    text += "\noptions:\n";
    for (std::size_t k = 0; k < sub.options.size(); ++k) {
        text += "  " + left[k] + std::string(width + 2 - left[k].size(), ' ') +
                std::string(sub.options[k].help) + "\n";
    }
    if (Cmd == Command::Session || Cmd == Command::Exec || Cmd == Command::Render) {
        text += "\nArguments after -- belong to the code, which reads them with sys:cmdarg.\n";
    }
    if (findOption(sub, "v")) {
        text += "\nMessages go to stderr. EXTEMPORE_LOG sets which: a level (error, warning, info,\n"
                "debug, trace) and COMPONENT=LEVEL filters, e.g. EXTEMPORE_LOG=debug,compiler=warning.\n";
    }
    return text;
}

}  // namespace extemp::cli
