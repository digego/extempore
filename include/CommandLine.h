#pragma once

// The extempore command line: subcommands, each with its own option table,
// which drives both its parser and its --help. Parsing is pure (arguments in,
// an Invocation out) so that it can be unit-tested; src/Extempore.cpp acts on
// the result. backlog/docs/doc-003 is the design.
//
// A command line with one of the old mode options (--run, --eval, --batch,
// --compile, --audio-outfile, --print-devices) is rewritten as the subcommand
// which replaces it and then parsed like any other, with a notice.

#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace extemp::cli {

enum class Command : std::uint8_t {
    Session,  // no subcommand: a live session
    Exec,
    Test,
    Render,
    Compile,
    Send,
    Repl,
    Lsp,
    Status,
    Devices,
    Version,
};

// What a command line asks for, before any of it is acted on.
struct Invocation {
    Command command = Command::Session;
    // Options by their current name without the leading dashes ("sample-rate",
    // "e"), in the order given; a flag's value is empty.
    std::vector<std::pair<std::string, std::string>> options;
    std::vector<std::string> files;  // the positional arguments
    // Arguments after "--", as name and value: what sys:cmdarg reads.
    std::vector<std::pair<std::string, std::string>> scriptArgs;
    std::vector<std::string> notices;  // deprecations, to print on stderr
    std::string error;                 // non-empty: a usage error (exit status 2)
    bool help = false;                 // print help(command) and exit

    bool has(std::string_view Name) const;
    // The last value given for Name.
    std::optional<std::string_view> get(std::string_view Name) const;
    // Every value given for Name, for options which repeat (--attr).
    std::vector<std::string_view> all(std::string_view Name) const;
};

// Args excludes the program name.
Invocation parse(std::span<const std::string_view> Args);

// Inv's options as arguments again, for another extempore to parse.
std::vector<std::string> optionArguments(const Invocation& Inv);

// The --help text for a command; Session's lists the subcommands too.
std::string help(Command Cmd);

}  // namespace extemp::cli
