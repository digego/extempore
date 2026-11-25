// The command-line grammar (src/CommandLine.cpp): what each form of command
// line asks for, and which are usage errors.

#include <gtest/gtest.h>

#include "CommandLine.h"

#include <initializer_list>
#include <string_view>
#include <vector>

using extemp::cli::Command;
using extemp::cli::Invocation;

namespace {

Invocation parse(std::initializer_list<std::string_view> Args) {
    const std::vector<std::string_view> args(Args);
    return extemp::cli::parse(args);
}

}  // namespace

TEST(CommandLine, NoArgumentsIsASession) {
    const auto inv = parse({});
    EXPECT_EQ(inv.command, Command::Session);
    EXPECT_TRUE(inv.error.empty());
    EXPECT_TRUE(inv.files.empty());
}

TEST(CommandLine, SessionTakesAFileAndAnExpression) {
    const auto inv =
        parse({"song.xtm", "-e", "(println 1)", "--sample-rate", "48000", "--no-audio"});
    EXPECT_EQ(inv.command, Command::Session);
    EXPECT_TRUE(inv.error.empty()) << inv.error;
    ASSERT_EQ(inv.files.size(), 1u);
    EXPECT_EQ(inv.files[0], "song.xtm");
    EXPECT_EQ(inv.get("e"), "(println 1)");
    EXPECT_EQ(inv.get("sample-rate"), "48000");
    EXPECT_TRUE(inv.has("no-audio"));
}

TEST(CommandLine, AValueMayFollowAnEqualsSign) {
    const auto inv = parse({"--sample-rate=48000", "--device=USB.*"});
    EXPECT_TRUE(inv.error.empty()) << inv.error;
    EXPECT_EQ(inv.get("sample-rate"), "48000");
    EXPECT_EQ(inv.get("device"), "USB.*");
}

TEST(CommandLine, FirstArgumentNamesTheSubcommand) {
    EXPECT_EQ(parse({"exec", "a.xtm"}).command, Command::Exec);
    EXPECT_EQ(parse({"test", "a.xtm", "b.xtm"}).command, Command::Test);
    EXPECT_EQ(parse({"devices"}).command, Command::Devices);
    EXPECT_EQ(parse({"version"}).command, Command::Version);
    // a file of the same name is given as a path
    const auto inv = parse({"./exec"});
    EXPECT_EQ(inv.command, Command::Session);
    ASSERT_EQ(inv.files.size(), 1u);
    EXPECT_EQ(inv.files[0], "./exec");
}

TEST(CommandLine, AnUnknownOptionIsAnError) {
    EXPECT_FALSE(parse({"--sampelrate", "48000"}).error.empty());
    EXPECT_FALSE(parse({"exec", "a.xtm", "--device", "2"}).error.empty());
    EXPECT_FALSE(parse({"-x"}).error.empty());
    EXPECT_FALSE(parse({"-sample-rate", "48000"}).error.empty());
}

TEST(CommandLine, ValuesAreChecked) {
    EXPECT_FALSE(parse({"--sample-rate", "fast"}).error.empty());
    EXPECT_FALSE(parse({"--sample-rate"}).error.empty());
    EXPECT_FALSE(parse({"--no-audio=yes"}).error.empty());
    EXPECT_FALSE(parse({"--color", "blue"}).error.empty());
    EXPECT_FALSE(parse({"--frames", "64", "--frames", "128"}).error.empty());
    EXPECT_TRUE(parse({"--color", "never"}).error.empty());
}

TEST(CommandLine, ExecTakesAFileOrAnExpression) {
    EXPECT_TRUE(parse({"exec", "a.xtm"}).error.empty());
    EXPECT_TRUE(parse({"exec", "-e", "(quit 0)", "--wait"}).error.empty());
    EXPECT_FALSE(parse({"exec"}).error.empty());
    EXPECT_FALSE(parse({"exec", "a.xtm", "-e", "1"}).error.empty());
    EXPECT_FALSE(parse({"exec", "a.xtm", "b.xtm"}).error.empty());
}

TEST(CommandLine, ArgumentsAfterTheDoubleDashBelongToTheScript) {
    const auto inv = parse({"exec", "a.xtm", "--", "--label=one two", "--dry-run"});
    EXPECT_TRUE(inv.error.empty()) << inv.error;
    ASSERT_EQ(inv.scriptArgs.size(), 2u);
    EXPECT_EQ(inv.scriptArgs[0].first, "label");
    EXPECT_EQ(inv.scriptArgs[0].second, "one two");
    EXPECT_EQ(inv.scriptArgs[1].first, "dry-run");
    EXPECT_EQ(inv.scriptArgs[1].second, "");
    // an option's own spelling is the script's there
    EXPECT_EQ(parse({"exec", "a.xtm", "--", "--wait"}).scriptArgs.size(), 1u);
    EXPECT_FALSE(parse({"exec", "a.xtm", "--", "stray"}).error.empty());
    EXPECT_FALSE(parse({"devices", "--", "--x"}).error.empty());
}

TEST(CommandLine, RenderNeedsAnOutputAndADuration) {
    EXPECT_TRUE(parse({"render", "a.xtm", "-o", "a.wav", "--duration", "2.5", "--seed", "7"})
                    .error.empty());
    EXPECT_FALSE(parse({"render", "a.xtm", "--duration", "2"}).error.empty());
    EXPECT_FALSE(parse({"render", "a.xtm", "-o", "a.wav"}).error.empty());
    EXPECT_FALSE(parse({"render", "a.xtm", "-o", "a.wav", "--duration", "0"}).error.empty());
    EXPECT_FALSE(parse({"render", "-o", "a.wav", "--duration", "2"}).error.empty());
}

TEST(CommandLine, CompileOptions) {
    const auto inv = parse({"compile", "--exe", "a.xtm", "--attr", "+avx", "--attr", "+fma"});
    EXPECT_TRUE(inv.error.empty()) << inv.error;
    EXPECT_EQ(inv.all("attr").size(), 2u);
    EXPECT_FALSE(parse({"compile", "a.xtm", "--link", "-lm"}).error.empty());
    EXPECT_FALSE(parse({"compile", "--exe", "a.xtm", "--name", "b"}).error.empty());
}

TEST(CommandLine, SendTakesCodeOrAFile) {
    EXPECT_TRUE(parse({"send", "(+ 1 2)", "--port", "7900", "--process", "utility"}).error.empty());
    EXPECT_TRUE(parse({"send"}).error.empty());  // the code is on stdin
    EXPECT_TRUE(parse({"send", "--file", "a.xtm", "--line", "3", "--column", "2"}).error.empty());
    EXPECT_FALSE(parse({"send", "(+ 1 2)", "--file", "a.xtm"}).error.empty());
    EXPECT_FALSE(parse({"send", "--line", "3"}).error.empty());
    EXPECT_FALSE(parse({"send", "--file", "a.xtm", "--column", "3"}).error.empty());
    EXPECT_FALSE(parse({"send", "--file", "a.xtm", "--line", "0"}).error.empty());
    EXPECT_FALSE(parse({"send", "(+", "1", "2)"}).error.empty());
    // here --port is the server to talk to, not a session's legacy port
    const auto inv = parse({"send", "1", "--port", "7900"});
    EXPECT_EQ(inv.get("port"), "7900");
    EXPECT_TRUE(inv.notices.empty());
}

TEST(CommandLine, ReplAndStatusTakeNoArguments) {
    EXPECT_EQ(parse({"lsp", "--port", "7900"}).command, Command::Lsp);
    EXPECT_EQ(parse({"repl", "--process", "utility"}).command, Command::Repl);
    EXPECT_EQ(parse({"status", "--host", "10.0.0.2"}).command, Command::Status);
    EXPECT_FALSE(parse({"status", "now"}).error.empty());
    EXPECT_FALSE(parse({"--repl", "--no-nrepl"}).error.empty());
}

TEST(CommandLine, RenamedOptionsAreAcceptedWithANotice) {
    const auto inv =
        parse({"--noaudio", "--samplerate", "48000", "--device-name", "USB", "--port", "7200"});
    EXPECT_TRUE(inv.error.empty()) << inv.error;
    EXPECT_TRUE(inv.has("no-audio"));
    EXPECT_EQ(inv.get("sample-rate"), "48000");
    EXPECT_EQ(inv.get("device"), "USB");
    EXPECT_EQ(inv.get("legacy-port"), "7200");
    EXPECT_EQ(inv.notices.size(), 4u);
    EXPECT_TRUE(parse({"--no-audio"}).notices.empty());
}

TEST(CommandLine, TermBecomesColor) {
    EXPECT_EQ(parse({"--term", "nocolor"}).get("color"), "never");
    EXPECT_EQ(parse({"--term", "ansi"}).get("color"), "always");
    EXPECT_EQ(parse({"--term", "nocolor"}).notices.size(), 1u);
}

TEST(CommandLine, RemovedOptionsSayWhy) {
    EXPECT_NE(parse({"--runtime", "/x"}).error.find("--share-dir"), std::string::npos);
    EXPECT_FALSE(parse({"--timediv", "4"}).error.empty());
}

TEST(CommandLine, HelpIsForTheSubcommandGiven) {
    const auto inv = parse({"render", "--help"});
    EXPECT_TRUE(inv.help);
    EXPECT_TRUE(inv.error.empty());  // not "needs -o"
    EXPECT_TRUE(parse({"-h"}).help);
    EXPECT_NE(extemp::cli::help(Command::Render).find("--duration"), std::string::npos);
    EXPECT_NE(extemp::cli::help(Command::Session).find("render FILE"), std::string::npos);
    EXPECT_EQ(extemp::cli::help(Command::Exec).find("--device"), std::string::npos);
}

TEST(CommandLine, OldModeOptionsBecomeTheSubcommandWhichReplacesThem) {
    auto inv = parse({"--nobase", "--batch", "(quit 0)"});
    EXPECT_TRUE(inv.error.empty()) << inv.error;
    EXPECT_EQ(inv.command, Command::Exec);
    EXPECT_EQ(inv.get("e"), "(quit 0)");
    EXPECT_TRUE(inv.has("wait"));
    EXPECT_TRUE(inv.has("no-base"));
    ASSERT_EQ(inv.notices.size(), 2u);
    EXPECT_NE(inv.notices[0].find("--batch is deprecated"), std::string::npos);
    EXPECT_NE(inv.notices[0].find("extempore exec"), std::string::npos);

    // --batch meant no audio, so saying it as well is still allowed
    EXPECT_TRUE(parse({"--noaudio", "--batch=(quit 0)"}).error.empty());

    inv = parse({"--run", "a.xtm", "--samplerate", "48000"});
    EXPECT_TRUE(inv.error.empty()) << inv.error;
    EXPECT_EQ(inv.command, Command::Session);
    EXPECT_EQ(inv.files, std::vector<std::string>{"a.xtm"});
    EXPECT_EQ(inv.get("sample-rate"), "48000");

    inv = parse({"--noaudio", "--eval=(println 1)"});
    EXPECT_EQ(inv.command, Command::Session);
    EXPECT_EQ(inv.get("e"), "(println 1)");
    EXPECT_TRUE(inv.has("no-audio"));

    inv = parse({"--audio-outfile", "a.wav", "--run", "a.xtm", "--duration", "2"});
    EXPECT_TRUE(inv.error.empty()) << inv.error;
    EXPECT_EQ(inv.command, Command::Render);
    EXPECT_EQ(inv.files, std::vector<std::string>{"a.xtm"});
    EXPECT_EQ(inv.get("o"), "a.wav");

    inv = parse({"--compile", "app.xtm"});
    EXPECT_EQ(inv.command, Command::Compile);
    EXPECT_TRUE(inv.has("exe"));

    EXPECT_EQ(parse({"--print-devices"}).command, Command::Devices);
}

TEST(CommandLine, OldModeOptionsWithNoReplacementAreErrors) {
    // a render takes a file and a duration
    EXPECT_FALSE(
        parse({"--batch", "(f)", "--audio-outfile", "a.wav", "--duration", "2"}).error.empty());
    EXPECT_FALSE(parse({"--run", "a.xtm", "--audio-outfile", "a.wav"}).error.empty());
    EXPECT_FALSE(parse({"--run", "a.xtm", "--eval", "(f)"}).error.empty());
    EXPECT_FALSE(parse({"--batch"}).error.empty());
    // an unknown option was once kept for sys:cmdarg
    const auto inv = parse({"--label=x", "--batch", "(quit 0)"});
    EXPECT_NE(inv.error.find("unknown option --label"), std::string::npos);
    EXPECT_EQ(inv.notices.size(), 1u);  // which still says what replaces --batch
}

TEST(CommandLine, OldModeOptionsAfterTheSeparatorBelongToTheScript) {
    const auto inv = parse({"exec", "a.xtm", "--", "--batch"});
    EXPECT_TRUE(inv.error.empty()) << inv.error;
    EXPECT_TRUE(inv.notices.empty());
    EXPECT_TRUE(parse({"--noaudio", "--port", "7200"}).error.empty());
}

TEST(CommandLine, OptionsGoBackToArguments) {
    using Args = std::vector<std::string>;
    EXPECT_EQ(extemp::cli::optionArguments(
                  parse({"test", "a.xtm", "b.xtm", "--no-base", "--junit=", "--opt-level", "2"})),
              (Args{"--no-base", "--junit", "", "--opt-level", "2"}));
    EXPECT_EQ(extemp::cli::optionArguments(parse({"exec", "-e", "(f)", "--nobase"})),
              (Args{"-e", "(f)", "--no-base"}));
}
