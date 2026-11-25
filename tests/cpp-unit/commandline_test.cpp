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

bool oldGrammar(std::initializer_list<std::string_view> Args) {
    const std::vector<std::string_view> args(Args);
    return extemp::cli::usesOldGrammar(args);
}

}  // namespace

TEST(CommandLine, NoArgumentsIsASession) {
    const auto inv = parse({});
    EXPECT_EQ(inv.command, Command::Session);
    EXPECT_TRUE(inv.error.empty());
    EXPECT_TRUE(inv.files.empty());
}

TEST(CommandLine, SessionTakesAFileAndAnExpression) {
    const auto inv = parse({"song.xtm", "-e", "(println 1)", "--sample-rate", "48000", "--no-audio"});
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

TEST(CommandLine, RenamedOptionsAreAcceptedWithANotice) {
    const auto inv = parse({"--noaudio", "--samplerate", "48000", "--device-name", "USB", "--port", "7200"});
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

TEST(CommandLine, OldModeOptionsSelectTheOldGrammar) {
    EXPECT_TRUE(oldGrammar({"--batch", "(quit 0)"}));
    EXPECT_TRUE(oldGrammar({"--noaudio", "--eval=(println 1)"}));
    EXPECT_TRUE(oldGrammar({"--audio-outfile", "a.wav", "--run", "a.xtm"}));
    EXPECT_TRUE(oldGrammar({"--print-devices"}));
    EXPECT_FALSE(oldGrammar({"--noaudio", "--port", "7200"}));
    EXPECT_FALSE(oldGrammar({"exec", "a.xtm", "--", "--batch"}));
    EXPECT_FALSE(oldGrammar({}));
}
