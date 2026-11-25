// The logger (src/Log.cpp): how EXTEMPORE_LOG reads, what a line looks like,
// which messages pass the thresholds, and the realtime path.

#include <gtest/gtest.h>

#include "Log.h"

#include <string>
#include <thread>
#include <vector>

using extemp::log::Filter;
using extemp::log::Level;

TEST(Log, FilterReadsALevelAndComponentLevels) {
    Filter filter;
    EXPECT_EQ(extemp::log::parse_filter("debug, compiler=warning,llvm=trace", filter), "");
    EXPECT_EQ(filter.base, Level::Debug);
    ASSERT_EQ(filter.components.size(), 2u);
    EXPECT_EQ(filter.components[0], (std::pair<std::string, Level>{"compiler", Level::Warning}));
    EXPECT_EQ(filter.components[1], (std::pair<std::string, Level>{"llvm", Level::Trace}));
}

TEST(Log, FilterNamesWhatItCantRead) {
    Filter filter;
    EXPECT_EQ(extemp::log::parse_filter("info,loud", filter), "loud");
    EXPECT_EQ(extemp::log::parse_filter("audio=", filter), "audio=");
    EXPECT_EQ(extemp::log::parse_filter("=debug", filter), "=debug");
    EXPECT_EQ(extemp::log::parse_filter("", filter), "");
}

TEST(Log, LevelNamesRoundTrip) {
    for (const Level level :
         {Level::Off, Level::Error, Level::Warning, Level::Info, Level::Debug, Level::Trace}) {
        EXPECT_EQ(extemp::log::parse_level(extemp::log::level_name(level)), level);
    }
    EXPECT_EQ(extemp::log::parse_level("warn"), Level::Warning);
    EXPECT_FALSE(extemp::log::parse_level("loud"));
}

TEST(Log, InfoIsPlainAndTheOtherLevelsAreLabelled) {
    using extemp::log::format_line;
    EXPECT_EQ(format_line(Level::Info, "compiler", "Compiled: f", "", false), "Compiled: f");
    EXPECT_EQ(format_line(Level::Error, "", "bad\n", "", false), "error: bad");
    EXPECT_EQ(format_line(Level::Warning, "audio", "late", "", false), "warning: audio: late");
    EXPECT_EQ(format_line(Level::Debug, "midi", "x", "12:00:01.002", false),
              "12:00:01.002 debug: midi: x");
    EXPECT_EQ(format_line(Level::Error, "", "bad", "", true), "\x1b[1;31merror:\x1b[0m bad");
}

TEST(Log, ComponentLevelsOverrideTheBase) {
    extemp::log::set_level(Level::Warning);
    extemp::log::set_level("unit-a", Level::Debug);
    extemp::log::set_level("unit-b", Level::Error);
    EXPECT_FALSE(extemp::log::enabled(Level::Info));
    EXPECT_TRUE(extemp::log::enabled(Level::Warning));
    EXPECT_TRUE(extemp::log::enabled(Level::Debug, "unit-a"));
    EXPECT_FALSE(extemp::log::enabled(Level::Warning, "unit-b"));
    EXPECT_TRUE(extemp::log::enabled(Level::Warning, "unit-c"));
    EXPECT_FALSE(extemp::log::enabled(Level::Off));
    EXPECT_EQ(extemp::log::level("unit-a"), Level::Debug);
    extemp::log::set_level(Level::Info);
}

TEST(Log, ErrorsAreCountedEvenWhenHidden) {
    extemp::log::set_level(Level::Off);
    testing::internal::CaptureStderr();
    const auto before = extemp::log::errors();
    extemp::log::error("unit test: {}", 1);
    extemp::log::warn("not counted");
    EXPECT_EQ(extemp::log::errors(), before + 1);
    EXPECT_EQ(testing::internal::GetCapturedStderr(), "");
    extemp::log::set_level(Level::Info);
}

namespace {
struct Forwarded {
    std::vector<std::pair<Level, std::string>> lines;
};
void collect(void* Context, Level L, std::string_view Line) {
    static_cast<Forwarded*>(Context)->lines.emplace_back(L, std::string(Line));
}
}  // namespace

TEST(Log, AForwardSeesThisThreadsMessagesWithoutColour) {
    extemp::log::set_level(Level::Info);
    Forwarded seen;
    testing::internal::CaptureStderr();
    {
        const extemp::log::ScopedForward forward(collect, &seen);
        extemp::log::warn(extemp::log::audio, "late by {} ms", 3);
        std::thread([] { extemp::log::warn("another thread"); }).join();
    }
    extemp::log::warn("after");
    const std::string written = testing::internal::GetCapturedStderr();
    EXPECT_NE(written.find("warning: another thread\n"), std::string::npos);
    ASSERT_EQ(seen.lines.size(), 1u);
    EXPECT_EQ(seen.lines[0].first, Level::Warning);
    EXPECT_EQ(seen.lines[0].second, "warning: audio: late by 3 ms");
}

TEST(Log, ARealtimeThreadQueuesAndDropsWhatDoesntFit) {
    extemp::log::set_level(Level::Info);
    extemp::log::set_level("unit-rt", Level::Info);
    const auto dropped = extemp::log::dropped();
    testing::internal::CaptureStderr();
    std::thread([] {
        extemp::log::mark_realtime_thread();
        EXPECT_TRUE(extemp::log::realtime_thread());
        for (int i = 0; i < 1000; ++i) {
            extemp::log::info(extemp::log::Component("unit-rt"), "message {}", i);
        }
    }).join();
    EXPECT_GT(extemp::log::dropped(), dropped);
    EXPECT_FALSE(extemp::log::realtime_thread());
    extemp::log::flush();
    const std::string written = testing::internal::GetCapturedStderr();
    EXPECT_NE(written.find("message 0\n"), std::string::npos);
    EXPECT_NE(written.find("messages from the audio thread were dropped"), std::string::npos);
}
