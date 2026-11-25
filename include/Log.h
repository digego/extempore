#pragma once

// Extempore's diagnostics: everything the runtime says about its own work
// (startup, a library loading, a function compiled, a warning, an error
// report). Every message goes to stderr, whatever the mode, which leaves
// stdout to what the user's code prints and to each subcommand's product.
//
// A message has a level and, optionally, a component ("audio", "compiler") to
// filter by: EXTEMPORE_LOG=debug,llvm=trace,compiler=warning. Info is plain
// text; the other levels carry a label ("warning: ...").
//
// Logging is safe from the audio thread (one marked with mark_realtime_thread):
// there a message is formatted into a fixed buffer and queued without locking
// or allocating, and a logger thread writes it out. A message which doesn't
// fit in the queue is dropped and counted.
//
// During an nREPL eval the eval's thread forwards its messages to the client
// as well (ScopedForward).

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace extemp::log {

enum class Level : std::uint8_t { Off, Error, Warning, Info, Debug, Trace };

// "error", "warning" (or "warn"), "info", "debug", "trace", "off".
std::optional<Level> parse_level(std::string_view Name);
std::string_view level_name(Level L);

// A part of extempore, named in a message and in EXTEMPORE_LOG's filters.
struct Component {
    std::string_view name;
    explicit constexpr Component(std::string_view Name) : name(Name) {}
};
inline constexpr Component audio{"audio"};
inline constexpr Component compiler{"compiler"};
inline constexpr Component jit{"llvm"};
inline constexpr Component midi{"midi"};
inline constexpr Component nrepl{"nrepl"};
inline constexpr Component osc{"osc"};
inline constexpr Component scheme{"scheme"};
inline constexpr Component server{"server"};
inline constexpr Component zone{"zone"};

// The thresholds: a base level, and levels for named components.
struct Filter {
    Level base = Level::Info;
    std::vector<std::pair<std::string, Level>> components;
};

// Parse an EXTEMPORE_LOG value into Into: comma-separated items, each a level
// (the base) or COMPONENT=LEVEL. Returns the first item it can't read, or "".
std::string parse_filter(std::string_view Spec, Filter& Into);

// The line a message becomes, without its newline. Time is "HH:MM:SS.mmm" or
// empty; Colour adds escape sequences to the label.
std::string format_line(Level L, std::string_view Component, std::string_view Message,
                        std::string_view Time, bool Colour);

// Set up from the command line and the environment, before the threads start.
struct Settings {
    Filter filter;
    bool timestamps = false;  // each line starts with the wall-clock time
};
void configure(const Settings& S);
// Start the thread which writes out what realtime threads log.
void start();
// Write out whatever realtime threads have queued: before the process exits.
void flush() noexcept;

// Whether stderr, where messages go, gets escape sequences.
bool colour();
// Change the base level, or (with a component name) one component's, while
// running. Not for a realtime thread.
void set_level(Level L);
void set_level(std::string_view Component, Level L);
Level level(std::string_view Component = {});

bool enabled(Level L, std::string_view Component = {});
// Write a message which is already formatted. Message may hold several lines.
void emit(Level L, std::string_view Component, std::string_view Message) noexcept;

// This thread runs audio: from now on its messages are queued, never written
// directly.
void mark_realtime_thread();
bool realtime_thread();
// Errors logged so far, shown or not: a run which logged one failed.
std::uint64_t errors();
// Messages dropped because the realtime queue was full, so far.
std::uint64_t dropped();

// While one is alive, messages logged on this thread are also handed to
// Forward(Context, level, line), the line without timestamp or colour.
class ScopedForward {
public:
    using Fn = void (*)(void* Context, Level L, std::string_view Line);
    ScopedForward(Fn Forward, void* Context);
    ~ScopedForward();
    ScopedForward(const ScopedForward&) = delete;
    ScopedForward& operator=(const ScopedForward&) = delete;

private:
    Fn mPrevious;
    void* mPreviousContext;
};

// The longest message a realtime thread can queue; the rest is cut off.
inline constexpr std::size_t kRealtimeMessage = 240;

namespace detail {
void emit_realtime(Level L, std::string_view Component, std::string_view Message) noexcept;
// A message below the threshold: an error is still counted.
void filtered(Level L) noexcept;

// Logging never throws: a message which can't be formatted (std::format can
// only fail to allocate, the format strings being checked at compile time) is
// replaced by a note that it couldn't be.
template <class... Args>
void log(Level L, std::string_view Component, std::format_string<Args...> Fmt,
         Args&&... A) noexcept {
    if (!enabled(L, Component)) {
        filtered(L);
        return;
    }
    try {
        if (realtime_thread()) [[unlikely]] {
            char buffer[kRealtimeMessage];
            const auto result =
                std::format_to_n(buffer, sizeof(buffer), Fmt, std::forward<Args>(A)...);
            const auto size = std::min<std::size_t>(std::size_t(result.size), sizeof(buffer));
            emit_realtime(L, Component, std::string_view(buffer, size));
            return;
        }
        emit(L, Component, std::format(Fmt, std::forward<Args>(A)...));
    } catch (...) {
        emit(L, Component, "(a message which couldn't be formatted)");
    }
}
}  // namespace detail

template <class... Args>
void error(std::format_string<Args...> Fmt, Args&&... A) noexcept {
    detail::log(Level::Error, {}, Fmt, std::forward<Args>(A)...);
}
template <class... Args>
void error(Component C, std::format_string<Args...> Fmt, Args&&... A) noexcept {
    detail::log(Level::Error, C.name, Fmt, std::forward<Args>(A)...);
}
template <class... Args>
void warn(std::format_string<Args...> Fmt, Args&&... A) noexcept {
    detail::log(Level::Warning, {}, Fmt, std::forward<Args>(A)...);
}
template <class... Args>
void warn(Component C, std::format_string<Args...> Fmt, Args&&... A) noexcept {
    detail::log(Level::Warning, C.name, Fmt, std::forward<Args>(A)...);
}
template <class... Args>
void info(std::format_string<Args...> Fmt, Args&&... A) noexcept {
    detail::log(Level::Info, {}, Fmt, std::forward<Args>(A)...);
}
template <class... Args>
void info(Component C, std::format_string<Args...> Fmt, Args&&... A) noexcept {
    detail::log(Level::Info, C.name, Fmt, std::forward<Args>(A)...);
}
template <class... Args>
void debug(std::format_string<Args...> Fmt, Args&&... A) noexcept {
    detail::log(Level::Debug, {}, Fmt, std::forward<Args>(A)...);
}
template <class... Args>
void debug(Component C, std::format_string<Args...> Fmt, Args&&... A) noexcept {
    detail::log(Level::Debug, C.name, Fmt, std::forward<Args>(A)...);
}
template <class... Args>
void trace(Component C, std::format_string<Args...> Fmt, Args&&... A) noexcept {
    detail::log(Level::Trace, C.name, Fmt, std::forward<Args>(A)...);
}

// Escape sequences for colouring text on stdout or stderr: "" when that stream
// isn't coloured. Foreground and background are ANSI colour numbers 0-7, 9 for
// the terminal's default.
std::string colour_code(std::FILE* Stream, bool Bold, unsigned Foreground, unsigned Background);
// Whether Stream gets escape sequences.
bool coloured(std::FILE* Stream);
// Decide colour for stdout and stderr: When is "auto", "always" or "never";
// auto colours a terminal unless NO_COLOR is set. On Windows, auto and always
// switch the console to ANSI escape sequences.
void decide_colour(std::string_view When);

}  // namespace extemp::log
