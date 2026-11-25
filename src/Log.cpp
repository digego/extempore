#include "Log.h"

#include <algorithm>
#include <array>
#include <atomic>
#include <chrono>
#include <cstdlib>
#include <cstring>
#include <ctime>
#include <mutex>
#include <thread>

#ifdef _WIN32
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#include <io.h>
#include <windows.h>
#else
#include <unistd.h>
#endif

namespace extemp::log {

namespace {

// Component thresholds: a fixed table, so that enabled() reads it without a
// lock from any thread, the audio thread included. Entries are only added.
constexpr std::size_t kMaxComponents = 32;
constexpr std::size_t kComponentName = 24;
struct ComponentLevel {
    char name[kComponentName] = {};
    std::atomic<std::uint8_t> level{0};
};
std::array<ComponentLevel, kMaxComponents> sComponents;
std::atomic<std::size_t> sComponentCount{0};
std::mutex sComponentMutex;  // for adding entries
std::atomic<std::uint8_t> sBase{std::uint8_t(Level::Info)};

bool sTimestamps = false;
std::atomic<bool> sColourOut{false};
std::atomic<bool> sColourErr{false};
std::mutex sWriteMutex;

thread_local bool tRealtime = false;
thread_local ScopedForward::Fn tForward = nullptr;
thread_local void* tForwardContext = nullptr;

std::atomic<std::uint64_t> sDropped{0};
std::atomic<std::uint64_t> sErrors{0};

// A bounded multi-producer multi-consumer queue (Vyukov's): a push or a pop
// claims a slot with one compare-and-swap and never waits for another thread.
class Queue {
public:
    struct Entry {
        Level level = Level::Off;
        std::int64_t millis = 0;  // wall clock, when it was logged
        std::uint8_t componentLength = 0;
        char component[kComponentName] = {};
        std::uint16_t length = 0;
        char text[kRealtimeMessage] = {};
    };

    Queue() {
        for (std::size_t i = 0; i < kSlots; ++i) {
            mSlots[i].seq.store(i, std::memory_order_relaxed);
        }
    }

    bool push(Level L, std::int64_t Millis, std::string_view Component,
              std::string_view Message) {
        std::size_t pos = mHead.load(std::memory_order_relaxed);
        Slot* slot = nullptr;
        for (;;) {
            slot = &mSlots[pos % kSlots];
            const std::size_t seq = slot->seq.load(std::memory_order_acquire);
            const auto diff = std::intptr_t(seq) - std::intptr_t(pos);
            if (diff == 0) {
                if (mHead.compare_exchange_weak(pos, pos + 1, std::memory_order_relaxed)) {
                    break;
                }
            } else if (diff < 0) {
                return false;  // full
            } else {
                pos = mHead.load(std::memory_order_relaxed);
            }
        }
        Entry& e = slot->entry;
        e.level = L;
        e.millis = Millis;
        e.componentLength = std::uint8_t(std::min(Component.size(), kComponentName));
        std::memcpy(e.component, Component.data(), e.componentLength);
        e.length = std::uint16_t(std::min(Message.size(), kRealtimeMessage));
        std::memcpy(e.text, Message.data(), e.length);
        slot->seq.store(pos + 1, std::memory_order_release);
        return true;
    }

    bool pop(Entry& Out) {
        std::size_t pos = mTail.load(std::memory_order_relaxed);
        Slot* slot = nullptr;
        for (;;) {
            slot = &mSlots[pos % kSlots];
            const std::size_t seq = slot->seq.load(std::memory_order_acquire);
            const auto diff = std::intptr_t(seq) - std::intptr_t(pos + 1);
            if (diff == 0) {
                if (mTail.compare_exchange_weak(pos, pos + 1, std::memory_order_relaxed)) {
                    break;
                }
            } else if (diff < 0) {
                return false;  // empty
            } else {
                pos = mTail.load(std::memory_order_relaxed);
            }
        }
        Out = slot->entry;
        slot->seq.store(pos + kSlots, std::memory_order_release);
        return true;
    }

private:
    static constexpr std::size_t kSlots = 128;
    struct Slot {
        std::atomic<std::size_t> seq{0};
        Entry entry;
    };
    std::array<Slot, kSlots> mSlots;
    alignas(64) std::atomic<std::size_t> mHead{0};
    alignas(64) std::atomic<std::size_t> mTail{0};
};

Queue& queue() {
    static Queue q;
    return q;
}

std::int64_t now_millis() {
    return std::chrono::duration_cast<std::chrono::milliseconds>(
               std::chrono::system_clock::now().time_since_epoch())
        .count();
}

std::string time_string(std::int64_t Millis) {
    const std::time_t seconds = std::time_t(Millis / 1000);
    std::tm local{};
#ifdef _WIN32
    localtime_s(&local, &seconds);
#else
    localtime_r(&seconds, &local);
#endif
    return std::format("{:02}:{:02}:{:02}.{:03}", local.tm_hour, local.tm_min, local.tm_sec,
                       int(Millis % 1000));
}

bool is_terminal(std::FILE* Stream) {
#ifdef _WIN32
    return _isatty(_fileno(Stream)) != 0;
#else
    return isatty(fileno(Stream)) != 0;
#endif
}

std::string strip_escapes(std::string_view Text) {
    std::string out;
    out.reserve(Text.size());
    for (std::size_t i = 0; i < Text.size(); ++i) {
        if (Text[i] == '\x1b' && i + 1 < Text.size() && Text[i + 1] == '[') {
            i += 2;
            while (i < Text.size() && !(Text[i] >= 0x40 && Text[i] <= 0x7e)) {
                ++i;
            }
            continue;
        }
        out += Text[i];
    }
    return out;
}

void write_line(Level L, std::string_view Component, std::string_view Message,
                std::int64_t Millis) {
    const std::string time = sTimestamps ? time_string(Millis) : std::string();
    std::string line =
        format_line(L, Component, Message, time, sColourErr.load(std::memory_order_relaxed));
    line += '\n';
    const std::lock_guard<std::mutex> lock(sWriteMutex);
    std::fwrite(line.data(), 1, line.size(), stderr);
    std::fflush(stderr);
}

std::mutex sDrainMutex;

void drain() {
    const std::lock_guard<std::mutex> lock(sDrainMutex);
    Queue::Entry entry;
    while (queue().pop(entry)) {
        write_line(entry.level, std::string_view(entry.component, entry.componentLength),
                   std::string_view(entry.text, entry.length), entry.millis);
    }
    static std::uint64_t reported = 0;
    const std::uint64_t dropped = sDropped.load(std::memory_order_relaxed);
    if (dropped > reported) {
        write_line(Level::Warning, audio.name,
                   std::format("{} messages from the audio thread were dropped: it logged "
                               "faster than they could be written",
                               dropped - reported),
                   now_millis());
        reported = dropped;
    }
}

}  // namespace

std::optional<Level> parse_level(std::string_view Name) {
    if (Name == "off") {
        return Level::Off;
    }
    if (Name == "error") {
        return Level::Error;
    }
    if (Name == "warning" || Name == "warn") {
        return Level::Warning;
    }
    if (Name == "info") {
        return Level::Info;
    }
    if (Name == "debug") {
        return Level::Debug;
    }
    if (Name == "trace") {
        return Level::Trace;
    }
    return std::nullopt;
}

std::string_view level_name(Level L) {
    switch (L) {
    case Level::Off:
        return "off";
    case Level::Error:
        return "error";
    case Level::Warning:
        return "warning";
    case Level::Info:
        return "info";
    case Level::Debug:
        return "debug";
    case Level::Trace:
        return "trace";
    }
    return "";
}

std::string parse_filter(std::string_view Spec, Filter& Into) {
    const auto trim = [](std::string_view S) {
        while (!S.empty() && (S.front() == ' ' || S.front() == '\t')) {
            S.remove_prefix(1);
        }
        while (!S.empty() && (S.back() == ' ' || S.back() == '\t')) {
            S.remove_suffix(1);
        }
        return S;
    };
    while (!Spec.empty()) {
        const auto comma = Spec.find(',');
        const std::string_view item = trim(Spec.substr(0, comma));
        Spec = comma == std::string_view::npos ? std::string_view() : Spec.substr(comma + 1);
        if (item.empty()) {
            continue;
        }
        const auto eq = item.find('=');
        if (eq == std::string_view::npos) {
            const auto level = parse_level(item);
            if (!level) {
                return std::string(item);
            }
            Into.base = *level;
            continue;
        }
        const std::string_view name = trim(item.substr(0, eq));
        const auto level = parse_level(trim(item.substr(eq + 1)));
        if (name.empty() || !level) {
            return std::string(item);
        }
        Into.components.emplace_back(name, *level);
    }
    return {};
}

std::string format_line(Level L, std::string_view Component, std::string_view Message,
                        std::string_view Time, bool Colour) {
    while (!Message.empty() && Message.back() == '\n') {
        Message.remove_suffix(1);
    }
    std::string line;
    if (!Time.empty()) {
        line += Time;
        line += ' ';
    }
    std::string_view style;
    switch (L) {
    case Level::Error:
        style = "\x1b[1;31m";
        break;
    case Level::Warning:
        style = "\x1b[1;33m";
        break;
    case Level::Debug:
    case Level::Trace:
        style = "\x1b[2m";
        break;
    case Level::Info:
    case Level::Off:
        break;
    }
    if (L != Level::Info && L != Level::Off) {
        if (Colour) {
            line += style;
        }
        line += level_name(L);
        line += ':';
        if (Colour) {
            line += "\x1b[0m";
        }
        line += ' ';
        if (!Component.empty()) {
            line += Component;
            line += ": ";
        }
    }
    line += Message;
    return line;
}

void configure(const Settings& S) {
    sBase.store(std::uint8_t(S.filter.base), std::memory_order_relaxed);
    for (const auto& [name, level] : S.filter.components) {
        set_level(name, level);
    }
    sTimestamps = S.timestamps;
}

void start() {
    static std::once_flag once;
    std::call_once(once, [] {
        // runs for the life of the process, like the other service threads
        std::thread([] {
            for (;;) {
                std::this_thread::sleep_for(std::chrono::milliseconds(20));
                drain();
            }
        }).detach();
    });
}

void flush() noexcept {
    try {
        drain();
    } catch (...) {
        std::fputs("extempore: out of memory while writing queued messages\n", stderr);
    }
    std::fflush(stderr);
}

bool colour() {
    return sColourErr.load(std::memory_order_relaxed);
}

void set_level(Level L) {
    sBase.store(std::uint8_t(L), std::memory_order_relaxed);
}

void set_level(std::string_view Component, Level L) {
    if (Component.empty()) {
        set_level(L);
        return;
    }
    const std::lock_guard<std::mutex> lock(sComponentMutex);
    const std::size_t n = sComponentCount.load(std::memory_order_acquire);
    for (std::size_t i = 0; i < n; ++i) {
        if (Component == sComponents[i].name) {
            sComponents[i].level.store(std::uint8_t(L), std::memory_order_relaxed);
            return;
        }
    }
    if (n == kMaxComponents || Component.size() >= kComponentName) {
        return;
    }
    std::memcpy(sComponents[n].name, Component.data(), Component.size());
    sComponents[n].name[Component.size()] = '\0';
    sComponents[n].level.store(std::uint8_t(L), std::memory_order_relaxed);
    sComponentCount.store(n + 1, std::memory_order_release);
}

Level level(std::string_view Component) {
    if (!Component.empty()) {
        const std::size_t n = sComponentCount.load(std::memory_order_acquire);
        for (std::size_t i = 0; i < n; ++i) {
            if (Component == sComponents[i].name) {
                return Level(sComponents[i].level.load(std::memory_order_relaxed));
            }
        }
    }
    return Level(sBase.load(std::memory_order_relaxed));
}

bool enabled(Level L, std::string_view Component) {
    return L != Level::Off && std::uint8_t(L) <= std::uint8_t(level(Component));
}

void emit(Level L, std::string_view Component, std::string_view Message) noexcept {
    if (!enabled(L, Component)) {
        detail::filtered(L);
        return;
    }
    if (tRealtime) [[unlikely]] {
        detail::emit_realtime(L, Component, Message);
        return;
    }
    if (L == Level::Error) {
        sErrors.fetch_add(1, std::memory_order_relaxed);
    }
    try {
        write_line(L, Component, Message, now_millis());
        if (tForward) {
            tForward(tForwardContext, L,
                     strip_escapes(format_line(L, Component, Message, "", false)));
        }
    } catch (...) {
        // out of memory: say so without allocating
        std::fputs("extempore: out of memory while logging a message\n", stderr);
    }
}

void detail::emit_realtime(Level L, std::string_view Component, std::string_view Message) noexcept {
    if (L == Level::Error) {
        sErrors.fetch_add(1, std::memory_order_relaxed);
    }
    if (!queue().push(L, now_millis(), Component, Message)) {
        sDropped.fetch_add(1, std::memory_order_relaxed);
    }
}

void detail::filtered(Level L) noexcept {
    if (L == Level::Error) {
        sErrors.fetch_add(1, std::memory_order_relaxed);
    }
}

void mark_realtime_thread() {
    tRealtime = true;
}

bool realtime_thread() {
    return tRealtime;
}

std::uint64_t errors() {
    return sErrors.load(std::memory_order_relaxed);
}

std::uint64_t dropped() {
    return sDropped.load(std::memory_order_relaxed);
}

ScopedForward::ScopedForward(Fn Forward, void* Context)
    : mPrevious(tForward), mPreviousContext(tForwardContext) {
    tForward = Forward;
    tForwardContext = Context;
}

ScopedForward::~ScopedForward() {
    tForward = mPrevious;
    tForwardContext = mPreviousContext;
}

std::string colour_code(std::FILE* Stream, bool Bold, unsigned Foreground, unsigned Background) {
    if (!coloured(Stream)) {
        return {};
    }
    Foreground = (Foreground > 9 || Foreground == 8) ? 9 : Foreground;
    Background = (Background > 9 || Background == 8) ? 9 : Background;
    return std::format("\x1b[{};{};{}m", Bold ? 1 : 0, Foreground + 30, Background + 40);
}

bool coloured(std::FILE* Stream) {
    if (Stream == stderr) {
        return sColourErr.load(std::memory_order_relaxed);
    }
    if (Stream == stdout) {
        return sColourOut.load(std::memory_order_relaxed);
    }
    return false;
}

void decide_colour(std::string_view When) {
    const char* noColour = std::getenv("NO_COLOR");
    const bool suppressed = noColour != nullptr && noColour[0] != '\0';
    const auto decide = [&](std::FILE* Stream) {
        if (When == "never") {
            return false;
        }
        const bool terminal = is_terminal(Stream);
        if (When != "always" && (!terminal || suppressed)) {
            return false;
        }
#ifdef _WIN32
        if (terminal) {
            HANDLE console = GetStdHandle(Stream == stderr ? STD_ERROR_HANDLE : STD_OUTPUT_HANDLE);
            DWORD mode = 0;
            if (!GetConsoleMode(console, &mode) ||
                !SetConsoleMode(console, mode | ENABLE_VIRTUAL_TERMINAL_PROCESSING)) {
                return false;
            }
        }
#endif
        return true;
    };
    sColourOut.store(decide(stdout), std::memory_order_relaxed);
    sColourErr.store(decide(stderr), std::memory_order_relaxed);
}

}  // namespace extemp::log
