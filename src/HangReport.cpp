#include "HangReport.h"

#ifndef _WIN32

#include "SchemeProcess.h"
#include "TaskScheduler.h"
#include "UNIV.h"

#include <atomic>
#include <csignal>
#include <cstring>
#include <ctime>
#include <execinfo.h>
#include <unistd.h>

#ifdef __linux__
#include <fcntl.h>
#include <sys/syscall.h>
#endif

namespace extemp::HangReport {

Line& Line::operator<<(const char* Text) {
    while (*Text && m_len < sizeof(m_buf) - 1) {
        m_buf[m_len++] = *Text++;
    }
    return *this;
}

Line& Line::operator<<(uint64_t Value) {
    char digits[20];
    int count = 0;
    do {
        digits[count++] = char('0' + Value % 10);
        Value /= 10;
    } while (Value);
    while (count && m_len < sizeof(m_buf) - 1) {
        m_buf[m_len++] = digits[--count];
    }
    return *this;
}

Line& Line::seconds(int64_t Samples) {
    if (Samples < 0) {
        *this << "-";
        Samples = -Samples;
    }
    const uint64_t tenths = uint64_t(Samples) * 10 / UNIV::SAMPLE_RATE;
    return *this << tenths / 10 << "." << tenths % 10 << " s";
}

void Line::write() {
    m_buf[m_len++] = '\n';  // operator<< leaves room for it
    const ssize_t written = ::write(STDERR_FILENO, m_buf, m_len);
    (void)written;
    m_len = 0;
}

namespace {

void traceThisThread() {
    void* frames[48];
    const int count = backtrace(frames, 48);
    backtrace_symbols_fd(frames, count, STDERR_FILENO);
}

#ifdef __linux__

std::atomic<bool> sTraced{false};

pid_t threadId() {
    return pid_t(syscall(SYS_gettid));
}

// "/proc/self/task/<tid>/comm" holds the name EXTThread gave the thread
void appendThreadName(Line& Out, pid_t Tid) {
    char path[64] = "/proc/self/task/";
    size_t len = strlen(path);
    char digits[12];
    int count = 0;
    for (auto value = unsigned(Tid); count == 0 || value; value /= 10) {
        digits[count++] = char('0' + value % 10);
    }
    while (count) {
        path[len++] = digits[--count];
    }
    strcpy(path + len, "/comm");
    char name[32] = {};
    const int fd = open(path, O_RDONLY);
    if (fd >= 0) {
        const ssize_t got = read(fd, name, sizeof(name) - 1);
        close(fd);
        if (got > 0 && name[got - 1] == '\n') {
            name[got - 1] = '\0';
        }
    }
    Out << name;
}

void onTraceSignal(int) {
    Line line;
    line << "--- thread " << uint64_t(threadId()) << " (";
    appendThreadName(line, threadId());
    line << ")";
    line.write();
    traceThisThread();
    sTraced.store(true);
}

struct LinuxDirent64 {
    uint64_t d_ino;
    int64_t d_off;
    unsigned short d_reclen;
    unsigned char d_type;
    char d_name[];
};

// Ask every other thread to trace itself, one at a time, so that their
// backtraces don't interleave.
void traceOtherThreads() {
    const int dir = open("/proc/self/task", O_RDONLY | O_DIRECTORY);
    if (dir < 0) {
        return;
    }
    const pid_t self = threadId();
    alignas(8) char buf[4096];
    long got;
    while ((got = syscall(SYS_getdents64, dir, buf, sizeof(buf))) > 0) {
        for (long offset = 0; offset < got;) {
            const auto* entry = reinterpret_cast<const LinuxDirent64*>(buf + offset);
            offset += entry->d_reclen;
            pid_t tid = 0;
            for (const char* c = entry->d_name; *c >= '0' && *c <= '9'; ++c) {
                tid = tid * 10 + (*c - '0');
            }
            if (tid == 0 || tid == self) {
                continue;
            }
            sTraced.store(false);
            if (syscall(SYS_tgkill, getpid(), tid, SIGUSR2) != 0) {
                continue;
            }
            const timespec tick{0, 10'000'000};
            for (int i = 0; i < 100 && !sTraced.load(); ++i) {
                nanosleep(&tick, nullptr);
            }
            if (!sTraced.load()) {
                Line line;
                line << "--- thread " << uint64_t(tid) << ": no backtrace within 1 s";
                line.write();
            }
        }
    }
    close(dir);
}

#endif  // __linux__

void onReportSignal(int) {
    static std::atomic<bool> busy{false};
    if (busy.exchange(true)) {
        return;
    }
    Line line;
    line << "=== extempore hang report (pid " << uint64_t(getpid()) << ")";
    line.write();
    TaskScheduler::I()->writeHangReport();
    SchemeProcess::writeHangReports();
#ifdef __linux__
    line << "--- thread " << uint64_t(threadId()) << " (";
    appendThreadName(line, threadId());
    line << ", the one reporting)";
    line.write();
    traceThisThread();
    traceOtherThreads();
#else
    line << "--- the reporting thread";
    line.write();
    traceThisThread();
#endif
    line << "=== end of hang report";
    line.write();
    busy.store(false);
}

}  // namespace

void install() {
    // backtrace() loads libgcc on its first call, which allocates: do that
    // now rather than in a signal handler
    void* frame;
    backtrace(&frame, 1);
    struct sigaction sa;
    memset(&sa, 0, sizeof(sa));
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = SA_RESTART;
    sa.sa_handler = onReportSignal;
    sigaction(SIGUSR1, &sa, nullptr);
#ifdef __linux__
    sa.sa_handler = onTraceSignal;
    sigaction(SIGUSR2, &sa, nullptr);
#endif
}

}  // namespace extemp::HangReport

#else  // _WIN32

namespace extemp::HangReport {
void install() {}
}  // namespace extemp::HangReport

#endif
