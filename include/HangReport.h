#pragma once

// On SIGUSR1, write to stderr what the scheduler and each Scheme process are
// doing, and (on Linux) a backtrace of every thread. ctest sends the signal to
// a test which times out (extras/cmake/tests.cmake), so a hang leaves a report
// in the test's log. Not available on Windows.

#include <cstddef>
#include <cstdint>

namespace extemp::HangReport {

// A line of the report, built without allocating and written with write(2),
// so that it is safe to use from a signal handler.
class Line {
  private:
    char m_buf[512];
    std::size_t m_len = 0;

  public:
    Line& operator<<(const char* Text);
    Line& operator<<(uint64_t Value);
    // seconds, to one decimal place, from a count of samples
    Line& seconds(int64_t Samples);
    void write();
};

void install();

}  // namespace extemp::HangReport
