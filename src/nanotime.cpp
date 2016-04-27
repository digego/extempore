#include <time.h>
#include <unistd.h>
#include <cstdio>

#include "nanotime.h"

uint64_t nanotime::sm_cyc2ns;

static uint64_t os_nano()
{
    timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return uint64_t(ts.tv_sec) * 1000000000ul + ts.tv_nsec;
}

void nanotime::calibrate(unsigned Seconds)
{
    auto os_last(os_nano());
    auto tsc_last(rdtsc());
    uint64_t last_val(0); // stupid compiler
    for (unsigned i = 0; i < Seconds * 100; ++i) {
        usleep(10000);
        auto os_now(os_nano());
        auto tsc_now(rdtsc());
        sm_cyc2ns = (((os_now - os_last) << SCALE) + (1 << (SCALE - 1))) / (tsc_now - tsc_last);
        os_last = os_now;
        tsc_last = tsc_now;
        if (i) {
            if (sm_cyc2ns == last_val) {
                break;
            }
            dprintf(2, "TSC calibration unstable: %u vs. %u\n", unsigned(sm_cyc2ns), unsigned(last_val));
        }
        last_val = sm_cyc2ns;
    }
}

struct NanotimeCalibrate {
    NanotimeCalibrate() {
        nanotime::calibrate();
    }
};

static NanotimeCalibrate sCalibrater;
