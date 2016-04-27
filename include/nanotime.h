#if !defined(nanotime_H_)
#define nanotime_H_

#include <stdint.h>

static inline uint64_t rdtsc()
{
    union {
        uint64_t tsc_64;
        struct {
            uint32_t lo_32;
            uint32_t hi_32;
        };
    } tsc;
    asm volatile("rdtsc" : "=a" (tsc.lo_32), "=d" (tsc.hi_32));
    return tsc.tsc_64;
}

class nanotime
{
private:
    static const unsigned SCALE = 12;
private:
    uint64_t m_cycles;

    static uint64_t sm_cyc2ns;
private:
    explicit nanotime(uint64_t Value): m_cycles(Value) {
    }
public:
    nanotime(): m_cycles(0) {
    }

    uint64_t cycles() const { return m_cycles; }
    uint64_t ns() const {
        return (m_cycles * sm_cyc2ns + (1 << (SCALE - 1))) >> SCALE;
    }
    uint_fast32_t total_seconds() const {
        return ns() / 1000000000;
    }
    uint_fast32_t total_milliseconds() const {
        return ns() / 1000000;
    }
    uint_fast32_t total_microseconds() const {
        return ns() / 1000;
    }
    uint_fast32_t fractional_seconds() const {
        return ns() % 1000000000;
    }

    nanotime& operator+=(const nanotime& Other) {
        m_cycles += Other.m_cycles;
        return *this;
    }
    nanotime& operator-=(const nanotime& Other) {
        m_cycles -= Other.m_cycles;
        return *this;
    }
    friend nanotime operator-(nanotime Lhs, nanotime Rhs) {
        return nanotime::from_cycles(Lhs.m_cycles - Rhs.m_cycles);
    }

    bool operator<(const nanotime& Other) const { return m_cycles < Other.m_cycles; }

    static nanotime from_ns(uint64_t Ns) {
        return nanotime((Ns << SCALE) / sm_cyc2ns);
    }
    static nanotime from_cycles(uint64_t Cycles) { return nanotime(Cycles); }
    static nanotime max() { return nanotime(uint64_t(-1) - sm_cyc2ns - (1 << (SCALE - 1))); }
    static void calibrate(unsigned Seconds = 10);
};

inline __attribute__((always_inline, unused)) nanotime getNanotime()
{
    return nanotime::from_cycles(rdtsc());
}

#endif /* nanotime_H_ */
