static pointer setClockOffset(scheme* Scheme, pointer Args)
{
    UNIV::CLOCK_OFFSET = argReal(Scheme, Args, 1);
    return pair_car(Args);
}

static pointer getClockOffset(scheme* Scheme, pointer Args)
{
    return mk_real(Scheme, UNIV::CLOCK_OFFSET);
}

static pointer adjustClockOffset(scheme* Scheme, pointer Args)
{
    UNIV::CLOCK_OFFSET = argReal(Scheme, Args, 1) + UNIV::CLOCK_OFFSET;
    return mk_real(Scheme, UNIV::CLOCK_OFFSET);
}

static pointer getClockTime(scheme* Scheme, pointer Args)
{
    return mk_real(Scheme, getRealTime() + UNIV::CLOCK_OFFSET);
}

static pointer lastSampleBlockClock(scheme* Scheme, pointer Args)
{
    pointer p1 = mk_integer(Scheme, UNIV::TIME);
    EnvInjector inject1(Scheme, p1);
    pointer p2 = mk_real(Scheme, AudioDevice::REALTIME + UNIV::CLOCK_OFFSET);
    EnvInjector inject2(Scheme, p2);
    return cons(Scheme, p1, p2);
}

#define CLOCK_DEFS \
        FFI_DEF("clock:set-offset", setClockOffset, 1, 0, false), \
        FFI_DEF("clock:get-offset", getClockOffset, 0, 0, false), \
        FFI_DEF("clock:adjust-offset", adjustClockOffset, 1, 0, false), \
        FFI_DEF("clock:clock", getClockTime, 0, 0, false), \
        FFI_DEF("clock:ad:clock", lastSampleBlockClock, 0, 0, false)
