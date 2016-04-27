pointer setClockOffset(scheme* Scheme, pointer Args)
{
    UNIV::CLOCK_OFFSET = rvalue(pair_car(Args));
    return pair_car(Args);
}

pointer getClockOffset(scheme* Scheme, pointer Args)
{
    return mk_real(Scheme, UNIV::CLOCK_OFFSET);
}

pointer adjustClockOffset(scheme* Scheme, pointer Args)
{
    UNIV::CLOCK_OFFSET = rvalue(pair_car(Args)) + UNIV::CLOCK_OFFSET;
    return mk_real(Scheme, UNIV::CLOCK_OFFSET);
}

pointer getClockTime(scheme* Scheme, pointer Args)
{
    return mk_real(Scheme, getRealTime() + UNIV::CLOCK_OFFSET);
}

pointer lastSampleBlockClock(scheme* Scheme, pointer Args)
{
    pointer p1 = mk_integer(Scheme, UNIV::TIME);
    EnvInjector(Scheme, p1);
    pointer p2 = mk_real(Scheme, AudioDevice::REALTIME + UNIV::CLOCK_OFFSET);
    EnvInjector(Scheme, p2);
    return cons(Scheme, p1, p2);
}

#define CLOCK_DEFS \
        { "clock:set-offset", &setClockOffset }, \
        { "clock:get-offset", &getClockOffset }, \
        { "clock:adjust-offset", &adjustClockOffset }, \
        { "clock:clock", &getClockTime }, \
        { "clock:ad:clock", &lastSampleBlockClock }
