static pointer randomReal(scheme* Scheme, pointer Args)
{
    return mk_real(Scheme, xtc_randd());
}

static pointer randomInt(scheme* Scheme, pointer Args)
{
    return mk_integer(Scheme, xtc_rand1_i64(ivalue(pair_car(Args))));
}

static pointer realToInteger(scheme* Scheme, pointer Args)
{
    // int64_t, not uint64_t: casting a negative double to an unsigned type is
    // UB, and on ARM64 it saturates to 0 rather than wrapping
    return mk_integer(Scheme, int64_t(rvalue(pair_car(Args))));
}

static pointer realToRational(scheme* Scheme, pointer Args)
{
    double val = rvalue(pair_car(Args));
    int64_t vali = val;
    double remain = val - vali;
    return mk_rational(Scheme, int64_t(remain*10000000.0) + vali * 10000000ll, 10000000ll);
}

static pointer rationalToReal(scheme* Scheme, pointer Args)
{
    return mk_real(Scheme, rvalue(pair_car(Args)));
}

static pointer integerToReal(scheme* Scheme, pointer Args)
{
    double val = ivalue(pair_car(Args));
    return mk_real(Scheme,val);
}

static pointer rationalToNumerator(scheme* Scheme, pointer Args)
{
    pointer rat = pair_car(Args);
    if (!is_rational(rat)) {
       return mk_integer(Scheme, ivalue(rat));
    }
    return mk_integer(Scheme, s7_numerator(rat));
}

static pointer rationalToDenominator(scheme* Scheme, pointer Args)
{
    pointer rat = pair_car(Args);
    if (!is_rational(rat)) {
        return mk_integer(Scheme, 1);
    }
    return mk_integer(Scheme, s7_denominator(rat));
}

#define NUMBER_DEFS \
        { "random-real", &randomReal }, \
        { "random-int", &randomInt }, \
        { "real->integer", &realToInteger }, \
        { "real->rational", &realToRational }, \
        { "rational->real", &rationalToReal }, \
        { "integer->real", &integerToReal }, \
        { "rational->n", &rationalToNumerator }, \
        { "rational->d", &rationalToDenominator }
