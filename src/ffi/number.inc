static pointer randomReal(scheme* Scheme, pointer Args)
{
    return mk_real(Scheme, imp_randd());
}

static pointer randomInt(scheme* Scheme, pointer Args)
{
    return mk_integer(Scheme, imp_rand1_i64(ivalue(pair_car(Args))));
}

static pointer realToInteger(scheme* Scheme, pointer Args)
{
    return mk_integer(Scheme, uint64_t(rvalue(pair_car(Args))));
}

static pointer realToRational(scheme* Scheme, pointer Args)
{
    double val = rvalue(pair_car(Args));
    uint64_t vali = val;
    double remain = val - vali;
    return mk_rational(Scheme, uint64_t(remain*10000000.0) + vali * 10000000ll, 10000000ll);
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
    return mk_integer(Scheme, rat->_object._number.value.ratvalue.n);
}

static pointer rationalToDenominator(scheme* Scheme, pointer Args)
{
    pointer rat = pair_car(Args);
    if (!is_rational(rat)) {
        return mk_integer(Scheme, 1);
    }
    return mk_integer(Scheme, rat->_object._number.value.ratvalue.d);
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
