/*
 * Copyright (c) 2011, Andrew Sorensen
 *
 * All rights reserved.
 *
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * Neither the name of the authors nor other contributors may be used to endorse
 * or promote products derived from this software without specific prior written
 * permission.
 *
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 */

#include "SchemeFFIRegistry.h"

// for xtc_randd and xtc_rand1_i64
#include "EXTRuntime.h"

#include <cstdint>

namespace extemp {

namespace SchemeFFI {

static pointer randomReal(scheme* Scheme, pointer Args)
{
    return mk_real(Scheme, xtc_randd());
}

static pointer randomInt(scheme* Scheme, pointer Args)
{
    return mk_integer(Scheme, xtc_rand1_i64(argInt(Scheme, Args, 1)));
}

static pointer realToInteger(scheme* Scheme, pointer Args)
{
    // int64_t, not uint64_t: casting a negative double to an unsigned type is
    // UB, and on ARM64 it saturates to 0 rather than wrapping
    return mk_integer(Scheme, int64_t(argReal(Scheme, Args, 1)));
}

// Denominator bound for real->rational. Matches the granularity of the fixed
// 1e7 denominator this replaced, but as a bound rather than a multiplier.
static constexpr int64_t REAL_TO_RATIONAL_MAX_DENOM = 10000000;

// Best rational approximation to Val with a denominator no larger than
// MaxDenom, by continued-fraction expansion. Multiplying through by a fixed
// denominator instead overflowed int64 for |Val| above about 9.2e11, and gave
// a needlessly large ratio for values that are exactly representable.
static void realToRatio(double Val, int64_t MaxDenom, int64_t& NumOut, int64_t& DenOut)
{
    // beyond this a double has no fractional part left anyway, and the cast to
    // int64_t would be UB; the comparison is written so NaN takes this branch
    constexpr double INT64_LIMIT = 9.2e18;
    bool negative = (Val < 0);
    double x = negative ? -Val : Val;
    if (!(x < INT64_LIMIT)) {
        NumOut = negative ? -INT64_MAX : INT64_MAX;
        DenOut = 1;
        return;
    }

    int64_t hPrev = 1, hPrev2 = 0;  // numerators of the last two convergents
    int64_t kPrev = 0, kPrev2 = 1;  // and their denominators
    int64_t num = 0, den = 1;
    double v = x;
    for (int i = 0; i < 64; ++i) {
        if (!(v < INT64_LIMIT)) {
            break;
        }
        int64_t a = int64_t(v);  // v >= 0, so this is floor(v)
        // stop rather than overflow the convergent (all terms are >= 0 here)
        if (a != 0 && hPrev != 0 && a > (INT64_MAX - hPrev2) / hPrev) {
            break;
        }
        if (a != 0 && kPrev != 0 && a > (INT64_MAX - kPrev2) / kPrev) {
            break;
        }
        int64_t h = a * hPrev + hPrev2;
        int64_t k = a * kPrev + kPrev2;
        if (k > MaxDenom) {
            break;
        }
        hPrev2 = hPrev;
        hPrev = h;
        kPrev2 = kPrev;
        kPrev = k;
        num = h;
        den = k;
        double frac = v - double(a);
        if (frac <= 0.0) {
            break;
        }
        v = 1.0 / frac;
    }
    NumOut = negative ? -num : num;
    DenOut = (den > 0) ? den : 1;
}

static pointer realToRational(scheme* Scheme, pointer Args)
{
    int64_t num = 0;
    int64_t den = 1;
    realToRatio(argReal(Scheme, Args, 1), REAL_TO_RATIONAL_MAX_DENOM, num, den);
    return mk_rational(Scheme, num, den);
}

static pointer rationalToReal(scheme* Scheme, pointer Args)
{
    return mk_real(Scheme, argReal(Scheme, Args, 1));
}

static pointer integerToReal(scheme* Scheme, pointer Args)
{
    return mk_real(Scheme, double(argInt(Scheme, Args, 1)));
}

static pointer rationalToNumerator(scheme* Scheme, pointer Args)
{
    pointer rat = argAt(Scheme, Args, 1);
    if (!is_rational(rat)) {
       return mk_integer(Scheme, argInt(Scheme, Args, 1));
    }
    return mk_integer(Scheme, s7_numerator(rat));
}

static pointer rationalToDenominator(scheme* Scheme, pointer Args)
{
    pointer rat = argAt(Scheme, Args, 1);
    if (!is_rational(rat)) {
        return mk_integer(Scheme, 1);
    }
    return mk_integer(Scheme, s7_denominator(rat));
}

std::span<const FFIEntry> numberDefs()
{
    static const FFIEntry defs[] = {
        FFI_DEF("random-real", randomReal, 0, 0, false),
        FFI_DEF("random-int", randomInt, 1, 0, false),
        FFI_DEF("real->integer", realToInteger, 1, 0, false),
        FFI_DEF("real->rational", realToRational, 1, 0, false),
        FFI_DEF("rational->real", rationalToReal, 1, 0, false),
        FFI_DEF("integer->real", integerToReal, 1, 0, false),
        FFI_DEF("rational->n", rationalToNumerator, 1, 0, false),
        FFI_DEF("rational->d", rationalToDenominator, 1, 0, false),
    };
    return defs;
}

}  // namespace SchemeFFI

}  // namespace extemp
