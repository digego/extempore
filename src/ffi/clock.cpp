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

#include "AudioDevice.h"
#include "UNIV.h"

namespace extemp {

namespace SchemeFFI {

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

std::span<const FFIEntry> clockDefs()
{
    static const FFIEntry defs[] = {
        FFI_DEF("clock:set-offset", setClockOffset, 1, 0, false),
        FFI_DEF("clock:get-offset", getClockOffset, 0, 0, false),
        FFI_DEF("clock:adjust-offset", adjustClockOffset, 1, 0, false),
        FFI_DEF("clock:clock", getClockTime, 0, 0, false),
        FFI_DEF("clock:ad:clock", lastSampleBlockClock, 0, 0, false),
    };
    return defs;
}

}  // namespace SchemeFFI

}  // namespace extemp
