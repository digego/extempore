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

#include <cstdint>
#include <stdexcept>
#include <string>

namespace extemp {

namespace SchemeFFI {

static pointer setDSPClosure(scheme* Scheme, pointer Args)
{
    AudioDevice::I()->setDSPClosure(cptr_value(pair_car(Args)));
    return Scheme->T;
}

static pointer setDSPMTClosure(scheme* Scheme, pointer Args)
{
    // The index selects one of a fixed number of realtime worker slots; a
    // Scheme integer must never index past the table.
    const int64_t index = ivalue(pair_cadr(Args));
    if (index < 0 || index >= int64_t(AudioDevice::MAX_RT_AUDIO_THREADS) ||
            !AudioDevice::I()->setDSPMTClosure(cptr_value(pair_car(Args)), int(index))) {
        throw std::runtime_error("sys:set-dspmt-closure: thread index " + std::to_string(index) +
                " is outside 0.." + std::to_string(AudioDevice::MAX_RT_AUDIO_THREADS - 1));
    }
    return Scheme->T;
}

static pointer setDSPWrapper(scheme* Scheme, pointer Args)
{
    AudioDevice::I()->setDSPWrapper(dsp_f_ptr(cptr_value(pair_car(Args))));
    return Scheme->T;
}

static pointer setDSPMTWrapper(scheme* Scheme, pointer Args)
{
    AudioDevice::I()->setDSPMTWrapper(dsp_f_ptr_sum(cptr_value(pair_car(Args))),
            dsp_f_ptr(cptr_value(pair_cadr(Args))));
    return Scheme->T;
}

static pointer initMTAudio(scheme* Scheme, pointer Args)
{
    bool zerolatency = (pair_cadr(Args) == Scheme->T);
    std::string reason;
    if (!AudioDevice::I()->initMTAudio(int(ivalue(pair_car(Args))), zerolatency, reason)) {
        throw std::runtime_error("sys:init-mt-audio: " + reason);
    }
    return Scheme->T;
}

static pointer getAudioLoad(scheme* Scheme, pointer Args)
{
    return mk_real(Scheme, double(AudioDevice::getCPULoad()));
}

// (underflows . overflows) since startup, as counted by the audio callback
static pointer getAudioXruns(scheme* Scheme, pointer Args)
{
    auto counts = AudioDevice::I()->getXrunCounts();
    return cons(Scheme, mk_integer(Scheme, int64_t(counts.first)),
            mk_integer(Scheme, int64_t(counts.second)));
}

std::span<const FFIEntry> sysDspDefs()
{
    static const FFIEntry defs[] = {
        FFI_DEF("sys:set-dsp-closure", setDSPClosure, 1, 0, false),
        FFI_DEF("sys:set-dspmt-closure", setDSPMTClosure, 2, 0, false),
        FFI_DEF("sys:set-dsp-wrapper", setDSPWrapper, 1, 0, false),
        FFI_DEF("sys:set-dspmt-wrapper", setDSPMTWrapper, 2, 0, false),
        FFI_DEF("sys:init-mt-audio", initMTAudio, 2, 0, false),
        FFI_DEF("sys:audio-load", getAudioLoad, 0, 0, false),
        FFI_DEF("sys:audio-xruns", getAudioXruns, 0, 0, false),
    };
    return defs;
}

}  // namespace SchemeFFI

}  // namespace extemp
