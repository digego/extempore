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

#include "SchemeFFI.h"
#include "SchemeFFIRegistry.h"

#include "UNIV.h"

#include <cstdint>
#include <span>

namespace extemp {

namespace SchemeFFI {

void initSchemeFFI(scheme* sc) {
    static struct {
        const char* name;
        uint32_t value;
    } integerTable[] = {
        {"*au:block-size*", UNIV::NUM_FRAMES},
        {"*au:samplerate*", UNIV::SAMPLE_RATE},
        {"*au:channels*", UNIV::CHANNELS},
        {"*au:in-channels*", UNIV::IN_CHANNELS},
    };
    for (auto& elem : integerTable) {
        scheme_define(sc, sc->global_env, mk_symbol(sc, elem.name), mk_integer(sc, elem.value));
    }
    // One group per src/ffi/*.cpp. Order is immaterial --- no name is defined
    // twice --- but it is the order the groups were listed in before they
    // became separate translation units.
    const std::span<const FFIEntry> groups[] = {
        utilityDefs(), ipcDefs(),  numberDefs(), sysDefs(),  sysDspDefs(),
        sysZoneDefs(), miscDefs(), regexDefs(),  llvmDefs(), clockDefs(),
    };
    for (const auto& group : groups) {
        for (const auto& elem : group) {
            scheme_define(sc, sc->global_env, mk_symbol(sc, elem.name),
                          mk_foreign_func(sc, elem.func));
        }
    }
}

}  // namespace SchemeFFI

}  // namespace extemp
