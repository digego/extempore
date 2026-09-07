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

#include <cstdio>
#include <cstdlib>
#include <sstream>

namespace extemp {

namespace SchemeFFI {

static pointer asciiColor(scheme* Scheme, pointer Args)
{
    ascii_text_color(bool(argInt(Scheme, Args, 1)), int(argInt(Scheme, Args, 2)),
            int(argInt(Scheme, Args, 3)));
    return Scheme->T;
}

static pointer emit(scheme* Scheme, pointer Args)
{
    int lgth = list_length(Scheme, Args);
    pointer io = list_ref(Scheme, lgth - 1, Args);

    if (s7_is_output_port(Scheme->sc, io)) {
        pointer a = Args;
        for (int i = 0; i < lgth - 1; ++i) {
            pointer arg = pair_car(a);
            if (is_string(arg)) {
                s7_display(Scheme->sc, arg, io);
            }
            a = pair_cdr(a);
        }
        return io;
    }

    std::stringstream ss;
    if (!is_string(io)) {
        ffiPrintError("Emit accepts only string arguments!\n");
        return Scheme->F;
    }
    ss << string_value(io);
    for (int i = 0; i < lgth - 1; ++i) {
        pointer arg = pair_car(Args);
        if (!is_string(arg)) {
            ffiPrintError("Emit accepts only string arguments!\n");
            return Scheme->F;
        }
        ss << string_value(arg);
        Args = pair_cdr(Args);
    }
    return mk_string(Scheme, ss.str().c_str());
}

static pointer exit_extempore(scheme* Scheme, pointer Args)
{
    int rc = (Args != Scheme->NIL) ? int(argInt(Scheme, Args, 1)) : 0;
    // std::_Exit bypasses destructors AND discards stdio buffers. Flush any
    // pending output (e.g. printf from xtlang tests) and finalize the offline
    // WAV before terminating.
    std::fflush(stdout);
    extemp::AudioDevice::stopFileDriver();
    std::_Exit(rc);
}

std::span<const FFIEntry> utilityDefs()
{
    static const FFIEntry defs[] = {
        FFI_DEF("ascii-print-color", asciiColor, 3, 0, false),
        FFI_DEF("emit", emit, 1, 0, true),
        FFI_DEF("quit", exit_extempore, 0, 1, false),
    };
    return defs;
}

}  // namespace SchemeFFI

}  // namespace extemp
