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

#include "EXTZones.h"
// for llvm_destroy_zone_after_delay
#include "EXTRuntime.h"
#include "SchemeProcess.h"

namespace extemp {

namespace SchemeFFI {

static pointer createMallocZone(scheme* Scheme, pointer Args)
{
    if (Args == Scheme->NIL) {
        return mk_cptr(Scheme, extemp::EXTZones::llvm_zone_create(1024 * 100));
    }
    return mk_cptr(Scheme, extemp::EXTZones::llvm_zone_create(argInt(Scheme, Args, 1)));
}

static pointer defaultMallocZone(scheme* Scheme, pointer Args)
{
    return mk_cptr(Scheme,Scheme->m_process->getDefaultZone());
}

static pointer destroyMallocZone(scheme* Scheme, pointer Args)
{
    llvm_zone_t* ptr = reinterpret_cast<llvm_zone_t*>(argCptr(Scheme, Args, 1));
    if (pair_cdr(Args) != Scheme->NIL) {
        llvm_destroy_zone_after_delay(ptr, argInt(Scheme, Args, 2));
    } else {
        extemp::EXTZones::llvm_zone_destroy(ptr);
    }
    return Scheme->T;
}

static pointer resetMallocZone(scheme* Scheme, pointer Args)
{
    llvm_zone_t* zone = reinterpret_cast<llvm_zone_t*>(argCptr(Scheme, Args, 1));
    extemp::EXTZones::llvm_zone_reset(zone);
    return Scheme->T;
}

static pointer peekMemoryZone(scheme* Scheme, pointer Args)
{
    return mk_cptr(Scheme, extemp::EXTZones::llvm_peek_zone_stack());
}

static pointer popMemoryZone(scheme* Scheme, pointer Args)
{
    return mk_cptr(Scheme, extemp::EXTZones::llvm_pop_zone_stack());
}

static pointer pushMemoryZone(scheme* Scheme, pointer Args)
{
    extemp::EXTZones::llvm_push_zone_stack(reinterpret_cast<llvm_zone_t*>(
            argCptr(Scheme, Args, 1)));
    return Scheme->T;
}

std::span<const FFIEntry> sysZoneDefs()
{
    static const FFIEntry defs[] = {
        FFI_DEF("sys:create-mzone", createMallocZone, 0, 1, false),
        FFI_DEF("sys:default-mzone", defaultMallocZone, 0, 0, false),
        FFI_DEF("sys:destroy-mzone", destroyMallocZone, 1, 1, false),
        FFI_DEF("sys:reset-mzone", resetMallocZone, 1, 0, false),
        FFI_DEF("sys:peek-memzone", peekMemoryZone, 0, 0, false),
        FFI_DEF("sys:pop-memzone", popMemoryZone, 0, 0, false),
        FFI_DEF("sys:push-memzone", pushMemoryZone, 1, 0, false),
    };
    return defs;
}

}  // namespace SchemeFFI

}  // namespace extemp
