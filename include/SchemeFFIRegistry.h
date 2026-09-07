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

#ifndef _SCHEME_FFI_REGISTRY_H
#define _SCHEME_FFI_REGISTRY_H

#include "SchemeS7.h"
#include "SchemeS7Private.h"
#include "UNIV.h"

#include <cstdio>
#include <span>

namespace extemp {

namespace SchemeFFI {

// One Scheme primitive: the name it binds to and the C function behind it.
// FFI_DEF (SchemeS7Private.h) expands to one of these and registers the
// declared arity with the s7 adapter on the way past.
struct FFIEntry {
    const char* name;
    foreign_func func;
};

// The primitives each src/ffi/*.cpp defines.  A group's table is built on the
// first call, so the arities reach the adapter at the point initSchemeFFI runs
// rather than during static initialisation.
std::span<const FFIEntry> clockDefs();
std::span<const FFIEntry> ipcDefs();
std::span<const FFIEntry> llvmDefs();
std::span<const FFIEntry> miscDefs();
std::span<const FFIEntry> numberDefs();
std::span<const FFIEntry> regexDefs();
std::span<const FFIEntry> sysDefs();
std::span<const FFIEntry> sysDspDefs();
std::span<const FFIEntry> sysZoneDefs();
std::span<const FFIEntry> utilityDefs();

// Console complaint from a primitive that reports a bad argument by returning
// #f rather than raising.  Message carries its own trailing newline; the
// colour is reset after it, as the Scheme-side print-error does.
inline void ffiPrintError(const char* Message)
{
    ascii_error();
    std::fputs(Message, stdout);
    ascii_normal();
}

}  // namespace SchemeFFI

}  // namespace extemp

#endif
