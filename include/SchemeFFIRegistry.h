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
#include <string_view>

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

// Write Text to s7's current output port: stdout by default, a string port
// inside with-output-to-string, and during an nREPL eval a port that copies it
// to the client as well as the console.
inline void schemeWrite(scheme* Scheme, std::string_view Text) {
    s7_scheme* sc = Scheme->sc;
    s7_display(sc, s7_make_string_with_length(sc, Text.data(), s7_int(Text.size())),
               s7_current_output_port(sc));
    std::fflush(stdout);
}

// Switch the text colour of whatever the current output port feeds. The
// escape sequence travels through the port like any other text; a Windows
// console in cmd mode is coloured through the console API instead, which only
// makes sense while the port is the console.
inline void schemeWriteColor(scheme* Scheme, bool Bold, unsigned Foreground, unsigned Background) {
    if (UNIV::EXT_TERM == UNIV::TerminalMode::Cmd) [[unlikely]] {
        s7_scheme* sc = Scheme->sc;
        if (s7_current_output_port(sc) == s7_name_to_value(sc, "*stdout*")) {
            ascii_text_color(Bold, Foreground, Background);
        }
        return;
    }
    const std::string code = ascii_color_code(Bold, Foreground, Background);
    if (!code.empty()) {
        schemeWrite(Scheme, code);
    }
}

// Console complaint from a primitive that reports a bad argument by returning
// #f rather than raising.  Message carries its own trailing newline; the
// colour is reset after it, as the Scheme-side print-error does.
inline void ffiPrintError(const char* Message) {
    ascii_error();
    std::fputs(Message, stdout);
    ascii_normal();
}

}  // namespace SchemeFFI

}  // namespace extemp

#endif
