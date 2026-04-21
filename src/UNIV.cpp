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

#include "UNIV.h"
#include <cmath>
#include <cstdlib>
#include <cstdio>
#include <regex>
#include <string>
#include <sstream>
#include <iosfwd>
#include <iomanip>
#include "SchemeFFI.h"
#include "SchemeS7Private.h"
#include "ext/FileUtil.h"

#ifdef __APPLE__
#include <CoreFoundation/CoreFoundation.h>
#include <AppKit/AppKit.h>
#else
#include <time.h>
#endif

#ifdef _WIN32
#include <malloc.h>
#include <Windows.h>

enum Windows_Color_Convert {
    Black = 0,
    Red = FOREGROUND_RED,
    Green = FOREGROUND_GREEN,
    Yellow = FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_INTENSITY,
    Blue = FOREGROUND_BLUE | FOREGROUND_INTENSITY,
    Magenta = FOREGROUND_RED | FOREGROUND_BLUE,
    Cyan = FOREGROUND_GREEN | FOREGROUND_BLUE,
    White = FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_BLUE | FOREGROUND_INTENSITY
};

enum Windows_BGColor_convert {
    BGBlack = 0,
    BGRed = BACKGROUND_RED,
    BGGreen = BACKGROUND_GREEN,
    BGYellow = BACKGROUND_RED | BACKGROUND_GREEN | BACKGROUND_INTENSITY,
    BGBlue = BACKGROUND_BLUE,
    BGMagenta = BACKGROUND_RED | BACKGROUND_BLUE,
    BGCyan = BACKGROUND_GREEN | BACKGROUND_BLUE,
    BGWhite = BACKGROUND_RED | BACKGROUND_GREEN | BACKGROUND_BLUE | BACKGROUND_INTENSITY,
};

int WINDOWS_COLORS[] = {Black, Red, Green, Yellow, Blue, Magenta, Cyan, White};
int WINDOWS_BGCOLORS[] = {BGBlack, BGRed, BGGreen, BGYellow, BGBlue, BGMagenta, BGCyan, BGWhite};

#endif

static char base64_codesafe_encoding_table[] = {
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
    'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',
    'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
    'w', 'x', 'y', 'z', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '_', '-'};

static char base64_std_encoding_table[] = {
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
    'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',
    'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
    'w', 'x', 'y', 'z', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '/'};

static char* base64_std_decoding_table = nullptr;
static char* base64_codesafe_decoding_table = nullptr;
static int _base64_mod_table[] = {0, 2, 1};

void base64_std_build_decoding_table() {

    base64_std_decoding_table = (char*)malloc(256);

    for (int i = 0; i < 64; i++)
        base64_std_decoding_table[(unsigned char)base64_std_encoding_table[i]] = i;
}

void base64_codesafe_build_decoding_table() {

    base64_codesafe_decoding_table = (char*)malloc(256);

    for (int i = 0; i < 64; i++)
        base64_codesafe_decoding_table[(unsigned char)base64_codesafe_encoding_table[i]] = i;
}

void base64_std_cleanup() {
    free(base64_std_decoding_table);
}

void base64_codesafe_cleanup() {
    free(base64_codesafe_decoding_table);
}

EXPORT char* cname_encode(char* data, size_t input_length, size_t* output_length) {
    *output_length = 4 * ((input_length + 2) / 3);

    // char *encoded_data = (char*) malloc(*output_length);
    char* encoded_data = (char*)malloc(*output_length + 1);
    encoded_data[*output_length] = 0;
    if (encoded_data == nullptr)
        return nullptr;

    for (unsigned i = 0, j = 0; i < input_length;) {

        uint32_t octet_a = i < input_length ? data[i++] : 0;
        uint32_t octet_b = i < input_length ? data[i++] : 0;
        uint32_t octet_c = i < input_length ? data[i++] : 0;

        uint32_t triple = (octet_a << 0x10) + (octet_b << 0x08) + octet_c;

        encoded_data[j++] = base64_codesafe_encoding_table[(triple >> 3 * 6) & 0x3F];
        encoded_data[j++] = base64_codesafe_encoding_table[(triple >> 2 * 6) & 0x3F];
        encoded_data[j++] = base64_codesafe_encoding_table[(triple >> 1 * 6) & 0x3F];
        encoded_data[j++] = base64_codesafe_encoding_table[(triple >> 0 * 6) & 0x3F];
    }

    for (int i = 0; i < _base64_mod_table[input_length % 3]; i++) {
        encoded_data[*output_length - 1 - i] = 0;  //'$';
    }

    // printf("ENCODE: %d:%s\n",*output_length,encoded_data);
    return encoded_data;
}

EXPORT char* cname_decode(char* data, size_t input_length, size_t* output_length) {
    if (base64_codesafe_decoding_table == nullptr)
        base64_codesafe_build_decoding_table();

    // Pad with $'s to align to 4 bytes. pad_buf is separate from the outer
    // scope so we always know which buffer to free at the end.
    char* pad_buf = nullptr;
    if (input_length % 4 != 0) {
        int lgthdiff = (4 - (input_length % 4));
        pad_buf = (char*)malloc(input_length + lgthdiff);
        memcpy(pad_buf, data, input_length);
        input_length = input_length + lgthdiff;
        for (int i = 0; i < lgthdiff; i++) {
            pad_buf[input_length - 1 - i] = '$';
        }
        data = pad_buf;
    }

    if (input_length % 4 != 0) {
        if (pad_buf)
            free(pad_buf);
        return nullptr;
    }
    *output_length = input_length / 4 * 3;
    if (data[input_length - 1] == '$')
        (*output_length)--;
    if (data[input_length - 2] == '$')
        (*output_length)--;

    char* decoded_data = (char*)malloc(*output_length + 1);
    if (decoded_data == nullptr) {
        if (pad_buf)
            free(pad_buf);
        return nullptr;
    }
    decoded_data[*output_length] = 0;

    auto fetch = [&](unsigned& i) -> uint32_t {
        unsigned char c = static_cast<unsigned char>(data[i++]);
        return c == '$' ? 0u : base64_codesafe_decoding_table[c];
    };

    for (unsigned i = 0, j = 0; i < input_length;) {
        uint32_t sextet_a = fetch(i);
        uint32_t sextet_b = fetch(i);
        uint32_t sextet_c = fetch(i);
        uint32_t sextet_d = fetch(i);

        uint32_t triple =
            (sextet_a << 3 * 6) + (sextet_b << 2 * 6) + (sextet_c << 1 * 6) + (sextet_d << 0 * 6);

        if (j < *output_length)
            decoded_data[j++] = (triple >> 2 * 8) & 0xFF;
        if (j < *output_length)
            decoded_data[j++] = (triple >> 1 * 8) & 0xFF;
        if (j < *output_length)
            decoded_data[j++] = (triple >> 0 * 8) & 0xFF;
    }
    if (pad_buf)
        free(pad_buf);
    return decoded_data;
}

EXPORT char* base64_encode(const unsigned char* data, size_t input_length, size_t* output_length) {
    *output_length = 4 * ((input_length + 2) / 3);

    char* encoded_data = (char*)malloc(*output_length + 1);
    encoded_data[*output_length] = 0;

    if (encoded_data == nullptr)
        return nullptr;

    for (unsigned i = 0, j = 0; i < input_length;) {

        uint32_t octet_a = i < input_length ? data[i++] : 0;
        uint32_t octet_b = i < input_length ? data[i++] : 0;
        uint32_t octet_c = i < input_length ? data[i++] : 0;

        uint32_t triple = (octet_a << 0x10) + (octet_b << 0x08) + octet_c;

        encoded_data[j++] = base64_std_encoding_table[(triple >> 3 * 6) & 0x3F];
        encoded_data[j++] = base64_std_encoding_table[(triple >> 2 * 6) & 0x3F];
        encoded_data[j++] = base64_std_encoding_table[(triple >> 1 * 6) & 0x3F];
        encoded_data[j++] = base64_std_encoding_table[(triple >> 0 * 6) & 0x3F];
    }

    for (int i = 0; i < _base64_mod_table[input_length % 3]; i++)
        encoded_data[*output_length - 1 - i] = '=';

    return encoded_data;
}

EXPORT unsigned char* base64_decode(const char* data, size_t input_length, size_t* output_length) {
    if (base64_std_decoding_table == nullptr)
        base64_std_build_decoding_table();

    if (input_length % 4 != 0)
        return nullptr;

    *output_length = input_length / 4 * 3;
    if (data[input_length - 1] == '=')
        (*output_length)--;
    if (data[input_length - 2] == '=')
        (*output_length)--;

    unsigned char* decoded_data = (unsigned char*)malloc(*output_length);
    if (decoded_data == nullptr)
        return nullptr;

    auto fetch = [&](unsigned& i) -> uint32_t {
        unsigned char c = static_cast<unsigned char>(data[i++]);
        return c == '=' ? 0u : base64_std_decoding_table[c];
    };

    for (unsigned i = 0, j = 0; i < input_length;) {

        uint32_t sextet_a = fetch(i);
        uint32_t sextet_b = fetch(i);
        uint32_t sextet_c = fetch(i);
        uint32_t sextet_d = fetch(i);

        uint32_t triple =
            (sextet_a << 3 * 6) + (sextet_b << 2 * 6) + (sextet_c << 1 * 6) + (sextet_d << 0 * 6);

        if (j < *output_length)
            decoded_data[j++] = (triple >> 2 * 8) & 0xFF;
        if (j < *output_length)
            decoded_data[j++] = (triple >> 1 * 8) & 0xFF;
        if (j < *output_length)
            decoded_data[j++] = (triple >> 0 * 8) & 0xFF;
    }

    return decoded_data;
}

EXPORT bool rmatch(char* regex, const char* str) {
    try {
        std::regex re(regex);
        return std::regex_search(str, re);
    } catch (const std::regex_error&) {
        return false;
    }
}

EXPORT int64_t rmatches(char* regex, char* str, char** results, int64_t maxnum) {
    try {
        std::string s(str);
        std::regex re(regex);
        int64_t num = 0;
        auto begin = std::sregex_iterator(s.begin(), s.end(), re);
        auto end = std::sregex_iterator();
        for (auto it = begin; it != end && num < maxnum; ++it) {
            std::string match = (*it)[0].str();
            char* tmp = (char*)malloc(match.length() + 1);
            strcpy(tmp, match.c_str());
            results[num] = tmp;
            num++;
        }
        return num;
    } catch (const std::regex_error&) {
        return 0;
    }
}

EXPORT bool rsplit(const char* regex, const char* str, char* a, char* b) {
    // Callers in libs/core/adt.xtm allocate 2048-byte buffers for a and b.
    // Bail rather than write past the buffer. A proper API with explicit
    // capacity is tracked as a follow-up backlog task.
    constexpr size_t kAssumedBufSize = 2048;
    try {
        std::regex re(regex);
        std::cmatch m;
        if (!std::regex_search(str, m, re) || m.size() != 1)
            return false;
        size_t range = static_cast<size_t>(m.position(0));
        size_t range2 = range + static_cast<size_t>(m.length(0));
        size_t length = strlen(str);
        if (range >= kAssumedBufSize || (length - range2) >= kAssumedBufSize)
            return false;
        memcpy(a, str, range);
        a[range] = '\0';
        memcpy(b, str + range2, length - range2);
        b[length - range2] = '\0';
        return true;
    } catch (const std::regex_error&) {
        return false;
    }
}

EXPORT char* rreplace(char* regex, char* str, char* replacement, char* result) {
    try {
        std::string s(str);
        std::regex re(regex);
        std::string res = std::regex_replace(s, re, std::string(replacement),
                                             std::regex_constants::format_first_only);
        if (res.length() >= 4096) {
            strcpy(result, str);
            return result;
        }
        strcpy(result, res.c_str());
        return result;
    } catch (const std::regex_error&) {
        strcpy(result, str);
        return result;
    }
}

EXPORT const char* sys_sharedir() {
    return extemp::UNIV::SHARE_DIR.c_str();
}

EXPORT char* sys_slurp_file(const char* fname) {
    // Try the raw path first, then prepend SHARE_DIR.
    if (char* buf = extemp::file_util::slurp_file(fname)) {
        return buf;
    }
    std::string sharedir_filename(extemp::UNIV::SHARE_DIR + "/" + fname);
    return extemp::file_util::slurp_file(sharedir_filename.c_str());
}

EXPORT int register_for_window_events() {
#ifdef __APPLE__
    // to give Extempore it's own dock icon, etc
    return (int)[NSApp setActivationPolicy:NSApplicationActivationPolicyRegular];
#else
    // implement the required "receive events" functionality for Linux
    // or Windows if necessary
    return 1;
#endif
}

namespace extemp {

namespace UNIV {

std::string SHARE_DIR = std::string(EXT_SHARE_DIR);
uint32_t NUM_FRAMES = 1024;
uint32_t CHANNELS = 2;
uint32_t IN_CHANNELS = 0;
uint32_t SAMPLE_RATE = 44100;
std::atomic<uint64_t> TIME{0};
std::atomic<uint64_t> DEVICE_TIME{0};
static_assert(std::atomic<uint64_t>::is_always_lock_free,
              "std::atomic<uint64_t> must be lock-free so the xtlang JIT's "
              "plain i64 loads of @TIME remain compatible");
double AUDIO_CLOCK_NOW = 0.0;
double AUDIO_CLOCK_BASE = 0.0;
uint64_t TIME_DIVISION = 1;
bool AUDIO_NONE = false;
bool BATCH_MODE = false;
uint32_t AUDIO_DEVICE = -1;
uint32_t AUDIO_IN_DEVICE = -1;
std::string AUDIO_DEVICE_NAME;
std::string AUDIO_IN_DEVICE_NAME;
double AUDIO_OUTPUT_LATENCY = 0.0;
std::string AUDIO_OUTFILE_PATH;
double AUDIO_OUTFILE_DURATION = 0.0;
double CLOCK_OFFSET = 0.0;
std::unordered_map<std::string, std::string> CMDPARAMS;
std::string ARCH;
std::string CPU;
std::vector<std::string> ATTRS;

#ifdef _WIN32
TerminalMode EXT_TERM = TerminalMode::Cmd;
#else
TerminalMode EXT_TERM = TerminalMode::Ansi;
#endif
bool EXT_LOADBASE = true;

double midi2frq(double pitch) {
    return 220.0 * pow(2.0, (pitch - 57.0) / 12);
}

double frqRatio(double semitones) {
    return pow(2.0, (semitones / 12.0));
}

struct dump_stack_frame {
    int op;
    pointer args;
    pointer envir;
    pointer code;
};

void printSchemeCell(scheme* _sc, std::stringstream& ss, pointer val, bool full,
                     bool stringquotes) {
    if (val == 0) {
        ss << "-ERROR BAD POINTER-";
        return;
    }

    // Use s7's object->string for a general fallback, but handle common types
    // explicitly for formatting compatibility with the previous TinyScheme output.

    if (val == _sc->NIL) {
        ss << (full ? "()" : "NIL");
    } else if (val == _sc->T) {
        ss << "#t";
    } else if (val == _sc->F) {
        ss << "#f";
    } else if (val == _sc->EOF_OBJ) {
        ss << "#<EOF>";
    } else if (is_string(val)) {
        if (stringquotes) {
            ss << "\"" << string_value(val) << "\"";
        } else {
            ss << string_value(val);
        }
    } else if (is_symbol(val)) {
        ss << symname(val);
    } else if (is_character(val)) {
        ss << charvalue(val);
    } else if (is_integer(val)) {
        ss << ivalue(val);
    } else if (is_rational(val)) {
        ss << s7_numerator(val) << "/" << s7_denominator(val);
    } else if (is_number(val)) {
        if (full) {
            ss << std::fixed << std::showpoint << std::setprecision(23) << rvalue(val);
        } else {
            ss << std::fixed << std::showpoint << rvalue(val);
        }
    } else if (is_cptr(val)) {
        void* p = cptr_value(val);
        ss << "#<CPTR: " << p << ">";
    } else if (is_vector(val)) {
        ss << "#(";
        long long num = vector_length(val);
        if (num > 1000 && !full) {
            ss << " -- " << num << " elements -- )";
            return;
        }
        for (long long i = 0; i < num; i++) {
            printSchemeCell(_sc, ss, vector_elem(val, i), full, stringquotes);
            if (i + 1 < num)
                ss << " ";
        }
        ss << ")";
    } else if (is_pair(val)) {
        int lgth = list_length(_sc, val);
        if (lgth < 0) {
            ss << "(";
            printSchemeCell(_sc, ss, pair_car(val), full, stringquotes);
            ss << " . ";
            printSchemeCell(_sc, ss, pair_cdr(val), full, stringquotes);
            ss << ")";
        } else if (lgth > 1000 && !full) {
            ss << "( -- " << lgth << " elements -- )";
        } else {
            ss << "(";
            for (int i = 0; i < lgth; i++) {
                printSchemeCell(_sc, ss, list_ref(_sc, i, val), full, stringquotes);
                if (i < lgth - 1)
                    ss << " ";
            }
            ss << ")";
        }
    } else if (is_environment(val)) {
        ss << "#<ENVIRONMENT " << val << ">";
    } else if (is_closure(val)) {
        ss << "#<CLOSURE " << val << ">";
        if (full) {
            char* repr = s7_object_to_c_string(_sc->sc, val);
            if (repr) {
                ss << " " << repr;
                free(repr);
            }
        }
    } else if (is_proc(val) || is_foreign(val)) {
        char* repr = s7_object_to_c_string(_sc->sc, val);
        ss << (repr ? repr : "#<PROCEDURE>");
        free(repr);
    } else {
        // Fallback: use s7's object->string
        char* repr = s7_object_to_c_string(_sc->sc, val);
        if (repr) {
            ss << repr;
            free(repr);
        } else {
            ss << "#<UNKNOWN " << val << ">";
        }
    }
}

}  // namespace UNIV

}  // namespace extemp
