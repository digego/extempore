#pragma once

// Turning an incoming OSC packet into the Scheme SOURCE TEXT that the
// interpreter evaluates is the one place where untrusted network bytes become
// code. Every packet-derived field has to be emitted as a literal that cannot
// escape its own syntax, so the encoders live here as small pure functions --
// testable on their own, with no socket or interpreter in the way.

#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <string>
#include <string_view>
#include <type_traits>

namespace extemp {
namespace osc_text {

// Characters the OSC 1.0 spec reserves for address-pattern matching, plus
// space and anything non-printable.
inline bool address_char_ok(unsigned char Char) {
    if (Char < 0x21 || Char > 0x7e) {
        return false;
    }
    switch (Char) {
        case '#':
        case '*':
        case ',':
        case '?':
        case '[':
        case ']':
        case '{':
        case '}': return false;
        default: return true;
    }
}

// An address we cannot name safely is a message we have no business
// dispatching, so a failure here means "drop the packet", not "sanitise it".
inline bool address_is_valid(std::string_view Address) {
    if (Address.empty() || Address.front() != '/' || Address.size() > 1024) {
        return false;
    }
    for (char c : Address) {
        if (!address_char_ok(static_cast<unsigned char>(c))) {
            return false;
        }
    }
    return true;
}

// Render Str as a Scheme string literal, surrounding quotes included. Bytes
// above 0x7f pass through untouched: s7 strings are byte strings and only '"'
// and '\' are special to its reader.
inline std::string scheme_string_literal(std::string_view Str) {
    static constexpr char HEX[] = "0123456789abcdef";
    std::string out;
    out.reserve(Str.size() + 2);
    out.push_back('"');
    for (char ch : Str) {
        auto c = static_cast<unsigned char>(ch);
        if (c == '\\' || c == '"') {
            out.push_back('\\');
            out.push_back(static_cast<char>(c));
        } else if (c < 0x20 || c == 0x7f) {
            // s7 reads \xNN; as a hex character escape
            out.push_back('\\');
            out.push_back('x');
            out.push_back(HEX[c >> 4]);
            out.push_back(HEX[c & 0xf]);
            out.push_back(';');
        } else {
            out.push_back(static_cast<char>(c));
        }
    }
    out.push_back('"');
    return out;
}

namespace detail {

inline bool round_trips(const char* Text, double Value) {
    char* end = nullptr;
    return std::strtod(Text, &end) == Value && end && *end == '\0';
}

inline bool round_trips(const char* Text, float Value) {
    char* end = nullptr;
    return std::strtof(Text, &end) == Value && end && *end == '\0';
}

}  // namespace detail

// Shortest decimal that reads back as the same value, tagged so the s7 reader
// produces a real rather than an integer. Non-finite values would otherwise
// print as "nan"/"inf", which the reader takes for symbols.
template <class T>
inline std::string scheme_real_literal(T Value) {
    static_assert(std::is_floating_point_v<T>, "scheme_real_literal is for floats");
    if (std::isnan(Value)) {
        return "+nan.0";
    }
    if (std::isinf(Value)) {
        return Value < 0 ? "-inf.0" : "+inf.0";
    }
    constexpr int maxPrecision = std::is_same_v<T, float> ? 9 : 17;
    char buf[64];
    for (int precision = 1; precision <= maxPrecision; ++precision) {
        std::snprintf(buf, sizeof(buf), "%.*g", precision, static_cast<double>(Value));
        if (detail::round_trips(buf, Value)) {
            break;
        }
    }
    std::string out(buf);
    if (out.find_first_of(".eE") == std::string::npos) {
        out += ".0";
    }
    return out;
}

}  // namespace osc_text
}  // namespace extemp
