#include <gtest/gtest.h>

#include "ext/OscText.h"
#include "ext/OscWire.h"

#include <cstring>
#include <limits>
#include <string>
#include <vector>

using extemp::osc_text::address_is_valid;
using extemp::osc_text::scheme_real_literal;
using extemp::osc_text::scheme_string_literal;

namespace {

// Minimal model of the s7 reader's string-constant rules, used to prove that a
// literal we emit decodes back to exactly the bytes we put in, and that it
// ends at its closing quote rather than spilling into surrounding source.
// Returns false if the literal is malformed or terminates early.
bool read_scheme_string(const std::string& Literal, std::string& Out) {
    Out.clear();
    if (Literal.size() < 2 || Literal.front() != '"') {
        return false;
    }
    size_t i = 1;
    for (; i < Literal.size(); ++i) {
        char c = Literal[i];
        if (c == '"') {
            break;  // closing quote
        }
        if (c != '\\') {
            Out.push_back(c);
            continue;
        }
        if (++i >= Literal.size()) {
            return false;
        }
        switch (Literal[i]) {
            case '\\': Out.push_back('\\'); break;
            case '"': Out.push_back('"'); break;
            case 'n': Out.push_back('\n'); break;
            case 't': Out.push_back('\t'); break;
            case 'r': Out.push_back('\r'); break;
            case 'x': {
                std::string hex;
                while (++i < Literal.size() && Literal[i] != ';') {
                    hex.push_back(Literal[i]);
                }
                if (i >= Literal.size() || hex.empty()) {
                    return false;
                }
                Out.push_back(static_cast<char>(std::stoi(hex, nullptr, 16)));
                break;
            }
            default: return false;  // an escape s7 would reject
        }
    }
    // the literal must be exactly one string constant: closing quote, then end
    return i == Literal.size() - 1 && Literal[i] == '"';
}

}  // namespace

// ---------------------------------------------------------------------------
// address validation
// ---------------------------------------------------------------------------

TEST(OscAddress, AcceptsOrdinaryAddresses) {
    EXPECT_TRUE(address_is_valid("/test/msg"));
    EXPECT_TRUE(address_is_valid("/"));
    EXPECT_TRUE(address_is_valid("/clock/bpm/update"));
    EXPECT_TRUE(address_is_valid("/a-b_c.1"));
}

TEST(OscAddress, RejectsMissingLeadingSlash) {
    EXPECT_FALSE(address_is_valid(""));
    EXPECT_FALSE(address_is_valid("test"));
    EXPECT_FALSE(address_is_valid(" /test"));
}

TEST(OscAddress, RejectsSpaceAndPatternCharacters) {
    EXPECT_FALSE(address_is_valid("/a b"));
    EXPECT_FALSE(address_is_valid("/a#b"));
    EXPECT_FALSE(address_is_valid("/a*b"));
    EXPECT_FALSE(address_is_valid("/a,b"));
    EXPECT_FALSE(address_is_valid("/a?b"));
    EXPECT_FALSE(address_is_valid("/a[b"));
    EXPECT_FALSE(address_is_valid("/a]b"));
    EXPECT_FALSE(address_is_valid("/a{b"));
    EXPECT_FALSE(address_is_valid("/a}b"));
}

TEST(OscAddress, RejectsNonPrintableBytes) {
    EXPECT_FALSE(address_is_valid(std::string("/a\x01" "b")));
    EXPECT_FALSE(address_is_valid(std::string("/a\x7f" "b")));
    EXPECT_FALSE(address_is_valid(std::string("/a\xe9" "b")));
    EXPECT_FALSE(address_is_valid(std::string("/a\n")));
    EXPECT_FALSE(address_is_valid(std::string("/a\0b", 4)));
}

TEST(OscAddress, RejectsInjectionPayloads) {
    // The classic vector: an address that closes the Scheme string literal and
    // opens a form of its own. Every one of these carries a space.
    EXPECT_FALSE(address_is_valid("/x\") (sys:exit) (\""));
    EXPECT_FALSE(address_is_valid("/x\\\") (println 1) (\""));
    EXPECT_FALSE(address_is_valid("/x\" (sys:eval-string \"(quit 0)\") \""));
}

TEST(OscAddress, RejectsOverlongAddresses) {
    EXPECT_TRUE(address_is_valid("/" + std::string(1023, 'a')));
    EXPECT_FALSE(address_is_valid("/" + std::string(1024, 'a')));
}

// ---------------------------------------------------------------------------
// Scheme string literals
// ---------------------------------------------------------------------------

TEST(SchemeStringLiteral, PlainText) {
    EXPECT_EQ(scheme_string_literal("hello"), "\"hello\"");
    EXPECT_EQ(scheme_string_literal(""), "\"\"");
}

TEST(SchemeStringLiteral, EscapesQuoteAndBackslash) {
    EXPECT_EQ(scheme_string_literal("he\"llo"), "\"he\\\"llo\"");
    EXPECT_EQ(scheme_string_literal("back\\slash"), "\"back\\\\slash\"");
    // A quote preceded by a backslash: the input that defeats "skip a quote
    // that already looks escaped" schemes, because the backslash escapes the
    // backslash and the quote then closes the literal.
    EXPECT_EQ(scheme_string_literal("\\\""), "\"\\\\\\\"\"");
}

TEST(SchemeStringLiteral, EscapesControlCharacters) {
    EXPECT_EQ(scheme_string_literal("a\nb"), "\"a\\x0a;b\"");
    EXPECT_EQ(scheme_string_literal("a\x7f"), "\"a\\x7f;\"");
    EXPECT_EQ(scheme_string_literal(std::string("a\0b", 3)), "\"a\\x00;b\"");
}

TEST(SchemeStringLiteral, PassesHighBytesThrough) {
    EXPECT_EQ(scheme_string_literal("caf\xc3\xa9"), "\"caf\xc3\xa9\"");
}

TEST(SchemeStringLiteral, HostileInputsRoundTrip) {
    const std::vector<std::string> hostile = {
        "\") (sys:exit) (\"",
        "\\\") (sys:exit) (\"",
        "\\\\\") (println 'pwned) (\"",
        "\"",
        "\\",
        "\\\\",
        "\"\"\"\"",
        "(quit 0)",
        "') (quit 0) ('",
        "\n(quit 0)\n",
        std::string("\0\") (quit 0) (\"", 15),
    };
    for (const auto& input : hostile) {
        const std::string literal = scheme_string_literal(input);
        std::string decoded;
        ASSERT_TRUE(read_scheme_string(literal, decoded))
            << "literal did not parse as a single string constant: " << literal;
        EXPECT_EQ(decoded, input) << "for literal " << literal;
    }
}

TEST(SchemeStringLiteral, EveryByteRoundTrips) {
    // One literal containing all 256 byte values, plus one literal per byte.
    std::string all;
    for (int i = 0; i < 256; ++i) {
        all.push_back(static_cast<char>(i));
        const std::string one(1, static_cast<char>(i));
        std::string decoded;
        ASSERT_TRUE(read_scheme_string(scheme_string_literal(one), decoded)) << "byte " << i;
        EXPECT_EQ(decoded, one) << "byte " << i;
    }
    std::string decodedAll;
    ASSERT_TRUE(read_scheme_string(scheme_string_literal(all), decodedAll));
    EXPECT_EQ(decodedAll, all);
}

// ---------------------------------------------------------------------------
// Scheme real literals
// ---------------------------------------------------------------------------

TEST(SchemeRealLiteral, NonFiniteUsesSchemeSpelling) {
    EXPECT_EQ(scheme_real_literal(std::numeric_limits<float>::quiet_NaN()), "+nan.0");
    EXPECT_EQ(scheme_real_literal(std::numeric_limits<double>::quiet_NaN()), "+nan.0");
    EXPECT_EQ(scheme_real_literal(std::numeric_limits<float>::infinity()), "+inf.0");
    EXPECT_EQ(scheme_real_literal(-std::numeric_limits<double>::infinity()), "-inf.0");
}

TEST(SchemeRealLiteral, WholeNumbersStayReal) {
    EXPECT_EQ(scheme_real_literal(1.0f), "1.0");
    EXPECT_EQ(scheme_real_literal(-2.0), "-2.0");
    EXPECT_EQ(scheme_real_literal(0.0), "0.0");
}

TEST(SchemeRealLiteral, ShortestRoundTrip) {
    EXPECT_EQ(scheme_real_literal(0.5f), "0.5");
    EXPECT_EQ(scheme_real_literal(0.1f), "0.1");
    EXPECT_EQ(scheme_real_literal(6.6), "6.6");
    EXPECT_EQ(std::strtod(scheme_real_literal(0.1).c_str(), nullptr), 0.1);
    EXPECT_EQ(std::strtof(scheme_real_literal(6.6f).c_str(), nullptr), 6.6f);
}

// ---------------------------------------------------------------------------
// wire encoding
// ---------------------------------------------------------------------------

TEST(OscWriter, MessageBytesMatchWireFormat) {
    extemp::osc::Writer args;
    args.string("hi");
    args.int32(500);
    args.float32(6.5f);

    extemp::osc::Writer msg;
    msg.string("/test");
    msg.string(",sif");
    msg.append(args);

    const std::vector<unsigned char> expected = {
        '/',  't',  'e',  's',  't',  0,    0,    0,     // address, padded to 8
        ',',  's',  'i',  'f',  0,    0,    0,    0,     // type tags, padded to 8
        'h',  'i',  0,    0,                             // "hi", padded to 4
        0x00, 0x00, 0x01, 0xf4,                          // 500, big-endian
        0x40, 0xd0, 0x00, 0x00,                          // 6.5f, big-endian
    };
    ASSERT_EQ(msg.size(), expected.size());
    EXPECT_EQ(std::memcmp(msg.data(), expected.data(), expected.size()), 0);
}

TEST(OscWriter, StringPaddingAlwaysLeavesATerminator) {
    struct {
        const char* text;
        size_t size;
    } cases[] = {{"", 4}, {"a", 4}, {"ab", 4}, {"abc", 4}, {"abcd", 8}, {"abcde", 8}};
    for (const auto& c : cases) {
        extemp::osc::Writer w;
        w.string(c.text);
        EXPECT_EQ(w.size(), c.size) << "for \"" << c.text << "\"";
        EXPECT_EQ(w.data()[w.size() - 1], '\0');
    }
}

TEST(OscWriter, GrowsPastTheOldFixedBuffers) {
    // A 4 KB string argument overflowed the 1 KB scratch/2 KB message buffers
    // the send path used to write into.
    const std::string big(4096, 'x');
    extemp::osc::Writer msg;
    msg.string("/big");
    msg.string(",s");
    msg.string(big);
    EXPECT_EQ(msg.size(), 8u + 4u + 4100u);
    EXPECT_EQ(std::memcmp(msg.data() + 12, big.data(), big.size()), 0);
}

TEST(OscWriter, ByteswapIsItsOwnInverse) {
    EXPECT_EQ(extemp::osc::byteswap(extemp::osc::byteswap(uint32_t(0x12345678))), 0x12345678u);
    EXPECT_EQ(extemp::osc::byteswap(extemp::osc::byteswap(uint64_t(0x0123456789abcdefULL))),
              0x0123456789abcdefULL);
}
