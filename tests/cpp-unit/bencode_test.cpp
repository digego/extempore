#include "Bencode.h"

#include <gtest/gtest.h>

using namespace extemp::bencode;

TEST(Bencode, EncodesEachKind) {
    EXPECT_EQ(encode(Value(42)), "i42e");
    EXPECT_EQ(encode(Value(-7)), "i-7e");
    EXPECT_EQ(encode(Value("spam")), "4:spam");
    EXPECT_EQ(encode(Value("")), "0:");
    EXPECT_EQ(encode(Value(List{Value("a"), Value(1)})), "l1:ai1ee");
}

TEST(Bencode, EncodesDictionaryKeysInSortedOrder) {
    Dict d{{"op", Value("eval")}, {"code", Value("(+ 2 2)")}, {"id", Value(1)}};
    EXPECT_EQ(encode(Value(d)), "d4:code7:(+ 2 2)2:idi1e2:op4:evale");
}

TEST(Bencode, RoundTripsNestedValues) {
    Dict inner{{"version-string", Value("0.11")}};
    Dict outer{{"versions", Value(Dict{{"extempore", Value(inner)}})},
               {"status", Value(List{Value("done")})}};
    const std::string bytes = encode(Value(outer));
    const Decoded d = decode(bytes);
    ASSERT_EQ(d.status, Decoded::Status::Complete);
    EXPECT_EQ(d.consumed, bytes.size());
    EXPECT_EQ(encode(d.value), bytes);
}

TEST(Bencode, KeepsBinaryBytesInStrings) {
    const std::string raw("a\0b\xff", 4);
    const Decoded d = decode(encode(Value(raw)));
    ASSERT_EQ(d.status, Decoded::Status::Complete);
    EXPECT_EQ(d.value.string, raw);
}

TEST(Bencode, ReportsIncompleteInputAtEveryPrefix) {
    const std::string bytes = "d4:code7:(+ 2 2)2:idi1e2:op4:evale";
    for (std::size_t n = 0; n < bytes.size(); ++n) {
        EXPECT_EQ(decode(std::string_view(bytes).substr(0, n)).status,
                  Decoded::Status::Incomplete)
            << "prefix of length " << n;
    }
    EXPECT_EQ(decode(bytes).status, Decoded::Status::Complete);
}

TEST(Bencode, StopsAfterTheFirstValue) {
    const Decoded d = decode("i1ei2e");
    ASSERT_EQ(d.status, Decoded::Status::Complete);
    EXPECT_EQ(d.value.integer, 1);
    EXPECT_EQ(d.consumed, 3u);
}

TEST(Bencode, RejectsMalformedInput) {
    for (const char* bad : {"x", "ie", "i1xe", "d1:ae1:b", "di1ei2ee", "-1:a", "i99999999999999999999999e"}) {
        EXPECT_EQ(decode(bad).status, Decoded::Status::Error) << bad;
    }
}

TEST(Bencode, RejectsOversizedAndDeeplyNestedInput) {
    EXPECT_EQ(decode("100:abc", 50).status, Decoded::Status::Error);
    EXPECT_EQ(decode(std::string(100, 'l'), 1024, 64).status, Decoded::Status::Error);
    EXPECT_EQ(decode(std::string(10, 'l') + std::string(10, 'e'), 1024, 64).status,
              Decoded::Status::Complete);
}

TEST(Bencode, FindsDictionaryEntries) {
    Dict d{{"op", Value("eval")}, {"id", Value(3)}};
    EXPECT_EQ(findString(d, "op"), "eval");
    EXPECT_EQ(findString(d, "id"), "");  // not a string
    EXPECT_EQ(findString(d, "missing"), "");
    ASSERT_NE(find(d, "id"), nullptr);
    EXPECT_EQ(find(d, "id")->integer, 3);
}
