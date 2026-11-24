#pragma once

// Bencode, the wire format of nREPL: integers, byte strings, lists and
// dictionaries with byte-string keys. Encoding writes dictionary keys in
// sorted order, as the format requires.

#include <cstddef>
#include <cstdint>
#include <map>
#include <string>
#include <string_view>
#include <vector>

namespace extemp::bencode {

struct Value;
using List = std::vector<Value>;
// std::map with the value type still incomplete here is fine in libstdc++,
// libc++ and MSVC's library alike.
using Dict = std::map<std::string, Value, std::less<>>;

struct Value {
    enum class Kind : std::uint8_t { Integer, String, ListValue, DictValue };

    Kind kind = Kind::String;
    std::int64_t integer = 0;
    std::string string;
    List list;
    Dict dict;

    Value() = default;
    Value(std::int64_t I) : kind(Kind::Integer), integer(I) {}
    Value(int I) : kind(Kind::Integer), integer(I) {}
    Value(std::string S) : kind(Kind::String), string(std::move(S)) {}
    Value(std::string_view S) : kind(Kind::String), string(S) {}
    Value(const char* S) : kind(Kind::String), string(S) {}
    Value(List L) : kind(Kind::ListValue), list(std::move(L)) {}
    Value(Dict D) : kind(Kind::DictValue), dict(std::move(D)) {}

    bool isInteger() const {
        return kind == Kind::Integer;
    }
    bool isString() const {
        return kind == Kind::String;
    }
    bool isList() const {
        return kind == Kind::ListValue;
    }
    bool isDict() const {
        return kind == Kind::DictValue;
    }
};

// The entry for Key in a dictionary, or nullptr.
const Value* find(const Dict& D, std::string_view Key);
// The string entry for Key, or "" if it is absent or not a string.
std::string_view findString(const Dict& D, std::string_view Key);

void encode(const Value& V, std::string& Out);
std::string encode(const Value& V);

struct Decoded {
    enum class Status : std::uint8_t { Complete, Incomplete, Error };
    Status status = Status::Incomplete;
    Value value;
    std::size_t consumed = 0;  // bytes of input the value took, when Complete
    std::string error;         // when Error
};

// Decode the first value in Input. Incomplete means Input ends before the
// value does; more bytes may complete it. A value that would need more than
// MaxBytes of input, or nests deeper than MaxDepth, is an Error.
Decoded decode(std::string_view Input, std::size_t MaxBytes = std::size_t{16} << 20,
               int MaxDepth = 64);

}  // namespace extemp::bencode
