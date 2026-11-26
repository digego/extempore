#include "Bencode.h"

#include <charconv>
#include <limits>

namespace extemp::bencode {

const Value* find(const Dict& D, std::string_view Key) {
    const auto it = D.find(Key);
    return it == D.end() ? nullptr : &it->second;
}

std::string_view findString(const Dict& D, std::string_view Key) {
    const Value* v = find(D, Key);
    return (v && v->isString()) ? std::string_view(v->string) : std::string_view();
}

static void encodeString(std::string_view S, std::string& Out) {
    Out += std::to_string(S.size());
    Out += ':';
    Out += S;
}

void encode(const Value& V, std::string& Out) {
    switch (V.kind) {
    case Value::Kind::Integer:
        Out += 'i';
        Out += std::to_string(V.integer);
        Out += 'e';
        break;
    case Value::Kind::String:
        encodeString(V.string, Out);
        break;
    case Value::Kind::ListValue:
        Out += 'l';
        for (const auto& item : V.list) {
            encode(item, Out);
        }
        Out += 'e';
        break;
    case Value::Kind::DictValue:
        Out += 'd';
        for (const auto& [key, item] : V.dict) {  // std::map iterates in key order
            encodeString(key, Out);
            encode(item, Out);
        }
        Out += 'e';
        break;
    }
}

std::string encode(const Value& V) {
    std::string out;
    encode(V, out);
    return out;
}

namespace {

class Decoder {
  public:
    Decoder(std::string_view Input, std::size_t MaxBytes, int MaxDepth)
        : m_in(Input), m_maxBytes(MaxBytes), m_maxDepth(MaxDepth) {}

    Decoded run() {
        Decoded result;
        if (value(result.value, 0)) {
            result.status = Decoded::Status::Complete;
            result.consumed = m_pos;
        } else if (m_error.empty()) {
            result.status = m_pos > m_maxBytes ? Decoded::Status::Error : Decoded::Status::Incomplete;
            if (result.status == Decoded::Status::Error) {
                result.error = "message too large";
            }
        } else {
            result.status = Decoded::Status::Error;
            result.error = m_error;
        }
        return result;
    }

  private:
    std::string_view m_in;
    std::size_t m_pos = 0;
    std::size_t m_maxBytes;
    int m_maxDepth;
    std::string m_error;  // set when the input can never decode

    bool fail(const char* Message) {
        m_error = Message;
        return false;
    }

    // Reads digits (with an optional leading '-') up to Terminator. False with
    // no error means the input ran out first.
    bool number(char Terminator, std::int64_t& Out) {
        const std::size_t end = m_in.find(Terminator, m_pos);
        if (end == std::string_view::npos) {
            // a number never needs more than 20 characters
            if (m_in.size() - m_pos > 21) {
                return fail("malformed number");
            }
            m_pos = m_in.size();
            return false;
        }
        const char* first = m_in.data() + m_pos;
        const char* last = m_in.data() + end;
        if (first == last) {
            return fail("malformed number");
        }
        auto [ptr, ec] = std::from_chars(first, last, Out);
        if (ec != std::errc() || ptr != last) {
            return fail("malformed number");
        }
        m_pos = end + 1;
        return true;
    }

    bool value(Value& Out, int Depth) {
        if (Depth > m_maxDepth) {
            return fail("nested too deeply");
        }
        if (m_pos >= m_in.size()) {
            return false;
        }
        const char c = m_in[m_pos];
        if (c == 'i') {
            ++m_pos;
            std::int64_t n = 0;
            if (!number('e', n)) {
                return false;
            }
            Out = Value(n);
            return true;
        }
        if (c >= '0' && c <= '9') {
            std::string s;
            if (!string(s)) {
                return false;
            }
            Out = Value(std::move(s));
            return true;
        }
        if (c == 'l') {
            ++m_pos;
            List items;
            for (;;) {
                if (m_pos >= m_in.size()) {
                    return false;
                }
                if (m_in[m_pos] == 'e') {
                    ++m_pos;
                    Out = Value(std::move(items));
                    return true;
                }
                Value item;
                if (!value(item, Depth + 1)) {
                    return false;
                }
                items.push_back(std::move(item));
            }
        }
        if (c == 'd') {
            ++m_pos;
            Dict entries;
            for (;;) {
                if (m_pos >= m_in.size()) {
                    return false;
                }
                if (m_in[m_pos] == 'e') {
                    ++m_pos;
                    Out = Value(std::move(entries));
                    return true;
                }
                std::string key;
                if (m_in[m_pos] < '0' || m_in[m_pos] > '9') {
                    return fail("dictionary key is not a string");
                }
                if (!string(key)) {
                    return false;
                }
                Value item;
                if (!value(item, Depth + 1)) {
                    return false;
                }
                entries.insert_or_assign(std::move(key), std::move(item));
            }
        }
        return fail("unexpected byte");
    }

    bool string(std::string& Out) {
        std::int64_t length = 0;
        if (!number(':', length)) {
            return false;
        }
        if (length < 0) {
            return fail("negative string length");
        }
        if (std::uint64_t(length) > m_maxBytes) {
            return fail("message too large");
        }
        if (m_in.size() - m_pos < std::size_t(length)) {
            // remember how far the value reaches, for the size limit
            m_pos += std::size_t(length);
            return false;
        }
        Out.assign(m_in.substr(m_pos, std::size_t(length)));
        m_pos += std::size_t(length);
        return true;
    }
};

}  // namespace

Decoded decode(std::string_view Input, std::size_t MaxBytes, int MaxDepth) {
    return Decoder(Input, MaxBytes, MaxDepth).run();
}

}  // namespace extemp::bencode
