#pragma once

// OSC wire encoding: byte order plus a growable writer.
//
// OSC is big-endian and pads every element out to a 4-byte boundary. The
// writer owns its buffer and grows it, so an oversized argument (a long string
// especially) extends the message instead of running off the end of a
// fixed-size stack array.

#include <algorithm>
#include <array>
#include <bit>
#include <cstddef>
#include <cstdint>
#include <string_view>
#include <vector>

namespace extemp {
namespace osc {

// On a little-endian host -- every platform we target -- converting a value
// to/from the wire is a byte reversal. std::bit_cast does the type-pun without
// the unsigned char* aliasing games.
template <class T>
inline T byteswap(T Value) {
    if constexpr (std::endian::native == std::endian::little) {
        auto bytes = std::bit_cast<std::array<std::byte, sizeof(T)>>(Value);
        std::reverse(bytes.begin(), bytes.end());
        return std::bit_cast<T>(bytes);
    } else {
        return Value;  // big-endian host: already in OSC wire order
    }
}

class Writer {
  public:
    // OSC strings are NUL-terminated and NUL-padded to a 4-byte boundary; a
    // string whose length is already a multiple of 4 still gets four bytes of
    // padding, so there is always at least one terminator.
    void string(std::string_view Str) {
        appendBytes(Str.data(), Str.size());
        m_buf.insert(m_buf.end(), 4 - (Str.size() % 4), std::byte{0});
    }
    void int32(int32_t Value) {
        appendScalar(byteswap(static_cast<uint32_t>(Value)));
    }
    void int64(int64_t Value) {
        appendScalar(byteswap(static_cast<uint64_t>(Value)));
    }
    void float32(float Value) {
        appendScalar(byteswap(std::bit_cast<uint32_t>(Value)));
    }
    void float64(double Value) {
        appendScalar(byteswap(std::bit_cast<uint64_t>(Value)));
    }
    void append(const Writer& Other) {
        m_buf.insert(m_buf.end(), Other.m_buf.begin(), Other.m_buf.end());
    }

    const char* data() const {
        return reinterpret_cast<const char*>(m_buf.data());
    }
    std::size_t size() const {
        return m_buf.size();
    }
    bool empty() const {
        return m_buf.empty();
    }

  private:
    template <class T>
    void appendScalar(T Value) {
        appendBytes(&Value, sizeof(Value));
    }
    void appendBytes(const void* Src, std::size_t Size) {
        const auto* bytes = static_cast<const std::byte*>(Src);
        m_buf.insert(m_buf.end(), bytes, bytes + Size);
    }

    std::vector<std::byte> m_buf;
};

}  // namespace osc
}  // namespace extemp
