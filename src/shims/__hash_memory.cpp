// hash_compat.cpp - Compatibility shim for libc++ ABI differences
//
// When LLVM is built with a different version of libc++ than the host compiler,
// the __hash_memory symbol may not be found. This provides the implementation.

#include <cstddef>
#include <cstdint>

// Only needed on Apple platforms where libc++ ABI version can differ
#if defined(__APPLE__)

namespace std {
inline namespace __1 {

// MurmurHash2 implementation matching libc++'s __hash_memory
// Marked weak so a libc++ that already exports this symbol wins.
__attribute__((visibility("default"), weak))
size_t __hash_memory(const void* ptr, size_t len) noexcept {
    static_assert(__SIZE_WIDTH__ == 64, "__hash_memory only needed on 64-bit macOS");
    const size_t m = 0xc6a4a7935bd1e995ULL;
    const int r = 47;
    size_t h = len * m;
    const unsigned char* data = static_cast<const unsigned char*>(ptr);
    const unsigned char* end = data + (len & ~7ULL);

    while (data != end) {
        size_t k;
        __builtin_memcpy(&k, data, sizeof(k));
        k *= m;
        k ^= k >> r;
        k *= m;
        h ^= k;
        h *= m;
        data += 8;
    }

    switch (len & 7) {
        case 7: h ^= static_cast<size_t>(data[6]) << 48; [[fallthrough]];
        case 6: h ^= static_cast<size_t>(data[5]) << 40; [[fallthrough]];
        case 5: h ^= static_cast<size_t>(data[4]) << 32; [[fallthrough]];
        case 4: h ^= static_cast<size_t>(data[3]) << 24; [[fallthrough]];
        case 3: h ^= static_cast<size_t>(data[2]) << 16; [[fallthrough]];
        case 2: h ^= static_cast<size_t>(data[1]) << 8;  [[fallthrough]];
        case 1: h ^= static_cast<size_t>(data[0]);
                h *= m;
    }

    h ^= h >> r;
    h *= m;
    h ^= h >> r;
    return h;
}

} // namespace __1
} // namespace std

#endif // __APPLE__
