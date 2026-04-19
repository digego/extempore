#pragma once

#include <cstdio>
#include <cstdlib>

namespace extemp {
namespace file_util {

inline char* slurp_file(const char* path) {
    std::FILE* fp = std::fopen(path, "rb");
    if (!fp) return nullptr;
    if (std::fseek(fp, 0, SEEK_END) != 0) { std::fclose(fp); return nullptr; }
    long sz = std::ftell(fp);
    if (sz < 0) { std::fclose(fp); return nullptr; }
    std::rewind(fp);
    char* buf = static_cast<char*>(std::malloc(static_cast<size_t>(sz) + 1));
    if (!buf) { std::fclose(fp); return nullptr; }
    size_t got = std::fread(buf, 1, static_cast<size_t>(sz), fp);
    std::fclose(fp);
    if (got != static_cast<size_t>(sz)) { std::free(buf); return nullptr; }
    buf[sz] = '\0';
    return buf;
}

}  // namespace file_util
}  // namespace extemp
