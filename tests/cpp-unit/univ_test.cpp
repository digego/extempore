#include <gtest/gtest.h>

#include <atomic>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <string>

#include "ext/FileUtil.h"

namespace {

std::string write_temp(const std::string& contents) {
    static std::atomic<unsigned> counter{0};
    auto p = std::filesystem::temp_directory_path() /
             ("extempore_test_" + std::to_string(counter.fetch_add(1)) + ".bin");
    std::ofstream f(p, std::ios::binary);
    f.write(contents.data(), static_cast<std::streamsize>(contents.size()));
    f.close();
    return p.string();
}

}  // namespace

TEST(SlurpFile, ReadsAsciiFullyWithNullTerminator) {
    auto path = write_temp("hello");
    char* out = extemp::file_util::slurp_file(path.c_str());
    ASSERT_NE(out, nullptr);
    EXPECT_EQ(std::string(out), "hello");
    std::free(out);
    std::filesystem::remove(path);
}

TEST(SlurpFile, HandlesEmpty) {
    auto path = write_temp("");
    char* out = extemp::file_util::slurp_file(path.c_str());
    ASSERT_NE(out, nullptr);
    EXPECT_EQ(out[0], '\0');
    std::free(out);
    std::filesystem::remove(path);
}

TEST(SlurpFile, PreservesLastByte) {
    // The pre-fix implementation wrote the terminator *into* the last content
    // byte, truncating files by one character. This test guards that regression.
    auto path = write_temp("abcde");
    char* out = extemp::file_util::slurp_file(path.c_str());
    ASSERT_NE(out, nullptr);
    EXPECT_EQ(out[4], 'e');
    EXPECT_EQ(out[5], '\0');
    std::free(out);
    std::filesystem::remove(path);
}

TEST(SlurpFile, PreservesBytesAcrossEmbeddedNull) {
    std::string contents("ab\0cd", 5);
    auto path = write_temp(contents);
    char* out = extemp::file_util::slurp_file(path.c_str());
    ASSERT_NE(out, nullptr);
    EXPECT_EQ(out[0], 'a');
    EXPECT_EQ(out[1], 'b');
    EXPECT_EQ(out[2], '\0');
    EXPECT_EQ(out[3], 'c');
    EXPECT_EQ(out[4], 'd');
    EXPECT_EQ(out[5], '\0');
    std::free(out);
    std::filesystem::remove(path);
}

TEST(SlurpFile, MissingFileReturnsNull) {
    char* out = extemp::file_util::slurp_file("/nonexistent/path/that/does/not/exist");
    EXPECT_EQ(out, nullptr);
}
