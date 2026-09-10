#pragma once

#include <filesystem>
#include <string>
#include <string_view>

namespace extemp {
namespace share_dir {

// runtime/init.xtm is the first file SchemeProcess loads, so its presence is
// what makes a directory usable as a share dir.
inline bool holds_runtime_files(const std::filesystem::path& dir) {
    std::error_code ec;
    return std::filesystem::is_regular_file(dir / "runtime" / "init.xtm", ec);
}

// Choose the share dir --- the directory holding runtime/, libs/ and examples/
// --- from three candidates, in order of decreasing authority.
//
// An explicit choice (--sharedir) always wins, even when it is wrong, so a
// mistyped path reports the missing init.xtm instead of silently running
// against a different tree.  Next comes the directory holding the executable,
// which is what an unzipped binary release and a `cmake --install` prefix both
// look like: preferring it makes those trees relocatable and runnable from any
// working directory.  Last is the compile-time default, which for an in-tree
// build is the source tree (and `build/` holds no runtime/, so an in-tree build
// falls through to it).
inline std::string pick(std::string_view explicit_dir, const std::filesystem::path& exe_dir,
                        std::string_view compile_time_default) {
    if (!explicit_dir.empty()) {
        return std::string(explicit_dir);
    }
    if (!exe_dir.empty() && holds_runtime_files(exe_dir)) {
        return exe_dir.string();
    }
    return std::string(compile_time_default);
}

}  // namespace share_dir
}  // namespace extemp
