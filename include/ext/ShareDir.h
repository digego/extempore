#pragma once

#include <filesystem>
#include <functional>
#include <string>
#include <string_view>

namespace extemp {
namespace share_dir {

// Where Extempore's files live. An installed tree --- a release archive, or a
// `cmake --install` prefix --- is
//
//   <prefix>/bin/extempore
//   <prefix>/share/extempore/   the share dir: runtime/, libs/, examples/, assets/
//   <prefix>/lib/extempore/     the lib dir: aot-cache/, platform-shlibs/
//
// and is found from the executable, so it can be moved anywhere. A source build
// runs build/extempore against the source tree it was built from, with what the
// build generated in build/libs/.
//
// The lib dir is read-only in an installed tree, so AOT compilation writes to a
// per-user cache dir instead. In a source build the two are one directory.
struct Dirs {
    std::string share;
    std::string lib;
    std::string cache;
};

// runtime/init.xtm is the first file SchemeProcess loads, so its presence is
// what makes a directory usable as a share dir.
inline bool holds_runtime_files(const std::filesystem::path& dir) {
    std::error_code ec;
    return std::filesystem::is_regular_file(dir / "runtime" / "init.xtm", ec);
}

// The prefix of the installed tree the executable sits in, or an empty path if
// it isn't in one.
inline std::filesystem::path installed_prefix(const std::filesystem::path& exe_dir) {
    if (exe_dir.empty()) {
        return {};
    }
    const auto prefix = exe_dir.parent_path();
    return holds_runtime_files(prefix / "share" / "extempore") ? prefix : std::filesystem::path();
}

// The per-user directory for cached files: $XDG_CACHE_HOME (or ~/.cache) on
// Linux, ~/Library/Caches on macOS, %LOCALAPPDATA% on Windows. Empty if the
// environment names none. `getenv` returns an empty string for an unset name.
inline std::filesystem::path
user_cache_root(const std::function<std::string(const char*)>& getenv) {
#if defined(_WIN32)
    return getenv("LOCALAPPDATA");
#elif defined(__APPLE__)
    const auto home = getenv("HOME");
    return home.empty() ? std::filesystem::path()
                        : std::filesystem::path(home) / "Library" / "Caches";
#else
    if (const auto xdg = getenv("XDG_CACHE_HOME"); !xdg.empty()) {
        return xdg;
    }
    const auto home = getenv("HOME");
    return home.empty() ? std::filesystem::path() : std::filesystem::path(home) / ".cache";
#endif
}

// Choose the directories. An explicit share dir (--share-dir) always wins, even
// when it is wrong, so a mistyped path reports the missing init.xtm instead of
// silently running against a different tree. The cache dir is
// $EXTEMPORE_CACHE_DIR when that is set, and is versioned in the user cache
// root, since AOT output only loads into the compiler which wrote it.
inline Dirs pick(std::string_view explicit_share_dir, const std::filesystem::path& exe_dir,
                 std::string_view compile_time_default, std::string_view version,
                 const std::function<std::string(const char*)>& getenv) {
    Dirs dirs;
    const auto prefix = installed_prefix(exe_dir);
    if (!explicit_share_dir.empty()) {
        dirs.share = explicit_share_dir;
    } else if (!prefix.empty()) {
        dirs.share = (prefix / "share" / "extempore").string();
    } else {
        dirs.share = compile_time_default;
    }
    if (!prefix.empty()) {
        dirs.lib = (prefix / "lib" / "extempore").string();
    } else if (!exe_dir.empty()) {
        dirs.lib = (exe_dir / "libs").string();
    } else {
        dirs.lib = (std::filesystem::path(dirs.share) / "libs").string();
    }
    if (const auto env = getenv("EXTEMPORE_CACHE_DIR"); !env.empty()) {
        dirs.cache = env;
    } else if (const auto root = user_cache_root(getenv); !prefix.empty() && !root.empty()) {
        dirs.cache = (root / "extempore" / std::string(version)).string();
    } else {
        dirs.cache = dirs.lib;
    }
    return dirs;
}

}  // namespace share_dir
}  // namespace extemp
