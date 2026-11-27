#include <gtest/gtest.h>

#include <atomic>
#include <filesystem>
#include <fstream>
#include <map>
#include <optional>
#include <string>

#include "ext/ShareDir.h"

namespace {

// A temporary directory, holding runtime/init.xtm at `runtime_at` (relative to
// it) when that is given: the share-dir probe only asks for that file, so it is
// all these fixtures need to contain.
class TempTree {
  public:
    explicit TempTree(std::optional<std::filesystem::path> runtime_at = std::nullopt) {
        static std::atomic<unsigned> counter{0};
        // ctest runs each test in its own process, in parallel, so the counter
        // alone does not make the name unique: the test name does.
        const auto* info = ::testing::UnitTest::GetInstance()->current_test_info();
        m_path = std::filesystem::temp_directory_path() /
                 (std::string("extempore_sharedir_test_") + info->test_suite_name() + "_" +
                  info->name() + "_" + std::to_string(counter.fetch_add(1)));
        std::filesystem::create_directories(m_path);
        if (runtime_at) {
            std::filesystem::create_directories(m_path / *runtime_at / "runtime");
            std::ofstream(m_path / *runtime_at / "runtime" / "init.xtm") << "; stub\n";
        }
    }
    ~TempTree() {
        std::error_code ec;
        std::filesystem::remove_all(m_path, ec);
    }
    TempTree(const TempTree&) = delete;
    TempTree& operator=(const TempTree&) = delete;

    const std::filesystem::path& path() const {
        return m_path;
    }

  private:
    std::filesystem::path m_path;
};

// An environment holding only `vars`.
std::function<std::string(const char*)> env(std::map<std::string, std::string> vars = {}) {
    return [vars](const char* name) {
        const auto it = vars.find(name);
        return it == vars.end() ? std::string() : it->second;
    };
}

// the variable user_cache_root reads on this platform, and what it gives
#if defined(_WIN32)
const std::map<std::string, std::string> kUserEnv{{"LOCALAPPDATA", "C:/Users/u/AppData/Local"}};
const std::filesystem::path kUserCacheRoot = "C:/Users/u/AppData/Local";
#elif defined(__APPLE__)
const std::map<std::string, std::string> kUserEnv{{"HOME", "/Users/u"}};
const std::filesystem::path kUserCacheRoot = "/Users/u/Library/Caches";
#else
const std::map<std::string, std::string> kUserEnv{{"HOME", "/home/u"}};
const std::filesystem::path kUserCacheRoot = "/home/u/.cache";
#endif

using extemp::share_dir::pick;

}  // namespace

TEST(PickDirs, AnInstalledTreeIsFoundFromTheExecutable) {
    TempTree prefix("share/extempore");
    const auto dirs = pick("", prefix.path() / "bin", "/compiled/in", "v1.2.3", env(kUserEnv));
    EXPECT_EQ(dirs.share, (prefix.path() / "share" / "extempore").string());
    EXPECT_EQ(dirs.lib, (prefix.path() / "lib" / "extempore").string());
    EXPECT_EQ(dirs.cache, (kUserCacheRoot / "extempore" / "v1.2.3").string());
}

TEST(PickDirs, ASourceBuildRunsAgainstItsSourceTree) {
    // The binary sits in build/, and build/.. (the source tree) holds no
    // share/extempore/.
    TempTree source("");
    const auto build = source.path() / "build";
    const auto dirs = pick("", build, "/compiled/in", "v1.2.3", env(kUserEnv));
    EXPECT_EQ(dirs.share, "/compiled/in");
    EXPECT_EQ(dirs.lib, (build / "libs").string());
    EXPECT_EQ(dirs.cache, dirs.lib);
}

TEST(PickDirs, AnExplicitShareDirWins) {
    TempTree prefix("share/extempore");
    const auto dirs =
        pick("/somewhere/else", prefix.path() / "bin", "/compiled/in", "v1", env(kUserEnv));
    EXPECT_EQ(dirs.share, "/somewhere/else");
    EXPECT_EQ(dirs.lib, (prefix.path() / "lib" / "extempore").string());
}

TEST(PickDirs, AnExplicitShareDirWinsEvenWhenItHoldsNoRuntime) {
    // A mistyped --share-dir must surface as a missing init.xtm, not as a
    // silent fallback to a tree the caller did not ask for.
    TempTree prefix("share/extempore");
    EXPECT_EQ(pick("/nonexistent", prefix.path() / "bin", "/compiled/in", "v1", env()).share,
              "/nonexistent");
}

TEST(PickDirs, TheCacheDirCanBeNamed) {
    TempTree prefix("share/extempore");
    auto vars = kUserEnv;
    vars["EXTEMPORE_CACHE_DIR"] = "/tmp/xtm-cache";
    EXPECT_EQ(pick("", prefix.path() / "bin", "/compiled/in", "v1", env(vars)).cache,
              "/tmp/xtm-cache");
    TempTree source("");
    EXPECT_EQ(pick("", source.path() / "build", "/compiled/in", "v1", env(vars)).cache,
              "/tmp/xtm-cache");
}

TEST(PickDirs, AnInstalledTreeWithNoUserCacheRootWritesToItsLibDir) {
    TempTree prefix("share/extempore");
    const auto dirs = pick("", prefix.path() / "bin", "/compiled/in", "v1", env());
    EXPECT_EQ(dirs.cache, dirs.lib);
}

TEST(PickDirs, FallsBackWhenTheExecutablePathIsUnknown) {
    const auto dirs = pick("", std::filesystem::path(), "/compiled/in", "v1", env(kUserEnv));
    EXPECT_EQ(dirs.share, "/compiled/in");
    EXPECT_EQ(dirs.lib, (std::filesystem::path("/compiled/in") / "libs").string());
    EXPECT_EQ(dirs.cache, dirs.lib);
}

TEST(PickDirs, AFlatTreeBesideTheExecutableIsNotAnInstall) {
    // the layout of releases before v0.13.0: runtime/ beside the binary
    TempTree flat("");
    EXPECT_EQ(pick("", flat.path(), "/compiled/in", "v1", env()).share, "/compiled/in");
}

TEST(HoldsRuntimeFiles, NeedsRuntimeInitXtm) {
    TempTree with_runtime("");
    TempTree without_runtime;
    EXPECT_TRUE(extemp::share_dir::holds_runtime_files(with_runtime.path()));
    EXPECT_FALSE(extemp::share_dir::holds_runtime_files(without_runtime.path()));
    EXPECT_FALSE(extemp::share_dir::holds_runtime_files("/nonexistent"));
}

#if !defined(_WIN32) && !defined(__APPLE__)
TEST(UserCacheRoot, PrefersXdgCacheHome) {
    EXPECT_EQ(extemp::share_dir::user_cache_root(
                  env({{"HOME", "/home/u"}, {"XDG_CACHE_HOME", "/var/cache/u"}})),
              std::filesystem::path("/var/cache/u"));
}
#endif

TEST(UserCacheRoot, IsEmptyWhenTheEnvironmentNamesNone) {
    EXPECT_TRUE(extemp::share_dir::user_cache_root(env()).empty());
}
