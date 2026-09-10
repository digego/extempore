#include <gtest/gtest.h>

#include <atomic>
#include <filesystem>
#include <fstream>
#include <string>

#include "ext/ShareDir.h"

namespace {

// A directory that looks like an Extempore tree: the share-dir probe only asks
// for runtime/init.xtm, so that is all these fixtures need to contain.
class TempTree {
public:
    explicit TempTree(bool with_runtime) {
        static std::atomic<unsigned> counter{0};
        m_path = std::filesystem::temp_directory_path() /
                 ("extempore_sharedir_test_" + std::to_string(counter.fetch_add(1)));
        std::filesystem::create_directories(m_path);
        if (with_runtime) {
            std::filesystem::create_directories(m_path / "runtime");
            std::ofstream(m_path / "runtime" / "init.xtm") << "; stub\n";
        }
    }
    ~TempTree() {
        std::error_code ec;
        std::filesystem::remove_all(m_path, ec);
    }
    TempTree(const TempTree&) = delete;
    TempTree& operator=(const TempTree&) = delete;

    const std::filesystem::path& path() const { return m_path; }

private:
    std::filesystem::path m_path;
};

}  // namespace

TEST(PickShareDir, ExplicitChoiceWins) {
    TempTree beside_exe(true);
    EXPECT_EQ(extemp::share_dir::pick("/somewhere/else", beside_exe.path(), "/compiled/in"),
              "/somewhere/else");
}

TEST(PickShareDir, ExplicitChoiceWinsEvenWhenItHoldsNoRuntime) {
    // A mistyped --sharedir must surface as a missing init.xtm, not as a silent
    // fallback to a tree the caller did not ask for.
    TempTree beside_exe(true);
    EXPECT_EQ(extemp::share_dir::pick("/nonexistent", beside_exe.path(), "/compiled/in"),
              "/nonexistent");
}

TEST(PickShareDir, PrefersTheTreeBesideTheExecutable) {
    TempTree beside_exe(true);
    EXPECT_EQ(extemp::share_dir::pick("", beside_exe.path(), "/compiled/in"),
              beside_exe.path().string());
}

TEST(PickShareDir, FallsBackWhenTheExecutableHasNoTreeBesideIt) {
    // An in-tree build: the binary sits in build/, which holds no runtime/.
    TempTree beside_exe(false);
    EXPECT_EQ(extemp::share_dir::pick("", beside_exe.path(), "/compiled/in"), "/compiled/in");
}

TEST(PickShareDir, FallsBackWhenTheExecutablePathIsUnknown) {
    EXPECT_EQ(extemp::share_dir::pick("", std::filesystem::path(), "/compiled/in"),
              "/compiled/in");
}

TEST(HoldsRuntimeFiles, NeedsRuntimeInitXtm) {
    TempTree with_runtime(true);
    TempTree without_runtime(false);
    EXPECT_TRUE(extemp::share_dir::holds_runtime_files(with_runtime.path()));
    EXPECT_FALSE(extemp::share_dir::holds_runtime_files(without_runtime.path()));
    EXPECT_FALSE(extemp::share_dir::holds_runtime_files("/nonexistent"));
}
