# C++ modernisation implementation plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Apply findings from the 2026-04-19 C++ audit to Extempore's runtime, in CI-gated phases that keep Linux x64 / Linux arm64 / macOS arm64 / Windows x64 green at every commit.

**Architecture:** Work in a `cpp-modernisation` branch, ship each phase as a small commit series, push after every phase, wait for `build-and-test.yml` to go green on all four platforms before starting the next. Phases are ordered so the highest-leverage work (compiler warnings) runs first — every subsequent phase benefits from it. A minimal C++ unit-test target (`tests/cpp-unit/`) is added in Phase 2 and grows throughout.

**Tech Stack:** C++17, CMake + Ninja, ctest, LLVM 22 ORC, portaudio, s7 Scheme, GoogleTest via FetchContent (added in Phase 2).

---

## Phase summary

| Phase | Focus                            | Outcome                                                              |
|-------|----------------------------------|----------------------------------------------------------------------|
| 1     | Build hygiene                    | `-Wall -Wextra`, `.clang-format`, `.clang-tidy`, sanitizer build opt |
| 2     | Critical UNIV.cpp correctness    | Fix UB + memory bugs in `UNIV.cpp`; add cpp-unit test harness        |
| 3     | Kill hand-rolled sync primitives | Delete `EXTMutex`/`EXTMonitor`/`EXTCondition`/`EXTThread`            |
| 4     | Networking safety                | `gethostbyname` → `getaddrinfo`; FD sentinel; signal-handler safety  |
| 5     | Spin off deferred backlog items  | Convert larger findings to backlog tasks                             |

CI gate between every phase: push, wait, confirm the 4-platform matrix is green.

---

## Phase 0: Branch setup

### Task 0.1: Create branch

**Files:** none

- [ ] **Step 1: Create and check out a clean branch**

```bash
git checkout -b cpp-modernisation
```

- [ ] **Step 2: Push to establish the remote and confirm CI triggers**

```bash
git push -u origin cpp-modernisation
gh run list --branch cpp-modernisation --limit 3
```

Expected: `build-and-test.yml` queued for all 4 matrix entries (builds pass; the branch hasn't changed anything yet).

---

## Phase 1: Build hygiene

**Files:**
- Modify: `CMakeLists.txt:271-276` (UNIX compile options) and `CMakeLists.txt:283` (Windows compile options)
- Create: `.clang-format`
- Create: `.clang-tidy`

### Task 1.1: Turn on the warning flags that find bugs

Rationale: `-Wall -Wextra` as warnings (not errors) shows us the lay of the land without blocking the build. `-Werror=` on a small set of *always bugs* gives us a regression gate.

- [ ] **Step 1: Edit `CMakeLists.txt` UNIX block**

In the `if(UNIX)` branch around line 271, replace the existing `target_compile_options(extempore PRIVATE ...)` block with:

```cmake
target_compile_options(extempore PRIVATE
    -fvisibility-inlines-hidden
    -fno-rtti
    -fno-common
    -Wall
    -Wextra
    -Woverloaded-virtual
    -Wshadow
    -Wno-unused-result
    -Wno-unused-parameter
    -Wno-missing-field-initializers
    -Wno-sign-compare
    -Werror=return-type
    -Werror=uninitialized
    -Werror=sometimes-uninitialized
    -Werror=return-stack-address)
```

The `-Wno-*` suppressions are the load-bearing ones: we're turning on `-Wall -Wextra` but silencing the categories that produce thousands of hits on vendored s7/pcre/LLVM code without pointing at real bugs. We can remove suppressions one at a time in follow-up tasks.

`-Werror=` on `return-type`, `uninitialized`, `sometimes-uninitialized`, `return-stack-address` turns always-bugs into build failures.

- [ ] **Step 2: Edit `CMakeLists.txt` Windows block**

In the `if(WIN32)` block around line 283-284, append warning flags:

```cmake
target_compile_options(extempore PRIVATE /bigobj /W3 /wd4244 /wd4267 /wd4018 /we4715)
```

`/we4715` promotes "not all paths return a value" to an error (matches the UNIX `-Werror=return-type`).

- [ ] **Step 3: Configure + build locally to see the warning volume**

```bash
cmake --build build --parallel 2 2>&1 | tee /tmp/warn.log
grep -cE "warning:|error:" /tmp/warn.log
```

Expected: build succeeds (errors should be zero, barring a real bug caught by `-Werror=return-type`). Warnings likely 50-500.

- [ ] **Step 4: If a `-Werror=` category fires, investigate**

If the build fails with one of the promoted-to-error warnings, read the error, fix the root cause, do *not* disable the error. Commit the fix alongside the flag change.

- [ ] **Step 5: Commit**

```bash
git add CMakeLists.txt
git commit -m "build: enable -Wall -Wextra with bug-finding errors

Turns on -Wall -Wextra as warnings and promotes four categories
(return-type, uninitialized, sometimes-uninitialized, return-stack-address)
to errors. Silences -Wno-unused-parameter, -Wno-missing-field-initializers,
and -Wno-sign-compare for now; these produce mostly noise from vendored
code and will be revisited in follow-up passes."
```

### Task 1.2: Sanitizer build option

- [ ] **Step 1: Add the option near the top of `CMakeLists.txt`**

Find the existing `option(BUILD_TESTS ...)` line (~line 8) and insert after it:

```cmake
set(EXTEMPORE_SANITIZE "" CACHE STRING "Build with sanitizer: asan, ubsan, tsan, asan+ubsan")
```

- [ ] **Step 2: Add sanitizer flag wiring to the UNIX block**

After the `target_compile_options` block in the `if(UNIX)` branch, append:

```cmake
if(EXTEMPORE_SANITIZE)
    set(SAN_FLAGS "")
    if(EXTEMPORE_SANITIZE MATCHES "asan")
        list(APPEND SAN_FLAGS -fsanitize=address -fno-omit-frame-pointer)
    endif()
    if(EXTEMPORE_SANITIZE MATCHES "ubsan")
        list(APPEND SAN_FLAGS -fsanitize=undefined -fno-sanitize-recover=undefined)
    endif()
    if(EXTEMPORE_SANITIZE MATCHES "tsan")
        list(APPEND SAN_FLAGS -fsanitize=thread)
    endif()
    target_compile_options(extempore PRIVATE ${SAN_FLAGS})
    target_link_options(extempore PRIVATE ${SAN_FLAGS})
    message(STATUS "Extempore sanitizers enabled: ${EXTEMPORE_SANITIZE}")
endif()
```

- [ ] **Step 3: Test locally**

```bash
rm -rf build-asan && cmake -B build-asan -G Ninja -DCMAKE_BUILD_TYPE=Debug -DEXTEMPORE_SANITIZE=asan
cmake --build build-asan --target extempore --parallel 2
./build-asan/extempore --batch "(begin (println 'hello) (quit 0))"
```

Expected: build succeeds; batch run either completes cleanly or reports real ASan findings. Record findings.

- [ ] **Step 4: Commit**

```bash
git add CMakeLists.txt
git commit -m "build: add EXTEMPORE_SANITIZE option for ASan/UBSan/TSan"
```

### Task 1.3: `.clang-format`

- [ ] **Step 1: Create `.clang-format` at repo root**

Mirror LLVM's own style since the codebase is LLVM-adjacent and roughly matches:

```yaml
---
BasedOnStyle: LLVM
IndentWidth: 4
TabWidth: 4
UseTab: Never
ColumnLimit: 100
AlignAfterOpenBracket: Align
AllowShortFunctionsOnASingleLine: Empty
AllowShortIfStatementsOnASingleLine: Never
AllowShortLoopsOnASingleLine: false
BreakBeforeBraces: Attach
NamespaceIndentation: None
PointerAlignment: Left
SpacesBeforeTrailingComments: 2
IncludeBlocks: Preserve
SortIncludes: Never
```

`SortIncludes: Never` is deliberate — header order in this codebase matters (Windows headers before POSIX headers in several TUs).

- [ ] **Step 2: Do NOT mass-reformat the codebase yet**

A reformat-everything commit pollutes `git blame`. Keep `.clang-format` available for incremental use (editor-on-save, `clang-format-diff` on new changes). A full reformat is TASK-023 in the backlog — leave it there.

- [ ] **Step 3: Commit**

```bash
git add .clang-format
git commit -m "style: add .clang-format (LLVM-based, no mass reformat)"
```

### Task 1.4: `.clang-tidy`

- [ ] **Step 1: Create `.clang-tidy` at repo root**

Start conservative — only the bug-finding checks, not style or modernize yet:

```yaml
---
Checks: >
  -*,
  bugprone-*,
  -bugprone-easily-swappable-parameters,
  -bugprone-macro-parentheses,
  -bugprone-narrowing-conversions,
  cppcoreguidelines-pro-type-static-cast-downcast,
  cppcoreguidelines-slicing,
  misc-const-correctness,
  misc-misplaced-const,
  performance-*,
  -performance-no-int-to-ptr,
  readability-misleading-indentation,
  readability-redundant-smartptr-get
WarningsAsErrors: ''
HeaderFilterRegex: '^(src|include)/(?!pcre|s7|networking-ts-impl|ffi|shims|linenoise).*'
```

- [ ] **Step 2: Add a CMake target for running clang-tidy**

Near the bottom of `CMakeLists.txt` (before the `assets` target), add:

```cmake
add_custom_target(tidy
    COMMAND clang-tidy -p ${CMAKE_BINARY_DIR} ${EXTEMPORE_SOURCES}
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    COMMENT "Running clang-tidy on Extempore sources")
```

This depends on CMake generating `compile_commands.json`, which Ninja does automatically.

- [ ] **Step 3: Run it locally to see the output volume**

```bash
cmake --build build --target tidy 2>&1 | tee /tmp/tidy.log
grep -c "warning:" /tmp/tidy.log
```

Record the count. Do NOT fix anything yet — this is a baseline.

- [ ] **Step 4: Commit**

```bash
git add .clang-tidy CMakeLists.txt
git commit -m "tooling: add .clang-tidy config and 'tidy' make target"
```

### Task 1.5: CI gate

- [ ] **Step 1: Push the phase**

```bash
git push
```

- [ ] **Step 2: Watch CI**

```bash
gh run watch --branch cpp-modernisation --exit-status
```

Expected: all 4 matrix platforms green. If Linux or macOS fails on a new warning, that's a bug — fix it and push again. If Windows fails on `/we4715`, investigate the missing-return path and fix.

---

## Phase 2: Critical UNIV.cpp correctness + cpp-unit test harness

**Files:**
- Modify: `src/UNIV.cpp:159-210` (`cname_decode`), `src/UNIV.cpp:212-265` (`base64_decode`), `src/UNIV.cpp:326-343` (`rreplace`/`rsplit`), `src/UNIV.cpp:349-372` (`sys_slurp_file`)
- Create: `tests/cpp-unit/CMakeLists.txt`, `tests/cpp-unit/univ_test.cpp`
- Modify: `CMakeLists.txt` (add GoogleTest via FetchContent and `add_subdirectory(tests/cpp-unit)`)
- Modify: `.github/workflows/build-and-test.yml:75` (add `compiler-unit|cpp-unit` to the ctest label regex — it's `compiler-unit` already; add `|cpp-unit`)

### Task 2.1: Add GoogleTest and the cpp-unit subdirectory

- [ ] **Step 1: Add GoogleTest to `CMakeLists.txt` via FetchContent**

Near the top where other `FetchContent_Declare`s live (grep for `FetchContent_Declare(llvm`), add:

```cmake
if(BUILD_TESTS)
    include(FetchContent)
    FetchContent_Declare(
        googletest
        URL https://github.com/google/googletest/archive/refs/tags/v1.15.2.tar.gz
        URL_HASH SHA256=7b42b4d6ed48810c5362c265a17faebe90dc2373c885e5216439d37927f02926)
    set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
    FetchContent_MakeAvailable(googletest)
endif()
```

The hash is the published one for v1.15.2; update if CMake complains.

- [ ] **Step 2: Add subdirectory**

Near the bottom of `CMakeLists.txt`, before the `assets` target:

```cmake
if(BUILD_TESTS)
    enable_testing()
    add_subdirectory(tests/cpp-unit)
endif()
```

Check that `enable_testing()` isn't already there — if it is, leave the existing call.

- [ ] **Step 3: Create `tests/cpp-unit/CMakeLists.txt`**

```cmake
add_executable(extempore_cpp_unit
    univ_test.cpp)

target_link_libraries(extempore_cpp_unit PRIVATE
    gtest_main)

target_include_directories(extempore_cpp_unit PRIVATE
    ${CMAKE_SOURCE_DIR}/include
    ${CMAKE_SOURCE_DIR}/src)

target_compile_features(extempore_cpp_unit PRIVATE cxx_std_17)

include(GoogleTest)
gtest_discover_tests(extempore_cpp_unit
    PROPERTIES LABELS "cpp-unit")
```

- [ ] **Step 4: Create a skeleton `tests/cpp-unit/univ_test.cpp`**

```cpp
#include <gtest/gtest.h>

TEST(UnivSmoke, PlaceholderUntilRealTests) {
    EXPECT_EQ(1 + 1, 2);
}
```

(Real tests arrive in 2.2 onward.)

- [ ] **Step 5: Build and run**

```bash
cmake --build build --target extempore_cpp_unit --parallel 2
ctest --test-dir build --label-regex cpp-unit --output-on-failure
```

Expected: the smoke test passes.

- [ ] **Step 6: Add `cpp-unit` to the CI label regex**

In `.github/workflows/build-and-test.yml:75`, change:

```yaml
run: ctest --test-dir build --build-config Release --label-regex "libs-core|libs-external|compiler-unit|examples-core|examples-audio" --output-on-failure
```

to:

```yaml
run: ctest --test-dir build --build-config Release --label-regex "libs-core|libs-external|compiler-unit|cpp-unit|examples-core|examples-audio" --output-on-failure
```

- [ ] **Step 7: Commit**

```bash
git add CMakeLists.txt tests/cpp-unit/ .github/workflows/build-and-test.yml
git commit -m "test: add cpp-unit test target with GoogleTest"
```

### Task 2.2: Test + fix `sys_slurp_file` off-by-one and unchecked `fread`

**Files:** `src/UNIV.cpp:349-372`, `tests/cpp-unit/univ_test.cpp`

- [ ] **Step 1: Expose a testable signature**

`sys_slurp_file` currently lives in the `extemp::UNIV` namespace (check `include/UNIV.h`). If it's already exposed as `extemp::UNIV::sys_slurp_file(const char*)`, no change needed. Otherwise add a declaration to `include/UNIV.h` in the namespace block (grep `rreplace` — it's declared nearby).

- [ ] **Step 2: Write failing tests in `tests/cpp-unit/univ_test.cpp`**

Replace the placeholder test:

```cpp
#include <gtest/gtest.h>
#include <cstdio>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <string>

#include "UNIV.h"

namespace {
std::string write_temp(const std::string& contents) {
    auto p = std::filesystem::temp_directory_path() /
             ("extempore_test_" + std::to_string(::getpid()) + "_" +
              std::to_string(rand()) + ".txt");
    std::ofstream(p, std::ios::binary) << contents;
    return p.string();
}
}

TEST(SysSlurpFile, PreservesAllBytes) {
    auto path = write_temp("hello");
    char* out = extemp::UNIV::sys_slurp_file(path.c_str());
    ASSERT_NE(out, nullptr);
    EXPECT_STREQ(out, "hello");
    std::free(out);
    std::filesystem::remove(path);
}

TEST(SysSlurpFile, HandlesEmpty) {
    auto path = write_temp("");
    char* out = extemp::UNIV::sys_slurp_file(path.c_str());
    ASSERT_NE(out, nullptr);
    EXPECT_STREQ(out, "");
    std::free(out);
    std::filesystem::remove(path);
}

TEST(SysSlurpFile, HandlesBinaryWithEmbeddedZero) {
    auto path = write_temp(std::string("ab\0cd", 5));
    char* out = extemp::UNIV::sys_slurp_file(path.c_str());
    ASSERT_NE(out, nullptr);
    // At minimum the last non-null byte must be preserved
    EXPECT_EQ(out[3], 'c');
    EXPECT_EQ(out[4], 'd');
    std::free(out);
    std::filesystem::remove(path);
}
```

- [ ] **Step 3: Run — the `PreservesAllBytes` test will fail because current impl truncates**

```bash
cmake --build build --target extempore_cpp_unit
ctest --test-dir build --label-regex cpp-unit --output-on-failure
```

Expected: `PreservesAllBytes` reports `out = "hell"` (last byte truncated by `buf[file_size-1] = '\0'`).

- [ ] **Step 4: Fix `sys_slurp_file` in `src/UNIV.cpp`**

Replace the current body (around lines 349-372) with:

```cpp
char* sys_slurp_file(const char* fname) {
    std::FILE* fp = std::fopen(fname, "rb");
    if (!fp) return nullptr;
    if (std::fseek(fp, 0, SEEK_END) != 0) { std::fclose(fp); return nullptr; }
    long file_size = std::ftell(fp);
    if (file_size < 0) { std::fclose(fp); return nullptr; }
    std::rewind(fp);
    char* buf = static_cast<char*>(std::malloc(static_cast<size_t>(file_size) + 1));
    if (!buf) { std::fclose(fp); return nullptr; }
    size_t read = std::fread(buf, 1, static_cast<size_t>(file_size), fp);
    std::fclose(fp);
    if (read != static_cast<size_t>(file_size)) { std::free(buf); return nullptr; }
    buf[file_size] = '\0';
    return buf;
}
```

- [ ] **Step 5: Re-run tests**

```bash
ctest --test-dir build --label-regex cpp-unit --output-on-failure
```

Expected: all three `SysSlurpFile` tests pass.

- [ ] **Step 6: Commit**

```bash
git add src/UNIV.cpp tests/cpp-unit/univ_test.cpp
git commit -m "fix(univ): sys_slurp_file off-by-one and unchecked fread

- Allocates file_size+1 bytes so the terminator doesn't overwrite content
- Checks fread's return to catch short reads instead of silently returning
  a partially-populated buffer
- Adds cpp-unit tests covering empty, ASCII, and embedded-null inputs"
```

### Task 2.3: Test + fix `cname_decode` shadow + memory leak

**Files:** `src/UNIV.cpp:159-210`, `tests/cpp-unit/univ_test.cpp`

The outer `char* d2 = nullptr` is shadowed by an inner `char* d2 = ...` inside the padding branch (line ~173). The outer `free(d2)` (line 207) is always a no-op; the inner `d2` leaks.

- [ ] **Step 1: Write a failing test**

Add to `univ_test.cpp`:

```cpp
TEST(CnameDecode, PadBranchDoesNotLeak) {
    // Input that triggers the padding branch — base64 encoding length ≡ 2 mod 4
    std::string encoded = "YWI";  // "ab" encoded
    char buf[16] = {};
    // Exact API signature may need adjustment based on actual declaration
    extemp::UNIV::cname_decode(encoded.data(), encoded.size(), buf);
    EXPECT_STREQ(buf, "ab");
    // Leak detection happens via ASan CI run (Phase 1.2 sanitizer build)
}
```

(If the real signature differs, adjust — check `include/UNIV.h` for the declaration.)

- [ ] **Step 2: Run — expect a correctness failure or leak under ASan**

```bash
cmake --build build-asan --target extempore_cpp_unit
ctest --test-dir build-asan --label-regex cpp-unit --output-on-failure
```

Expected: ASan reports `direct leak`.

- [ ] **Step 3: Rename the inner variable in `src/UNIV.cpp`**

Find the inner `char* d2` allocation (line ~173) and rename to `pad_buf`:

```cpp
// original:
char* d2 = (char*)malloc(padded_len + 1);
// ... uses d2 ...

// fixed:
char* pad_buf = (char*)malloc(padded_len + 1);
// ... uses pad_buf ...
// At the end of the padding branch, BEFORE overwriting the outer d2:
data = pad_buf;  // or whatever the original assignment was
// Also ensure the outer free cleans up pad_buf: the outer free becomes
// if (pad_buf) free(pad_buf) — rename the outer d2 accordingly,
// OR keep pad_buf local and free it before the branch exits.
```

Exact structure depends on the function flow — read lines 159-210 carefully. The goal: outer scope variable names no longer shadow, every `malloc` has a matching `free`.

- [ ] **Step 4: Re-run under ASan**

```bash
cmake --build build-asan --target extempore_cpp_unit
ctest --test-dir build-asan --label-regex cpp-unit --output-on-failure
```

Expected: no leak reports.

- [ ] **Step 5: Commit**

```bash
git add src/UNIV.cpp tests/cpp-unit/univ_test.cpp
git commit -m "fix(univ): cname_decode pad-branch leak and variable shadow"
```

### Task 2.4: Test + fix `cname_decode` / `base64_decode` unsequenced side effects

**Files:** `src/UNIV.cpp:193-196, 257-260`, `tests/cpp-unit/univ_test.cpp`

The expression `data[i] == '=' ? 0 & i++ : table[unsigned(data[i++])]` evaluates `i++` in multiple sub-expressions without sequencing → UB.

- [ ] **Step 1: Fix by splitting into explicit statements**

For both sites, rewrite:

```cpp
// before:
foo = data[i] == '=' ? 0 & i++ : base64_decoding_table[unsigned(data[i++])];

// after:
unsigned char ch = static_cast<unsigned char>(data[i++]);
foo = (ch == '=') ? 0 : base64_decoding_table[ch];
```

- [ ] **Step 2: Add a UBSan-exercising test**

```cpp
TEST(Base64Decode, HandlesPaddingWithoutUB) {
    // Exact input that hits the '=' branch; run under UBSan
    std::string encoded = "YWI=";
    char buf[16] = {};
    extemp::UNIV::base64_decode(encoded.data(), encoded.size(), buf);
    EXPECT_STREQ(buf, "ab");
}
```

- [ ] **Step 3: Run under UBSan**

```bash
rm -rf build-ubsan && cmake -B build-ubsan -G Ninja -DCMAKE_BUILD_TYPE=Debug -DEXTEMPORE_SANITIZE=ubsan
cmake --build build-ubsan --target extempore_cpp_unit
ctest --test-dir build-ubsan --label-regex cpp-unit --output-on-failure
```

Expected: pre-fix, UBSan reports unsequenced modification; post-fix, passes cleanly.

- [ ] **Step 4: Commit**

```bash
git add src/UNIV.cpp tests/cpp-unit/univ_test.cpp
git commit -m "fix(univ): sequence data[i++] reads in base64/cname decode"
```

### Task 2.5: Test + fix `rreplace` / `rsplit` buffer overrun

**Files:** `src/UNIV.cpp:326-343`, `include/UNIV.h`, `tests/cpp-unit/univ_test.cpp`

Current signatures write into a caller-supplied `char*` with a post-hoc 4096 check. Change to return `std::string`.

- [ ] **Step 1: Add new overloads returning `std::string`**

In `include/UNIV.h` namespace `UNIV`:

```cpp
std::string rreplace(const std::string& pattern,
                     const std::string& replacement,
                     const std::string& input);
std::vector<std::string> rsplit(const std::string& pattern,
                                const std::string& input);
```

In `src/UNIV.cpp`, implement both using `std::regex_replace` and `std::sregex_token_iterator` respectively.

- [ ] **Step 2: Deprecate the raw-pointer versions**

Mark the old signatures `[[deprecated("use std::string overload")]]`.

- [ ] **Step 3: Write tests**

```cpp
TEST(RReplace, BasicReplacement) {
    EXPECT_EQ(extemp::UNIV::rreplace("foo", "bar", "a foo in a foo"),
              "a bar in a bar");
}
TEST(RSplit, BasicSplit) {
    auto parts = extemp::UNIV::rsplit(",", "a,b,c");
    ASSERT_EQ(parts.size(), 3u);
    EXPECT_EQ(parts[0], "a");
    EXPECT_EQ(parts[1], "b");
    EXPECT_EQ(parts[2], "c");
}
TEST(RReplace, VeryLongInputDoesNotCrash) {
    std::string big(100000, 'x');
    auto out = extemp::UNIV::rreplace("x", "yy", big);
    EXPECT_EQ(out.size(), 200000u);
}
```

- [ ] **Step 4: Run, commit**

```bash
cmake --build build --target extempore_cpp_unit
ctest --test-dir build --label-regex cpp-unit --output-on-failure
git add src/UNIV.cpp include/UNIV.h tests/cpp-unit/univ_test.cpp
git commit -m "feat(univ): add std::string rreplace/rsplit overloads

Deprecates the char* versions that had an unchecked buffer-bound.
Call sites in libs/ and xtlang will migrate in a follow-up."
```

### Task 2.6: CI gate

- [ ] **Step 1: Push**

```bash
git push
```

- [ ] **Step 2: Watch CI**

```bash
gh run watch --branch cpp-modernisation --exit-status
```

Expected: all 4 platforms green. `cpp-unit` tests now run as part of `ctest`.

---

## Phase 3: Kill hand-rolled sync primitives

**Files to delete:** `include/EXTMutex.h`, `src/EXTThread.cpp`, `include/EXTThread.h`, `include/EXTMonitor.h`, `include/EXTCondition.h`
**Files to modify:** every include site — use `grep -rn "EXTMutex\|EXTThread\|EXTMonitor\|EXTCondition" src/ include/` for the full list. Known users:
- `src/SchemeProcess.cpp`, `src/SchemeREPL.cpp`, `src/AudioDevice.cpp`, `src/TaskScheduler.cpp`, `src/EXTZones.cpp`, `src/EXTLLVM.cpp`
- `include/SchemeProcess.h`, `include/SchemeREPL.h`, `include/AudioDevice.h`, `include/TaskScheduler.h`

### Task 3.1: Enumerate call sites

- [ ] **Step 1: Map the usage**

```bash
grep -rn "EXTMutex\|EXTMonitor\|EXTCondition\|EXTThread\|ScopedLock" src/ include/ > /tmp/sync_sites.txt
wc -l /tmp/sync_sites.txt
```

Review the file. Group by pattern:
1. `EXTMutex m; m.lock(); ...; m.unlock();` → `std::mutex` + `std::lock_guard`
2. `EXTMutex::ScopedLock l(m);` → `std::lock_guard<std::mutex> l(m);`
3. `EXTMonitor` (mutex + condvar bundle) → `std::mutex` + `std::condition_variable`
4. `EXTCondition c; c.wait(m); c.signal();` → `std::condition_variable c; c.wait(lock); c.notify_one();`
5. `EXTThread t(fn, arg); t.start(); t.stop();` → `std::thread`

### Task 3.2: Replace `EXTMutex`

- [ ] **Step 1: Read `include/EXTMutex.h` to confirm it's a pure `std::recursive_mutex` wrapper**

Confirm the semantics — it uses `std::recursive_mutex` unconditionally. Most call sites probably don't need recursion. Be careful: if a call site locks the same mutex from within the lock's critical section, we must keep `std::recursive_mutex` there.

- [ ] **Step 2: For each call site, pick the right mutex type**

Default: `std::mutex`. Use `std::recursive_mutex` only if you verify reentrancy.

- [ ] **Step 3: Replace include + type**

For example in `src/SchemeProcess.cpp`:

```cpp
// before:
#include "EXTMutex.h"
extemp::EXTMutex m_mutex{"scheme-proc"};
extemp::EXTMutex::ScopedLock lock(m_mutex);

// after:
#include <mutex>
std::mutex m_mutex;
std::lock_guard<std::mutex> lock(m_mutex);
```

- [ ] **Step 4: Build after each file**

```bash
cmake --build build --target extempore --parallel 2
```

Commit every 1-3 files — small commits make bisection cheap. Suggested commit cadence:

```bash
git add src/SchemeProcess.cpp include/SchemeProcess.h
git commit -m "refactor(sync): SchemeProcess uses std::mutex directly"
```

### Task 3.3: Replace `EXTMonitor` / `EXTCondition`

Same pattern as 3.2. `EXTMonitor` becomes `std::mutex m; std::condition_variable cv;` as members; `wait` becomes `cv.wait(lock, pred)`.

Commit per-file.

### Task 3.4: Replace `EXTThread`

- [ ] **Step 1: Read `src/EXTThread.cpp` and `include/EXTThread.h`**

Identify its API surface — most likely `start(fn, arg)`, `stop()`, `join()`, `detach()`, priority setting. Note any custom-stack-size or realtime-priority logic — `std::thread` doesn't natively support RT priority, so check if that needs preserving via `pthread_setschedparam` on the native handle.

- [ ] **Step 2: Pick a migration strategy**

If `EXTThread` does have RT-priority logic we need, don't delete the file yet — reduce it to a thin helper around `std::thread::native_handle()`. Name it `ext::rt_thread` and keep the file if it's genuinely pulling weight. Otherwise delete.

- [ ] **Step 3: Replace call sites**

Typical:

```cpp
// before:
auto* t = new EXTThread(&func, arg, "name");
t->start();

// after:
std::jthread t([arg] { func(arg); });
// (assuming func signature suits the lambda)
```

For long-lived threads owned by singletons, prefer `std::jthread` (stops on destruction) over raw `std::thread`.

### Task 3.5: Delete the headers

- [ ] **Step 1: Confirm zero remaining references**

```bash
grep -rn "EXTMutex\|EXTMonitor\|EXTCondition\|EXTThread" src/ include/
```

Expected: nothing except file-deletion candidates themselves.

- [ ] **Step 2: Delete**

```bash
git rm include/EXTMutex.h include/EXTMonitor.h include/EXTCondition.h include/EXTThread.h src/EXTThread.cpp
```

Remove these from any `EXTEMPORE_SOURCES` list in `CMakeLists.txt` or explicit source lists.

- [ ] **Step 3: Commit**

```bash
git add -u
git commit -m "refactor(sync): remove hand-rolled EXT{Mutex,Monitor,Condition,Thread}

All call sites now use the standard library directly. The EXT wrappers
added no value over std::mutex / std::condition_variable / std::jthread,
used std::recursive_mutex unconditionally (masking reentrancy bugs),
and predated std::lock_guard/std::scoped_lock."
```

### Task 3.6: Run .xtm integration tests locally

- [ ] **Step 1: Build and test**

```bash
cmake --build build --target extempore --parallel 2
ctest --test-dir build --label-regex "libs-core|compiler-unit|cpp-unit" -j4 --output-on-failure
```

Expected: green. If failures occur, most likely a subtle lock-scope change during migration.

### Task 3.7: CI gate

- [ ] **Step 1: Push and watch**

```bash
git push
gh run watch --branch cpp-modernisation --exit-status
```

Expected: all 4 platforms green. Windows is highest risk — `std::thread::native_handle()` and recursive mutex semantics differ subtly. Read any Windows failure output carefully.

---

## Phase 4: Networking safety

**Files:**
- Modify: `src/OSC.cpp:1109`, `src/SchemeREPL.cpp:135`, `src/EXTLLVM.cpp:286`, `src/LinenoiseREPL.cpp:21`
- Modify: `src/Extempore.cpp:112-117` (signal handler) and `src/Extempore.cpp:176` (`freopen(stderr)`)
- Modify: `src/SchemeREPL.cpp:208-222` (FD sentinel)

### Task 4.1: Replace `gethostbyname` with `getaddrinfo`

- [ ] **Step 1: Write a helper in `include/UNIV.h` / `src/UNIV.cpp`**

```cpp
// include/UNIV.h, in the UNIV namespace:
// Resolves `host` (IPv4 only, to match existing behaviour) to a
// network-order uint32_t. Returns 0 on failure.
uint32_t resolve_ipv4(const char* host);
```

```cpp
// src/UNIV.cpp:
#include <sys/types.h>
#ifdef _WIN32
  #include <winsock2.h>
  #include <ws2tcpip.h>
#else
  #include <sys/socket.h>
  #include <netdb.h>
  #include <arpa/inet.h>
#endif

uint32_t resolve_ipv4(const char* host) {
    struct addrinfo hints{};
    hints.ai_family = AF_INET;
    hints.ai_socktype = SOCK_STREAM;
    struct addrinfo* res = nullptr;
    if (getaddrinfo(host, nullptr, &hints, &res) != 0 || !res) return 0;
    uint32_t addr = reinterpret_cast<sockaddr_in*>(res->ai_addr)->sin_addr.s_addr;
    freeaddrinfo(res);
    return addr;
}
```

- [ ] **Step 2: Write a unit test**

```cpp
TEST(ResolveIpv4, LocalhostResolves) {
    uint32_t a = extemp::UNIV::resolve_ipv4("127.0.0.1");
    EXPECT_NE(a, 0u);
    // 127.0.0.1 in network byte order
    EXPECT_EQ(a, htonl(0x7f000001));
}
TEST(ResolveIpv4, BogusHostReturnsZero) {
    uint32_t a = extemp::UNIV::resolve_ipv4("this.host.definitely.does.not.resolve.invalid");
    EXPECT_EQ(a, 0u);
}
```

- [ ] **Step 3: Replace each call site**

`src/OSC.cpp:1109`, `src/EXTLLVM.cpp:286`, `src/SchemeREPL.cpp:135`, `src/LinenoiseREPL.cpp:21`:

```cpp
// before:
hen = gethostbyname(host);
if (hen == nullptr) return ...;
memcpy(&addr.sin_addr, hen->h_addr, hen->h_length);

// after:
uint32_t resolved = extemp::UNIV::resolve_ipv4(host);
if (resolved == 0) return ...;
addr.sin_addr.s_addr = resolved;
```

- [ ] **Step 4: Build, test, commit**

```bash
cmake --build build --target extempore extempore_cpp_unit
ctest --test-dir build --label-regex cpp-unit --output-on-failure
git add src/ include/UNIV.h tests/cpp-unit/univ_test.cpp
git commit -m "refactor(net): replace gethostbyname with getaddrinfo helper

gethostbyname is deprecated, not thread-safe, and IPv4-only.
Consolidates four call sites behind extemp::UNIV::resolve_ipv4.
IPv6 support is future work — the helper signature stays IPv4 to
preserve existing callers' expected layout."
```

### Task 4.2: Fix FD sentinel in `SchemeREPL.cpp`

- [ ] **Step 1: Find the default initialiser**

```bash
grep -n "m_serverSocket" src/SchemeREPL.cpp include/SchemeREPL.h
```

If `m_serverSocket` defaults to `0`, change to `-1`. Update every check from `m_serverSocket > 0` or `!= 0` to `!= -1`.

- [ ] **Step 2: Wrap the close path**

In `closeREPL` (lines ~208-222), change `close(m_serverSocket)` to guard against `-1`:

```cpp
if (m_serverSocket != -1) {
    ::shutdown(m_serverSocket, SHUT_RDWR);
    ::close(m_serverSocket);
    m_serverSocket = -1;
}
```

- [ ] **Step 3: Commit**

```bash
git add src/SchemeREPL.cpp include/SchemeREPL.h
git commit -m "fix(repl): use -1 as socket sentinel to avoid closing stdin"
```

### Task 4.3: Signal-handler safety

**File:** `src/Extempore.cpp:112-117`

- [ ] **Step 1: Rewrite `sig_handler` to be async-signal-safe**

```cpp
static void sig_handler(int sig) {
    // Async-signal-safe: no printf, no malloc, no exit()
    const char msg[] = "\nextempore: caught signal, exiting\n";
    ssize_t r = ::write(STDERR_FILENO, msg, sizeof(msg) - 1);
    (void)r;  // ignore return; there's nothing useful to do
    _Exit(128 + sig);
}
```

Include `<unistd.h>` (POSIX) and `<cstdlib>` (for `_Exit`). On Windows, keep whatever the existing `#ifdef _WIN32` guard expects.

- [ ] **Step 2: Commit**

```bash
git add src/Extempore.cpp
git commit -m "fix: make sig_handler async-signal-safe"
```

### Task 4.4: Remove or gate the stderr silencing

**File:** `src/Extempore.cpp:176`

- [ ] **Step 1: Gate behind a command-line flag**

Add a `--quiet` option or simply remove the line. The line silences all LLVM diagnostics for the life of the process, making crash investigation nearly impossible.

Recommendation: remove outright. If a user wants quiet stderr, they can shell-redirect.

- [ ] **Step 2: Commit**

```bash
git add src/Extempore.cpp
git commit -m "fix: stop silencing stderr (was hiding LLVM diagnostics)"
```

### Task 4.5: CI gate

- [ ] **Step 1: Push and watch**

```bash
git push
gh run watch --branch cpp-modernisation --exit-status
```

Expected: all green. Network-stack changes are the most platform-sensitive work in the whole plan; Windows `winsock2.h` header ordering is fragile. Be ready to add a `#define WIN32_LEAN_AND_MEAN` or reorder includes if Windows explodes.

---

## Phase 5: Convert deferred findings to backlog tasks

For the audit items too large or design-sensitive for this refactor, create focused backlog tasks so they aren't forgotten.

### Task 5.1: Create backlog entries

- [ ] **Step 1: Create one backlog task per deferred item**

```bash
backlog task create "RAII conversion: SchemeProcess/REPL/AudioDevice thread ownership" \
  --priority medium -l cpp,refactor \
  -d "Convert remaining owning raw pointers to unique_ptr/value types:
- AudioDevice::m_threads[MAX_RT_AUDIO_THREADS] → std::array<std::jthread, N>
- SchemeProcess/SchemeREPL singletons → Meyers singletons
- SchemeTask::m_ptr → std::variant
- Per-thread thread_local PRNGs (EXTLLVM.cpp:392,397) → values not pointers
See docs/superpowers/plans/2026-04-19-cpp-modernisation.md §5."

backlog task create "Audit LLVM_SCHEME_FF_MAP and FFI table thread safety" \
  --priority high -l cpp,concurrency \
  -d "src/EXTLLVM.cpp:137 (LLVM_SCHEME_FF_MAP) and src/SchemeS7.cpp:25-57
(s_ffiCount/s_ffiTable) are acknowledged in comments as not thread safe.
Audit real access patterns and guard with shared_mutex or make atomic
where appropriate."

backlog task create "Zone allocator mutex on audio thread" \
  --priority high -l cpp,rt-audio \
  -d "src/EXTZones.cpp:69-73 takes a global std::mutex on every allocation
in the audio-callback path. This is an RT-audio anti-pattern (priority
inversion). Investigate lock-free zone design or per-thread zones."

backlog task create "Consolidate src/OSC.cpp (UDP/TCP × Win/POSIX duplication)" \
  --priority medium -l cpp,refactor \
  -d "1700+ lines with UDP/TCP paths duplicated across Windows and POSIX.
Byte-swap helpers reimplement __builtin_bswap*. Large standalone project."

backlog task create "Audio thread dispatcher race conditions" \
  --priority high -l cpp,rt-audio,concurrency \
  -d "src/AudioDevice.cpp:169-170, 391 — multi-thread dispatch uses
spin-sleep on atomics and an unsynchronised m_numThreads read (comment
at 391 says 'this is a race :('). Replace with std::counting_semaphore
or condvar. TSan run required."

backlog task create "Modernise llvm_scheme_env_set dispatch" \
  --priority low -l cpp,refactor \
  -d "src/EXTLLVM.cpp:448-590 is a 140-line strcmp-dispatched type handler.
Replace with enum + table or std::unordered_map<std::string_view,handler>."

backlog task create "Replace select() with poll/epoll in SchemeProcess::serverImpl" \
  --priority low -l cpp,refactor \
  -d "src/SchemeProcess.cpp:469-602 uses select() (1024-fd limit).
Consider poll on POSIX, WSAPoll on Windows, or the existing asio
networking-TS bundle."
```

- [ ] **Step 2: Commit the plan document itself**

```bash
git add docs/superpowers/plans/2026-04-19-cpp-modernisation.md backlog/tasks/
git commit -m "docs: C++ modernisation refactor plan and follow-up tasks"
```

### Task 5.2: Final CI validation

- [ ] **Step 1: One more full CI check on the branch**

```bash
gh run watch --branch cpp-modernisation --exit-status
```

- [ ] **Step 2: Open PR**

```bash
gh pr create --title "C++ modernisation: phases 1-4 (build hygiene, UNIV fixes, sync, networking)" \
  --body "$(cat <<'EOF'
## Summary
Applies phases 1-4 of the 2026-04-19 C++ audit as a single coordinated branch.

- **Phase 1** — build hygiene: -Wall/-Wextra with bug-finding errors; sanitizer build option; .clang-format; .clang-tidy.
- **Phase 2** — UNIV.cpp correctness: sys_slurp_file off-by-one, cname_decode leak/shadow, base64 unsequenced side effects, rreplace/rsplit bounds. Adds a cpp-unit test target with GoogleTest.
- **Phase 3** — deletes EXTMutex/EXTMonitor/EXTCondition/EXTThread in favour of std::mutex / std::condition_variable / std::jthread.
- **Phase 4** — networking safety: getaddrinfo replaces gethostbyname; FD sentinel fix; async-signal-safe sig_handler; removes stderr silencing.

See `docs/superpowers/plans/2026-04-19-cpp-modernisation.md` for the phased plan and `backlog/tasks/` for deferred follow-ups.

## Test plan
- [x] CI green on Linux x64 / Linux arm64 / macOS arm64 / Windows x64
- [x] cpp-unit tests pass under ASan and UBSan locally
- [x] All existing libs-core and libs-external ctest labels remain green
- [x] Manual smoke: `./extempore --batch "(begin (println 'hello) (quit 0))"`
EOF
)"
```

---

## Out of scope for this plan

Deliberately left for the follow-up backlog tasks created in Task 5.1:

- Broad RAII refactor of audio/scheme subsystem thread ownership
- Audio-thread race conditions in `AudioDevice.cpp`
- Zone allocator realtime-safety redesign
- `LLVM_SCHEME_FF_MAP` / s7 FFI table synchronisation
- `OSC.cpp` consolidation
- `llvm_scheme_env_set` dispatch table
- `select()` → `poll`/`epoll`
- Mass clang-format of the codebase (already tracked as TASK-023)
- Full clang-tidy fixup wave (already tracked as TASK-030)
- `typedef` → `using`, `NULL` → `nullptr` cleanup (mechanical, large diff — separate pass)

These are each independently valuable and each warrants its own focused PR.
