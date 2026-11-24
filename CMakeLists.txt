cmake_minimum_required(VERSION 4.0...4.5)

# Read when project() picks the SDK, so it has to be set before it.
if(CMAKE_HOST_APPLE)
    set(CMAKE_OSX_DEPLOYMENT_TARGET 11.0 CACHE STRING "Minimum macOS version")
endif()

project(Extempore LANGUAGES C CXX)
if(APPLE)
    enable_language(OBJC OBJCXX)
endif()

set(CMAKE_POSITION_INDEPENDENT_CODE ON)

# Version --- the git tag is the single source of truth. CI passes the tag
# explicitly (-DEXTEMPORE_VERSION=${github.ref_name}) since its checkout is
# shallow; local dev builds derive it from `git describe`; a git-less source
# build falls back to "unknown". Captured at configure time into a generated
# header (reconfigure to refresh).
if(NOT EXTEMPORE_VERSION)
    find_package(Git QUIET)
    if(GIT_FOUND AND EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/.git")
        execute_process(
            COMMAND ${GIT_EXECUTABLE} describe --tags --always --dirty
            WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
            OUTPUT_VARIABLE EXTEMPORE_VERSION
            OUTPUT_STRIP_TRAILING_WHITESPACE
            ERROR_QUIET)
    endif()
endif()
if(NOT EXTEMPORE_VERSION)
    set(EXTEMPORE_VERSION "unknown")
endif()
message(STATUS "Extempore version: ${EXTEMPORE_VERSION}")
configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/extras/cmake/ExtemporeVersion.h.in
    ${CMAKE_BINARY_DIR}/generated/ExtemporeVersion.h
    @ONLY)

option(ASSETS "download multimedia assets (approx 250MB)" OFF)
option(BUILD_TESTS "build test targets (including examples)" ON)
set(EXTEMPORE_SANITIZE "" CACHE STRING "Build with sanitizer: asan, ubsan, tsan, asan+ubsan")
set_property(CACHE EXTEMPORE_SANITIZE PROPERTY STRINGS "" "asan" "ubsan" "tsan" "asan+ubsan")
# The flags below are selected by substring match, so a typo (e.g. "addr")
# would quietly produce an uninstrumented build. Reject it instead.
if(EXTEMPORE_SANITIZE AND NOT EXTEMPORE_SANITIZE MATCHES "^(asan|ubsan|tsan|asan\\+ubsan)$")
    message(FATAL_ERROR
        "EXTEMPORE_SANITIZE must be one of: asan, ubsan, tsan, asan+ubsan "
        "(got '${EXTEMPORE_SANITIZE}')")
endif()
option(EXTERNAL_SHLIBS_AUDIO "download & build audio-related external library dependencies" ON)
option(EXTERNAL_SHLIBS_GRAPHICS "WebGPU graphics (GLFW, wgpu-native)" OFF)
option(JACK "use the Jack Portaudio backend (see BUILDING.md)" OFF)
option(EXTEMPORE_SYSTEM_LLVM "use an installed LLVM of the pinned minor version instead of building it" OFF)

set_property(GLOBAL PROPERTY USE_FOLDERS ON)

# What the build generates for the runtime --- the AOT cache and the shared
# libraries xtlang binds --- goes under libs/ beside the executable, where the
# runtime looks for it (UNIV::BUILD_DIR). The source tree holds sources only.
set(EXTEMPORE_AOT_CACHE_DIR ${CMAKE_BINARY_DIR}/libs/aot-cache)
set(EXT_PLATFORM_SHLIBS_DIR ${CMAKE_BINARY_DIR}/libs/platform-shlibs)
foreach(_stale libs/aot-cache libs/platform-shlibs)
    if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/${_stale})
        message(WARNING
            "${CMAKE_CURRENT_SOURCE_DIR}/${_stale} is stale build output: builds write "
            "it to ${CMAKE_BINARY_DIR}/${_stale}. Delete it, since code that loads "
            "${_stale}/... by a relative path from the source tree would find it first.")
    endif()
endforeach()
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

#########
# setup #
#########

if(NOT CMAKE_SIZEOF_VOID_P EQUAL 8)
    message(FATAL_ERROR "Extempore currently only runs on 64-bit platforms.")
endif()

if(NOT CMAKE_BUILD_TYPE)
    message(STATUS "Building 'Release' configuration")
    set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build." FORCE)
    set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
endif()

##########
# ccache #
##########

# Route C/C++ compilation through ccache when it's installed. Set before the
# LLVM FetchContent below so the sub-build inherits it. This is the big
# repeat-build win: a sanitizer build only instruments Extempore's own objects,
# so LLVM's ~1800 translation units are byte-identical across build dirs, and a
# warm cache turns a from-scratch LLVM rebuild from minutes into seconds. Opt
# out with -DEXTEMPORE_CCACHE=OFF.
option(EXTEMPORE_CCACHE "use ccache as the compiler launcher when available" ON)
if(EXTEMPORE_CCACHE AND NOT CMAKE_CXX_COMPILER_LAUNCHER)
    find_program(CCACHE_PROGRAM ccache)
    if(CCACHE_PROGRAM)
        message(STATUS "ccache enabled: ${CCACHE_PROGRAM}")
        set(CMAKE_C_COMPILER_LAUNCHER ${CCACHE_PROGRAM})
        set(CMAKE_CXX_COMPILER_LAUNCHER ${CCACHE_PROGRAM})
        set(CMAKE_OBJC_COMPILER_LAUNCHER ${CCACHE_PROGRAM})
        set(CMAKE_OBJCXX_COMPILER_LAUNCHER ${CCACHE_PROGRAM})
    endif()
endif()

#####################################
# external sub-build toolchain args #
#####################################

# The shared libraries xtlang loads (extras/cmake/external_deps.cmake) are
# built by ExternalProject, a nested CMake project which re-detects its own
# toolchain from scratch. Forward the settings that have to match --- compiler,
# ccache launcher, macOS deployment settings --- but only the ones actually set,
# since an empty -D would override the sub-build's own detection with nothing.
# Values are joined with `|` and every consumer passes LIST_SEPARATOR, so a
# multi-value setting such as CMAKE_OSX_ARCHITECTURES survives the trip.
set(EXT_DEPS_TOOLCHAIN_ARGS "")
foreach(_var
        CMAKE_C_COMPILER
        CMAKE_CXX_COMPILER
        CMAKE_C_COMPILER_LAUNCHER
        CMAKE_CXX_COMPILER_LAUNCHER
        CMAKE_OSX_DEPLOYMENT_TARGET
        CMAKE_OSX_ARCHITECTURES)
    if(${_var})
        string(REPLACE ";" "|" _value "${${_var}}")
        list(APPEND EXT_DEPS_TOOLCHAIN_ARGS "-D${_var}=${_value}")
    endif()
endforeach()

#############################
# dependency version pinning #
#############################

# Every archive is pinned by SHA256 so a re-tagged or tampered-with download is
# rejected rather than built. Hashes come from the upstream release assets /
# GitHub source archives at the pinned version.
set(DEP_LLVM_VERSION "22.1.8")
set(DEP_LLVM_SHA256 "922f1817a0df7b1489272d18134ee0087a8b068828f87ac63b9861b1a9965888")
# PortAudio tags releases rarely (v19.7.0 is from 2021), so pin master by commit
# SHA for its modern CMake and five years of platform fixes.
set(DEP_PORTAUDIO_VERSION "b0fe9de7ec86ebe5a26086f1d662ab74d7ebfae4")
set(DEP_PORTAUDIO_SHA256 "bb0a1a388b523e7c2e46675bd157b026a1c4676d069c254c89b33d9d2b7ac7d2")
set(DEP_PORTMIDI_VERSION "v2.0.8")
set(DEP_PORTMIDI_SHA256 "e4ee3152ae42362f30b5e59fc5729b8b7e2a98023e1752f641d849543602f31e")
set(DEP_GLFW_VERSION "3.5.1")
set(DEP_GLFW_SHA256 "ea79bc5feffc254c87291980c2d0bce9acebb68c4983b79f961dcd2cb8a611a0")
set(DEP_WGPU_VERSION "27.0.4.0")
# wgpu-native ships prebuilt binaries, so the hash is per platform archive.
set(DEP_WGPU_SHA256_LINUX_X86_64 "271481ef76fbf3ea09631a6079e9493636ecf813cd9c92306c44a1a452991ba1")
set(DEP_WGPU_SHA256_LINUX_AARCH64 "a2f22248200997b69373273b10d50a58164f6ed840877289f3e46bff317b134e")
set(DEP_WGPU_SHA256_MACOS_X86_64 "660fe9be59b555ec1d7c839e5cf8b6c71762938af61ab444a7a58dd87970dba2")
set(DEP_WGPU_SHA256_MACOS_AARCH64 "15367c26fdbe6892db35007d39f3883593384e777360b70e6bd704cb5dedde53")
set(DEP_WGPU_SHA256_WINDOWS_X86_64 "f14ca334b4d253881bde2605bd147f332178d705f56fbd74f81458797c77fce1")
set(DEP_GOOGLETEST_VERSION "1.18.0")
set(DEP_GOOGLETEST_SHA256 "6e3191c1455468b3fc35a417fb565c1c5071aee1b7e7f85e30cf48a98d37d8b5")
set(DEP_PCRE2_VERSION "10.49")
set(DEP_PCRE2_SHA256 "929f0b20e62879252a15886b06c89f1edef61a363cbd5826fb041080a5e557ae")

######
# s7 #
######

add_library(s7_lib STATIC src/s7.c)
target_include_directories(s7_lib PUBLIC include)
target_compile_definitions(s7_lib PRIVATE
    HAVE_COMPLEX_NUMBERS=0
    WITH_SYSTEM_EXTRAS=0
    WITH_C_LOADER=0
    WITH_GMP=0)
if(UNIX)
    target_compile_options(s7_lib PRIVATE -Wno-unused-result)
    target_link_libraries(s7_lib PRIVATE ${CMAKE_DL_LIBS} m)
endif()
set_property(TARGET s7_lib PROPERTY C_STANDARD 17)

############
# linenoise #
############

if(UNIX)
    add_library(linenoise STATIC src/linenoise/linenoise.c)
    target_include_directories(linenoise PRIVATE src/linenoise)
endif()

#############
# portaudio #
#############

include(FetchContent)

# Static, so nothing has to ship beside the binary. PortAudio turns on every
# host API whose development files it finds; name the ones a build uses, so a
# stray libpulse-dev can't add a runtime dependency to a release binary.
FetchContent_Declare(portaudio
    URL https://github.com/PortAudio/portaudio/archive/${DEP_PORTAUDIO_VERSION}.zip
    URL_HASH SHA256=${DEP_PORTAUDIO_SHA256}
    SYSTEM
    EXCLUDE_FROM_ALL)
set(PA_BUILD_SHARED_LIBS OFF CACHE BOOL "" FORCE)
set(PA_USE_JACK ${JACK} CACHE BOOL "" FORCE)
set(PA_USE_PULSEAUDIO OFF CACHE BOOL "" FORCE)
set(PA_USE_SNDIO OFF CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(portaudio)

########
# LLVM #
########

# CMAKE_OSX_ARCHITECTURES names the target when it's set (cross-compiling, or
# an arm64 build under Rosetta); otherwise the processor CMake detected does.
if(CMAKE_OSX_ARCHITECTURES)
    set(_target_processor ${CMAKE_OSX_ARCHITECTURES})
else()
    set(_target_processor ${CMAKE_SYSTEM_PROCESSOR})
endif()
if(_target_processor MATCHES "^(aarch64|arm64|ARM64)$")
    set(LLVM_TARGET_ARCH "AArch64")
elseif(_target_processor MATCHES "^(x86_64|AMD64|amd64)$")
    set(LLVM_TARGET_ARCH "X86")
else()
    message(FATAL_ERROR "Unsupported target processor: ${_target_processor}")
endif()

message(STATUS "LLVM target architecture: ${LLVM_TARGET_ARCH}")

if(EXTEMPORE_SYSTEM_LLVM)
    # LLVM's package config accepts any patch release of the same minor.
    string(REGEX MATCH "^[0-9]+\\.[0-9]+" _llvm_minor ${DEP_LLVM_VERSION})
    find_package(LLVM ${_llvm_minor} CONFIG REQUIRED)
    message(STATUS "Using LLVM ${LLVM_PACKAGE_VERSION} from ${LLVM_DIR}")
else()
    set(LLVM_TARGETS_TO_BUILD ${LLVM_TARGET_ARCH} CACHE STRING "" FORCE)
    set(LLVM_ENABLE_ZLIB OFF CACHE BOOL "" FORCE)
    set(LLVM_ENABLE_ZSTD OFF CACHE BOOL "" FORCE)
    set(LLVM_ENABLE_LIBXML2 OFF CACHE BOOL "" FORCE)
    set(LLVM_INCLUDE_TOOLS OFF CACHE BOOL "" FORCE)
    set(LLVM_BUILD_TOOLS OFF CACHE BOOL "" FORCE)
    set(LLVM_INCLUDE_UTILS OFF CACHE BOOL "" FORCE)
    set(LLVM_BUILD_RUNTIME OFF CACHE BOOL "" FORCE)
    set(LLVM_INCLUDE_EXAMPLES OFF CACHE BOOL "" FORCE)
    set(LLVM_INCLUDE_TESTS OFF CACHE BOOL "" FORCE)
    set(LLVM_INCLUDE_BENCHMARKS OFF CACHE BOOL "" FORCE)
    set(LLVM_INCLUDE_DOCS OFF CACHE BOOL "" FORCE)
    set(LLVM_ENABLE_BINDINGS OFF CACHE BOOL "" FORCE)
    set(LLVM_ENABLE_OCAMLDOC OFF CACHE BOOL "" FORCE)
    # A Debug build would otherwise run a Debug tablegen, which is very slow.
    set(LLVM_OPTIMIZED_TABLEGEN ON CACHE BOOL "" FORCE)

    # DOWNLOAD_EXTRACT_TIMESTAMP keeps the archive's mtimes on the extracted
    # sources. CI restores a cached LLVM build into a fresh checkout, where
    # configure extracts the sources again; stamped with the extraction time they
    # would be newer than every cached object and Ninja would rebuild all of LLVM.
    FetchContent_Declare(LLVM
        URL https://github.com/llvm/llvm-project/releases/download/llvmorg-${DEP_LLVM_VERSION}/llvm-project-${DEP_LLVM_VERSION}.src.tar.xz
        URL_HASH SHA256=${DEP_LLVM_SHA256}
        SOURCE_SUBDIR llvm
        DOWNLOAD_EXTRACT_TIMESTAMP TRUE
        EXCLUDE_FROM_ALL)

    # LLVM's cmake_minimum_required predates CMP0219 (CMake 4.4), which warns
    # about its backslash-carrying macro calls. Keep the old behaviour it was
    # written for, without the warning.
    set(CMAKE_POLICY_DEFAULT_CMP0219 OLD)
    FetchContent_MakeAvailable(LLVM)
    unset(CMAKE_POLICY_DEFAULT_CMP0219)

    set(LLVM_INCLUDE_DIRS
        ${llvm_SOURCE_DIR}/llvm/include
        ${llvm_BINARY_DIR}/include)
endif()

# GoogleTest for C++ unit tests.
if(BUILD_TESTS)
    FetchContent_Declare(
        googletest
        URL https://github.com/google/googletest/archive/refs/tags/v${DEP_GOOGLETEST_VERSION}.tar.gz
        URL_HASH SHA256=${DEP_GOOGLETEST_SHA256})
    set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
    set(INSTALL_GTEST OFF CACHE BOOL "" FORCE)
    FetchContent_MakeAvailable(googletest)
endif()

#########
# PCRE2 #
#########

# The regex FFI builtins (src/ffi/regex.cpp) need a PCRE engine: the xtlang
# compiler's type-string grammars use recursive subpatterns and named groups,
# which std::regex has no answer for.
#
# Built as a static library and linked in, so nothing has to be shipped
# alongside the binary. pcre2-8-static carries PCRE2_STATIC as a PUBLIC compile
# definition, which is what keeps MSVC from decorating the API with
# __declspec(dllimport).
FetchContent_Declare(pcre2
    URL https://github.com/PCRE2Project/pcre2/releases/download/pcre2-${DEP_PCRE2_VERSION}/pcre2-${DEP_PCRE2_VERSION}.tar.gz
    URL_HASH SHA256=${DEP_PCRE2_SHA256}
    SYSTEM
    EXCLUDE_FROM_ALL)

set(PCRE2_BUILD_PCRE2GREP OFF CACHE BOOL "" FORCE)
set(PCRE2_BUILD_TESTS OFF CACHE BOOL "" FORCE)
set(PCRE2_SHOW_REPORT OFF CACHE BOOL "" FORCE)
set(PCRE2_STATIC_PIC ON CACHE BOOL "" FORCE)

# BUILD_SHARED_LIBS picks which of the two builds `pcre2-8` aliases, so pin it
# for pcre2 alone rather than for the whole project.
block()
    set(BUILD_SHARED_LIBS OFF)
    set(BUILD_STATIC_LIBS ON)
    FetchContent_MakeAvailable(pcre2)
endblock()

set(EXTEMPORE_LLVM_COMPONENTS
    OrcJIT
    ${LLVM_TARGET_ARCH}
    AsmParser
    Passes
    MCDisassembler
    IRPrinter
    Linker)

llvm_map_components_to_libnames(LLVM_LIBRARIES ${EXTEMPORE_LLVM_COMPONENTS})
message(STATUS "LLVM libraries: ${LLVM_LIBRARIES}")

# Target for building just LLVM (useful for CI caching)
if(NOT EXTEMPORE_SYSTEM_LLVM)
    add_custom_target(llvm-libs DEPENDS ${LLVM_LIBRARIES})
endif()

#############
# extempore #
#############

set(EXTEMPORE_SOURCES
    src/Extempore.cpp
    src/AudioDevice.cpp
    src/EXTZones.cpp
    src/EXTClosureAddressTable.cpp
    src/EXTLLVM.cpp
    src/EXTThread.cpp
    src/shims/__hash_memory.cpp
    src/OSC.cpp
    src/SchemeS7.cpp
    src/SchemeFFI.cpp
    src/ffi/clock.cpp
    src/ffi/ipc.cpp
    src/ffi/llvm.cpp
    src/ffi/misc.cpp
    src/ffi/number.cpp
    src/ffi/regex.cpp
    src/ffi/sys.cpp
    src/ffi/sys_dsp.cpp
    src/ffi/sys_zone.cpp
    src/ffi/utility.cpp
    src/SchemeProcess.cpp
    src/SchemeREPL.cpp
    src/TaskScheduler.cpp
    src/UNIV.cpp
    src/LinenoiseREPL.cpp)

add_executable(extempore ${EXTEMPORE_SOURCES})

# At the top of the build tree, beside libs/, for every generator: the generator
# expression stops multi-config generators adding a per-config subdirectory.
set_target_properties(extempore PROPERTIES RUNTIME_OUTPUT_DIRECTORY $<1:${CMAKE_BINARY_DIR}>)

# Set per target, not with CMAKE_CXX_STANDARD, so LLVM builds at the standard it
# chooses for itself.
set_target_properties(extempore PROPERTIES
    CXX_STANDARD 23
    CXX_STANDARD_REQUIRED ON)


target_include_directories(extempore PRIVATE
    include
    ${CMAKE_BINARY_DIR}/generated)

target_include_directories(extempore SYSTEM PRIVATE
    src/oscpp/include
    ${LLVM_INCLUDE_DIRS})

target_link_libraries(extempore PRIVATE s7_lib pcre2-8 portaudio ${LLVM_LIBRARIES})

# Compile definitions
if(EXT_SHARE_DIR)
    target_compile_definitions(extempore PRIVATE EXT_SHARE_DIR="${EXT_SHARE_DIR}")
else()
    target_compile_definitions(extempore PRIVATE EXT_SHARE_DIR="${CMAKE_CURRENT_SOURCE_DIR}")
endif()

# Warning flags, shared with the C++ unit tests (tests/cpp-unit).
if(MSVC)
    set(EXTEMPORE_WARNING_FLAGS /W3 /wd4244 /wd4267 /wd4018 /wd4996 /we4715)
else()
    set(EXTEMPORE_WARNING_FLAGS
        -Wall
        -Wextra
        -Woverloaded-virtual
        -Wshadow
        -Wno-unused-result
        -Wno-unused-parameter
        -Wno-unused-function
        -Wno-missing-field-initializers
        -Wno-sign-compare
        -Wno-deprecated-declarations
        -Werror=return-type
        -Werror=uninitialized)
endif()

# Platform-specific configuration
if(UNIX)
    target_compile_definitions(extempore PRIVATE
        _GNU_SOURCE
        __STDC_CONSTANT_MACROS
        __STDC_FORMAT_MACROS
        __STDC_LIMIT_MACROS)
    target_compile_options(extempore PRIVATE
        -fvisibility-inlines-hidden
        -fno-rtti
        -fno-common
        ${EXTEMPORE_WARNING_FLAGS})

    if(EXTEMPORE_SANITIZE)
        set(SAN_FLAGS "")
        if(EXTEMPORE_SANITIZE MATCHES "asan")
            list(APPEND SAN_FLAGS -fsanitize=address -fno-omit-frame-pointer)
            # also instrument the JIT-compiled xtlang (src/ffi/llvm.cpp) and
            # poison zone memory outside live allocations (src/EXTZones.cpp)
            target_compile_definitions(extempore PRIVATE EXTEMPORE_ASAN)
        endif()
        if(EXTEMPORE_SANITIZE MATCHES "ubsan")
            list(APPEND SAN_FLAGS -fsanitize=undefined -fno-sanitize-recover=undefined)
        endif()
        if(EXTEMPORE_SANITIZE MATCHES "tsan")
            list(APPEND SAN_FLAGS -fsanitize=thread)
        endif()
        target_compile_options(extempore PRIVATE ${SAN_FLAGS})
        target_link_options(extempore PRIVATE ${SAN_FLAGS})
        # GCC's -Wmaybe-uninitialized (promoted to an error by -Werror=uninitialized
        # above) fires false positives inside libstdc++'s own <regex> headers once
        # the sanitizers perturb inlining at -O3 --- see the std::regex use in
        # AudioDevice.cpp. Demote it to a plain warning for sanitizer builds so a
        # spurious diagnostic can't fail the build and mask the actual sanitizer run.
        # The un-instrumented Release build doesn't trip it, so it stays strict.
        if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
            target_compile_options(extempore PRIVATE -Wno-error=maybe-uninitialized)
        endif()
        message(STATUS "Extempore sanitizers enabled: ${EXTEMPORE_SANITIZE}")
    endif()

    find_package(Threads REQUIRED)
    target_link_libraries(extempore PRIVATE Threads::Threads linenoise)
endif()

if(WIN32)
    target_compile_definitions(extempore PRIVATE _CRT_SECURE_NO_WARNINGS NOMINMAX)
    target_compile_options(extempore PRIVATE /bigobj ${EXTEMPORE_WARNING_FLAGS})
    target_link_options(extempore PRIVATE "/DEF:${CMAKE_CURRENT_SOURCE_DIR}/src/extempore.def")
    # The import library, which AOT-compiled code links against
    # (runtime/xtc-aot.xtm).
    set_target_properties(extempore PROPERTIES
        ARCHIVE_OUTPUT_DIRECTORY $<1:${EXT_PLATFORM_SHLIBS_DIR}>)
elseif(APPLE)
    set_source_files_properties(src/Extempore.cpp src/SchemeFFI.cpp src/UNIV.cpp
        PROPERTIES LANGUAGE OBJCXX)
    target_link_libraries(extempore PRIVATE
        "-framework Cocoa"
        "-framework CoreAudio"
        "-framework AudioUnit"
        "-framework AudioToolbox")
else()
    # Linux. ENABLE_EXPORTS puts the executable's symbols in the dynamic symbol
    # table, where the JIT resolves the runtime's C functions.
    find_package(ALSA REQUIRED)
    target_link_libraries(extempore PRIVATE ${CMAKE_DL_LIBS} ALSA::ALSA)
    set_property(TARGET extempore PROPERTY ENABLE_EXPORTS ON)
endif()

##############
# clang-tidy #
##############

# Requires CMAKE_EXPORT_COMPILE_COMMANDS=ON (Ninja sets this automatically).
# Filters to extempore source files only, not vendored third-party code.
set(TIDY_SOURCES "")
foreach(_src ${EXTEMPORE_SOURCES})
    if(NOT _src MATCHES "(s7\\.c|shims/|linenoise)")
        list(APPEND TIDY_SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/${_src}")
    endif()
endforeach()

add_custom_target(tidy
    COMMAND clang-tidy -p ${CMAKE_BINARY_DIR} ${TIDY_SOURCES}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    COMMENT "Running clang-tidy on Extempore sources"
    USES_TERMINAL
    VERBATIM)

##########
# assets #
##########

add_custom_target(assets
    COMMAND ${CMAKE_COMMAND}
        -DASSETS_PATH=${CMAKE_CURRENT_SOURCE_DIR}/assets
        -DASSETS_VERSION=v0.9.0-beta
        -P ${CMAKE_CURRENT_SOURCE_DIR}/extras/cmake/download_assets.cmake)

if(ASSETS)
    add_dependencies(extempore assets)
endif()

###########
# install #
###########

# `cmake --install build --prefix <dir>` lays out a binary release: the
# executable at the top of the prefix with runtime/, libs/ and examples/ beside
# it, the build's AOT cache and platform shlibs merged into libs/. There the
# share dir and the build dir are one directory, which the binary finds beside
# itself (UNIV::resolve_share_dir), so an installed tree is relocatable and runs
# from any working directory. `cpack --config build/CPackConfig.cmake` zips it,
# which is how the release workflow builds its archives.
install(TARGETS extempore RUNTIME DESTINATION .)
install(DIRECTORY
    ${CMAKE_CURRENT_SOURCE_DIR}/runtime
    ${CMAKE_CURRENT_SOURCE_DIR}/examples
    DESTINATION .)
install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/libs DESTINATION .
    PATTERN aot-cache EXCLUDE
    PATTERN platform-shlibs EXCLUDE)
install(DIRECTORY ${EXTEMPORE_AOT_CACHE_DIR} ${EXT_PLATFORM_SHLIBS_DIR}
    DESTINATION libs
    OPTIONAL
    PATTERN "*.exp" EXCLUDE)  # MSVC export files, a by-product of linking
install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/assets DESTINATION . OPTIONAL)
install(FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/README.md
    ${CMAKE_CURRENT_SOURCE_DIR}/BUILDING.md
    ${CMAKE_CURRENT_SOURCE_DIR}/CHANGELOG.md
    DESTINATION .)

set(CPACK_GENERATOR ZIP)
set(CPACK_SOURCE_GENERATOR "")
set(CPACK_PACKAGE_NAME extempore)
set(CPACK_PACKAGE_VERSION ${EXTEMPORE_VERSION})
set(CPACK_PACKAGE_FILE_NAME extempore-${EXTEMPORE_VERSION})
include(CPack)

########################################
# external shared library dependencies #
########################################

include(${CMAKE_CURRENT_SOURCE_DIR}/extras/cmake/external_deps.cmake)

###################
# AOT compilation #
###################

# AOT compilation with proper per-library dependencies
if(APPLE)
    find_program(TIMEOUT_CMD gtimeout)
    if(NOT TIMEOUT_CMD)
        find_program(TIMEOUT_CMD timeout)
    endif()
elseif(NOT WIN32)
    find_program(TIMEOUT_CMD timeout)
endif()
set(AOT_TIMEOUT_SECONDS 300)

# Each AOT step spawns an extempore process with LLVM's JIT behind it, which is
# far more memory-hungry than a C++ compile. Cap the concurrent ones with a
# Ninja job pool rather than throttling the whole build: the C++ compiles still
# run at full parallelism. (Ignored by other generators, which don't have pools.)
set_property(GLOBAL PROPERTY JOB_POOLS aot=2)

# The xtlang compiler is live-loaded from runtime/ at startup, so an AOT output
# is stale whenever the compiler sources or the binary that runs them change.
# Both have to be real inputs of the custom command (not just order-only target
# dependencies) or Ninja will happily keep a cache built by an older compiler.
file(GLOB AOT_RUNTIME_DEPS CONFIGURE_DEPENDS
    ${CMAKE_CURRENT_SOURCE_DIR}/runtime/*.xtm
    ${CMAKE_CURRENT_SOURCE_DIR}/runtime/*.ll)

# aotcompile_lib(libfile group deps...) names the cache after the library's
# file, and AOT_EXTRA_SOURCES_<name> lists any other files it loads into the
# same cache. aotcompile_lib_as(name libfile group deps...) gives it another name,
# for a library whose file name is too common for the cache's one flat
# directory (libs/studio/dsp.xtm is cached as studio_dsp). The deps are cache
# names.
function(aotcompile_lib libfile group)
    get_filename_component(_aot_name ${libfile} NAME_WE)
    aotcompile_lib_as(${_aot_name} ${libfile} ${group} ${ARGN})
endfunction()

function(aotcompile_lib_as _basename libfile group)
    set(_targetname aot_${_basename})
    set(_ll_file ${EXTEMPORE_AOT_CACHE_DIR}/xtm${_basename}.ll)
    set(_xtm_file ${EXTEMPORE_AOT_CACHE_DIR}/${_basename}.xtm)

    if(TIMEOUT_CMD)
        set(_extempore_cmd ${TIMEOUT_CMD} --kill-after=10 ${AOT_TIMEOUT_SECONDS} $<TARGET_FILE:extempore>)
    else()
        set(_extempore_cmd $<TARGET_FILE:extempore>)
    endif()

    set(_dep_files "")
    foreach(_dep ${ARGN})
        list(APPEND _dep_files ${EXTEMPORE_AOT_CACHE_DIR}/xtm${_dep}.ll)
        list(APPEND _dep_files ${EXTEMPORE_AOT_CACHE_DIR}/${_dep}.xtm)
    endforeach()

    add_custom_command(OUTPUT ${_ll_file} ${_xtm_file}
        COMMAND ${_extempore_cmd} --nobase
            --batch "(xtc:aot:compile-xtm-file \"${libfile}\" #f \"${_basename}\")"
        DEPENDS ${libfile} ${AOT_EXTRA_SOURCES_${_basename}} ${_dep_files}
            $<TARGET_FILE:extempore> ${AOT_RUNTIME_DEPS}
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        JOB_POOL aot
        VERBATIM)

    add_custom_target(${_targetname} DEPENDS ${_ll_file} ${_xtm_file} extempore)
    set_target_properties(${_targetname} PROPERTIES FOLDER AOT)

    if(NOT ${group} STREQUAL "core")
        add_dependencies(${_targetname} external_shlibs_${group})
        add_dependencies(aot_external_${group} ${_targetname})
    else()
        add_dependencies(aot_core ${_targetname})
    endif()

    foreach(_dep ${ARGN})
        add_dependencies(${_targetname} aot_${_dep})
    endforeach()
endfunction()

# Core libs. ALL builds every one, including those (png) no other AOT
# target depends on.
add_custom_target(aot_core ALL)
set_target_properties(aot_core PROPERTIES FOLDER AOT)
add_dependencies(aot_core extempore)
aotcompile_lib(libs/base/base.xtm core)
aotcompile_lib(libs/core/math.xtm core base)
aotcompile_lib(libs/core/rational.xtm core base)
aotcompile_lib(libs/core/audiofile.xtm core base)
aotcompile_lib(libs/core/audiobuffer.xtm core base audiofile)
aotcompile_lib(libs/core/fft.xtm core base math)
aotcompile_lib(libs/core/png.xtm core base)
file(GLOB AOT_EXTRA_SOURCES_midi CONFIGURE_DEPENDS
     ${CMAKE_CURRENT_SOURCE_DIR}/libs/core/midi/*.xtm)
aotcompile_lib(libs/core/midi.xtm core base)
aotcompile_lib(libs/core/audio_dsp.xtm core base rational audiobuffer)
aotcompile_lib(libs/core/instruments.xtm core base audio_dsp)
aotcompile_lib(libs/core/audio_dsp_ext.xtm core base fft audiofile)
aotcompile_lib(libs/core/instruments_ext.xtm core base audiofile instruments)

# The studio's xtlang libraries. libs/studio/aot.xtm has how a cache is checked
# against its sources when it is loaded.
add_custom_target(aot_studio ALL)
set_target_properties(aot_studio PROPERTIES FOLDER AOT)
aotcompile_lib_as(studio_engine libs/studio/engine.xtm core base audio_dsp)
aotcompile_lib_as(studio_dsp libs/studio/dsp.xtm core base audio_dsp)
aotcompile_lib_as(studio_fx libs/studio/fx.xtm core base audio_dsp studio_dsp)
aotcompile_lib_as(studio_hall libs/studio/hall.xtm core base audio_dsp studio_dsp)
aotcompile_lib_as(studio_mixer libs/studio/mixer.xtm core base audio_dsp studio_engine studio_fx studio_hall)
add_dependencies(aot_studio aot_studio_engine aot_studio_dsp aot_studio_fx aot_studio_hall aot_studio_mixer)

# External audio libs
if(EXTERNAL_SHLIBS_AUDIO)
    add_custom_target(aot_external_audio ALL)
    set_target_properties(aot_external_audio PROPERTIES FOLDER AOT)
    add_dependencies(aot_external_audio extempore external_shlibs_audio)
    aotcompile_lib(libs/external/portmidi.xtm audio base midi)
endif()

# External graphics libs (WebGPU)
if(EXTERNAL_SHLIBS_GRAPHICS)
    add_custom_target(aot_external_graphics)
    set_target_properties(aot_external_graphics PROPERTIES FOLDER AOT)
    add_dependencies(aot_external_graphics extempore external_shlibs_graphics)
    aotcompile_lib(libs/external/glfw3.xtm graphics base)
    aotcompile_lib(libs/external/webgpu.xtm graphics base glfw3)
endif()

add_custom_target(clean_aot
    COMMAND ${CMAKE_COMMAND} -E rm -rf ${EXTEMPORE_AOT_CACHE_DIR}
    COMMENT "Removing AOT-compiled libs")

#########
# tests #
#########

include(${CMAKE_CURRENT_SOURCE_DIR}/extras/cmake/tests.cmake)

##########
# xtmdoc #
##########

add_custom_target(xtmdoc
    COMMAND extempore --port 17095
        --eval "(begin (sys:load \"libs/core/audio_dsp.xtm\") (sys:load \"libs/core/instruments.xtm\") (sys:load \"libs/core/math.xtm\") (sys:load \"libs/base/base.xtm\") (sys:load \"libs/core/fft.xtm\") (sys:load \"libs/core/audiofile.xtm\") (sys:load \"libs/core/png.xtm\") (sys:load \"libs/external/glfw3.xtm\") (sys:load \"libs/core/instruments_ext.xtm\") (sys:load \"libs/external/webgpu.xtm\") (xtmdoc-export-caches-to-json \"/tmp/xtmdoc.json\" #f) (quit 0))"
    COMMENT "Generating xtmdoc output in /tmp/xtmdoc.json"
    VERBATIM)
add_dependencies(xtmdoc extempore)
