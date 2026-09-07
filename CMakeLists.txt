cmake_minimum_required(VERSION 3.28...3.31)
project(Extempore)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

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
option(EXTERNAL_SHLIBS_GRAPHICS "WebGPU graphics (GLFW, wgpu-native, stb_image)" OFF)
option(JACK "use the Jack Portaudio backend (see BUILDING.md)" OFF)

set_property(GLOBAL PROPERTY USE_FOLDERS ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

#########
# setup #
#########

include(${CMAKE_CURRENT_SOURCE_DIR}/extras/cmake/platform.cmake)
extempore_detect_platform()

if(NOT ${CMAKE_SIZEOF_VOID_P} EQUAL 8)
    message(FATAL_ERROR "Extempore currently only runs on 64-bit platforms.")
endif()

if(NOT CMAKE_BUILD_TYPE)
    message(STATUS "Building 'Release' configuration")
    set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build." FORCE)
    set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
endif()

if(APPLE AND NOT DEFINED CMAKE_OSX_DEPLOYMENT_TARGET)
    if(CMAKE_SYSTEM_PROCESSOR MATCHES "arm64" OR CMAKE_HOST_SYSTEM_PROCESSOR MATCHES "arm64")
        set(CMAKE_OSX_DEPLOYMENT_TARGET 11.0)
    endif()
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
    endif()
endif()

#####################################
# external sub-build toolchain args #
#####################################

# Every dependency is built by a nested CMake project (ExternalProject or
# FetchContent), which re-detects its own toolchain from scratch. Forward the
# settings that have to match --- compiler, ccache launcher, macOS deployment
# settings --- but only the ones actually set, since an empty -D would override
# the sub-build's own detection with nothing. Values are joined with `|` and
# every consumer passes LIST_SEPARATOR, so a multi-value setting such as
# CMAKE_OSX_ARCHITECTURES survives the trip.
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
set(DEP_LLVM_VERSION "22.1.6")
set(DEP_LLVM_SHA256 "6e0b376a1f6d9873e7dfb09ae6e04b9c7024400f01733fa4c29be69d5c138bc2")
# PortAudio tags releases rarely (v19.7.0 is from 2021), so pin master by commit
# SHA for its modern CMake (3.10 -- which lets us drop the policy-version floor
# the old 2.8 CMake needed) and five years of platform fixes. Still built static.
set(DEP_PORTAUDIO_VERSION "b0fe9de7ec86ebe5a26086f1d662ab74d7ebfae4")
set(DEP_PORTAUDIO_SHA256 "bb0a1a388b523e7c2e46675bd157b026a1c4676d069c254c89b33d9d2b7ac7d2")
set(DEP_PORTMIDI_VERSION "v2.0.8")
set(DEP_PORTMIDI_SHA256 "e4ee3152ae42362f30b5e59fc5729b8b7e2a98023e1752f641d849543602f31e")
set(DEP_RTMIDI_VERSION "6.0.0")
set(DEP_RTMIDI_SHA256 "55f1e7636160204a8b04dc7aacf959b7c66df0011124e7ddfa6d9c06c8b3363e")
set(DEP_KISS_FFT_VERSION "1.3.0")
set(DEP_KISS_FFT_SHA256 "fd6d82f4aa4a22d2fc3d192a66b919abf43b9c001b78585dc5c3290218981fcd")
set(DEP_SNDFILE_COMMIT "ae64caf9b5946d365971c550875000342e763de6")
set(DEP_SNDFILE_SHA256 "d9335f7c7113b5030ab5e6c1d77a5efb2cfcae6c5ddaae5010dd0cb9b3107f02")
set(DEP_STB_COMMIT "152a250a702bf28951bb0220d63bc0c99830c498")
set(DEP_STB_SHA256 "6772c4240031ba162ed317d8e13fce5a4e18987937e6f28364819c0a262a0d22")
set(DEP_GLFW_VERSION "3.4")
set(DEP_GLFW_SHA256 "b5ec004b2712fd08e8861dc271428f048775200a2df719ccf575143ba749a3e9")
set(DEP_WGPU_VERSION "27.0.4.0")
# wgpu-native ships prebuilt binaries, so the hash is per platform archive.
set(DEP_WGPU_SHA256_LINUX_X86_64 "271481ef76fbf3ea09631a6079e9493636ecf813cd9c92306c44a1a452991ba1")
set(DEP_WGPU_SHA256_LINUX_AARCH64 "a2f22248200997b69373273b10d50a58164f6ed840877289f3e46bff317b134e")
set(DEP_WGPU_SHA256_MACOS_X86_64 "660fe9be59b555ec1d7c839e5cf8b6c71762938af61ab444a7a58dd87970dba2")
set(DEP_WGPU_SHA256_MACOS_AARCH64 "15367c26fdbe6892db35007d39f3883593384e777360b70e6bd704cb5dedde53")
set(DEP_WGPU_SHA256_WINDOWS_X86_64 "f14ca334b4d253881bde2605bd147f332178d705f56fbd74f81458797c77fce1")
set(DEP_GOOGLETEST_VERSION "1.15.2")
set(DEP_GOOGLETEST_SHA256 "7b42b4d6ed48810c5362c265a17faebe90dc2373c885e5216439d37927f02926")
set(DEP_PCRE2_VERSION "10.48")
set(DEP_PCRE2_SHA256 "ebcc25aadf2a51fa1fefa9b8bc9e7a79b3dae86870a0f1152a22e42befd46888")

######
# s7 #
######

add_library(s7_lib STATIC src/s7.c)
target_include_directories(s7_lib PUBLIC include)
target_compile_definitions(s7_lib PRIVATE
    -DHAVE_COMPLEX_NUMBERS=0
    -DWITH_SYSTEM_EXTRAS=0
    -DWITH_C_LOADER=0
    -DWITH_GMP=0)
if(UNIX)
    target_compile_options(s7_lib PRIVATE -Wno-unused-result)
    target_link_libraries(s7_lib PRIVATE dl m)
endif()
set_property(TARGET s7_lib PROPERTY C_STANDARD 17)
if(NOT WIN32)
    set_property(TARGET s7_lib PROPERTY POSITION_INDEPENDENT_CODE ON)
endif()

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

include(ExternalProject)

ExternalProject_Add(portaudio_static
    PREFIX portaudio
    URL https://github.com/PortAudio/portaudio/archive/${DEP_PORTAUDIO_VERSION}.zip
    URL_HASH SHA256=${DEP_PORTAUDIO_SHA256}
    LIST_SEPARATOR |
    CMAKE_ARGS
        -DPA_BUILD_SHARED_LIBS=OFF
        -DPA_USE_JACK=${JACK}
        -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
        -DCMAKE_INSTALL_PREFIX=${CMAKE_BINARY_DIR}/portaudio
        ${EXT_DEPS_TOOLCHAIN_ARGS})

########
# LLVM #
########

if(APPLE)
    if(UNAME_MACHINE_NAME STREQUAL "arm64")
        set(LLVM_TARGET_ARCH "AArch64")
    else()
        set(LLVM_TARGET_ARCH "X86")
    endif()
elseif(UNIX)
    if(CMAKE_SYSTEM_PROCESSOR MATCHES "aarch64|arm64")
        set(LLVM_TARGET_ARCH "AArch64")
    else()
        set(LLVM_TARGET_ARCH "X86")
    endif()
else()
    set(LLVM_TARGET_ARCH "X86")
endif()

message(STATUS "LLVM target architecture: ${LLVM_TARGET_ARCH}")

include(FetchContent)

# Use direct FetchContent population (CMake 3.30+) to avoid sub-build issues on macOS
if(POLICY CMP0168)
    cmake_policy(SET CMP0168 NEW)
endif()

set(LLVM_TARGETS_TO_BUILD ${LLVM_TARGET_ARCH} CACHE STRING "" FORCE)
set(LLVM_ENABLE_TERMINFO OFF CACHE BOOL "" FORCE)
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

FetchContent_Declare(LLVM
    URL https://github.com/llvm/llvm-project/releases/download/llvmorg-${DEP_LLVM_VERSION}/llvm-project-${DEP_LLVM_VERSION}.src.tar.xz
    URL_HASH SHA256=${DEP_LLVM_SHA256}
    SOURCE_SUBDIR llvm
    EXCLUDE_FROM_ALL)

FetchContent_MakeAvailable(LLVM)

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

# The regex FFI builtins (src/ffi/regex.inc) need a PCRE engine: the xtlang
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
add_custom_target(llvm-libs DEPENDS ${LLVM_LIBRARIES})

set(LLVM_INCLUDE_DIRS
    ${llvm_SOURCE_DIR}/llvm/include
    ${llvm_BINARY_DIR}/include)

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
    src/SchemeProcess.cpp
    src/SchemeREPL.cpp
    src/TaskScheduler.cpp
    src/UNIV.cpp
    src/LinenoiseREPL.cpp)

add_executable(extempore ${EXTEMPORE_SOURCES})

add_dependencies(extempore s7_lib portaudio_static)

target_include_directories(extempore PRIVATE
    include
    ${CMAKE_BINARY_DIR}/generated)

target_include_directories(extempore SYSTEM PRIVATE
    src/oscpp/include
    ${CMAKE_BINARY_DIR}/portaudio/include
    ${LLVM_INCLUDE_DIRS})

target_link_directories(extempore PRIVATE ${CMAKE_BINARY_DIR}/portaudio/lib)
target_link_libraries(extempore PRIVATE s7_lib pcre2-8 portaudio${CMAKE_STATIC_LIBRARY_SUFFIX} ${LLVM_LIBRARIES})

# Compile definitions
if(EXT_SHARE_DIR)
    target_compile_definitions(extempore PRIVATE -DEXT_SHARE_DIR="${EXT_SHARE_DIR}")
else()
    target_compile_definitions(extempore PRIVATE -DEXT_SHARE_DIR="${CMAKE_CURRENT_SOURCE_DIR}")
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
        # src/ffi/llvm.inc defines printLLVMModule for use from a debugger.
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
        -D_GNU_SOURCE
        -D__STDC_CONSTANT_MACROS
        -D__STDC_FORMAT_MACROS
        -D__STDC_LIMIT_MACROS)
    target_compile_options(extempore PRIVATE
        -fvisibility-inlines-hidden
        -fno-rtti
        -fno-common
        ${EXTEMPORE_WARNING_FLAGS})

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

    target_link_libraries(extempore PRIVATE pthread linenoise)
endif()

if(WIN32)
    target_include_directories(extempore SYSTEM PRIVATE src/networking-ts-impl/include)
    target_compile_definitions(extempore PRIVATE -D_CRT_SECURE_NO_WARNINGS -DNOMINMAX)
    target_compile_options(extempore PRIVATE /bigobj ${EXTEMPORE_WARNING_FLAGS})
    target_link_options(extempore PRIVATE "/DEF:${CMAKE_CURRENT_SOURCE_DIR}/src/extempore.def")
    set_target_properties(extempore PROPERTIES
        RUNTIME_OUTPUT_DIRECTORY_DEBUG ${CMAKE_CURRENT_SOURCE_DIR}
        RUNTIME_OUTPUT_DIRECTORY_RELEASE ${CMAKE_CURRENT_SOURCE_DIR}
        LIBRARY_OUTPUT_DIRECTORY_DEBUG ${CMAKE_CURRENT_SOURCE_DIR}
        LIBRARY_OUTPUT_DIRECTORY_RELEASE ${CMAKE_CURRENT_SOURCE_DIR}
        ARCHIVE_OUTPUT_DIRECTORY_DEBUG ${CMAKE_CURRENT_SOURCE_DIR}/libs/platform-shlibs
        ARCHIVE_OUTPUT_DIRECTORY_RELEASE ${CMAKE_CURRENT_SOURCE_DIR}/libs/platform-shlibs)
elseif(APPLE)
    set_source_files_properties(src/Extempore.cpp src/SchemeFFI.cpp src/UNIV.cpp
        PROPERTIES COMPILE_FLAGS "-x objective-c++")
    target_link_libraries(extempore PRIVATE
        "-framework Cocoa"
        "-framework CoreAudio"
        "-framework AudioUnit"
        "-framework AudioToolbox")
    add_custom_command(TARGET extempore POST_BUILD
        COMMENT "clear file attributes (avoid 'extempore is damaged' error on Big Sur)"
        COMMAND xattr -cr "$<TARGET_FILE_DIR:extempore>")
else()
    # Linux
    set_property(TARGET extempore PROPERTY POSITION_INDEPENDENT_CODE ON)
    target_link_libraries(extempore PRIVATE dl asound)
    target_link_options(extempore PRIVATE -rdynamic)
endif()

##############
# clang-tidy #
##############

# Requires CMAKE_EXPORT_COMPILE_COMMANDS=ON (Ninja sets this automatically).
# Filters to extempore source files only, not vendored third-party code.
set(TIDY_SOURCES "")
foreach(_src ${EXTEMPORE_SOURCES})
    if(NOT _src MATCHES "(s7\\.c|networking-ts-impl|ffi/|shims/|linenoise)")
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

# `cmake --install build --prefix <dir>` reproduces the layout of the binary
# release archives (see .github/workflows/release-binary.yml): the executable at
# the top of the prefix with runtime/, libs/ (AOT cache and platform shlibs
# included) and examples/ beside it. That is what a binary configured with
# -DEXT_SHARE_DIR=. (the `release` preset) expects to find relative to its
# working directory; a binary built with the default share dir --- which is the
# source tree, so uninstalled builds are unaffected --- can be pointed at an
# installed tree with `extempore --sharedir <dir>`.
install(TARGETS extempore RUNTIME DESTINATION .)
install(DIRECTORY
    ${CMAKE_CURRENT_SOURCE_DIR}/runtime
    ${CMAKE_CURRENT_SOURCE_DIR}/libs
    ${CMAKE_CURRENT_SOURCE_DIR}/examples
    DESTINATION .)
install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/assets DESTINATION . OPTIONAL)
install(FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/README.md
    ${CMAKE_CURRENT_SOURCE_DIR}/BUILDING.md
    ${CMAKE_CURRENT_SOURCE_DIR}/CHANGELOG.md
    DESTINATION .)

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

macro(aotcompile_lib libfile group)
    get_filename_component(_basename ${libfile} NAME_WE)
    set(_targetname aot_${_basename})
    set(_ll_file ${CMAKE_CURRENT_SOURCE_DIR}/libs/aot-cache/xtm${_basename}.ll)
    set(_xtm_file ${CMAKE_CURRENT_SOURCE_DIR}/libs/aot-cache/${_basename}.xtm)

    if(TIMEOUT_CMD)
        set(_extempore_cmd ${TIMEOUT_CMD} --kill-after=10 ${AOT_TIMEOUT_SECONDS} $<TARGET_FILE:extempore>)
    else()
        set(_extempore_cmd $<TARGET_FILE:extempore>)
    endif()

    set(_dep_files "")
    foreach(_dep ${ARGN})
        list(APPEND _dep_files ${CMAKE_CURRENT_SOURCE_DIR}/libs/aot-cache/xtm${_dep}.ll)
        list(APPEND _dep_files ${CMAKE_CURRENT_SOURCE_DIR}/libs/aot-cache/${_dep}.xtm)
    endforeach()

    add_custom_command(OUTPUT ${_ll_file} ${_xtm_file}
        COMMAND ${_extempore_cmd} --nobase
            --batch "(xtc:aot:compile-xtm-file \"${libfile}\")"
        DEPENDS ${libfile} ${_dep_files} $<TARGET_FILE:extempore> ${AOT_RUNTIME_DEPS}
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
endmacro()

# Core libs
add_custom_target(aot_core)
set_target_properties(aot_core PROPERTIES FOLDER AOT)
add_dependencies(aot_core extempore)
aotcompile_lib(libs/base/base.xtm core)
aotcompile_lib(libs/core/math.xtm core base)
aotcompile_lib(libs/core/rational.xtm core base)
aotcompile_lib(libs/core/audiobuffer.xtm core base)
aotcompile_lib(libs/core/audio_dsp.xtm core base rational audiobuffer)
aotcompile_lib(libs/core/instruments.xtm core base audio_dsp)

# External audio libs
if(EXTERNAL_SHLIBS_AUDIO)
    add_custom_target(aot_external_audio ALL)
    set_target_properties(aot_external_audio PROPERTIES FOLDER AOT)
    add_dependencies(aot_external_audio extempore external_shlibs_audio)
    aotcompile_lib(libs/external/fft.xtm audio base math)
    aotcompile_lib(libs/external/sndfile.xtm audio base)
    aotcompile_lib(libs/external/audio_dsp_ext.xtm audio base fft sndfile)
    aotcompile_lib(libs/external/instruments_ext.xtm audio base sndfile instruments)
    aotcompile_lib(libs/external/portmidi.xtm audio base)
endif()

# External graphics libs (WebGPU)
if(EXTERNAL_SHLIBS_GRAPHICS)
    add_custom_target(aot_external_graphics)
    set_target_properties(aot_external_graphics PROPERTIES FOLDER AOT)
    add_dependencies(aot_external_graphics extempore external_shlibs_graphics)
    aotcompile_lib(libs/external/stb_image.xtm graphics base)
    aotcompile_lib(libs/external/glfw3.xtm graphics base)
    aotcompile_lib(libs/external/webgpu.xtm graphics base glfw3)
endif()

add_custom_target(clean_aot
    COMMAND ${CMAKE_COMMAND} -E rm -rf ${CMAKE_CURRENT_SOURCE_DIR}/libs/aot-cache
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
        --eval "(begin (sys:load \"libs/core/audio_dsp.xtm\") (sys:load \"libs/core/instruments.xtm\") (sys:load \"libs/core/math.xtm\") (sys:load \"libs/base/base.xtm\") (sys:load \"libs/external/fft.xtm\") (sys:load \"libs/external/glfw3.xtm\") (sys:load \"libs/external/instruments_ext.xtm\") (sys:load \"libs/external/sndfile.xtm\") (sys:load \"libs/external/stb_image.xtm\") (sys:load \"libs/external/webgpu.xtm\") (xtmdoc-export-caches-to-json \"/tmp/xtmdoc.json\" #f) (quit 0))"
    COMMENT "Generating xtmdoc output in /tmp/xtmdoc.json"
    VERBATIM)
add_dependencies(xtmdoc extempore)
