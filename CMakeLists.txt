cmake_minimum_required(VERSION 3.28)
project(Extempore VERSION 0.8.9)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

option(ASSETS "download multimedia assets (approx 500MB)" OFF)
option(BUILD_TESTS "build test targets (including examples)" ON)
option(EXTERNAL_SHLIBS_AUDIO "download & build audio-related external library dependencies" ON)
option(EXTERNAL_SHLIBS_GRAPHICS "WebGPU graphics (GLFW, wgpu-native, stb_image)" OFF)
option(EXTERNAL_SHLIBS_GRAPHICS_OPENGL "legacy OpenGL graphics (nanovg, assimp)" OFF)
option(EXT_DYLIB "build extempore as a dynamic library" OFF)
option(JACK "use the Jack Portaudio backend (see BUILDING.md)" OFF)

set_property(GLOBAL PROPERTY USE_FOLDERS ON)

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

if(EXT_DYLIB)
    set(EXTERNAL_SHLIBS_AUDIO OFF)
    set(EXTERNAL_SHLIBS_GRAPHICS OFF)
    set(EXTERNAL_SHLIBS_GRAPHICS_OPENGL OFF)
    message(STATUS "Building Extempore as a library without external dependencies")
endif()

if(APPLE AND NOT DEFINED CMAKE_OSX_DEPLOYMENT_TARGET)
    if(CMAKE_SYSTEM_PROCESSOR MATCHES "arm64" OR CMAKE_HOST_SYSTEM_PROCESSOR MATCHES "arm64")
        set(CMAKE_OSX_DEPLOYMENT_TARGET 11.0)
    endif()
endif()

#############################
# dependency version pinning #
#############################

set(DEP_LLVM_VERSION "22.1.0")
set(DEP_PORTAUDIO_VERSION "v19.7.0")
set(DEP_PORTAUDIO_MD5 "54297c72a852669f9987ee7bb9fba0a6")
set(DEP_PORTMIDI_VERSION "v2.0.8")
set(DEP_PORTMIDI_MD5 "212d4fa92fbb40c8bbaee054af138bf8")
set(DEP_RTMIDI_VERSION "6.0.0")
set(DEP_RTMIDI_MD5 "4fd20d9f3227023d4b369f21d29667ef")
set(DEP_KISS_FFT_VERSION "1.3.0")
set(DEP_SNDFILE_COMMIT "ae64caf9b5946d365971c550875000342e763de6")
set(DEP_NANOVG_COMMIT "3c60175fcc2e5fe305b04355cdce35d499c80310")
set(DEP_STB_COMMIT "152a250a702bf28951bb0220d63bc0c99830c498")
set(DEP_GLFW_VERSION "3.4")
set(DEP_WGPU_VERSION "27.0.4.0")
set(DEP_ASSIMP_VERSION "5.4.3")

########
# PCRE #
########

add_library(pcre STATIC
    src/pcre/config.h
    src/pcre/pcre.h
    src/pcre/ucp.h
    src/pcre/pcre_chartables.c
    src/pcre/pcre_compile.c
    src/pcre/pcre_exec.c
    src/pcre/pcre_globals.c
    src/pcre/pcre_internal.h
    src/pcre/pcre_newline.c
    src/pcre/pcre_tables.c)
target_compile_definitions(pcre PRIVATE -DHAVE_CONFIG_H)

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
    URL_MD5 ${DEP_PORTAUDIO_MD5}
    CMAKE_ARGS
        -DPA_BUILD_STATIC=ON
        -DPA_BUILD_SHARED=OFF
        -DPA_LIBNAME_ADD_SUFFIX=OFF
        -DPA_USE_JACK=${JACK}
        -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
        -DCMAKE_INSTALL_PREFIX=${CMAKE_BINARY_DIR}/portaudio
        -DCMAKE_POLICY_VERSION_MINIMUM=3.5)

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
    SOURCE_SUBDIR llvm
    EXCLUDE_FROM_ALL)

FetchContent_MakeAvailable(LLVM)

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
    src/Scheme.cpp
    src/SchemeFFI.cpp
    src/SchemeProcess.cpp
    src/SchemeREPL.cpp
    src/TaskScheduler.cpp
    src/UNIV.cpp
    src/LinenoiseREPL.cpp)

if(EXT_DYLIB)
    include(${CMAKE_CURRENT_SOURCE_DIR}/CMakeRC.cmake)
    cmrc_add_resource_library(rc_xtm NAMESPACE "xtm"
        runtime/bitcode.ll
        runtime/init.ll
        runtime/init.xtm
        runtime/llvmir.xtm
        runtime/llvmti-globals.xtm
        runtime/llvmti-caches.xtm
        runtime/llvmti-aot.xtm
        runtime/llvmti-transforms.xtm
        runtime/llvmti-ast.xtm
        runtime/llvmti-typecheck.xtm
        runtime/llvmti-bind.xtm
        runtime/scheme.xtm)
    add_library(extempore SHARED ${EXTEMPORE_SOURCES})
    target_link_libraries(extempore PRIVATE rc_xtm)
else()
    add_executable(extempore ${EXTEMPORE_SOURCES})
endif()

add_dependencies(extempore pcre portaudio_static)

target_include_directories(extempore PRIVATE
    include
    src/pcre
    ${CMAKE_BINARY_DIR}/portaudio/include
    ${LLVM_INCLUDE_DIRS})

target_link_directories(extempore PRIVATE ${CMAKE_BINARY_DIR}/portaudio/lib)
target_link_libraries(extempore PRIVATE pcre portaudio${CMAKE_STATIC_LIBRARY_SUFFIX} ${LLVM_LIBRARIES})

# Compile definitions
if(EXT_SHARE_DIR)
    target_compile_definitions(extempore PRIVATE -DEXT_SHARE_DIR="${EXT_SHARE_DIR}")
elseif(EXT_DYLIB)
    target_compile_definitions(extempore PRIVATE -DEXT_DYLIB=1 -DEXT_SHARE_DIR=".")
else()
    target_compile_definitions(extempore PRIVATE -DEXT_SHARE_DIR="${CMAKE_CURRENT_SOURCE_DIR}")
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
        -Woverloaded-virtual
        -Wno-unused-result)
    set_source_files_properties(src/Scheme.cpp PROPERTIES COMPILE_FLAGS -Wno-switch)
    target_link_libraries(extempore PRIVATE pthread linenoise)
endif()

if(WIN32)
    target_include_directories(extempore PRIVATE src/networking-ts-impl/include)
    target_compile_definitions(extempore PRIVATE -DPCRE_STATIC -D_CRT_SECURE_NO_WARNINGS -DNOMINMAX)
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /DEF:${CMAKE_CURRENT_SOURCE_DIR}/src/extempore.def")
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
    set_property(TARGET pcre PROPERTY POSITION_INDEPENDENT_CODE ON)
    set_property(TARGET extempore PROPERTY POSITION_INDEPENDENT_CODE ON)
    target_link_libraries(extempore PRIVATE dl asound)
    target_link_options(extempore PRIVATE -rdynamic)
endif()

##########
# assets #
##########

add_custom_target(assets
    COMMAND ${CMAKE_COMMAND}
        -DASSETS_PATH=${CMAKE_CURRENT_SOURCE_DIR}/assets
        -DASSETS_GIT_REF=0c9f32c
        -P ${CMAKE_CURRENT_SOURCE_DIR}/extras/cmake/download_assets.cmake)

if(ASSETS)
    add_dependencies(extempore assets)
endif()

###########
# install #
###########

install(TARGETS extempore RUNTIME DESTINATION bin)

########################################
# external shared library dependencies #
########################################

include(${CMAKE_CURRENT_SOURCE_DIR}/extras/cmake/external_deps.cmake)

###################
# AOT compilation #
###################

set(AOT_CORE_LIBS
    "libs/base/base.xtm"
    "libs/core/xthread.xtm"
    "libs/core/rational.xtm"
    "libs/core/math.xtm"
    "libs/core/scheduler.xtm"
    "libs/core/audiobuffer.xtm"
    "libs/core/audio_dsp.xtm"
    "libs/core/instruments.xtm")

set(AOT_EXTERNAL_AUDIO_LIBS
    "libs/external/fft.xtm"
    "libs/external/sndfile.xtm"
    "libs/external/audio_dsp_ext.xtm"
    "libs/external/instruments_ext.xtm"
    "libs/external/portmidi.xtm")

set(AOT_EXTERNAL_GRAPHICS_LIBS
    "libs/external/stb_image.xtm"
    "libs/external/glfw3.xtm"
    "libs/external/webgpu.xtm")

if(DEFINED ENV{EXTEMPORE_FORCE_GL_GETPROCADDRESS})
    set(GL_BIND_METHOD getprocaddress)
else()
    set(GL_BIND_METHOD directbind)
endif()

set(AOT_EXTERNAL_GRAPHICS_OPENGL_LIBS
    "libs/contrib/gl/glcore-${GL_BIND_METHOD}.xtm"
    "libs/contrib/gl/gl-objects.xtm"
    "libs/contrib/gl/gl-objects2.xtm"
    "libs/contrib/nanovg.xtm"
    "libs/contrib/gl/glcompat-${GL_BIND_METHOD}.xtm"
    "libs/contrib/graphics-pipeline.xtm"
    "libs/contrib/assimp.xtm")

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
            --batch "(impc:aot:compile-xtm-file \"${libfile}\")"
        DEPENDS ${libfile} ${_dep_files}
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
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

# External graphics libs (legacy OpenGL)
if(EXTERNAL_SHLIBS_GRAPHICS_OPENGL)
    add_custom_target(aot_external_graphics_opengl)
    set_target_properties(aot_external_graphics_opengl PROPERTIES FOLDER AOT)
    add_dependencies(aot_external_graphics_opengl extempore external_shlibs_graphics_opengl)
    if(NOT EXTERNAL_SHLIBS_GRAPHICS)
        aotcompile_lib(libs/external/stb_image.xtm graphics_opengl base)
        aotcompile_lib(libs/external/glfw3.xtm graphics_opengl base)
    endif()
    aotcompile_lib(libs/contrib/gl/glcore-${GL_BIND_METHOD}.xtm graphics_opengl base)
    aotcompile_lib(libs/contrib/gl/gl-objects.xtm graphics_opengl base math glcore-${GL_BIND_METHOD} stb_image)
    aotcompile_lib(libs/contrib/gl/gl-objects2.xtm graphics_opengl base glcore-${GL_BIND_METHOD} stb_image)
    aotcompile_lib(libs/contrib/graphics-pipeline.xtm graphics_opengl base math glcore-${GL_BIND_METHOD} stb_image gl-objects2)
    aotcompile_lib(libs/contrib/nanovg.xtm graphics_opengl base glcore-${GL_BIND_METHOD})
    aotcompile_lib(libs/contrib/assimp.xtm graphics_opengl base stb_image graphics-pipeline)
    aotcompile_lib(libs/contrib/gl/glcompat-${GL_BIND_METHOD}.xtm graphics_opengl base)
endif()

add_custom_target(clean_aot
    COMMAND ${CMAKE_COMMAND} -E remove_directory ${CMAKE_CURRENT_SOURCE_DIR}/libs/aot-cache
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
