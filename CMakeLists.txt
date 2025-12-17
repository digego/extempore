cmake_minimum_required(VERSION 3.19) # we use target_sources()
project(Extempore VERSION 0.8.9)

if(POLICY CMP0135)
  cmake_policy(SET CMP0135 NEW)
endif()

option(ASSETS "download multimedia assets (approx 500MB)" OFF)
option(BUILD_TESTS "build test targets (including examples)" ON)
option(PACKAGE "set up install targets for packaging" OFF)

option(EXTERNAL_SHLIBS_AUDIO "download & build audio-related external library dependencies" ON)
option(EXTERNAL_SHLIBS_GRAPHICS "download & build graphics-related external library dependencies" ON)
set(EXTERNAL_SHLIBS (EXTERNAL_SHLIBS_AUDIO OR EXTERNAL_SHLIBS_GRAPHICS))

option(EXT_DYLIB "build extempore as a dynamic library" OFF)

## see note about the status of the Jack backend in BUILDING.md
option(JACK "use the Jack Portaudio backend" OFF)

## this is useful because we can group targets together (e.g. all the AOT libs)
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

####################
# option wrangling #
####################

if(EXT_DYLIB)
  set(EXTERNAL_SHLIBS_AUDIO OFF)
  set(EXTERNAL_SHLIBS_GRAPHICS OFF)
  set(EXTERNAL_SHLIBS OFF)
  message(STATUS "Building Extempore as a library")
  message(STATUS "Building without external dependencies")
endif()

# share directory

# if -DEXT_SHARE_DIR=/path/to/share-dir is provided at the command
# line it will override these values

# packaging (binary distribution)

# ARM64 requires macOS 11.0 (Big Sur) minimum
if(APPLE)
  if(NOT DEFINED CMAKE_OSX_DEPLOYMENT_TARGET)
    if(CMAKE_SYSTEM_PROCESSOR MATCHES "arm64" OR CMAKE_HOST_SYSTEM_PROCESSOR MATCHES "arm64")
      set(CMAKE_OSX_DEPLOYMENT_TARGET 11.0)
    endif()
  endif()
endif()

if(PACKAGE)
  # For packaged binaries on Intel, enforce a 10.12 floor if still unset
  if(NOT DEFINED CMAKE_OSX_DEPLOYMENT_TARGET)
    set(CMAKE_OSX_DEPLOYMENT_TARGET 10.12)
  endif()
  set(ASSETS ON) # necessary for packaging
  message(STATUS "Building Extempore for binary distribution (assets directory will be downloaded)")
endif()

# LLVM

if(DEFINED ENV{EXT_LLVM_DIR})
  # if there's an EXT_LLVM_DIR environment variable, use that
  set(EXT_LLVM_DIR $ENV{EXT_LLVM_DIR})
  set(BUILD_LLVM OFF)
else()
  set(EXT_LLVM_DIR ${CMAKE_SOURCE_DIR}/llvm)
  set(BUILD_LLVM ON)
endif()

# building external shared library dependencies

if(EXTERNAL_SHLIBS)

  include(ExternalProject)

  set(EXT_DEPS_INSTALL_DIR ${CMAKE_BINARY_DIR}/deps-install)
  set(EXT_PLATFORM_SHLIBS_DIR ${CMAKE_SOURCE_DIR}/libs/platform-shlibs)
  if(PACKAGE)
    # Use generic tuning for x86_64 packages, but native tuning for ARM64
    if(CMAKE_SYSTEM_PROCESSOR MATCHES "arm64|aarch64|ARM64")
      set(EXT_DEPS_C_FLAGS "${CMAKE_C_FLAGS_RELEASE}")
      set(EXT_DEPS_CXX_FLAGS "${CMAKE_CXX_FLAGS_RELEASE}")
      message(STATUS "ARM64 detected: using native CPU tuning")
    else()
    set(EXT_DEPS_C_FLAGS "${CMAKE_C_FLAGS_RELEASE} -mtune=generic")
    set(EXT_DEPS_CXX_FLAGS "${CMAKE_CXX_FLAGS_RELEASE} -mtune=generic")
    endif()
    message(STATUS "compiler flags for packaging:\nC    ${EXT_DEPS_C_FLAGS}\nCXX  ${EXT_DEPS_CXX_FLAGS}")
  endif()
endif()

if (EXT_DYLIB)
  set(EXT_DEPS_C_FLAGS "${EXT_DEPS_C_FLAGS} -fPIC")
  set(EXT_DEPS_CXX_FLAGS "${EXT_DEPS_CXX_FLAGS} -fPIC")
endif()

if(NOT ${CMAKE_SIZEOF_VOID_P} EQUAL 8)
  message(FATAL_ERROR "Extempore currently only runs on 64-bit platforms.")
endif()

# Set a default build type if none was specified

if(NOT CMAKE_BUILD_TYPE)
  message(STATUS "Building 'Release' configuration")
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build." FORCE)
  # Set the possible values of build type for cmake-gui
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
endif()

# # set_target_properties(extempore PROPERTIES RUNTIME_OUTPUT_DIRECTORY "${CMAKE_HOME_DIRECTORY}")
# set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_SOURCE_DIR}/extras/cmake CACHE PATH
#   "path to Extempore's cmake modules")

####################
# platform/version #
####################

# this stuff is handy to make sure that the packages/test platforms
# get sensible names

if(UNIX)
  find_program(UNAME_PROGRAM uname)
  execute_process(COMMAND ${UNAME_PROGRAM} -m
    OUTPUT_VARIABLE UNAME_MACHINE_NAME
    OUTPUT_STRIP_TRAILING_WHITESPACE)
  execute_process(COMMAND ${UNAME_PROGRAM} -r
    OUTPUT_VARIABLE UNAME_OS_RELEASE
    OUTPUT_STRIP_TRAILING_WHITESPACE)
  execute_process(COMMAND ${UNAME_PROGRAM} -s
    OUTPUT_VARIABLE UNAME_OS_NAME
    OUTPUT_STRIP_TRAILING_WHITESPACE)
endif(UNIX)

if(APPLE)
  set(EXTEMPORE_SYSTEM_NAME "osx")
  execute_process(COMMAND sw_vers -productVersion
    OUTPUT_VARIABLE EXTEMPORE_SYSTEM_VERSION
    OUTPUT_STRIP_TRAILING_WHITESPACE)
  string(REGEX MATCH "^[0-9]+\\.?[0-9]*" EXTEMPORE_SYSTEM_VERSION ${EXTEMPORE_SYSTEM_VERSION})
  set(EXTEMPORE_SYSTEM_ARCHITECTURE ${UNAME_MACHINE_NAME})
elseif(UNIX)
  # try lsb_release first - better at giving the distro name
  execute_process(COMMAND lsb_release -is
    OUTPUT_VARIABLE EXTEMPORE_SYSTEM_NAME
    OUTPUT_STRIP_TRAILING_WHITESPACE)
  if(NOT EXTEMPORE_SYSTEM_NAME)
    # otherwise use uname output
    set(EXTEMPORE_SYSTEM_NAME ${UNAME_OS_NAME})
  endif()
  set(EXTEMPORE_SYSTEM_VERSION ${UNAME_OS_RELEASE})
  set(EXTEMPORE_SYSTEM_ARCHITECTURE ${UNAME_MACHINE_NAME})
elseif(WIN32)
  set(EXTEMPORE_SYSTEM_NAME "Windows")
  string(REGEX MATCH "^[0-9]+" EXTEMPORE_SYSTEM_VERSION ${CMAKE_SYSTEM_VERSION})
  # deal with Windows version number shenanigans
  if(${EXTEMPORE_SYSTEM_VERSION} LESS 10)
    string(CONCAT ACTUAL_VERSION_EXPRESSION "${EXTEMPORE_SYSTEM_VERSION}" " + 1")
    math(EXPR EXTEMPORE_SYSTEM_VERSION ${ACTUAL_VERSION_EXPRESSION})
  endif()
  set(EXTEMPORE_SYSTEM_ARCHITECTURE ${CMAKE_SYSTEM_PROCESSOR})
else()
  message(FATAL_ERROR "Sorry, Extempore isn't supported on this platform - macOS, Linux & Windows only.")
endif()

########
# PCRE #
########

# current in-tree PCRE version: 8.38

add_library(pcre STATIC
  # headers
  src/pcre/config.h
  src/pcre/pcre.h
  src/pcre/ucp.h
  # source files
  src/pcre/pcre_chartables.c
  src/pcre/pcre_compile.c
  src/pcre/pcre_exec.c
  src/pcre/pcre_globals.c
  src/pcre/pcre_internal.h
  src/pcre/pcre_newline.c
  src/pcre/pcre_tables.c
  )

target_compile_definitions(pcre
  PRIVATE -DHAVE_CONFIG_H
  )

if(PACKAGE)
  if(NOT CMAKE_SYSTEM_PROCESSOR MATCHES "arm64|aarch64|ARM64")
  target_compile_options(pcre
    PRIVATE -mtune=generic)
  endif()
endif()

#############
# portaudio #
#############

include(ExternalProject)

ExternalProject_Add(portaudio_static
  PREFIX portaudio
  URL https://github.com/PortAudio/portaudio/archive/3f7bee79a65327d2e0965e8a74299723ed6f072d.zip
  URL_MD5 182b76e05f6ef21d9f5716da7489905d
  CMAKE_ARGS
  -DPA_BUILD_STATIC=ON
  -DPA_BUILD_SHARED=OFF
  -DPA_LIBNAME_ADD_SUFFIX=OFF
  -DPA_USE_JACK=${JACK}
  -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
  -DCMAKE_C_FLAGS=${EXT_DEPS_C_FLAGS}
  -DCMAKE_CXX_FLAGS=${EXT_DEPS_CXX_FLAGS}
  -DCMAKE_INSTALL_PREFIX=${CMAKE_BINARY_DIR}/portaudio)

##############
# LLVM 3.8.0 #
##############

# if you need to build LLVM by hand, the command will be something like
# cmake .. -DLLVM_TARGETS_TO_BUILD=X86 -DCMAKE_BUILD_TYPE=Release -DLLVM_ENABLE_TERMINFO=OFF -DLLVM_ENABLE_ZLIB=OFF -DCMAKE_INSTALL_PREFIX=c:/Users/ben/Code/extempore/llvm-3.8.0-release

if(NOT BUILD_LLVM)
  add_custom_target(LLVM)
else()
  include(ExternalProject)

  if(PACKAGE)
    ExternalProject_Add(LLVM
      PREFIX llvm
	  URL https://github.com/digego/extempore/releases/download/v0.8.9/llvm-3.8.0.src-patched-for-extempore.tar.xz
      URL_MD5 600ee9a94d2e104f53be739568f3508e
      CMAKE_ARGS
      -DLLVM_TARGETS_TO_BUILD=X86
      -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
      -DLLVM_ENABLE_TERMINFO=OFF
      -DLLVM_ENABLE_ZLIB=OFF
      -DLLVM_INCLUDE_TOOLS=ON
      -DLLVM_BUILD_TOOLS=ON
      -DLLVM_INCLUDE_UTILS=OFF
      -DLLVM_BUILD_RUNTIME=OFF
      -DLLVM_INCLUDE_EXAMPLES=OFF
      -DLLVM_INCLUDE_TESTS=OFF
      -DLLVM_INCLUDE_GO_TESTS=OFF
      -DLLVM_INCLUDE_GO_TESTS=OFF
      -DLLVM_INCLUDE_DOCS=OFF
      -DCMAKE_C_FLAGS=${EXT_DEPS_C_FLAGS}
      -DCMAKE_CXX_FLAGS=${EXT_DEPS_CXX_FLAGS}
      -DCMAKE_INSTALL_PREFIX=${EXT_LLVM_DIR})
  else()
    ExternalProject_Add(LLVM
      PREFIX llvm
	  URL https://github.com/digego/extempore/releases/download/v0.8.9/llvm-3.8.0.src-patched-for-extempore.tar.xz
      URL_MD5 600ee9a94d2e104f53be739568f3508e
      CMAKE_ARGS
      -DLLVM_TARGETS_TO_BUILD=X86
      -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
      -DLLVM_ENABLE_TERMINFO=OFF
      -DLLVM_ENABLE_ZLIB=OFF
      -DLLVM_INCLUDE_TOOLS=ON
      -DLLVM_BUILD_TOOLS=ON
      -DLLVM_INCLUDE_UTILS=OFF
      -DLLVM_BUILD_RUNTIME=OFF
      -DLLVM_INCLUDE_EXAMPLES=OFF
      -DLLVM_INCLUDE_TESTS=OFF
      -DLLVM_INCLUDE_GO_TESTS=OFF
      -DLLVM_INCLUDE_GO_TESTS=OFF
      -DLLVM_INCLUDE_DOCS=OFF
      -DCMAKE_INSTALL_PREFIX=${EXT_LLVM_DIR})
  endif()
  ExternalProject_Add_StepTargets(LLVM install)
endif()

# the ordering of these libs matters, especially with the gcc linker.
# Check the output of "llvm-config --libnames" to be sure
set(EXT_LLVM_LIBRARIES "LLVMLTO;LLVMObjCARCOpts;LLVMSymbolize;LLVMDebugInfoPDB;LLVMDebugInfoDWARF;LLVMMIRParser;LLVMLibDriver;LLVMOption;LLVMTableGen;LLVMOrcJIT;LLVMPasses;LLVMipo;LLVMVectorize;LLVMLinker;LLVMIRReader;LLVMAsmParser;LLVMX86Disassembler;LLVMX86AsmParser;LLVMX86CodeGen;LLVMSelectionDAG;LLVMAsmPrinter;LLVMX86Desc;LLVMMCDisassembler;LLVMX86Info;LLVMX86AsmPrinter;LLVMX86Utils;LLVMMCJIT;LLVMLineEditor;LLVMDebugInfoCodeView;LLVMInterpreter;LLVMExecutionEngine;LLVMRuntimeDyld;LLVMCodeGen;LLVMTarget;LLVMScalarOpts;LLVMInstCombine;LLVMInstrumentation;LLVMProfileData;LLVMObject;LLVMMCParser;LLVMTransformUtils;LLVMMC;LLVMBitWriter;LLVMBitReader;LLVMAnalysis;LLVMCore;LLVMSupport")
foreach(llvm_lib ${EXT_LLVM_LIBRARIES})
  get_filename_component(LLVM_LIB_FULLPATH "${EXT_LLVM_DIR}/lib/${CMAKE_STATIC_LIBRARY_PREFIX}${llvm_lib}${CMAKE_STATIC_LIBRARY_SUFFIX}" ABSOLUTE)
  list(APPEND LLVM_LIBRARIES ${LLVM_LIB_FULLPATH})
endforeach()

#############
# extempore #
#############

# source files

if (EXT_DYLIB)
  include(${CMAKE_SOURCE_DIR}/CMakeRC.cmake)

  # bundle these files into extempore dylib
  cmrc_add_resource_library(rc_xtm NAMESPACE "xtm"
    runtime/bitcode.ll
    runtime/init.ll
    runtime/init.xtm
    runtime/inline.ll
    runtime/llvmir.xtm
    runtime/llvmti.xtm
    runtime/scheme.xtm
  )

  add_library(extempore SHARED 
    src/Extempore.cpp
    src/AudioDevice.cpp
    src/EXTZones.cpp
    src/EXTClosureAddressTable.cpp
    src/EXTLLVM.cpp
    src/EXTThread.cpp
    src/Extempore.cpp
    src/OSC.cpp
    src/Scheme.cpp
    src/SchemeFFI.cpp
    src/SchemeProcess.cpp
    src/SchemeREPL.cpp
    src/TaskScheduler.cpp
    src/UNIV.cpp
  )
  target_link_libraries(extempore PRIVATE rc_xtm)
else()
  add_executable(extempore src/Extempore.cpp
    src/AudioDevice.cpp
    src/EXTZones.cpp
    src/EXTClosureAddressTable.cpp
    src/EXTLLVM.cpp
    src/EXTThread.cpp
    src/Extempore.cpp
    src/OSC.cpp
    src/Scheme.cpp
    src/SchemeFFI.cpp
    src/SchemeProcess.cpp
    src/SchemeREPL.cpp
    src/TaskScheduler.cpp
    src/UNIV.cpp
    )
endif()

if(MSVC)
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /DEF:${CMAKE_SOURCE_DIR}/src/extempore.def")
endif()

target_include_directories(extempore PRIVATE include)

# suppress the warning about the opcode switch statement
if(UNIX)
  set_source_files_properties(src/Scheme.cpp PROPERTIES COMPILE_FLAGS -Wno-switch)
endif()

# static extempore build dependencies

add_dependencies(extempore pcre)
add_dependencies(extempore portaudio_static)

if(BUILD_LLVM)
  if(WIN32)
    add_dependencies(extempore LLVM-install)
  else()
    add_dependencies(extempore LLVM)
  endif()
endif()

if(WIN32)
  target_include_directories(extempore PRIVATE src/networking-ts-impl/include)
endif()

target_include_directories(extempore
  PRIVATE
  src/pcre
  ${CMAKE_BINARY_DIR}/portaudio/include # installed by ExternalProject
  ${EXT_LLVM_DIR}/include)

target_link_directories(extempore PRIVATE ${CMAKE_BINARY_DIR}/portaudio/lib)
target_link_libraries(extempore PRIVATE pcre portaudio${CMAKE_STATIC_LIBRARY_SUFFIX} ${LLVM_LIBRARIES})
if(UNIX AND NOT APPLE)
  target_link_libraries(extempore PRIVATE asound)
endif()

# compiler options

if(PACKAGE)
  target_compile_definitions(extempore
    PRIVATE -DEXT_SHARE_DIR=".")
  if(NOT CMAKE_SYSTEM_PROCESSOR MATCHES "arm64|aarch64|ARM64")
  target_compile_options(extempore
    PRIVATE -mtune=generic)
  endif()
elseif(EXT_SHARE_DIR)
  target_compile_definitions(extempore
    PRIVATE -DEXT_SHARE_DIR="${EXT_SHARE_DIR}")
elseif(EXT_DYLIB)
  target_compile_definitions(extempore
    PRIVATE -DEXT_DYLIB=1 
    PRIVATE -DEXT_SHARE_DIR="."
  )
else()
  target_compile_definitions(extempore
    PRIVATE -DEXT_SHARE_DIR="${CMAKE_SOURCE_DIR}")
endif()

# platform-specific config

if(UNIX)
  target_compile_definitions(extempore
    PRIVATE -D_GNU_SOURCE
    PRIVATE -D__STDC_CONSTANT_MACROS
    PRIVATE -D__STDC_FORMAT_MACROS
    PRIVATE -D__STDC_LIMIT_MACROS)
  target_compile_options(extempore
    PRIVATE -std=c++11
    PRIVATE -fvisibility-inlines-hidden
    # PRIVATE -fno-exceptions
    PRIVATE -fno-rtti
    PRIVATE -fno-common
    PRIVATE -Woverloaded-virtual
    # PRIVATE -Wcast-qual
    PRIVATE -Wno-unused-result)
  target_link_libraries(extempore PRIVATE pthread)
endif()

if(WIN32)
  target_compile_definitions(extempore
    PRIVATE -DPCRE_STATIC
    PRIVATE -D_CRT_SECURE_NO_WARNINGS
	# NOTE: this next define is necessary because VS2019 deprecated the std::tr2
	# namespace, but setting CXX_STANDARD to c++17 (required for "normal"
	# std::filesystem) breaks a bunch of LLVM 3.8. So, when we finally upgrade
	# LLVM, we should switch to std::filesystem, but for now let's just hold our
	# nose and do this.
	PRIVATE -D_SILENCE_EXPERIMENTAL_FILESYSTEM_DEPRECATION_WARNING)
  set_source_files_properties(
    PROPERTIES
    COMPILE_FLAGS "/EHsc")

elseif(APPLE) # macOS
  # use clang++ by default
  set(CMAKE_C_COMPILER clang)
  set(CMAKE_CXX_COMPILER clang++)
  # tell the compiler about the few ObjC++ source files on macOS
  set_source_files_properties(
    src/Extempore.cpp
    src/SchemeFFI.cpp
    src/UNIV.cpp
    PROPERTIES
    COMPILE_FLAGS "-x objective-c++")
  # frameworks
  target_link_libraries(extempore
    PRIVATE "-framework Cocoa"
    PRIVATE "-framework CoreAudio"
    PRIVATE "-framework AudioUnit"
    PRIVATE "-framework AudioToolbox")

elseif(UNIX AND NOT APPLE) # Linux
  set_property(TARGET pcre PROPERTY POSITION_INDEPENDENT_CODE ON)
  set_property(TARGET extempore PROPERTY POSITION_INDEPENDENT_CODE ON)
  # target_link_libraries(extempore PRIVATE --export-dynamic)
  target_link_libraries(extempore PRIVATE dl)
endif()

# on Windows, put the created extempore.exe straight into the source
# directory, and the .lib file into libs/platform-shlibs
if(WIN32)
  set_target_properties(extempore
    PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY_DEBUG ${CMAKE_SOURCE_DIR}
    RUNTIME_OUTPUT_DIRECTORY_RELEASE ${CMAKE_SOURCE_DIR}
    LIBRARY_OUTPUT_DIRECTORY_DEBUG ${CMAKE_SOURCE_DIR}
    LIBRARY_OUTPUT_DIRECTORY_RELEASE ${CMAKE_SOURCE_DIR}
    ARCHIVE_OUTPUT_DIRECTORY_DEBUG ${CMAKE_SOURCE_DIR}/libs/platform-shlibs
    ARCHIVE_OUTPUT_DIRECTORY_RELEASE ${CMAKE_SOURCE_DIR}/libs/platform-shlibs)
endif()

##########
# assets #
##########

add_custom_target(assets
  COMMAND
  ${CMAKE_COMMAND}
  -DASSETS_PATH=${CMAKE_SOURCE_DIR}/assets
  -DASSETS_GIT_REF=0c9f32c
  -P ${CMAKE_SOURCE_DIR}/extras/cmake/download_assets.cmake)

if(ASSETS)
  add_dependencies(extempore assets)
endif()

###########
# install #
###########

if(NOT PACKAGE)
  # if we're not packaging, installation just involves moving the
  # binary into the toplevel source directory
  install(TARGETS extempore
    RUNTIME
    DESTINATION bin)
else()
  install(TARGETS extempore
    RUNTIME
    DESTINATION ".")
  install(DIRECTORY assets runtime libs examples tests
    DESTINATION "."
    PATTERN ".DS_Store" EXCLUDE)
endif()

###################
# AOT compilation #
###################

if(WIN32)

  ## this is the "just run the AOT script" approach - it doesn't create individual
  ## targets or a dependency graph, so it doesn't parallelise. but it's simpler

  macro(aotcompile file)
    configure_file(
      ${CMAKE_SOURCE_DIR}/extras/cmake/${file}.cmake.in
      ${CMAKE_SOURCE_DIR}/extras/cmake/${file}.cmake
      @ONLY)
    add_custom_target(${file} ALL
      COMMAND ${CMAKE_COMMAND} -P ${CMAKE_SOURCE_DIR}/extras/cmake/${file}.cmake
      COMMENT "Ahead-of-time compiling the ${file} standard library..."
      WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
    set_target_properties(${file} PROPERTIES FOLDER AOT)
  endmacro(aotcompile)

  aotcompile(aot)

else(WIN32)

  # this approach requires specifying the inter-lib dependencies by hand, but
  # allows us to do AOT compilation in parallel

  set(EXTEMPORE_AOT_COMPILE_PORT 17099)

  macro(aotcompile_lib libfile group) # deps are optional, and go at the end
    get_filename_component(basename ${libfile} NAME_WE)
    set(targetname aot_${basename})
    set(filename ${CMAKE_SOURCE_DIR}/libs/aot-cache/xtm${basename}.so)
    if(PACKAGE)
      add_custom_command (OUTPUT ${filename}
        COMMAND extempore --nobase --noaudio --mcpu=generic --attr=none --port=${EXTEMPORE_AOT_COMPILE_PORT}
        --eval "(impc:aot:compile-xtm-file \"${libfile}\")"
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
        VERBATIM)
    else(PACKAGE)
      add_custom_command(OUTPUT ${filename}
        COMMAND extempore --nobase --noaudio --port=${EXTEMPORE_AOT_COMPILE_PORT}
        --eval "(impc:aot:compile-xtm-file \"${libfile}\")"
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
        VERBATIM)
    endif(PACKAGE)
    add_custom_target(${targetname}
      DEPENDS ${filename} extempore)
    set_target_properties(${targetname} PROPERTIES FOLDER AOT)
    if(NOT ${group} STREQUAL "core")
      add_dependencies(${targetname} external_shlibs_${group})
      add_dependencies(aot_external_${group} ${targetname})
    endif()
    foreach(dep ${ARGN})
      add_dependencies(${targetname} aot_${dep})
    endforeach()
    # decrement port number by 2
    math(EXPR EXTEMPORE_AOT_COMPILE_PORT "${EXTEMPORE_AOT_COMPILE_PORT} - 2")
  endmacro(aotcompile_lib)

  # core
  add_custom_target(aot_core)
  aotcompile_lib(libs/base/base.xtm core) # no lib dependency for base.xtm
  aotcompile_lib(libs/core/math.xtm core base)
  aotcompile_lib(libs/core/rational.xtm core base)
  aotcompile_lib(libs/core/audiobuffer.xtm core base)
  aotcompile_lib(libs/core/audio_dsp.xtm core base rational audiobuffer)
  aotcompile_lib(libs/core/instruments.xtm core base audio_dsp)

  set_target_properties(aot_core PROPERTIES FOLDER AOT)
  add_dependencies(aot_core extempore)

endif(WIN32)

# uninstall only AOT-compiled libs
add_custom_target(clean_aot
  COMMAND ${CMAKE_COMMAND} -E remove_directory ${CMAKE_SOURCE_DIR}/libs/aot-cache
  COMMENT "Removing AOT-compiled libs")

########################################
# external shared library dependencies #
########################################

# this is (basically) a huge if statement for separating windows & macos/linux
# (including some copy-paste between the two branches). Here be dragons!

if(UNIX)
  if(EXTERNAL_SHLIBS_AUDIO)

    # first, download & build the shared libraries themselves (these are all external to Extempore)

    ExternalProject_Add(portmidi
      PREFIX portmidi
      URL https://github.com/extemporelang/portmidi/archive/8602f548f71daf5ef638b2f7d224753400cb2158.zip
      CMAKE_ARGS
      -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
      -DCMAKE_C_FLAGS=${EXT_DEPS_C_FLAGS}
      -DCMAKE_CXX_FLAGS=${EXT_DEPS_CXX_FLAGS}
      -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR})
    set_target_properties(portmidi PROPERTIES FOLDER EXTERNAL_SHLIBS)

    ExternalProject_Add(rtmidi
      PREFIX rtmidi
      URL https://github.com/thestk/rtmidi/archive/84d130bf22d878ff1b0e224346e2e0f9e3ba8099.zip
      URL_MD5 d932b9fef01b859a1b8b86a3c8dc6621
      CMAKE_ARGS
      -DRTMIDI_BUILD_TESTING=OFF
      -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
      -DCMAKE_C_FLAGS=${EXT_DEPS_C_FLAGS}
      -DCMAKE_CXX_FLAGS=${EXT_DEPS_CXX_FLAGS}
      -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR})
    set_target_properties(rtmidi PROPERTIES FOLDER EXTERNAL_SHLIBS)

    ExternalProject_Add(kiss_fft
      PREFIX kiss_fft
      URL https://github.com/extemporelang/kiss_fft/archive/1.3.0.zip
      CMAKE_ARGS
      -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
      -DCMAKE_C_FLAGS=${EXT_DEPS_C_FLAGS}
      -DCMAKE_CXX_FLAGS=${EXT_DEPS_CXX_FLAGS}
      -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR})
    set_target_properties(kiss_fft PROPERTIES FOLDER EXTERNAL_SHLIBS)

    # build with as few deps as we can get away with

    ExternalProject_Add(sndfile
    PREFIX libsndfile
    URL https://github.com/erikd/libsndfile/archive/ae64caf9b5946d365971c550875000342e763de6.zip
    CMAKE_ARGS
    -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
    -DCMAKE_C_FLAGS=${EXT_DEPS_C_FLAGS}
    -DCMAKE_CXX_FLAGS=${EXT_DEPS_CXX_FLAGS}
    -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR}
    -DBUILD_SHARED_LIBS=ON
    -DBUILD_PROGRAMS=OFF
    -DBUILD_EXAMPLES=OFF
	-DENABLE_EXTERNAL_LIBS=OFF
    -DBUILD_TESTING=OFF
    -DENABLE_CPACK=OFF
    -DENABLE_PACKAGE_CONFIG=OFF)
    set_target_properties(sndfile PROPERTIES FOLDER EXTERNAL_SHLIBS)

    add_custom_target(aot_external_audio ALL)
    set_target_properties(aot_external_audio PROPERTIES FOLDER AOT)
    aotcompile_lib(libs/external/fft.xtm audio base math)
    aotcompile_lib(libs/external/sndfile.xtm audio base)
    aotcompile_lib(libs/external/audio_dsp_ext.xtm audio base fft sndfile)
    aotcompile_lib(libs/external/instruments_ext.xtm audio base sndfile instruments)
    aotcompile_lib(libs/external/portmidi.xtm audio base)

    add_custom_target(external_shlibs_audio
      COMMENT "moving shared libs into ${EXT_PLATFORM_SHLIBS_DIR}"
      DEPENDS LLVM sndfile kiss_fft portmidi rtmidi
      COMMAND ${CMAKE_COMMAND} -E make_directory ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy libkiss_fft${CMAKE_SHARED_LIBRARY_SUFFIX} ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy libportmidi${CMAKE_SHARED_LIBRARY_SUFFIX} ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy librtmidi${CMAKE_SHARED_LIBRARY_SUFFIX} ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy libsndfile${CMAKE_SHARED_LIBRARY_SUFFIX} ${EXT_PLATFORM_SHLIBS_DIR}
      WORKING_DIRECTORY ${EXT_DEPS_INSTALL_DIR}/lib)
    set_target_properties(external_shlibs_audio PROPERTIES FOLDER EXTERNAL_SHLIBS)

    add_dependencies(aot_external_audio extempore)
    add_dependencies(aot_external_audio external_shlibs_audio)

  endif(EXTERNAL_SHLIBS_AUDIO)

  if(EXTERNAL_SHLIBS_GRAPHICS)

    ExternalProject_Add(nanovg
      PREFIX nanovg
      URL https://github.com/extemporelang/nanovg/archive/3c60175fcc2e5fe305b04355cdce35d499c80310.tar.gz
      CMAKE_ARGS
      -DEXTEMPORE_LIB_PATH=${CMAKE_SOURCE_DIR}/libs/platform-shlibs/extempore.lib
      -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
      -DCMAKE_C_FLAGS=${EXT_DEPS_C_FLAGS}
      -DCMAKE_CXX_FLAGS=${EXT_DEPS_CXX_FLAGS}
      -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR})
    set_target_properties(nanovg PROPERTIES FOLDER EXTERNAL_SHLIBS)

    add_dependencies(nanovg extempore)

    ExternalProject_Add(stb_image
      PREFIX stb_image
      URL https://github.com/extemporelang/stb/archive/152a250a702bf28951bb0220d63bc0c99830c498.zip
      CMAKE_ARGS
      -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
      -DCMAKE_C_FLAGS=${EXT_DEPS_C_FLAGS}
      -DCMAKE_CXX_FLAGS=${EXT_DEPS_CXX_FLAGS}
      -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR})
    set_target_properties(nanovg PROPERTIES FOLDER EXTERNAL_SHLIBS)

    ExternalProject_Add(glfw3
      PREFIX glfw3
      URL https://github.com/glfw/glfw/releases/download/3.2.1/glfw-3.2.1.zip
      CMAKE_ARGS
      -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
      -DCMAKE_C_FLAGS=${EXT_DEPS_C_FLAGS}
      -DCMAKE_CXX_FLAGS=${EXT_DEPS_CXX_FLAGS}
      -DBUILD_SHARED_LIBS=ON
      -DGLFW_BUILD_EXAMPLES=OFF
      -DGLFW_BUILD_TESTS=OFF
      -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR})
    set_target_properties(glfw3 PROPERTIES FOLDER EXTERNAL_SHLIBS)

    ExternalProject_Add(assimp
      PREFIX assimp
      URL https://github.com/assimp/assimp/archive/v3.2.zip
      CMAKE_ARGS
      -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
      -DCMAKE_C_FLAGS=${EXT_DEPS_C_FLAGS}
      -DCMAKE_CXX_FLAGS=${EXT_DEPS_CXX_FLAGS}
      -DCMAKE_DEBUG_POSTFIX=
      -DASSIMP_BUILD_ASSIMP_TOOLS=OFF
      -DASSIMP_BUILD_SAMPLES=OFF
      -DASSIMP_BUILD_TESTS=OFF
      -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR})
    set_target_properties(assimp PROPERTIES FOLDER EXTERNAL_SHLIBS)

    add_custom_target(aot_external_graphics ALL)
    set_target_properties(assimp PROPERTIES FOLDER AOT)

    aotcompile_lib(libs/external/stb_image.xtm graphics base)
    aotcompile_lib(libs/external/glfw3.xtm graphics base)
    if(DEFINED ENV{EXTEMPORE_FORCE_GL_GETPROCADDRESS})
      set(GL_BIND_METHOD getprocaddress)
    else()
      set(GL_BIND_METHOD directbind)
    endif()
    aotcompile_lib(libs/external/gl/glcore-${GL_BIND_METHOD}.xtm graphics base)
    aotcompile_lib(libs/external/gl/gl-objects.xtm graphics base math glcore-${GL_BIND_METHOD} stb_image)
    aotcompile_lib(libs/external/gl/gl-objects2.xtm graphics base glcore-${GL_BIND_METHOD} stb_image)
    aotcompile_lib(libs/external/graphics-pipeline.xtm graphics base math glcore-${GL_BIND_METHOD} stb_image gl-objects2)
    aotcompile_lib(libs/external/nanovg.xtm graphics base glcore-${GL_BIND_METHOD})
    aotcompile_lib(libs/external/assimp.xtm graphics base stb_image graphics-pipeline)
    aotcompile_lib(libs/external/gl/glcompat-${GL_BIND_METHOD}.xtm graphics base)

    add_custom_target(external_shlibs_graphics
      COMMENT "moving shared libs into ${EXT_PLATFORM_SHLIBS_DIR}"
      DEPENDS LLVM assimp glfw3 stb_image nanovg
      COMMAND ${CMAKE_COMMAND} -E make_directory ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy libassimp${CMAKE_SHARED_LIBRARY_SUFFIX} ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy libglfw${CMAKE_SHARED_LIBRARY_SUFFIX} ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy libnanovg${CMAKE_SHARED_LIBRARY_SUFFIX} ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy libstb_image${CMAKE_SHARED_LIBRARY_SUFFIX} ${EXT_PLATFORM_SHLIBS_DIR}
      WORKING_DIRECTORY ${EXT_DEPS_INSTALL_DIR}/lib)
    set_target_properties(external_shlibs_graphics PROPERTIES FOLDER EXTERNAL_SHLIBS)

    # set up these libs for AOT compilation
    add_dependencies(aot_external_graphics extempore)
    add_dependencies(aot_external_graphics external_shlibs_graphics)

  endif(EXTERNAL_SHLIBS_GRAPHICS)
endif(UNIX)

# aaand here's the Windows version

if(WIN32)
  if(EXTERNAL_SHLIBS_AUDIO)

    # first, download & build the shared libraries themselves (these are all external to Extempore)

    ExternalProject_Add(portmidi
      PREFIX portmidi
      URL https://github.com/extemporelang/portmidi/archive/8602f548f71daf5ef638b2f7d224753400cb2158.zip
      CMAKE_ARGS
      -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
      -DCMAKE_C_FLAGS=${EXT_DEPS_C_FLAGS}
      -DCMAKE_CXX_FLAGS=${EXT_DEPS_CXX_FLAGS}
      -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR})
    set_target_properties(portmidi PROPERTIES FOLDER EXTERNAL_SHLIBS)

    ExternalProject_Add(rtmidi
      PREFIX rtmidi
      URL https://github.com/thestk/rtmidi/archive/84d130bf22d878ff1b0e224346e2e0f9e3ba8099.zip
      URL_MD5 d932b9fef01b859a1b8b86a3c8dc6621
      CMAKE_ARGS
      -DRTMIDI_BUILD_TESTING=OFF
      -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
      -DCMAKE_C_FLAGS=${EXT_DEPS_C_FLAGS}
      -DCMAKE_CXX_FLAGS=${EXT_DEPS_CXX_FLAGS}
      -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR}
	  # these are necessary because RTMidi's CMake config is a law unto itself
      -DCMAKE_INSTALL_LIBDIR=${EXT_DEPS_INSTALL_DIR}
      -DCMAKE_INSTALL_BINDIR=${EXT_DEPS_INSTALL_DIR})
    set_target_properties(rtmidi PROPERTIES FOLDER EXTERNAL_SHLIBS)

    ExternalProject_Add(kiss_fft
      PREFIX kiss_fft
      URL https://github.com/extemporelang/kiss_fft/archive/1.3.0.zip
      CMAKE_ARGS
      -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
      -DCMAKE_C_FLAGS=${EXT_DEPS_C_FLAGS}
      -DCMAKE_CXX_FLAGS=${EXT_DEPS_CXX_FLAGS}
      -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR})
    set_target_properties(kiss_fft PROPERTIES FOLDER EXTERNAL_SHLIBS)

    ExternalProject_Add(sndfile
      PREFIX libsndfile
      URL https://github.com/erikd/libsndfile/archive/ae64caf9b5946d365971c550875000342e763de6.zip
      CMAKE_ARGS
      -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
      -DCMAKE_C_FLAGS=${EXT_DEPS_C_FLAGS}
      -DCMAKE_CXX_FLAGS=${EXT_DEPS_CXX_FLAGS}
      -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR}
      -DBUILD_SHARED_LIBS=ON
      -DBUILD_PROGRAMS=OFF
      -DBUILD_EXAMPLES=OFF
	  -DENABLE_EXTERNAL_LIBS=OFF
	  -DENABLE_STATIC_RUNTIME=OFF
      -DBUILD_TESTING=OFF
      -DENABLE_CPACK=OFF
      -DENABLE_PACKAGE_CONFIG=OFF)
    set_target_properties(sndfile PROPERTIES FOLDER EXTERNAL_SHLIBS)

  endif(EXTERNAL_SHLIBS_AUDIO)

  if(EXTERNAL_SHLIBS_GRAPHICS)

    ExternalProject_Add(nanovg
      PREFIX nanovg
      URL https://github.com/extemporelang/nanovg/archive/3c60175fcc2e5fe305b04355cdce35d499c80310.tar.gz
      CMAKE_ARGS
      -DEXTEMPORE_LIB_PATH=${CMAKE_SOURCE_DIR}/libs/platform-shlibs/extempore.lib
      -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
      -DCMAKE_C_FLAGS=${EXT_DEPS_C_FLAGS}
      -DCMAKE_CXX_FLAGS=${EXT_DEPS_CXX_FLAGS}
      -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR})
    set_target_properties(nanovg PROPERTIES FOLDER EXTERNAL_SHLIBS)

    add_dependencies(nanovg extempore)

    ExternalProject_Add(stb_image
      PREFIX stb_image
      URL https://github.com/extemporelang/stb/archive/152a250a702bf28951bb0220d63bc0c99830c498.zip
      CMAKE_ARGS
      -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
      -DCMAKE_C_FLAGS=${EXT_DEPS_C_FLAGS}
      -DCMAKE_CXX_FLAGS=${EXT_DEPS_CXX_FLAGS}
      -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR})
    set_target_properties(nanovg PROPERTIES FOLDER EXTERNAL_SHLIBS)

    ExternalProject_Add(glfw3
      PREFIX glfw3
      URL https://github.com/glfw/glfw/releases/download/3.2.1/glfw-3.2.1.zip
      CMAKE_ARGS
      -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
      -DCMAKE_C_FLAGS=${EXT_DEPS_C_FLAGS}
      -DCMAKE_CXX_FLAGS=${EXT_DEPS_CXX_FLAGS}
      -DBUILD_SHARED_LIBS=ON
      -DGLFW_BUILD_EXAMPLES=OFF
      -DGLFW_BUILD_TESTS=OFF
      -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR})
    set_target_properties(glfw3 PROPERTIES FOLDER EXTERNAL_SHLIBS)

    ExternalProject_Add(assimp
      PREFIX assimp
      URL https://github.com/assimp/assimp/archive/v3.2.zip
      CMAKE_ARGS
      -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
      -DCMAKE_C_FLAGS=${EXT_DEPS_C_FLAGS}
      -DCMAKE_CXX_FLAGS=${EXT_DEPS_CXX_FLAGS}
      -DCMAKE_DEBUG_POSTFIX=
      -DASSIMP_BUILD_ASSIMP_TOOLS=OFF
      -DASSIMP_BUILD_SAMPLES=OFF
      -DASSIMP_BUILD_TESTS=OFF
      -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR})
    set_target_properties(assimp PROPERTIES FOLDER EXTERNAL_SHLIBS)

  endif(EXTERNAL_SHLIBS_GRAPHICS)

  # now, figure out which aot*.cmake.in file to run
  if(EXTERNAL_SHLIBS_GRAPHICS AND EXTERNAL_SHLIBS_AUDIO)

    aotcompile(aot_external)

    add_custom_target(external_shlibs_audio
      COMMENT "moving .dll and .lib files into ${EXT_PLATFORM_SHLIBS_DIR}"
      DEPENDS LLVM sndfile kiss_fft portmidi rtmidi
      COMMAND ${CMAKE_COMMAND} -E make_directory ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy bin/sndfile.dll ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy lib/kiss_fft.dll ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy lib/kiss_fft.lib ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy lib/portmidi.dll ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy lib/portmidi.lib ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy rtmidi.dll ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy rtmidi.lib ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy lib/sndfile.lib ${EXT_PLATFORM_SHLIBS_DIR}
      WORKING_DIRECTORY ${EXT_DEPS_INSTALL_DIR})
    set_target_properties(external_shlibs_audio PROPERTIES FOLDER EXTERNAL_SHLIBS)

    add_custom_target(external_shlibs_graphics
      COMMENT "moving .dll and .lib files into ${EXT_PLATFORM_SHLIBS_DIR}"
      DEPENDS LLVM assimp glfw3 stb_image nanovg
      COMMAND ${CMAKE_COMMAND} -E make_directory ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy bin/assimp-vc130-mt.dll ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy lib/assimp-vc130-mt.lib ${EXT_PLATFORM_SHLIBS_DIR}
      # note that glfw3 has different base names for the .dll and .lib
      COMMAND ${CMAKE_COMMAND} -E copy lib/glfw3.dll ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy lib/glfw3dll.lib ${EXT_PLATFORM_SHLIBS_DIR}/glfw3.lib
      COMMAND ${CMAKE_COMMAND} -E copy lib/nanovg.dll ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy lib/nanovg.lib ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy lib/stb_image.dll ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy lib/stb_image.lib ${EXT_PLATFORM_SHLIBS_DIR}
      WORKING_DIRECTORY ${EXT_DEPS_INSTALL_DIR})
    set_target_properties(external_shlibs_graphics PROPERTIES FOLDER EXTERNAL_SHLIBS)

    add_dependencies(aot_external extempore)
    add_dependencies(aot_external external_shlibs_audio)
    add_dependencies(aot_external external_shlibs_graphics)

  # audio only
  elseif(EXTERNAL_SHLIBS_AUDIO)

    aotcompile(aot_external_audio)

    add_custom_target(external_shlibs_audio
      COMMENT "moving .dll and .lib files into ${EXT_PLATFORM_SHLIBS_DIR}"
      DEPENDS LLVM sndfile kiss_fft portmidi rtmidi
      COMMAND ${CMAKE_COMMAND} -E make_directory ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy lib/kiss_fft.dll ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy lib/kiss_fft.lib ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy lib/portmidi.dll ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy lib/portmidi.lib ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy lib/rtmidi.dll ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy lib/rtmidi.lib ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy bin/sndfile.dll ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy lib/sndfile.lib ${EXT_PLATFORM_SHLIBS_DIR}
      WORKING_DIRECTORY ${EXT_DEPS_INSTALL_DIR})
    set_target_properties(external_shlibs_audio PROPERTIES FOLDER EXTERNAL_SHLIBS)

    add_dependencies(aot_external_audio extempore)
    add_dependencies(aot_external_audio external_shlibs_audio)

  # graphics only
  elseif(EXTERNAL_SHLIBS_GRAPHICS)

    aotcompile(aot_external_graphics)

    add_custom_target(external_shlibs_graphics
      COMMENT "moving .dll and .lib files into ${EXT_PLATFORM_SHLIBS_DIR}"
      DEPENDS LLVM assimp glfw3 stb_image nanovg
      COMMAND ${CMAKE_COMMAND} -E make_directory ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy bin/assimp-vc130-mt.dll ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy lib/assimp-vc130-mt.lib ${EXT_PLATFORM_SHLIBS_DIR}
      # note that glfw3 has different base names for the .dll and .lib
      COMMAND ${CMAKE_COMMAND} -E copy lib/glfw3.dll ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy lib/glfw3dll.lib ${EXT_PLATFORM_SHLIBS_DIR}/glfw3.lib
      COMMAND ${CMAKE_COMMAND} -E copy lib/nanovg.dll ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy lib/nanovg.lib ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy lib/stb_image.dll ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy lib/stb_image.lib ${EXT_PLATFORM_SHLIBS_DIR}
      WORKING_DIRECTORY ${EXT_DEPS_INSTALL_DIR})
    set_target_properties(external_shlibs_graphics PROPERTIES FOLDER EXTERNAL_SHLIBS)

    # set up these libs for AOT compilation
    add_dependencies(aot_external_graphics extempore)
    add_dependencies(aot_external_graphics external_shlibs_graphics)
  endif()
endif(WIN32)

if(APPLE)
  add_custom_command(TARGET extempore POST_BUILD
	COMMENT "clear all file attributes (to avoid the \"extempore is damaged and can't be opened. You should move it to the Bin\" error on Big Sur)"
	COMMAND xattr -cr "$<TARGET_FILE_DIR:extempore>")
endif()

#########
# tests #
#########

if(BUILD_TESTS)

  include(CTest)

  set(EXTEMPORE_TEST_PORT 17099)

  macro(extempore_add_test testfile label)
    add_test(NAME ${testfile}
      COMMAND extempore --noaudio --term nocolor --port=${EXTEMPORE_TEST_PORT} --eval "(xtmtest-run-tests \"${testfile}\" #t #t)")
    set_tests_properties(${testfile}
      PROPERTIES
      WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
      LABELS ${label})
    # decrement port number by 2
    math(EXPR EXTEMPORE_TEST_PORT "${EXTEMPORE_TEST_PORT} - 2")
  endmacro()

  macro(extempore_add_example_as_test examplefile timeout label)
    add_test(NAME ${examplefile}
      COMMAND extempore --noaudio --term nocolor --port=${EXTEMPORE_TEST_PORT} --eval "(sys:load-then-quit \"${examplefile}\" ${timeout})")
    set_tests_properties(${examplefile}
      PROPERTIES
      WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
      TIMEOUT 300 # nothing should take longer than 5mins
      LABELS ${label})
    # decrement port number by 2
    math(EXPR EXTEMPORE_TEST_PORT "${EXTEMPORE_TEST_PORT} - 2")
  endmacro()

  # tests - core
  extempore_add_test(tests/core/system.xtm libs-core)
  extempore_add_test(tests/core/adt.xtm libs-core)
  extempore_add_test(tests/core/math.xtm libs-core)
  extempore_add_test(tests/core/std.xtm libs-core)
  extempore_add_test(tests/core/xtlang.xtm libs-core)
  extempore_add_test(tests/core/generics.xtm libs-core)
  # tests - external
  extempore_add_test(tests/external/fft.xtm libs-external)
  # examples - core
  extempore_add_example_as_test(examples/core/audio_101.xtm 10 examples-audio)
  # extempore_add_example_as_test(examples/core/extempore_lang.xtm 10 examples-core) # doesn't terminate
  # extempore_add_example_as_test(examples/core/fasta_lang_shootout.xtm 10 examples-core) # no fdopen on Windows at this point
  extempore_add_example_as_test(examples/core/fmsynth.xtm 10 examples-audio)
  extempore_add_example_as_test(examples/core/mtaudio.xtm 10 examples-audio)
  extempore_add_example_as_test(examples/core/nbody_lang_shootout.xtm 10 examples-core)
  # extempore_add_example_as_test(examples/core/osc_101.xtm 10 examples-core) # currently non-working
  # extempore_add_example_as_test(examples/core/polysynth.xtm 10 examples-audio)
  extempore_add_example_as_test(examples/core/scheduler.xtm 10 examples-audio)
  extempore_add_example_as_test(examples/core/topclock_metro.xtm 10 examples-audio)
  extempore_add_example_as_test(examples/core/typeclasses.xtm 10 examples-core)
  extempore_add_example_as_test(examples/core/xthread.xtm 10 examples-core)
  # examples - external
  extempore_add_example_as_test(examples/external/audio_player.xtm 10 examples-audio)
  extempore_add_example_as_test(examples/external/convolution_reverb.xtm 10 examples-audio)
  extempore_add_example_as_test(examples/external/electrofunk.xtm 10 examples-audio)
  extempore_add_example_as_test(examples/external/gl-compatibility.xtm 10 examples-graphics)
  # extempore_add_example_as_test(examples/external/going-native.xtm 60 examples-graphics) # broken for now
  extempore_add_example_as_test(examples/external/granulator.xtm 10 examples-audio)
  extempore_add_example_as_test(examples/external/openvg.xtm 10 examples-graphics)
  extempore_add_example_as_test(examples/external/portmidi-output.xtm 10 examples-audio) # no audio output, but sends MIDI messages
  extempore_add_example_as_test(examples/external/portmidi.xtm 10 examples-audio)        # no audio output, but sends MIDI messages
  extempore_add_example_as_test(examples/external/raymarcher.xtm 10 examples-graphics)
  extempore_add_example_as_test(examples/external/sampler.xtm 10 examples-audio)
  extempore_add_example_as_test(examples/external/shader-tutorials/arrows.xtm 10 examples-graphics)
  extempore_add_example_as_test(examples/external/shader-tutorials/framebuffer.xtm 10 examples-graphics)
  extempore_add_example_as_test(examples/external/shader-tutorials/heatmap.xtm 10 examples-graphics)
  extempore_add_example_as_test(examples/external/shader-tutorials/particles.xtm 10 examples-graphics)
  extempore_add_example_as_test(examples/external/shader-tutorials/points.xtm 10 examples-graphics)
  extempore_add_example_as_test(examples/external/shader-tutorials/shadertoy.xtm 10 examples-graphics)
  extempore_add_example_as_test(examples/external/shader-tutorials/simple-triangle.xtm 10 examples-graphics)
  extempore_add_example_as_test(examples/external/shader-tutorials/texture.xtm 10 examples-graphics)
  extempore_add_example_as_test(examples/external/shader-tutorials/triangle.xtm 10 examples-graphics)
  extempore_add_example_as_test(examples/external/sing_a_song.xtm 10 examples-audio)
  extempore_add_example_as_test(examples/external/spectrogram.xtm 10 examples-graphics) # contains audio as well
  extempore_add_example_as_test(examples/external/xtmrender1.xtm 10 examples-graphics)
  extempore_add_example_as_test(examples/external/xtmrender2.xtm 10 examples-graphics)
  extempore_add_example_as_test(examples/external/xtmrender3.xtm 10 examples-graphics)
  extempore_add_example_as_test(examples/external/xtmrender4.xtm 10 examples-graphics)

endif(BUILD_TESTS)

##########
# xtmdoc #
##########

add_custom_target(xtmdoc
  COMMAND extempore
           --port 17095
           --eval "(begin (sys:load \"libs/core/audio_dsp.xtm\") (sys:load \"libs/core/instruments.xtm\") (sys:load \"libs/core/math.xtm\") (sys:load \"libs/base/base.xtm\") (sys:load \"libs/external/fft.xtm\") (sys:load \"libs/external/gl.xtm\") (sys:load \"libs/external/glfw3.xtm\") (sys:load \"libs/external/instruments_ext.xtm\") (sys:load \"libs/external/nanovg.xtm\") (sys:load \"libs/external/sndfile.xtm\") (sys:load \"libs/external/stb_image.xtm\") (xtmdoc-export-caches-to-json \"/tmp/xtmdoc.json\" #f) (quit 0))"
  COMMENT "Generating xtmdoc output in /tmp/xtmdoc.json"
  VERBATIM)

add_dependencies(xtmdoc extempore)

#########
# cpack #
#########

# cpack is cmake's tool for providing distributable
# binaries/installers on various platforms.

set(CPACK_PACKAGE_NAME "Extempore")
set(CPACK_PACKAGE_VENDOR "Andrew Sorensen")
set(CPACK_PACKAGE_CONTACT "Ben Swift")
set(CPACK_PACKAGE_VERSION_MAJOR ${PROJECT_VERSION_MAJOR})
set(CPACK_PACKAGE_VERSION_MINOR ${PROJECT_VERSION_MINOR})
set(CPACK_PACKAGE_VERSION_PATCH ${PROJECT_VERSION_PATCH})
string(TIMESTAMP EXTEMPORE_BUILD_DATE "%Y%m%d")

# zipball will be called extempore.zip
set(CPACK_PACKAGE_FILE_NAME extempore)
set(CPACK_GENERATOR ZIP)

set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "The Extempore programming environment (https://extemporelang.github.io)")


include(CPack)
