cmake_minimum_required(VERSION 3.1) # we use target_sources()

option(BOOST "use boost library" OFF)
option(ASIO "use the ASIO Portaudio backend" OFF)
option(JACK "use the Jack Portaudio backend" OFF)
option(PACKAGE "set up install targets for packaging" OFF)
option(BUILD_DEPS "download & build deps for aot_extended" ON)

####################
# option wrangling #
####################

# share directory

# if -DEXT_SHARE_DIR=/path/to/share-dir is provided at the command
# line it will override these values

# packaging (binary distribution)

if(PACKAGE)
  # this needs to be set before project() is called
  set(CMAKE_OSX_SYSROOT macosx)
  set(CMAKE_OSX_DEPLOYMENT_TARGET 10.9)
  message(STATUS "Building Extempore for binary distribution")
  # need deps if we're packaging
  set(BUILD_DEPS ON CACHE BOOL "" FORCE)
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

if(BUILD_DEPS)
  set(EXT_DEPS_INSTALL_DIR ${CMAKE_BINARY_DIR}/deps-install)
  set(EXT_PLATFORM_SHLIBS_DIR ${CMAKE_SOURCE_DIR}/libs/platform-shlibs)
  if(PACKAGE)
    set(EXT_DEPS_C_FLAGS "${CMAKE_C_FLAGS_RELEASE} -mtune=generic")
    set(EXT_DEPS_CXX_FLAGS "${CMAKE_CXX_FLAGS_RELEASE} -mtune=generic")
    message(STATUS "compiler flags for packaging:\nC    ${EXT_DEPS_C_FLAGS}\nCXX  ${EXT_DEPS_CXX_FLAGS}")
  endif()
endif()

project(Extempore VERSION 0.7.0)

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

if(NOT LLVM_BUILD_TYPE)
  message(STATUS "Building 'Release' LLVM configuration")
  set(LLVM_BUILD_TYPE Release CACHE STRING "Choose the type of build." FORCE)
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
  string(REGEX MATCH "^10.[0-9]+" EXTEMPORE_SYSTEM_VERSION ${EXTEMPORE_SYSTEM_VERSION})
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
  message(FATAL_ERROR "Sorry, Extempore isn't supported on this platform - OSX, Linux & Windows only.")
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
  target_compile_options(pcre
    PRIVATE -mtune=generic)
endif()

#############
# portaudio #
#############

add_library(portaudio STATIC
  src/portaudio/src/common/pa_allocation.c
  src/portaudio/src/common/pa_allocation.h
  src/portaudio/src/common/pa_converters.c
  src/portaudio/src/common/pa_converters.h
  src/portaudio/src/common/pa_cpuload.c
  src/portaudio/src/common/pa_cpuload.h
  src/portaudio/src/common/pa_debugprint.c
  src/portaudio/src/common/pa_debugprint.h
  src/portaudio/src/common/pa_dither.c
  src/portaudio/src/common/pa_dither.h
  src/portaudio/src/common/pa_endianness.h
  src/portaudio/src/common/pa_front.c
  src/portaudio/src/common/pa_hostapi.h
  src/portaudio/src/common/pa_memorybarrier.h
  src/portaudio/src/common/pa_process.c
  src/portaudio/src/common/pa_process.h
  src/portaudio/src/common/pa_ringbuffer.c
  src/portaudio/src/common/pa_ringbuffer.h
  src/portaudio/src/common/pa_stream.c
  src/portaudio/src/common/pa_stream.h
  src/portaudio/src/common/pa_trace.c
  src/portaudio/src/common/pa_trace.h
  src/portaudio/src/common/pa_types.h
  src/portaudio/src/common/pa_util.h
  )

target_include_directories(portaudio
  PRIVATE src/portaudio/include
  PRIVATE src/portaudio/src/common)

# platform-specific

if(APPLE)
  # use CoreAudio on OSX
  target_sources(portaudio
    PRIVATE src/portaudio/src/hostapi/coreaudio/pa_mac_core.c
    PRIVATE src/portaudio/src/hostapi/coreaudio/pa_mac_core_blocking.c
    # PRIVATE src/portaudio/src/hostapi/coreaudio/pa_mac_core_old.c
    PRIVATE src/portaudio/src/hostapi/coreaudio/pa_mac_core_utilities.c
    PRIVATE src/portaudio/src/os/unix/pa_unix_hostapis.c
    PRIVATE src/portaudio/src/os/unix/pa_unix_util.c
    )
  target_include_directories(portaudio
    PRIVATE src/portaudio/src/os/unix)
  target_compile_definitions(portaudio
    PRIVATE -DPA_USE_COREAUDIO)
  target_compile_options(portaudio
    PRIVATE -Wno-deprecated-declarations)
  target_link_libraries(portaudio
    PRIVATE "-framework CoreAudio"
    PRIVATE "-framework AudioUnit"
    PRIVATE "-framework AudioToolbox")

elseif(UNIX AND NOT APPLE)
  # use ALSA on Linux
  target_sources(portaudio
    PRIVATE src/portaudio/src/hostapi/alsa/pa_linux_alsa.c
    PRIVATE src/portaudio/src/os/unix/pa_unix_hostapis.c
    PRIVATE src/portaudio/src/os/unix/pa_unix_util.c
    )
  target_include_directories(portaudio
    PRIVATE src/portaudio/src/os/unix)
  target_compile_definitions(portaudio
    PRIVATE -DPA_USE_ALSA)
  target_link_libraries(portaudio
    PRIVATE asound)

  if(JACK)
    target_sources(portaudio
      PRIVATE src/portaudio/src/hostapi/jack/pa_jack.c)
    target_compile_definitions(portaudio
      PRIVATE -DPA_USE_JACK)
    target_link_libraries(portaudio
      PRIVATE jack)
  endif()

elseif(WIN32)
  # use everything except for ASIO on Windows by default, but you can
  # use asio with the cmake ASIO option (see below)
  target_sources(portaudio
    PRIVATE src/portaudio/src/hostapi/wdmks/pa_win_wdmks.c
    # PRIVATE src/portaudio/src/hostapi/wmme/pa_win_wmme.c
    PRIVATE src/portaudio/src/hostapi/dsound/pa_win_ds.c
    PRIVATE src/portaudio/src/hostapi/dsound/pa_win_ds_dynlink.c
    # PRIVATE src/portaudio/src/hostapi/wasapi/pa_win_wasapi.c
    PRIVATE src/portaudio/src/os/win/pa_win_coinitialize.c
    PRIVATE src/portaudio/src/os/win/pa_win_hostapis.c
    PRIVATE src/portaudio/src/os/win/pa_win_util.c
    PRIVATE src/portaudio/src/os/win/pa_win_waveformat.c
    PRIVATE src/portaudio/src/os/win/pa_win_wdmks_utils.c
    PRIVATE src/portaudio/src/os/win/pa_x86_plain_converters.c
    )
  target_include_directories(portaudio
    PRIVATE src/portaudio/src/os/win)
  target_compile_definitions(portaudio
    PRIVATE -DPA_USE_WDMKS
    # PRIVATE -DPA_USE_WMME
    # PRIVATE -DPA_USE_WASAPI
    PRIVATE -DPA_USE_DS)
endif()

if(ASIO)
  # if you want to build with ASIO on Windows, you need the ASIO SDK.
  # See src/portaudio/src/hostapi/asio/ASIO-README.txt for
  # instructions on how to get it

  # then, copy it into the src/portaudio/src/hostapi/asio directory in
  # the Extempore source folder
  target_sources(portaudio
    PRIVATE src/portaudio/src/hostapi/asio/pa_asio.cpp
    PRIVATE src/portaudio/src/hostapi/asio/ASIOSDK/common/asio.cpp
    PRIVATE src/portaudio/src/hostapi/asio/ASIOSDK/host/asiodrivers.cpp
    PRIVATE src/portaudio/src/hostapi/asio/ASIOSDK/host/pc/asiolist.cpp
    )
  target_include_directories(portaudio
    PRIVATE src/portaudio/src/hostapi/asio/ASIOSDK/common
    PRIVATE src/portaudio/src/hostapi/asio/ASIOSDK/host
    PRIVATE src/portaudio/src/hostapi/asio/ASIOSDK/host/pc)
  target_compile_definitions(portaudio
    PRIVATE -DPA_USE_ASIO)
endif()

if(PACKAGE)
  target_compile_options(portaudio
    PRIVATE -mtune=generic)
endif()

##############
# LLVM 3.8.0 #
##############

# if you need to build LLVM by hand, the command will be something like
# cmake .. -DLLVM_TARGETS_TO_BUILD=X86 -DCMAKE_BUILD_TYPE=Release -DLLVM_ENABLE_TERMINFO=OFF -DLLVM_ENABLE_ZLIB=OFF -DCMAKE_INSTALL_PREFIX=c:/Users/ben/Code/extempore/llvm-3.8.0-release

if(BUILD_LLVM)
  include(ExternalProject)
  find_program(PATCH_PROGRAM patch)

  if(NOT PATCH_PROGRAM)
    message(WARNING "Cannot find \"patch.exe\", which is required to patch LLVM")
  endif()

  if(PACKAGE)
    ExternalProject_Add(LLVM
      PREFIX llvm
      URL http://www.llvm.org/releases/3.8.0/llvm-3.8.0.src.tar.xz
      URL_MD5 07a7a74f3c6bd65de4702bf941b511a0
      PATCH_COMMAND patch -p0 < ${CMAKE_SOURCE_DIR}/extras/extempore-llvm-3.8.0.patch
      CMAKE_ARGS
      -DCMAKE_OSX_SYSROOT=macosx
      -DCMAKE_OSX_DEPLOYMENT_TARGET=10.9
      -DLLVM_TARGETS_TO_BUILD=X86
      -DCMAKE_BUILD_TYPE=${LLVM_BUILD_TYPE}
      -DLLVM_ENABLE_TERMINFO=OFF
      -DLLVM_ENABLE_ZLIB=OFF
      -DLLVM_INCLUDE_UTILS=OFF
      -DLLVM_BUILD_RUNTIME=OFF
      -DLLVM_INCLUDE_EXAMPLES=OFF
      -DLLVM_INCLUDE_TESTS=OFF
      -DLLVM_INCLUDE_GO_TESTS=OFF
      -DLLVM_INCLUDE_DOCS=OFF
      -DCMAKE_C_FLAGS=${EXT_DEPS_C_FLAGS}
      -DCMAKE_CXX_FLAGS=${EXT_DEPS_CXX_FLAGS}
      -DCMAKE_INSTALL_PREFIX=${EXT_LLVM_DIR})
  else()
    ExternalProject_Add(LLVM
      PREFIX llvm
      URL http://www.llvm.org/releases/3.8.0/llvm-3.8.0.src.tar.xz
      URL_MD5 07a7a74f3c6bd65de4702bf941b511a0
      PATCH_COMMAND patch -p0 < ${CMAKE_SOURCE_DIR}/extras/extempore-llvm-3.8.0.patch
      CMAKE_ARGS
      -DLLVM_TARGETS_TO_BUILD=X86
      -DCMAKE_BUILD_TYPE=${LLVM_BUILD_TYPE}
      -DLLVM_ENABLE_TERMINFO=OFF
      -DLLVM_ENABLE_ZLIB=OFF
      -DLLVM_INCLUDE_UTILS=OFF
      -DLLVM_BUILD_RUNTIME=OFF
      -DLLVM_INCLUDE_EXAMPLES=OFF
      -DLLVM_INCLUDE_TESTS=OFF
      -DLLVM_INCLUDE_GO_TESTS=OFF
      -DLLVM_INCLUDE_DOCS=OFF
      -DCMAKE_INSTALL_PREFIX=${EXT_LLVM_DIR})
  endif()
  if(WIN32)
    add_custom_target(LLVM_install
      COMMENT "This is necessary to get CMake to install LLVM (needed to build Extempore) before the Extempore install step"
      DEPENDS LLVM
      COMMAND ${CMAKE_COMMAND} -DBUILD_TYPE=${LLVM_BUILD_TYPE} -P ${CMAKE_BINARY_DIR}/llvm/src/LLVM-build/cmake_install.cmake)
  else()
    ExternalProject_Add_StepTargets(LLVM install)
  endif()
endif()

# the ordering of these libs matters, especially with the gcc linker.
# Check the output of "llvm-config --libnames" to be sure
set(EXT_LLVM_LIBRARIES "LLVMLTO;LLVMObjCARCOpts;LLVMSymbolize;LLVMDebugInfoPDB;LLVMDebugInfoDWARF;LLVMMIRParser;LLVMLibDriver;LLVMOption;LLVMTableGen;LLVMOrcJIT;LLVMPasses;LLVMipo;LLVMVectorize;LLVMLinker;LLVMIRReader;LLVMAsmParser;LLVMX86Disassembler;LLVMX86AsmParser;LLVMX86CodeGen;LLVMSelectionDAG;LLVMAsmPrinter;LLVMX86Desc;LLVMMCDisassembler;LLVMX86Info;LLVMX86AsmPrinter;LLVMX86Utils;LLVMMCJIT;LLVMLineEditor;LLVMDebugInfoCodeView;LLVMInterpreter;LLVMExecutionEngine;LLVMRuntimeDyld;LLVMCodeGen;LLVMTarget;LLVMScalarOpts;LLVMInstCombine;LLVMInstrumentation;LLVMProfileData;LLVMObject;LLVMMCParser;LLVMTransformUtils;LLVMMC;LLVMBitWriter;LLVMBitReader;LLVMAnalysis;LLVMCore;LLVMSupport")
foreach(llvm_lib ${EXT_LLVM_LIBRARIES})
  get_filename_component(LLVM_LIB_FULLPATH "${EXT_LLVM_DIR}/lib/${CMAKE_STATIC_LIBRARY_PREFIX}${llvm_lib}${CMAKE_STATIC_LIBRARY_SUFFIX}" ABSOLUTE)
  list(APPEND LLVM_LIBRARIES ${LLVM_LIB_FULLPATH})
endforeach()

#########
# Boost #
#########

# *must* have Boost on Windows
if(WIN32)
  set(BOOST ON)
  if(NOT BOOST_DIR)
    # if BOOST_DIR has been set elsewhere, assume it's all present and
    # correct and go with that. Otherwise, download boost with nuget
    # commandline tool and stick it in ${CMAKE_SOURCE_DIR}/boost
    set(BOOST_DIR ${CMAKE_SOURCE_DIR}/boost)
    # check nuget commandline tool is installed
    find_program(NUGET nuget)
    if(NOT NUGET)
      message(FATAL "Cannot find nuget command line tool.\nEither install it with e.g. choco install nuget.commandline, or get boost some other way and set the -DBOOST_DIR variable")
    else()
      if(NOT IS_DIRECTORY ${BOOST_DIR})
        # if directory exists, assume this step has already been done
        message(STATUS "Downloading boost components into ${CMAKE_SOURCE_DIR}/boost with \"nuget install\"")
        file(MAKE_DIRECTORY ${BOOST_DIR})
        execute_process(
          COMMAND ${NUGET} install -Version 1.59 boost-vc140
          WORKING_DIRECTORY ${BOOST_DIR})
      endif()
    endif()
  endif()
endif()

#############
# extempore #
#############

# source files

add_executable(extempore src/Extempore.cpp
  src/AudioDevice.cpp
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

if(WIN32)
  target_sources(extempore PRIVATE src/extempore.def)
endif()

target_include_directories(extempore PRIVATE include)

# suppress the warning about the opcode switch statement
if(UNIX)
  set_source_files_properties(src/Scheme.cpp PROPERTIES COMPILE_FLAGS -Wno-switch)
endif()

# dependencies

add_dependencies(extempore pcre portaudio)

if(BUILD_LLVM)
  if(WIN32)
    add_dependencies(extempore LLVM_install)
  else()
    add_dependencies(extempore LLVM)
  endif()
endif()

target_include_directories(extempore
  PRIVATE src/pcre
  PRIVATE src/portaudio/include)

target_link_libraries(extempore
  PRIVATE pcre
  PRIVATE portaudio)

target_include_directories(extempore PRIVATE ${EXT_LLVM_DIR}/include)
target_link_libraries(extempore PRIVATE ${LLVM_LIBRARIES})

# compiler options

if(PACKAGE)
  target_compile_definitions(extempore
    PRIVATE -DEXT_SHARE_DIR=".")
  target_compile_options(extempore
    PRIVATE -mtune=generic)
elseif(${EXT_SHARE_DIR})
  target_compile_definitions(extempore
    PRIVATE -DEXT_SHARE_DIR="${EXT_SHARE_DIR}")
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
    PRIVATE -D_CRT_SECURE_NO_WARNINGS)
  set_source_files_properties(
    PROPERTIES
    COMPILE_FLAGS "/EHsc")
elseif(APPLE) # OSX
  # use clang++ by default
  set(CMAKE_C_COMPILER clang)
  set(CMAKE_CXX_COMPILER clang++)
  # tell the compiler about the few ObjC++ source files on OSX
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

if(BOOST)
  target_include_directories(extempore
    PRIVATE ${BOOST_DIR}/boost.1.59.0.0/lib/native/include)
  target_compile_definitions(extempore
    PRIVATE -DEXT_BOOST -DBOOST_EXCEPTION_DISABLE)
  target_link_libraries(extempore
    PRIVATE ${BOOST_DIR}/boost_date_time-vc140.1.59.0.0/lib/native/address-model-64/lib/libboost_date_time-vc140-mt$<$<CONFIG:Debug>:-gd>-1_59.lib
    PRIVATE ${BOOST_DIR}/boost_regex-vc140.1.59.0.0/lib/native/address-model-64/lib/libboost_regex-vc140-mt$<$<CONFIG:Debug>:-gd>-1_59.lib
    PRIVATE ${BOOST_DIR}/boost_system-vc140.1.59.0.0/lib/native/address-model-64/lib/libboost_system-vc140-mt$<$<CONFIG:Debug>:-gd>-1_59.lib)
endif()

##########
# assets #
##########

add_custom_target(assets
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  COMMAND curl -O http://extempore.moso.com.au/extras/assets.tgz
  COMMAND ${CMAKE_COMMAND} -E tar xf assets.tgz
  COMMAND ${CMAKE_COMMAND} -E remove assets.tgz
  COMMENT "Downloading assets from http://extempore.moso.com.au/extras/assets.tgz")

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
    DESTINATION extempore)
  install(DIRECTORY assets runtime libs examples tests
    DESTINATION extempore
    PATTERN ".DS_Store" EXCLUDE)
  install(FILES extras/.emacs extras/extempore.el extras/extempore.vim
    DESTINATION extempore/extras)
endif()

###################
# AOT compilation #
###################

if(WIN32)

  configure_file(
    ${CMAKE_SOURCE_DIR}/extras/cmake/aot.cmake.in
    ${CMAKE_SOURCE_DIR}/extras/cmake/aot.cmake
    @ONLY)

  configure_file(
    ${CMAKE_SOURCE_DIR}/extras/cmake/aot_extended.cmake.in
    ${CMAKE_SOURCE_DIR}/extras/cmake/aot_extended.cmake
    @ONLY)

  add_custom_target(aot
    COMMAND ${CMAKE_COMMAND} -P ${CMAKE_SOURCE_DIR}/extras/cmake/aot.cmake
    COMMENT "Ahead-of-time compiling the standard library..."
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})

  add_custom_target(aot_extended ALL
    COMMAND ${CMAKE_COMMAND} -P ${CMAKE_SOURCE_DIR}/extras/cmake/aot_extended.cmake
    COMMENT "Ahead-of-time compiling the standard library..."
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})

else(WIN32)

  # this approach requires specifying the inter-lib dependencies by hand, but allows us to do AOT compilation in parallell

  set(EXTEMPORE_AOT_COMPILE_PORT 17099)

  macro(aotcompile_lib libfile group) # deps are optional, and go at the end
    get_filename_component(basename ${libfile} NAME_WE)
    set(targetname aot_${basename})
    if(PACKAGE)
      add_custom_target(${targetname}
        COMMAND extempore --nobase --noaudio --mcpu=generic --attr=none --port=${EXTEMPORE_AOT_COMPILE_PORT}
        --eval "(impc:aot:compile-xtm-file \"${libfile}\")"
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
        VERBATIM)
    else(PACKAGE)
      add_custom_target(${targetname}
        COMMAND extempore --nobase --noaudio --port=${EXTEMPORE_AOT_COMPILE_PORT}
        --eval "(impc:aot:compile-xtm-file \"${libfile}\")"
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
        VERBATIM)
    endif(PACKAGE)
    add_dependencies(${targetname} extempore extended_deps)
    add_dependencies(${group} ${targetname})
    foreach(dep ${ARGN})
      add_dependencies(${targetname} aot_${dep})
    endforeach()
    # decrement port number by 2
    math(EXPR EXTEMPORE_AOT_COMPILE_PORT "${EXTEMPORE_AOT_COMPILE_PORT} - 2")
  endmacro(aotcompile_lib)

  # core
  add_custom_target(aot)
  aotcompile_lib(libs/base/base.xtm aot) # no lib dependency for base.xtm
  aotcompile_lib(libs/core/math.xtm aot base)
  aotcompile_lib(libs/core/audiobuffer.xtm aot base)
  aotcompile_lib(libs/core/audio_dsp.xtm aot base math audiobuffer)
  aotcompile_lib(libs/core/instruments.xtm aot base math audio_dsp)

  # extended
  add_custom_target(aot_extended ALL)
  aotcompile_lib(libs/external/fft.xtm aot_extended base math)
  aotcompile_lib(libs/external/sndfile.xtm aot_extended base)
  aotcompile_lib(libs/external/audio_dsp_ext.xtm aot_extended base fft sndfile)
  aotcompile_lib(libs/external/instruments_ext.xtm aot_extended base sndfile instruments)
  aotcompile_lib(libs/external/portmidi.xtm aot_extended base)
  aotcompile_lib(libs/external/stb_image.xtm aot_extended base)
  aotcompile_lib(libs/external/glfw3.xtm aot_extended base)
  if(WIN32 OR DEFINED ENV{EXTEMPORE_FORCE_GL_GETPROCADDRESS})
    set(GL_BIND_METHOD getprocaddress)
  else()
    set(GL_BIND_METHOD directbind)
  endif()
  aotcompile_lib(libs/external/gl/glcore-${GL_BIND_METHOD}.xtm aot_extended base)
  aotcompile_lib(libs/external/gl/gl-objects.xtm aot_extended base glcore-${GL_BIND_METHOD} stb_image)
  aotcompile_lib(libs/external/nanovg.xtm aot_extended base glcore-${GL_BIND_METHOD})
  aotcompile_lib(libs/external/gl/glcompat-${GL_BIND_METHOD}.xtm aot_extended base)
  aotcompile_lib(libs/external/graphics-pipeline.xtm aot_extended base glcompat-${GL_BIND_METHOD})
  if(NOT WIN32)
    # these ones don't currently work on Windows
    # aotcompile_lib(libs/external/glib.xtm aot_extended base)
    aotcompile_lib(libs/external/assimp.xtm aot_extended base stb_image graphics-pipeline)
  endif()

endif(WIN32)

add_dependencies(aot extempore)
add_dependencies(aot_extended extempore)

# uninstall only AOT-compiled libs
add_custom_target(clean_aot
  COMMAND ${CMAKE_COMMAND} -E remove_directory ${CMAKE_SOURCE_DIR}/libs/aot-cache
  COMMENT "Removing AOT-compiled libs")

#########################
# deps for AOT-extended #
#########################

if(BUILD_DEPS)

  include(ExternalProject)

  ExternalProject_Add(portmidi
    PREFIX portmidi
    URL https://github.com/extemporelang/portmidi/archive/217.zip
    CMAKE_ARGS
    -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
    -DCMAKE_C_FLAGS=${EXT_DEPS_C_FLAGS}
    -DCMAKE_CXX_FLAGS=${EXT_DEPS_CXX_FLAGS}
    -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR})

  ExternalProject_Add(nanovg
    PREFIX nanovg
    URL https://github.com/extemporelang/nanovg/archive/0.7.1.zip
    CMAKE_ARGS
    -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
    -DCMAKE_C_FLAGS=${EXT_DEPS_C_FLAGS}
    -DCMAKE_CXX_FLAGS=${EXT_DEPS_CXX_FLAGS}
    -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR})

  ExternalProject_Add(kiss_fft
    PREFIX kiss_fft
    URL https://github.com/extemporelang/kiss_fft/archive/master.zip
    CMAKE_ARGS
    -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
    -DCMAKE_C_FLAGS=${EXT_DEPS_C_FLAGS}
    -DCMAKE_CXX_FLAGS=${EXT_DEPS_CXX_FLAGS}
    -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR})

  ExternalProject_Add(stb_image
    PREFIX stb_image
    URL https://github.com/extemporelang/stb/archive/master.zip
    CMAKE_ARGS
    -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
    -DCMAKE_C_FLAGS=${EXT_DEPS_C_FLAGS}
    -DCMAKE_CXX_FLAGS=${EXT_DEPS_CXX_FLAGS}
    -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR})

  ExternalProject_Add(glfw3
    PREFIX glfw3
    URL https://github.com/glfw/glfw/archive/3.2.zip
    CMAKE_ARGS
    -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
    -DCMAKE_C_FLAGS=${EXT_DEPS_C_FLAGS}
    -DCMAKE_CXX_FLAGS=${EXT_DEPS_CXX_FLAGS}
    -DBUILD_SHARED_LIBS=ON
    -DGLFW_BUILD_EXAMPLES=OFF
    -DGLFW_BUILD_TESTS=OFF
    -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR})

  ExternalProject_Add(assimp
    PREFIX assimp
    URL https://github.com/assimp/assimp/archive/v3.2.zip
    CMAKE_ARGS
    -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
    -DCMAKE_C_FLAGS=${EXT_DEPS_C_FLAGS}
    -DCMAKE_CXX_FLAGS=${EXT_DEPS_CXX_FLAGS}
    -DASSIMP_BUILD_ASSIMP_TOOLS=OFF
    -DASSIMP_BUILD_SAMPLES=OFF
    -DASSIMP_BUILD_TESTS=OFF
    -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR})

  if(WIN32)
    # on Windows, we need expat for apr
    ExternalProject_Add(expat
      PREFIX expat
      URL http://downloads.sourceforge.net/project/expat/expat/2.1.0/expat-2.1.0.tar.gz
      CMAKE_ARGS
      -DBUILD_tools=OFF
      -DBUILD_examples=OFF
      -DBUILD_tests=OFF
      -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
      -DCMAKE_C_FLAGS=${EXT_DEPS_C_FLAGS}
      -DCMAKE_CXX_FLAGS=${EXT_DEPS_CXX_FLAGS}
      -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR})

    # need to tell CMake where to find libexpat
    set(CMAKE_MODULE_PATH ${EXT_DEPS_INSTALL_DIR})
    ExternalProject_Add(apr
      PREFIX apr
      URL http://mirror.ventraip.net.au/apache/apr/apr-1.5.2.tar.gz
      CMAKE_ARGS
      -DAPR_HAVE_ODBC=OFF
      -DINSTALL_PDB=OFF
      -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
      -DCMAKE_C_FLAGS=${EXT_DEPS_C_FLAGS}
      -DCMAKE_CXX_FLAGS=${EXT_DEPS_CXX_FLAGS}
      -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR})

    add_dependencies(apr expat)

    # aargh! sndfile on Windows still requires sh, sed, etc. and
    # doesn't work. so we'll just grab the pre-built one manually. for
    # the moment I've just hosted the (unmodified) libsndfile binaries
    # (a bit of a hack), but you can get an installer from
    # http://www.mega-nerd.com/libsndfile/#Download
    file(DOWNLOAD https://dl.dropboxusercontent.com/u/18333720/libsndfile-1.dll ${EXT_PLATFORM_SHLIBS_DIR}/libsndfile-1.dll)
    file(DOWNLOAD https://dl.dropboxusercontent.com/u/18333720/libsndfile-1.lib ${EXT_PLATFORM_SHLIBS_DIR}/libsndfile-1.lib)

    # ExternalProject_Add(sndfile
    #   PREFIX libsndfile
    #   URL https://github.com/erikd/libsndfile/archive/master.zip
    #   CONFIGURE_COMMAND ${CMAKE_BINARY_DIR}/libsndfile/src/sndfile/autogen.sh cmake && ${CMAKE_COMMAND} ${CMAKE_SOURCE_DIR}
    #   CMAKE_ARGS
    #   -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
    #   -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR})
  else()
    # build with as few deps as we can get away with
    ExternalProject_Add(sndfile
      PREFIX libsndfile
      URL http://www.mega-nerd.com/libsndfile/files/libsndfile-1.0.26.tar.gz
      CONFIGURE_COMMAND sh configure --prefix=${EXT_DEPS_INSTALL_DIR} --disable-static --disable-cpu-clip --disable-external-libs --disable-sqlite --disable-octave
      BUILD_COMMAND $(MAKE) CFLAGS=${EXT_DEPS_C_FLAGS}
      INSTALL_COMMAND make install
      BUILD_IN_SOURCE ON)

    ExternalProject_Add(apr
      PREFIX apr
      URL http://mirror.ventraip.net.au/apache/apr/apr-1.5.2.tar.gz
      CONFIGURE_COMMAND sh configure --prefix=${EXT_DEPS_INSTALL_DIR} --enable-threads --disable-static
      BUILD_COMMAND $(MAKE)
      INSTALL_COMMAND make install
      BUILD_IN_SOURCE ON)
  endif()

  # wrap the whole thing in a target

  if(UNIX)
    add_custom_target(extended_deps
      COMMENT "moving shared libs into ${EXT_PLATFORM_SHLIBS_DIR}"
      DEPENDS sndfile assimp glfw3 stb_image kiss_fft nanovg portmidi LLVM sndfile apr
      COMMAND ${CMAKE_COMMAND} -E make_directory ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy libassimp${CMAKE_SHARED_LIBRARY_SUFFIX} ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy libkiss_fft${CMAKE_SHARED_LIBRARY_SUFFIX} ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy libnanovg${CMAKE_SHARED_LIBRARY_SUFFIX} ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy libportmidi${CMAKE_SHARED_LIBRARY_SUFFIX} ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy libsndfile${CMAKE_SHARED_LIBRARY_SUFFIX} ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy libstb_image${CMAKE_SHARED_LIBRARY_SUFFIX} ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy libglfw${CMAKE_SHARED_LIBRARY_SUFFIX} ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy libapr-1${CMAKE_SHARED_LIBRARY_SUFFIX} ${EXT_PLATFORM_SHLIBS_DIR}
      WORKING_DIRECTORY ${EXT_DEPS_INSTALL_DIR}/lib)
  elseif(WIN32)
    add_custom_target(extended_deps
      COMMENT "moving .dll and .lib files into ${EXT_PLATFORM_SHLIBS_DIR}"
      DEPENDS sndfile assimp glfw3 stb_image kiss_fft nanovg portmidi LLVM sndfile apr
      COMMAND ${CMAKE_COMMAND} -E make_directory ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy bin/assimp-vc130-mt.dll ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy lib/kiss_fft.dll ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy lib/nanovg.dll ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy lib/portmidi.dll ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy lib/stb_image.dll ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy lib/glfw3.dll ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy bin/libapr-1.dll ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy lib/assimp-vc130-mt.lib ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy lib/kiss_fft.lib ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy lib/nanovg.lib ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy lib/portmidi.lib ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND ${CMAKE_COMMAND} -E copy lib/stb_image.lib ${EXT_PLATFORM_SHLIBS_DIR}
      # glfw3 has different base names for the .dll and .lib, so fix that
      COMMAND ${CMAKE_COMMAND} -E copy lib/glfw3dll.lib ${EXT_PLATFORM_SHLIBS_DIR}/glfw3.lib
      COMMAND ${CMAKE_COMMAND} -E copy lib/libapr-1.lib ${EXT_PLATFORM_SHLIBS_DIR}
      WORKING_DIRECTORY ${EXT_DEPS_INSTALL_DIR})
  endif()

add_dependencies(aot_extended extended_deps)

endif(BUILD_DEPS)

#########
# tests #
#########

option(BUILD_TESTS "build test targets (including examples)" ON)

if(BUILD_TESTS)

  include(CTest)

  set(EXTEMPORE_TEST_PORT 17099)

  macro(extempore_add_test testfile label)
    add_test(NAME ${testfile}
      COMMAND extempore --term nocolor --port=${EXTEMPORE_TEST_PORT} --eval "(xtmtest-run-tests \"${testfile}\" #t #t)")
    set_tests_properties(${testfile}
      PROPERTIES
      WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
      LABELS ${label})
    # decrement port number by 2
    math(EXPR EXTEMPORE_TEST_PORT "${EXTEMPORE_TEST_PORT} - 2")
  endmacro()

  macro(extempore_add_example_as_test examplefile timeout label)
    add_test(NAME ${examplefile}
      COMMAND extempore --term nocolor --port=${EXTEMPORE_TEST_PORT} --eval "(sys:load-then-quit \"${examplefile}\" ${timeout})")
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
  extempore_add_example_as_test(examples/core/fasta_lang_shootout.xtm 10 examples-core)
  extempore_add_example_as_test(examples/core/fmsynth.xtm 10 examples-audio)
  extempore_add_example_as_test(examples/core/midi_synth.xtm 10 examples-audio)
  extempore_add_example_as_test(examples/core/mtaudio.xtm 10 examples-audio)
  extempore_add_example_as_test(examples/core/nbody_lang_shootout.xtm 10 examples-core)
  extempore_add_example_as_test(examples/core/osc_101.xtm 10 examples-core)
  extempore_add_example_as_test(examples/core/polysynth.xtm 10 examples-audio)
  extempore_add_example_as_test(examples/core/scheduler.xtm 10 examples-audio)
  extempore_add_example_as_test(examples/core/topclock_metro.xtm 10 examples-audio)
  extempore_add_example_as_test(examples/core/typeclasses.xtm 10 examples-core)
  extempore_add_example_as_test(examples/core/xthread.xtm 10 examples-core)
  # examples - external
  extempore_add_example_as_test(examples/external/audio_player.xtm 10 examples-audio)
  extempore_add_example_as_test(examples/external/convolution_reverb.xtm 10 examples-audio)
  extempore_add_example_as_test(examples/external/electrofunk.xtm 10 examples-audio)
  extempore_add_example_as_test(examples/external/gl-compatibility.xtm 10 examples-graphics)
  extempore_add_example_as_test(examples/external/going-native.xtm 60 examples-graphics)
  extempore_add_example_as_test(examples/external/granulator.xtm 10 examples-audio)
  extempore_add_example_as_test(examples/external/gui.xtm 10 examples-graphics)
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

# give the zipball a nice name on each platform
if(APPLE)
  set(CPACK_PACKAGE_FILE_NAME Extempore-${PROJECT_VERSION}-osx10.9+)
elseif(UNIX)
  set(CPACK_PACKAGE_FILE_NAME Extempore-${PROJECT_VERSION}-${EXTEMPORE_SYSTEM_NAME})
elseif(WIN32)
  set(CPACK_PACKAGE_FILE_NAME Extempore-${PROJECT_VERSION}-win${EXTEMPORE_SYSTEM_VERSION})
endif()

set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "The Extempore programming environment (http://extempore.moso.com.au)")

if(APPLE)
  set(CPACK_GENERATOR DragNDrop)
elseif(UNIX AND NOT APPLE)
  set(CPACK_GENERATOR TGZ)
elseif(WIN32)
  set(CPACK_GENERATOR ZIP)
  # set(CPACK_GENERATOR NSIS64)
endif()

include(CPack)
