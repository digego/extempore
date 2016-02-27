cmake_minimum_required(VERSION 3.1) # we use target_sources()

option(MCJIT "use LLVM's MCJIT for codegen" ON)
option(BOOST "use boost library" OFF)
option(IN_TREE "use the Extempore source dir as EXT_SHARE_DIR - useful for developers" ON)
option(ASIO "use the ASIO Portaudio backend" OFF)
option(JACK "use the Jack Portaudio backend" OFF)
option(DOWNLOAD_LLVM "download and build LLVM (patched for Extempore)" ON)
option(PACKAGE "set up install targets for packaging" OFF)
option(EXTENDED_DEPS "download & build deps for aot_extended" OFF)

if(PACKAGE)
  # this needs to be set before project() is called
  set(CMAKE_OSX_SYSROOT macosx)
  set(CMAKE_OSX_DEPLOYMENT_TARGET 10.9)
  message(STATUS "Building Extempore for binary distribution")
  # need deps if we're packaging
  set(EXTENDED_DEPS ON CACHE BOOL "" FORCE)
  if(IN_TREE)
    set(IN_TREE OFF CACHE BOOL "use the Extempore source dir as EXT_SHARE_DIR - useful for developers" FORCE)
  endif()
endif()

project(Extempore VERSION 0.6.1)
if(NOT ${CMAKE_SIZEOF_VOID_P} EQUAL 8)
  message(FATAL_ERROR "Extempore currently only runs on 64-bit platforms.")
endif()

# if -DEXT_SHARE_DIR=/path/to/share-dir is provided at the command
# line it will override these values

if(WIN32)
  # Windows doesn't really do "installation", so we'll assume that
  # extempore.exe's always going to be run from the Extempore
  # directory
  set(EXT_SHARE_DIR "." CACHE PATH
    "path to Extempore's shared runtime files")
else()
  if(IN_TREE)
    set(EXT_SHARE_DIR "${CMAKE_SOURCE_DIR}" CACHE PATH
      "path to Extempore's shared runtime files")
  else()
    set(EXT_SHARE_DIR "${CMAKE_INSTALL_PREFIX}/share/extempore" CACHE PATH
      "path to Extempore's shared runtime files")
  endif()
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
# LLVM 3.7.0 #
##############

# if you need to build LLVM by hand, the command will be something like
# cmake .. -DLLVM_TARGETS_TO_BUILD=X86 -DCMAKE_BUILD_TYPE=Release -DLLVM_ENABLE_TERMINFO=OFF -DLLVM_ENABLE_ZLIB=OFF -DCMAKE_INSTALL_PREFIX=c:/Users/ben/Code/extempore/llvm-3.8.0rc2-release

if(PACKAGE)
  # when packaging, we'll build LLVM a bit differently (see the "deps
  # for AOT-extended" section further down in this file)
  set(EXT_LLVM_DIR ${CMAKE_BINARY_DIR}/deps-install)
  set(DOWNLOAD_LLVM OFF)
elseif(DEFINED ENV{EXT_LLVM_DIR})
# if there's an environment variable, use that
  set(EXT_LLVM_DIR $ENV{EXT_LLVM_DIR})
  set(DOWNLOAD_LLVM OFF)
else()
  if(WIN32)
    set(EXT_LLVM_DIR ${CMAKE_SOURCE_DIR}/llvm)
  else()
    set(EXT_LLVM_DIR ${EXT_SHARE_DIR}/llvm)
  endif()
endif()

# assume that if we can find llvm-config, we already have a working LLVM install
find_program(LLVM_CONFIG llvm-config
  ${EXT_LLVM_DIR}/bin
  NO_DEFAULT_PATH)

if(LLVM_CONFIG)
  set(DOWNLOAD_LLVM OFF)
endif()

# if we're gonna download and build LLVM, do it here

if(DOWNLOAD_LLVM)
  message(STATUS "Downloading and building LLVM (patched for Extempore)")

  include(ExternalProject)

  if(WIN32)
    if(DEFINED PYTHON_EXECUTABLE)
      message(STATUS "building LLVM using using python at ${PYTHON_EXECUTABLE}")
      ExternalProject_Add(LLVM
        PREFIX llvm
        URL http://extempore.moso.com.au/extras/extempore-llvm-3.7.0.src.zip
        URL_MD5 a2fb39e154f62c1731d32f0c789d8f99
        CMAKE_ARGS
        -DLLVM_TARGETS_TO_BUILD=X86
        -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
        -DLLVM_ENABLE_TERMINFO=OFF
        -DLLVM_ENABLE_ZLIB=OFF
        -DCMAKE_INSTALL_PREFIX=${EXT_LLVM_DIR}
        -DPYTHON_EXECUTABLE=${PYTHON_EXECUTABLE})
    else()
      ExternalProject_Add(LLVM
        PREFIX llvm
        URL http://extempore.moso.com.au/extras/extempore-llvm-3.7.0.src.zip
        URL_MD5 a2fb39e154f62c1731d32f0c789d8f99
        CMAKE_ARGS
        CMAKE_ARGS
        -DLLVM_TARGETS_TO_BUILD=X86
        -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
        -DLLVM_ENABLE_TERMINFO=OFF
        -DLLVM_ENABLE_ZLIB=OFF
        -DCMAKE_INSTALL_PREFIX=${EXT_LLVM_DIR})
    endif()
  else()
    find_program(PATCH_PROGRAM patch)

    if(NOT PATCH_PROGRAM)
      message(FATAL_ERROR "Cannot find \"patch\" to patch LLVM")
    endif()

    ExternalProject_Add(LLVM
      PREFIX llvm
      URL http://www.llvm.org/releases/3.7.0/llvm-3.7.0.src.tar.xz
      URL_MD5 b98b9495e5655a672d6cb83e1a180f8e
      PATCH_COMMAND patch -p0 < ${CMAKE_SOURCE_DIR}/extras/extempore-llvm-3.7.0.patch
      CMAKE_ARGS
      -DLLVM_TARGETS_TO_BUILD=X86
      -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
      -DLLVM_ENABLE_TERMINFO=OFF
      -DLLVM_ENABLE_ZLIB=OFF
      -DCMAKE_INSTALL_PREFIX=${EXT_LLVM_DIR})
  endif()
endif()

# the ordering of these libs matters, especially with the gcc linker.
# Check the output of "llvm-config --libnames" to be sure
set(EXT_LLVM_LIBRARIES "LLVMLTO;LLVMObjCARCOpts;LLVMLinker;LLVMBitWriter;LLVMIRReader;LLVMAsmParser;LLVMOption;LLVMTableGen;LLVMX86Disassembler;LLVMX86AsmParser;LLVMX86CodeGen;LLVMSelectionDAG;LLVMAsmPrinter;LLVMX86Desc;LLVMMCDisassembler;LLVMX86Info;LLVMX86AsmPrinter;LLVMX86Utils;LLVMMCJIT;LLVMipo;LLVMVectorize;LLVMInterpreter;LLVMExecutionEngine;LLVMRuntimeDyld;LLVMCodeGen;LLVMTarget;LLVMScalarOpts;LLVMObject;LLVMMCParser;LLVMBitReader;LLVMInstCombine;LLVMInstrumentation;LLVMTransformUtils;LLVMipa;LLVMMC;LLVMAnalysis;LLVMCore;LLVMSupport")

foreach(llvm_lib ${EXT_LLVM_LIBRARIES})
  list(APPEND LLVM_LIBRARIES "${EXT_LLVM_DIR}/lib/${CMAKE_STATIC_LIBRARY_PREFIX}${llvm_lib}${CMAKE_STATIC_LIBRARY_SUFFIX}")
endforeach()

#########
# Boost #
#########

# *must* have Boost on Windows
if(WIN32)
  set(BOOST ON)
endif()

#############
# extempore #
#############

# source files

add_executable(extempore src/Extempore.cpp
  src/AudioDevice.cpp
  src/EXTCondition.cpp
  src/EXTLLVM.cpp
  src/EXTMonitor.cpp
  src/EXTMutex.cpp
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

if(DOWNLOAD_LLVM)
  add_dependencies(extempore LLVM)
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

if(MCJIT)
  target_compile_definitions(extempore PRIVATE -DEXT_MCJIT)
  message(STATUS "Using the new LLVM MCJIT backend...")
else()
  message(STATUS "Using the old LLVM JIT backend...")
endif()

if(PACKAGE)
  target_compile_definitions(extempore
    PRIVATE -DEXT_SHARE_DIR=".")
  target_compile_options(extempore
    PRIVATE -mtune=generic)
else()
  target_compile_definitions(extempore
    PRIVATE -DEXT_SHARE_DIR="${EXT_SHARE_DIR}")
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

# boost

if(BOOST)
  if(NOT BOOST_DIR)
    message(FATAL_ERROR "BOOST_DIR must be defined")
  endif()
  target_compile_definitions(extempore PRIVATE -DEXT_BOOST)
  # asio header
  find_path(BOOST_ASIO_INCLUDE_DIR
    NAMES boost/asio.hpp
    PATHS ${BOOST_DIR}/include)
  if(BOOST_ASIO_INCLUDE_DIR)
    target_include_directories(extempore PRIVATE ${BOOST_ASIO_INCLUDE_DIR})
  else()
    message(FATAL_ERROR "Cannot find boost/asio.hpp header")
  endif()
  target_link_libraries(extempore
    PRIVATE ${BOOST_DIR}/lib/libboost_date_time-vc140-mt$<$<CONFIG:Debug>:-gd>-1_59.lib
    PRIVATE ${BOOST_DIR}/lib/libboost_regex-vc140-mt$<$<CONFIG:Debug>:-gd>-1_59.lib
    PRIVATE ${BOOST_DIR}/lib/libboost_system-vc140-mt$<$<CONFIG:Debug>:-gd>-1_59.lib)
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

if(IN_TREE)
  # if we're developing in-tree, installation just involves moving the
  # binary into the toplevel source directory
  install(TARGETS extempore
    RUNTIME
    DESTINATION bin)
else()
  # otherwise, install all the things
  if(PACKAGE)
    set(EXT_INSTALL_PREFIX extempore)
    install(TARGETS extempore
      RUNTIME
      DESTINATION ${EXT_INSTALL_PREFIX})
  else()
    set(EXT_INSTALL_PREFIX share/extempore)
    install(TARGETS extempore
      RUNTIME
      DESTINATION bin)
  endif()
  # install the rest of the necessary files into the "share" directory
  install(DIRECTORY runtime libs examples tests extras
    DESTINATION ${EXT_INSTALL_PREFIX}
    PATTERN ".DS_Store" EXCLUDE
    PATTERN "extras/cmake" EXCLUDE
    PATTERN "extras/extempore-emacs-mode" EXCLUDE)
  # assets are optional
  install(DIRECTORY assets
    OPTIONAL
    DESTINATION ${EXT_INSTALL_PREFIX}
    PATTERN ".DS_Store" EXCLUDE)
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
    COMMENT "Ahead-of-time compiling the standard library...")

  add_custom_target(aot_extended
    COMMAND ${CMAKE_COMMAND} -P ${CMAKE_SOURCE_DIR}/extras/cmake/aot_extended.cmake
    COMMENT "Ahead-of-time compiling the standard library...")

else(WIN32)

  # this approach requires specifying the inter-lib dependencies by hand, but allows us to do AOT compilation in parallell

  function(aotcompile_lib lib group port) # deps are optional, and go at the end
    get_filename_component(basename ${lib} NAME_WE)
    set(targetname aot_${basename})
    add_custom_target(${targetname}
      COMMAND extempore --nobase
                        $<$<NOT:$<PLATFORM_ID:Windows>>:--noaudio>
                        $<$<AND:$<PLATFORM_ID:Darwin>,$<BOOL:PACKAGE>>:--mcpu=generic>
                        $<$<BOOL:PACKAGE>:--attr="">
                        $<$<BOOL:PACKAGE>:--sharedir=${EXT_SHARE_DIR}>
                        --port=${port}
                        --eval "(impc:aot:compile-xtm-file \"${lib}\" #t #t)"
      BYPRODUCTS libs/aot-cache/xtm${basename}${CMAKE_SHARED_LIBRARY_SUFFIX}
      $<PLATFORM_ID:Windows>:WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}>
      VERBATIM)
    add_dependencies(${targetname} extempore)
    add_dependencies(${group} ${targetname})
    foreach(dep ${ARGN})
      add_dependencies(${targetname} aot_${dep})
    endforeach()
  endfunction(aotcompile_lib)

  # core
  add_custom_target(aot)
  aotcompile_lib(libs/base/base.xtm aot 17099) # no lib dependency for base.xtm
  aotcompile_lib(libs/core/math.xtm aot 17097 base)
  aotcompile_lib(libs/core/audiobuffer.xtm aot 17087 base)
  aotcompile_lib(libs/core/audio_dsp.xtm aot 17095 base math audiobuffer)
  aotcompile_lib(libs/core/instruments.xtm aot 17093 base math audio_dsp)

  # extended
  add_custom_target(aot_extended)
  aotcompile_lib(libs/external/fft.xtm aot_extended 17091 base math)
  aotcompile_lib(libs/external/sndfile.xtm aot_extended 17089 base)
  aotcompile_lib(libs/external/audio_dsp_ext.xtm aot_extended 17085 base fft sndfile)
  aotcompile_lib(libs/external/instruments_ext.xtm aot_extended 17083 base sndfile instruments)
  aotcompile_lib(libs/external/portmidi.xtm aot_extended 17081 base)
  aotcompile_lib(libs/external/stb_image.xtm aot_extended 17077 base)
  aotcompile_lib(libs/external/glfw3.xtm aot_extended 17075 base)
  if(WIN32)
    set(GL_BIND_METHOD getprocaddress)
  else()
    set(GL_BIND_METHOD directbind)
  endif()
  aotcompile_lib(libs/external/gl/glcore-${GL_BIND_METHOD}.xtm aot_extended 17073 base)
  aotcompile_lib(libs/external/gl/gl-objects.xtm aot_extended 17071 base glcore-${GL_BIND_METHOD} stb_image)
  aotcompile_lib(libs/external/nanovg.xtm aot_extended 17069 base glcore-${GL_BIND_METHOD})
  aotcompile_lib(libs/external/gl/glcompat-${GL_BIND_METHOD}.xtm aot_extended 17067 base)
  aotcompile_lib(libs/external/graphics-pipeline.xtm aot_extended 17065 base glcompat-${GL_BIND_METHOD})
  if(NOT WIN32)
    # these ones don't currently work on Windows
    # aotcompile_lib(libs/external/glib.xtm aot_extended 17079 base)
    aotcompile_lib(libs/external/assimp.xtm aot_extended 17063 base stb_image graphics-pipeline)
  endif()

endif(WIN32)

add_dependencies(aot extempore)
add_dependencies(aot_extended extempore)

# uninstall only AOT-compiled libs
add_custom_target(clean_aot
  COMMAND ${CMAKE_COMMAND} -E remove_directory ${EXT_SHARE_DIR}/libs/aot-cache
  COMMENT "Removing AOT-compiled libs")

#########################
# deps for AOT-extended #
#########################

if(EXTENDED_DEPS)

  set(EXT_DEPS_INSTALL_DIR ${CMAKE_BINARY_DIR}/deps-install)
  if(WIN32)
    set(EXT_PLATFORM_SHLIBS_DIR ${CMAKE_SOURCE_DIR}/libs/platform-shlibs)
  else()
    set(EXT_PLATFORM_SHLIBS_DIR ${EXT_SHARE_DIR}/libs/platform-shlibs)
  endif()

  if(PACKAGE)
    set(EXT_DEPS_C_FLAGS "${CMAKE_C_FLAGS_RELEASE} -mtune=generic")
    set(EXT_DEPS_CXX_FLAGS "${CMAKE_CXX_FLAGS_RELEASE} -mtune=generic")
    message(STATUS "compiler flags for packaging:\nC    ${EXT_DEPS_C_FLAGS}\nCXX  ${EXT_DEPS_CXX_FLAGS}")
  endif()

  include(ExternalProject)

  ExternalProject_Add(portmidi
    PREFIX portmidi
    URL https://github.com/extemporelang/portmidi/archive/217.zip
    CMAKE_ARGS
    -DCMAKE_OSX_SYSROOT=macosx
    -DCMAKE_OSX_DEPLOYMENT_TARGET=10.9
    -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
    -DCMAKE_C_FLAGS=${EXT_DEPS_C_FLAGS}
    -DCMAKE_CXX_FLAGS=${EXT_DEPS_CXX_FLAGS}
    -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR})

  ExternalProject_Add(nanovg
    PREFIX nanovg
    URL https://github.com/extemporelang/nanovg/archive/master.zip
    CMAKE_ARGS
    -DCMAKE_OSX_SYSROOT=macosx
    -DCMAKE_OSX_DEPLOYMENT_TARGET=10.9
    -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
    -DCMAKE_C_FLAGS=${EXT_DEPS_C_FLAGS}
    -DCMAKE_CXX_FLAGS=${EXT_DEPS_CXX_FLAGS}
    -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR})

  ExternalProject_Add(kiss_fft
    PREFIX kiss_fft
    URL https://github.com/extemporelang/kiss_fft/archive/master.zip
    CMAKE_ARGS
    -DCMAKE_OSX_SYSROOT=macosx
    -DCMAKE_OSX_DEPLOYMENT_TARGET=10.9
    -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
    -DCMAKE_C_FLAGS=${EXT_DEPS_C_FLAGS}
    -DCMAKE_CXX_FLAGS=${EXT_DEPS_CXX_FLAGS}
    -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR})

  ExternalProject_Add(stb_image
    PREFIX stb_image
    URL https://github.com/extemporelang/stb/archive/master.zip
    CMAKE_ARGS
    -DCMAKE_OSX_SYSROOT=macosx
    -DCMAKE_OSX_DEPLOYMENT_TARGET=10.9
    -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
    -DCMAKE_C_FLAGS=${EXT_DEPS_C_FLAGS}
    -DCMAKE_CXX_FLAGS=${EXT_DEPS_CXX_FLAGS}
    -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR})

  ExternalProject_Add(glfw3
    PREFIX glfw3
    URL https://github.com/glfw/glfw/archive/3.1.2.zip
    CMAKE_ARGS
    -DCMAKE_OSX_SYSROOT=macosx
    -DCMAKE_OSX_DEPLOYMENT_TARGET=10.9
    -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
    -DCMAKE_C_FLAGS=${EXT_DEPS_C_FLAGS}
    -DCMAKE_CXX_FLAGS=${EXT_DEPS_CXX_FLAGS}
    -DBUILD_SHARED_LIBS=ON
    -DGLFW_BUILD_EXAMPLES=OFF
    -DGLFW_BUILD_TESTS=OFF
    -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR})

  ExternalProject_Add(assimp
    PREFIX assimp
    URL https://github.com/assimp/assimp/archive/v3.1.1.zip
    CMAKE_ARGS
    -DCMAKE_OSX_SYSROOT=macosx
    -DCMAKE_OSX_DEPLOYMENT_TARGET=10.9
    -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
    -DCMAKE_C_FLAGS=${EXT_DEPS_C_FLAGS}
    -DCMAKE_CXX_FLAGS=${EXT_DEPS_CXX_FLAGS}
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
      -DCMAKE_OSX_SYSROOT=macosx
      -DCMAKE_OSX_DEPLOYMENT_TARGET=10.9
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
      -DCMAKE_OSX_SYSROOT=macosx
      -DCMAKE_OSX_DEPLOYMENT_TARGET=10.9
      -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
      -DCMAKE_C_FLAGS=${EXT_DEPS_C_FLAGS}
      -DCMAKE_CXX_FLAGS=${EXT_DEPS_CXX_FLAGS}
      -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR})

    add_dependencies(apr expat)

    # aargh! sndfile on Windows still requires sh, sed, etc. and
    # doesn't work. so we'll just grab the pre-built one manually

    # ExternalProject_Add(sndfile
    #   PREFIX libsndfile
    #   URL https://github.com/erikd/libsndfile/archive/master.zip
    #   CONFIGURE_COMMAND ${CMAKE_BINARY_DIR}/libsndfile/src/sndfile/autogen.sh cmake && ${CMAKE_COMMAND} ${CMAKE_SOURCE_DIR}
    #   CMAKE_ARGS
    #   -DCMAKE_OSX_SYSROOT=macosx
    #   -DCMAKE_OSX_DEPLOYMENT_TARGET=10.9
    #   -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
    #   -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR})
  else()
    # build with as few deps as we can get away with
    ExternalProject_Add(sndfile
      PREFIX libsndfile
      URL http://www.mega-nerd.com/libsndfile/files/libsndfile-1.0.26.tar.gz
      CONFIGURE_COMMAND sh configure --prefix=${EXT_DEPS_INSTALL_DIR} --disable-static --disable-cpu-clip --disable-external-libs --disable-sqlite
      BUILD_COMMAND make CFLAGS=${EXT_DEPS_C_FLAGS}
      INSTALL_COMMAND make install
      BUILD_IN_SOURCE ON)

    ExternalProject_Add(apr
      PREFIX apr
      URL http://mirror.ventraip.net.au/apache/apr/apr-1.5.2.tar.gz
      CONFIGURE_COMMAND sh configure --prefix=${CMAKE_BINARY_DIR}/deps-install --enable-threads --disable-static
      BUILD_COMMAND make
      INSTALL_COMMAND make install
      BUILD_IN_SOURCE ON)
  endif()

  if(PACKAGE)
    ExternalProject_Add(LLVM-packaging
      PREFIX llvm
      URL http://www.llvm.org/releases/3.7.0/llvm-3.7.0.src.tar.xz
      URL_MD5 b98b9495e5655a672d6cb83e1a180f8e
      PATCH_COMMAND patch -p0 < ${CMAKE_SOURCE_DIR}/extras/extempore-llvm-3.7.0.patch
      CMAKE_ARGS
      -DCMAKE_OSX_SYSROOT=macosx
      -DCMAKE_OSX_DEPLOYMENT_TARGET=10.9
      -DLLVM_TARGETS_TO_BUILD=X86
      -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
      -DLLVM_ENABLE_TERMINFO=OFF
      -DLLVM_ENABLE_ZLIB=OFF
      -DCMAKE_C_FLAGS=${EXT_DEPS_C_FLAGS}
      -DCMAKE_CXX_FLAGS=${EXT_DEPS_CXX_FLAGS}
      -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR})

    add_dependencies(extempore LLVM-packaging)
  endif()

  # wrap the whole thing in a target

  if(APPLE)
    add_custom_target(extended_deps
      COMMENT "moving shared libs into libs/platform-shlibs"
      DEPENDS sndfile assimp glfw3 stb_image kiss_fft nanovg portmidi LLVM-packaging sndfile apr
      COMMAND cmake -E copy ${EXT_DEPS_INSTALL_DIR}/lib/libassimp.dylib ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND cmake -E copy ${EXT_DEPS_INSTALL_DIR}/lib/libkiss_fft.dylib ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND cmake -E copy ${EXT_DEPS_INSTALL_DIR}/lib/libnanovg.dylib ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND cmake -E copy ${EXT_DEPS_INSTALL_DIR}/lib/libportmidi.dylib ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND cmake -E copy ${EXT_DEPS_INSTALL_DIR}/lib/libsndfile.dylib ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND cmake -E copy ${EXT_DEPS_INSTALL_DIR}/lib/libstb_image.dylib ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND cmake -E copy ${EXT_DEPS_INSTALL_DIR}/lib/libglfw.3.dylib ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND cmake -E copy ${EXT_DEPS_INSTALL_DIR}/lib/libapr-1.0.dylib ${EXT_PLATFORM_SHLIBS_DIR})
  elseif(WIN32)
    add_custom_target(extended_deps
      COMMENT "moving shared libs into libs/platform-shlibs"
      DEPENDS sndfile assimp glfw3 stb_image kiss_fft nanovg portmidi LLVM-packaging sndfile apr
      COMMAND cmake -E copy ${EXT_DEPS_INSTALL_DIR}/bin/assimp.dll ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND cmake -E copy ${EXT_DEPS_INSTALL_DIR}/lib/kiss_fft.dll ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND cmake -E copy ${EXT_DEPS_INSTALL_DIR}/lib/nanovg.dll ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND cmake -E copy ${EXT_DEPS_INSTALL_DIR}/lib/portmidi.dll ${EXT_PLATFORM_SHLIBS_DIR}
      # COMMAND cmake -E copy ${EXT_DEPS_INSTALL_DIR}/lib/libsndfile-1.dll ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND cmake -E copy ${EXT_DEPS_INSTALL_DIR}/lib/stb_image.dll ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND cmake -E copy ${EXT_DEPS_INSTALL_DIR}/lib/glfw3.dll ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND cmake -E copy ${EXT_DEPS_INSTALL_DIR}/bin/libapr-1.dll ${EXT_PLATFORM_SHLIBS_DIR}
      # on Windows, move the .lib files as well
      COMMAND cmake -E copy ${EXT_DEPS_INSTALL_DIR}/lib/assimp.lib ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND cmake -E copy ${EXT_DEPS_INSTALL_DIR}/lib/kiss_fft.lib ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND cmake -E copy ${EXT_DEPS_INSTALL_DIR}/lib/nanovg.lib ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND cmake -E copy ${EXT_DEPS_INSTALL_DIR}/lib/portmidi.lib ${EXT_PLATFORM_SHLIBS_DIR}
      # COMMAND cmake -E copy ${EXT_DEPS_INSTALL_DIR}/lib/libsndfile-1.lib ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND cmake -E copy ${EXT_DEPS_INSTALL_DIR}/lib/stb_image.lib ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND cmake -E copy ${EXT_DEPS_INSTALL_DIR}/lib/glfw3dll.lib ${EXT_PLATFORM_SHLIBS_DIR}
      COMMAND cmake -E copy ${EXT_DEPS_INSTALL_DIR}/lib/libapr-1.lib ${EXT_PLATFORM_SHLIBS_DIR})
  endif()

  add_dependencies(aot_extended extended_deps)

endif(EXTENDED_DEPS)

#########
# tests #
#########

include(CTest)

if(BUILD_TESTING)
  add_test(
    NAME core
    COMMAND extempore --noaudio --port=17097 --run tests/all-core.xtm)
  add_test(
    NAME extended
    COMMAND extempore --noaudio --port=17097 --run tests/all-external.xtm)
endif()

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

# CPACK_PACKAGE_DIRECTORY
set(CPACK_PACKAGE_VERSION_MAJOR ${PROJECT_VERSION_MAJOR})
set(CPACK_PACKAGE_VERSION_MINOR ${PROJECT_VERSION_MINOR})
set(CPACK_PACKAGE_VERSION_PATCH ${PROJECT_VERSION_PATCH})

# CPACK_PACKAGE_DESCRIPTION_FILE

set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "The Extempore programming environment (http://extempore.moso.com.au)")

if(APPLE)
  set(CPACK_GENERATOR DragNDrop)
elseif(UNIX AND NOT APPLE)
  set(CPACK_GENERATOR DEB)
elseif(WIN32)
  set(CPACK_GENERATOR ZIP)
  # set(CPACK_GENERATOR NSIS64)
endif()

include(CPack)
