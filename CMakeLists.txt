cmake_minimum_required(VERSION 3.1) # we use target_sources()

project(Extempore VERSION 0.60)

option(MCJIT "use LLVM's MCJIT for codegen" ON)
option(BOOST "use boost library" OFF)
option(IN_TREE "use the Extempore source dir as EXT_SHARE_DIR - useful for developers" ON)
option(ASSETS "download the Extempore assets (used in many of the examples)" OFF)

# if -DEXT_SHARE_DIR=/path/to/share-dir is provided at the command
# line it will override these values
if(IN_TREE)
  set(EXT_SHARE_DIR "${CMAKE_SOURCE_DIR}" CACHE PATH
    "path to Extempore's shared runtime files")
else()
  set(EXT_SHARE_DIR "${CMAKE_INSTALL_PREFIX}/share/extempore" CACHE PATH
    "path to Extempore's shared runtime files")
endif()

########
# PCRE #
########

# current in-tree PCRE version: 8.37

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
  PRIVATE src/portaudio/src/common)

# platform-specific

if(APPLE)
  # use CoreAudio on OSX
  target_sources(portaudio
    PRIVATE src/portaudio/src/hostapi/coreaudio/pa_mac_core.c
    PRIVATE src/portaudio/src/hostapi/coreaudio/pa_mac_core_blocking.c
    PRIVATE src/portaudio/src/hostapi/coreaudio/pa_mac_core_blocking.h
    PRIVATE src/portaudio/src/hostapi/coreaudio/pa_mac_core_internal.h
    # PRIVATE src/portaudio/src/hostapi/coreaudio/pa_mac_core_old.c
    PRIVATE src/portaudio/src/hostapi/coreaudio/pa_mac_core_utilities.c
    PRIVATE src/portaudio/src/hostapi/coreaudio/pa_mac_core_utilities.h
    PRIVATE src/portaudio/src/os/unix/pa_unix_hostapis.c
    PRIVATE src/portaudio/src/os/unix/pa_unix_util.c
    PRIVATE src/portaudio/src/os/unix/pa_unix_util.h)
  target_compile_definitions(portaudio
    PRIVATE -DPA_USE_COREAUDIO)
  target_compile_options(portaudio
    PRIVATE -Wno-deprecated-declarations)
  target_link_libraries(portaudio
    PRIVATE "-framework CoreAudio"
    PRIVATE "-framework AudioToolbox"
    PRIVATE "-framework AudioUnit")

elseif(LINUX)
  # use ALSA on Linux
  target_sources(portaudio
    PRIVATE src/portaudio/src/hostapi/alsa/pa_linux_alsa.c
    PRIVATE src/portaudio/src/os/unix/pa_unix_hostapis.c
    PRIVATE src/portaudio/src/os/unix/pa_unix_util.c
    PRIVATE src/portaudio/src/os/unix/pa_unix_util.h)
  target_compile_definitions(portaudio
    PRIVATE -DPA_USE_ALSA)
  target_link_libraries(portaudio
    PRIVATE asound)
  
elseif(WIN32)
  # use wdmks on Windows
  target_sources(portaudio
    PRIVATE src/portaudio/src/hostapi/wdmks/pa_win_wdmks.c
    PRIVATE src/portaudio/src/os/win/pa_win_coinitialize.c
    PRIVATE src/portaudio/src/os/win/pa_win_coinitialize.h
    PRIVATE src/portaudio/src/os/win/pa_win_hostapis.c
    PRIVATE src/portaudio/src/os/win/pa_win_util.c
    PRIVATE src/portaudio/src/os/win/pa_win_waveformat.c
    PRIVATE src/portaudio/src/os/win/pa_win_wdmks_utils.c
    PRIVATE src/portaudio/src/os/win/pa_win_wdmks_utils.h
    PRIVATE src/portaudio/src/os/win/pa_x86_plain_converters.c
    PRIVATE src/portaudio/src/os/win/pa_x86_plain_converters.h)
    target_compile_definitions(portaudio
      PRIVATE -DPA_USE_WDMKS)

endif()

########################
# extempore executable #
########################

if(WIN32)
  set(WIN64_PACKAGE_DIR ${CMAKE_SOURCE_DIR}/libs/win64)
  link_directories(${WIN64_PACKAGE_DIR}/lib)
endif()

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
if(NOT WIN32)
  set_source_files_properties(src/Scheme.cpp PROPERTIES COMPILE_FLAGS -Wno-switch)
endif()

target_compile_definitions(extempore
  PRIVATE -DEXT_SHARE_DIR="${EXT_SHARE_DIR}"
  PRIVATE -D_GNU_SOURCE
	PRIVATE -D__STDC_CONSTANT_MACROS
	PRIVATE -D__STDC_FORMAT_MACROS
	PRIVATE -D__STDC_LIMIT_MACROS
  )

if(NOT MSVC)
  target_compile_options(extempore
    PRIVATE -std=c++11
    PRIVATE -fvisibility-inlines-hidden
    # PRIVATE -fno-exceptions
    PRIVATE -fno-rtti
    PRIVATE -fno-common
    PRIVATE -Woverloaded-virtual
    # PRIVATE -Wcast-qual
    PRIVATE -Wno-unused-result
    )
endif()

# set_target_properties(extempore PROPERTIES RUNTIME_OUTPUT_DIRECTORY "${CMAKE_HOME_DIRECTORY}")
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_SOURCE_DIR}/extras/cmake CACHE PATH
  "path to Extempore's cmake modules")

if(MCJIT)
  target_compile_definitions(extempore PRIVATE -DEXT_MCJIT)
  message(STATUS "Using the new LLVM MCJIT backend...")
else()
  message(STATUS "Using the old LLVM JIT backend...")
endif()

# Set a default build type if none was specified

if(NOT CMAKE_BUILD_TYPE)
  message(STATUS "Building 'Release' configuration")
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build." FORCE)
  # Set the possible values of build type for cmake-gui
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
endif()

###########################
# build-time dependencies #
###########################

# tell Extempore about the PCRE & portuaudio targets

add_dependencies(extempore pcre portaudio)
target_include_directories(extempore
  PRIVATE src/pcre
  PRIVATE src/portaudio/include)
target_link_libraries(extempore
  PRIVATE pcre
  PRIVATE portaudio)

# Extempore's build-time dependencies are:
#
# - LLVM 3.7 (patched for Extempore)
# - Boost (required on Windows, optional on other platforms)

# cmake will try and find these deps in the usual places.

# On *OSX/Linux*, this assumes you've got them through your package
# manager

# On *Windows*, it assumes the dependencies have been obtained through
# NuGet, either through Visual Studio or the NuGet command line
# interface (choco install nuget.commandline). As an example (assuming
# VS2015), something like this might get you what you need:

# nuget install boost-vc140

# after that, you'll probably still have to move the relevant headers
# and libs into libs/win64/{lib,include} - which is messy I know, but
# should be pretty straightforwards

# LLVM

if(DEFINED ENV{EXT_LLVM_DIR})
  set(EXT_LLVM_DIR $ENV{EXT_LLVM_DIR}
    CACHE PATH "path to the (patched) LLVM build")
  set(LLVM_ROOT_DIR ${EXT_LLVM_DIR})
elseif(EXT_LLVM_DIR)
  set(LLVM_ROOT_DIR ${EXT_LLVM_DIR})
else()
  message(FATAL_ERROR "You must set the EXT_LLVM_DIR environment variable or -DLLVM_ROOT_DIR cmake variable to point to your (patched for Extempore) LLVM build")
endif()

if(MCJIT)
  find_package(LLVM 3.7 EXACT)
  if(NOT ${LLVM_FOUND})
    message(FATAL_ERROR "Extempore requires LLVM 3.7 if you're using MCJIT")
  endif()
else()
  # for the old JIT we need an older LLVM - the old JIT has been
  # removed completely as of LLVM 3.6
  find_package(LLVM 3.4.2 EXACT)
  if(NOT ${LLVM_FOUND})
    message(FATAL_ERROR "Extempore requires LLVM 3.4.2 if you're using the old JIT (i.e. not MCJIT)")
  endif()
endif()
target_include_directories(extempore PRIVATE ${LLVM_INCLUDE_DIRS})
target_link_libraries(extempore PRIVATE ${LLVM_LIBRARIES})

if(WIN32)
  string(REPLACE " " ";" LLVM_COMPILE_OPTIONS ${LLVM_CXXFLAGS})
endif()

# Boost

# *must* have Boost on Windows
if(WIN32)
  set(BOOST ON)
  target_compile_definitions(extempore
    PRIVATE -DPCRE_STATIC
    PRIVATE -D_CRT_SECURE_NO_WARNINGS)
endif()

if(BOOST)
  target_compile_definitions(extempore PRIVATE -DEXT_BOOST)
  # asio header
  find_path(BOOST_ASIO_INCLUDE_DIR
    NAMES boost/asio.hpp
    PATHS ${WIN64_PACKAGE_DIR}/include)
  if(BOOST_ASIO_INCLUDE_DIR)
    target_include_directories(extempore PRIVATE ${BOOST_ASIO_INCLUDE_DIR})
  else()
    message(FATAL_ERROR "Cannot find boost/asio.hpp header")
  endif()
  # boost libraries - assumes boost 1.59
  target_link_libraries(extempore
    PRIVATE libboost_date_time-vc140-mt-1_59
    PRIVATE libboost_regex-vc140-mt-1_59
    PRIVATE libboost_system-vc140-mt-1_59)
endif()

if(APPLE) # OSX
  # use clang++ by default
  set(CMAKE_C_COMPILIER clang)
  set(CMAKE_CXX_COMPILIER clang++)
  # tell the compiler about the few ObjC++ source files on OSX
  set_source_files_properties(
    src/Extempore.cpp
    src/SchemeFFI.cpp
    src/UNIV.cpp
    PROPERTIES
    COMPILE_FLAGS "-x objective-c++")

  target_link_libraries(extempore PRIVATE pthread)

  # frameworks
  target_link_libraries(extempore
    PRIVATE "-framework Cocoa"
    PRIVATE "-framework CoreAudio"
    PRIVATE "-framework AudioToolbox"
    PRIVATE "-framework AudioUnit")

elseif(UNIX AND NOT APPLE) # Linux
  set_property(TARGET extempore PROPERTY POSITION_INDEPENDENT_CODE ON)
  # target_link_libraries(extempore PRIVATE --export-dynamic)
  target_link_libraries(extempore PRIVATE pthread)
endif()

# on Windows, put the created extempore.exe straight into the source
# directory, and the .lib file into libs/win64/lib
if(WIN32)
  set_target_properties(extempore
    PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY_DEBUG ${CMAKE_SOURCE_DIR}
    RUNTIME_OUTPUT_DIRECTORY_RELEASE ${CMAKE_SOURCE_DIR}
    LIBRARY_OUTPUT_DIRECTORY_DEBUG ${CMAKE_SOURCE_DIR}
    LIBRARY_OUTPUT_DIRECTORY_RELEASE ${CMAKE_SOURCE_DIR}
    ARCHIVE_OUTPUT_DIRECTORY_DEBUG ${CMAKE_SOURCE_DIR}/libs/win64/lib
    ARCHIVE_OUTPUT_DIRECTORY_RELEASE ${CMAKE_SOURCE_DIR}/libs/win64/lib)
endif()

if(ASSETS)
  file(DOWNLOAD http://extempore.moso.com.au/extras/assets.tgz assets.tgz)
  execute_process(COMMAND ${CMAKE_COMMAND} -E tar xf assets.tgz)
  execute_process(COMMAND ${CMAKE_COMMAND} -E remove assets.tgz)
endif()

###########
# install #
###########

install(TARGETS extempore
  RUNTIME DESTINATION bin)

# install the rest of the necessary files into the "share" directory
install(DIRECTORY runtime examples tests extras
  DESTINATION ${EXT_SHARE_DIR}
  PATTERN ".DS_Store" EXCLUDE)
# only install assets if it exists
if(EXISTS ${CMAKE_SOURCE_DIR}/assets)
  install(DIRECTORY assets
    DESTINATION ${EXT_SHARE_DIR}
    PATTERN ".DS_Store" EXCLUDE)
endif()
# need to filter out precomp guff from libs
install(DIRECTORY libs
  DESTINATION ${EXT_SHARE_DIR}
  PATTERN "*.xtm"
  PATTERN "aot-cache" EXCLUDE
  PATTERN ".DS_Store" EXCLUDE)

add_custom_target(uninstall
  COMMAND ${CMAKE_COMMAND} -E remove ${CMAKE_INSTALL_PREFIX}/bin/extempore
  COMMAND ${CMAKE_COMMAND} -E remove_directory ${EXT_SHARE_DIR}
  COMMENT "Removing ${CMAKE_INSTALL_PREFIX}/bin/extempore and ${EXT_SHARE_DIR}")

###################
# AOT compilation #
###################

configure_file(
  ${CMAKE_SOURCE_DIR}/extras/cmake/aot_stdlib.cmake.in
  ${CMAKE_SOURCE_DIR}/extras/cmake/aot_stdlib.cmake
  @ONLY)

add_custom_target(aot_stdlib
  COMMAND ${CMAKE_COMMAND} -P ${CMAKE_SOURCE_DIR}/extras/cmake/aot_stdlib.cmake
  COMMENT "Ahead-of-time compiling the standard library...")

add_dependencies(aot_stdlib extempore)

# uninstall only AOT-compiled libs
add_custom_target(uninstall_aot_stdlib
  COMMAND ${CMAKE_COMMAND} -E remove_directory ${EXT_SHARE_DIR}/libs/aot-cache
  COMMENT "Removing AOT-compiled libs")

##########
# xtmdoc #
##########

add_custom_target(xtmdoc
  COMMAND ${CMAKE_INSTALL_PREFIX}/bin/extempore
           --runtime ${EXT_SHARE_DIR}
           --port 17095
           --eval "(begin (sys:load \"libs/core/adt.xtm\") (sys:load \"libs/core/audio_dsp.xtm\") (sys:load \"libs/core/instruments.xtm\") (sys:load \"libs/core/math.xtm\") (sys:load \"libs/core/math_ext.xtm\") (sys:load \"libs/core/std.xtm\") (sys:load \"libs/external/fft.xtm\") (sys:load \"libs/external/gl.xtm\") (sys:load \"libs/external/glfw3.xtm\") (sys:load \"libs/external/instruments_ext.xtm\") (sys:load \"libs/external/nanovg.xtm\") (sys:load \"libs/external/sndfile.xtm\") (sys:load \"libs/external/stb_image.xtm\") (xtmdoc-export-caches-to-json \"/tmp/xtmdoc.json\" #f) (quit 0))"
  COMMENT "Generating xtmdoc output in /tmp/xtmdoc.json"
  VERBATIM)

add_dependencies(xtmdoc extempore)
