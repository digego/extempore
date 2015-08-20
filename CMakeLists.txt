project(Extempore)

cmake_minimum_required(VERSION 3.0)

set(EXTMPORE_VERSION_MAJOR "0")
set(EXTMPORE_VERSION_MINOR "60")
set(EXTEMPORE_VERSION "${EXTEMPORE_VERSION_MAJOR}.${EXTEMPORE_VERSION_MINOR}")

option(MCJIT "use LLVM's MCJIT for codegen" ON)
option(BOOST "use boost library" OFF)

set(EXT_SHARE_DIR "${CMAKE_INSTALL_PREFIX}/share/extempore" CACHE PATH
  "path to Extempore's shared runtime files")

########################
# extempore executable #
########################

if(WIN32)
  set(NUGET_PACKAGE_DIR ${CMAKE_SOURCE_DIR}/packages)
  link_directories(${NUGET_PACKAGE_DIR}/lib)
endif()

add_executable(extempore src/Extempore.cpp)
target_sources(extempore
  PRIVATE src/AudioDevice.cpp
  PRIVATE src/EXTCondition.cpp
  PRIVATE src/EXTLLVM.cpp
  PRIVATE src/EXTMonitor.cpp
  PRIVATE src/EXTMutex.cpp
  PRIVATE src/EXTThread.cpp
  PRIVATE src/Extempore.cpp
  PRIVATE src/OSC.cpp
  PRIVATE src/Scheme.cpp
  PRIVATE src/SchemeFFI.cpp
  PRIVATE src/SchemeProcess.cpp
  PRIVATE src/SchemeREPL.cpp
  PRIVATE src/TaskScheduler.cpp
  PRIVATE src/UNIV.cpp
  )

if(WIN32)
  target_sources(extempore PRIVATE msvc/extempore.def)
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
	PRIVATE -D__STDC_LIMIT_MACROS
  )

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

# Extempore's build-time dependencies are:
#
# - portaudio
# - PCRE
# - LLVM 3.4.2 (patched for Extempore)
# - Boost (optional)

# cmake will try and find these deps in the usual places.

# On *OSX/Linux*, this assumes you've got them through your package
# manager

# On *Windows*, it assumes the dependencies have been obtained through
# NuGet, either through Visual Studio or the NuGet command line
# interface (choco install nuget.commandline). As an example (assuming
# VS2015), something like this might get you what you need:

# if(WIN32)
#   execute_process(
#     COMMAND nuget install portaudio & nuget install pcre & nuget install boost-vc140
#     RESULT_VARIABLE nuget_retval
#     WORKING_DIRECTORY ${NUGET_PACKAGE_DIR}
#     OUTPUT_QUIET)
#   if(NOT "${nuget_retval}" STREQUAL 0)
#     message(FATAL_ERROR "Problem installing dependencies through NuGet - do you have the NuGet command line tool installed?")
#   endif()
# endif()

# LLVM

if(EXISTS $ENV{EXT_LLVM_DIR})
  set(LLVM_ROOT_DIR $ENV{EXT_LLVM_DIR} CACHE PATH
    "path to the (patched) LLVM build")
elseif(EXISTS ${LLVM_ROOT_DIR})
  # if we specify LLVM_ROOT_DIR through cmake, then we set the
  # EXT_LLVM_DIR environment variable as well
  set($ENV{EXT_LLVM_DIR} ${LLVM_ROOT_DIR})
else()
  message(FATAL_ERROR "You must set the EXT_LLVM_DIR environment variable or -DLLVM_ROOT_DIR cmake variable to point to your (patched for Extempore) LLVM build")
endif()

find_package(LLVM 3.4 REQUIRED)
target_include_directories(extempore PRIVATE ${LLVM_INCLUDE_DIRS})
target_link_libraries(extempore
  -L${LLVM_LIBRARY_DIRS}
  -lLLVMLTO
  -lLLVMLinker
  -lLLVMipo
  -lLLVMVectorize
  -lLLVMBitWriter
  -lLLVMTableGen
  -lLLVMDebugInfo
  -lLLVMOption
  -lLLVMX86Disassembler
  -lLLVMX86AsmParser
  -lLLVMX86CodeGen
  -lLLVMSelectionDAG
  -lLLVMAsmPrinter
  -lLLVMX86Desc
  -lLLVMX86Info
  -lLLVMX86AsmPrinter
  -lLLVMX86Utils
  -lLLVMIRReader
  -lLLVMBitReader
  -lLLVMAsmParser
  -lLLVMMCDisassembler
  -lLLVMMCParser
  -lLLVMInstrumentation
  -lLLVMInterpreter
  -lLLVMMCJIT
  -lLLVMJIT
  -lLLVMCodeGen
  -lLLVMObjCARCOpts
  -lLLVMScalarOpts
  -lLLVMInstCombine
  -lLLVMTransformUtils
  -lLLVMipa
  -lLLVMAnalysis
  -lLLVMRuntimeDyld
  -lLLVMExecutionEngine
  -lLLVMTarget
  -lLLVMMC
  -lLLVMObject
  -lLLVMCore
  -lLLVMSupport)

# portaudio

if(WIN32)
  # give find_package a hint about where to find portaudio
  set(Portaudio_ROOT ${NUGET_PACKAGE_DIR})
endif()
find_package(Portaudio REQUIRED)
target_include_directories(extempore PRIVATE ${PORTAUDIO_INCLUDE_DIRS})
target_link_libraries(extempore ${PORTAUDIO_LIBRARIES})

# PCRE

if(WIN32)
  # give find_package a hint about where to find PCRE
  set(PCRE_ROOT ${NUGET_PACKAGE_DIR})
  target_compile_definitions(extempore PRIVATE -DPCRE_STATIC)
endif()
find_package(PCRE REQUIRED)
target_include_directories(extempore PRIVATE ${PCRE_INCLUDE_DIRS})
target_link_libraries(extempore ${PCRE_LIBRARIES})

# Boost

# *must* have Boost on Windows
if(WIN32)
  set(BOOST ON)
endif()

if(BOOST)
  target_compile_definitions(extempore PRIVATE -DEXT_BOOST)
  # target_compile_options(extempore PRIVATE -std=c++11)
  find_path(BOOST_ASIO_INCLUDE_DIR
    NAMES boost/asio.hpp
    PATHS ${NUGET_PACKAGE_DIR}/include)
  if(BOOST_ASIO_INCLUDE_DIR)
    target_include_directories(extempore PRIVATE ${BOOST_ASIO_INCLUDE_DIR})
  else()
    message(FATAL_ERROR "Cannot find boost/asio.hpp header")
  endif()
endif()

# OSX
if(APPLE)
  # use clang++ by default
  set(CMAKE_C_COMPILIER clang)
  set(CMAKE_CXX_COMPILIER clang++)
  # tell the compiler that we're using ObjC++
  target_compile_options(extempore PRIVATE -x objective-c++)

  target_link_libraries(extempore pthread ncurses)

  # frameworks
  target_link_libraries(extempore
    "-framework Cocoa"
    "-framework CoreAudio"
    "-framework AudioToolbox"
    "-framework AudioUnit")
endif()

# Linux
if(UNIX AND NOT APPLE)
  set_property(TARGET extempore PROPERTY POSITION_INDEPENDENT_CODE ON)
  target_link_libraries(extempore --export-dynamic)
  find_package(X11 REQUIRED)
  target_include_directories(extempore PRIVATE ${X11_INCLUDE_DIR})
  target_link_libraries(extempore ${X11_LIBRARIES})
  target_link_libraries(extempore pthread ncurses)
endif()

###########
# install #
###########

install(TARGETS extempore
  RUNTIME DESTINATION bin)

# install the rest of the necessary files into the "share" directory
install(DIRECTORY runtime examples tests assets extras
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

##########
# assets #
##########

add_custom_target(assets
  WORKING_DIR ${EXT_SHARE_DIR}
  COMMAND curl -O http://extempore.moso.com.au/extras/assets.tgz
  COMMAND ${CMAKE_COMMAND} -E tar xf assets.tgz
  COMMAND ${CMAKE_COMMAND} -E assets.tgz
  COMMAND ${CMAKE_COMMAND} -E remove assets.tgz
  COMMENT "Downloading assets from http://extempore.moso.com.au/extras/assets.tgz")

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

#########
# cpack #
#########

# cpack is cmake's tool for providing distributable
# binaries/installers on various platforms.

set(CPACK_PACKAGE_NAME "Extempore")
set(CPACK_PACKAGE_VENDOR "Andrew Sorensen")
set(CPACK_PACKAGE_CONTACT "Ben Swift")

# CPACK_PACKAGE_DIRECTORY

set(CPACK_PACKAGE_VERSION_MAJOR EXTEMPORE_VERSION_MAJOR)
set(CPACK_PACKAGE_VERSION_MINOR EXTEMPORE_VERSION_MINOR)
set(CPACK_PACKAGE_VERSION_PATCH "0")

# CPACK_PACKAGE_DESCRIPTION_FILE

set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "The Extempore programming environment (http://extempore.moso.com.au)")

# CPACK_PACKAGE_FILE_NAME

# CPACK_PACKAGE_INSTALL_DIRECTORY

if(APPLE)
  set(CPACK_GENERATOR DragNDrop)
  include(CPackDMG)
elseif(UNIX AND NOT APPLE) # Linux
  set(CPACK_GENERATOR DEB)
  include(CPackDeb)
elseif(WIN32)
  set(CPACK_GENERATOR NSIS64)
  include(CPackNSIS)
endif()
