project(Extempore)

cmake_minimum_required(VERSION 3.0)

set(EXTMPORE_VERSION_MAJOR "0")
set(EXTMPORE_VERSION_MINOR "60")
set(EXTEMPORE_VERSION "${EXTEMPORE_VERSION_MAJOR}.${EXTEMPORE_VERSION_MINOR}")

option(MCJIT "use LLVM's MCJIT for codegen" ON)
option(BOOST "use boost library" OFF)

set(EXT_SHARE_DIR "${CMAKE_INSTALL_PREFIX}/share/extempore" CACHE PATH
  "path to Extempore's shared runtime files")

# set up the extempore target

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
target_include_directories(extempore PRIVATE include)

# set_target_properties(extempore PROPERTIES RUNTIME_OUTPUT_DIRECTORY "${CMAKE_HOME_DIRECTORY}")
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/extras/cmake" CACHE PATH
  "path to Extempore's cmake modules")

if(MCJIT)
  target_compile_definitions(extempore PRIVATE -DEXT_MCJIT)
  message(STATUS "-- Using the new LLVM MCJIT backend...")
else()
  message(STATUS "-- Using the old LLVM JIT backend...")
endif()

# setup dependencies

if(EXISTS "$ENV{EXT_LLVM_DIR}")
  set(LLVM_ROOT_DIR $ENV{EXT_LLVM_DIR} CACHE PATH
    "path to the (patched) LLVM build")
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
else()
  message(FATAL_ERROR "You must set the EXT_LLVM_DIR environment variable to point to your (patched for Extempore) LLVM build")
endif()

find_package(Portaudio REQUIRED)
target_include_directories(extempore PRIVATE ${PORTAUDIO_INCLUDE_DIRS})
target_link_libraries(extempore ${PORTAUDIO_LIBRARIES})

find_package(PCRE REQUIRED)
target_include_directories(extempore PRIVATE ${PCRE_INCLUDE_DIR})
target_link_libraries(extempore ${PCRE_LIBRARY})

find_package(OpenGL REQUIRED)
target_include_directories(extempore PRIVATE ${OPENGL_INCLUDE_DIR})
target_link_libraries(extempore ${OPENGL_LIBRARIES})

# all platforms

target_compile_definitions(extempore
  PRIVATE -DEXT_SHARE_DIR="${EXT_SHARE_DIR}")

target_compile_definitions(extempore
  PRIVATE -D_GNU_SOURCE
	PRIVATE -D__STDC_CONSTANT_MACROS
	PRIVATE -D__STDC_LIMIT_MACROS
  )

target_compile_options(extempore
  # PRIVATE  -w -O3 -MMD
  PRIVATE  -fexceptions
  PRIVATE  -frtti
  )

# need Boost on Windows (for the moment, at least)
if(WIN32)
  set(BOOST ON)
endif()

if(BOOST)
  find_package(Boost 1.47.0 REQUIRED
    COMPONENTS filesystem system thread date_time regex serialization)
  if(Boost_FOUND)
    target_compile_options(extempore PRIVATE -std=c++11)
    target_compile_definitions(extempore PRIVATE -DEXT_BOOST)
    target_include_directories(extempore PRIVATE ${Boost_INCLUDE_DIRS})
    target_link_libraries(extempore ${Boost_LIBRARIES})
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

# if(MSVC)
#   add_definitions(-D_CRT_SECURE_NO_WARNINGS)

#   if (NOT USE_MSVC_RUNTIME_LIBRARY_DLL)
#     foreach (flag CMAKE_C_FLAGS
#         CMAKE_C_FLAGS_DEBUG
#         CMAKE_C_FLAGS_RELEASE
#         CMAKE_C_FLAGS_MINSIZEREL
#         CMAKE_C_FLAGS_RELWITHDEBINFO)

#       if (${flag} MATCHES "/MD")
#         string(REGEX REPLACE "/MD" "/MT" ${flag} "${${flag}}")
#       endif()
#       if (${flag} MATCHES "/MDd")
#         string(REGEX REPLACE "/MDd" "/MTd" ${flag} "${${flag}}")
#       endif()

#     endforeach()
#   endif()
# endif()

# install the executable
install(TARGETS extempore
  RUNTIME DESTINATION bin)

# install the rest of the necessary files into the "share" directory
install(DIRECTORY runtime examples tests assets extras
  DESTINATION ${EXT_SHARE_DIR}
  PATTERN ".DS_Store" EXCLUDE)
# need to filter out precomp guff from libs
install(DIRECTORY libs
  DESTINATION ${EXT_SHARE_DIR}
  PATTERN "*.xtm"
  PATTERN "aot-cache" EXCLUDE
  PATTERN ".DS_Store" EXCLUDE)
