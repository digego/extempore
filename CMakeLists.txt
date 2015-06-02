project(Extempore)

cmake_minimum_required(VERSION 3.0)

set(EXTMPORE_VERSION_MAJOR "0")
set(EXTMPORE_VERSION_MINOR "60")
set(EXTEMPORE_VERSION "${EXTEMPORE_VERSION_MAJOR}.${EXTEMPORE_VERSION_MINOR}")

option(MCJIT "Use LLVM's MCJIT for codegen" OFF)

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
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/extras/cmake")

if(MCJIT)
  target_compile_definitions(extempore PRIVATE -DEXT_MCJIT)
  message("Extempore will use the \"MCJIT\" LLVM backend.")
endif()

# setup dependencies

if(EXISTS "$ENV{EXT_LLVM_DIR}")
  set(LLVM_ROOT_DIR $ENV{EXT_LLVM_DIR})
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
  PRIVATE -D_GNU_SOURCE
	PRIVATE -D__STDC_CONSTANT_MACROS
	PRIVATE -D__STDC_LIMIT_MACROS
  )

target_compile_options(extempore
  # PRIVATE  -w -O3 -MMD
  PRIVATE  -fexceptions
  PRIVATE  -frtti
  )


# Windows
if(WIN32)
  target_compile_definitions(extempore PRIVATE -DTARGET_OS_WINDOWS)
  # need Boost on Windows (for the moment, at least)
  find_package(Boost 1.47.0 REQUIRED)
  if(Boost_FOUND)
    target_include_directories(extempore PRIVATE ${Boost_INCLUDE_DIRS})
    target_link_libraries(extempore ${Boost_LIBRARIES})
  endif()
endif()

# OSX
if(APPLE)
  target_compile_definitions(extempore
    PRIVATE -DTARGET_OS_MAC
    )
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
  target_compile_definitions(extempore PRIVATE -DTARGET_OS_LINUX)
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

# copy libs into place

# install the executable
install(TARGETS extempore
  RUNTIME DESTINATION .)

# runtime directory
install(DIRECTORY runtime/ DESTINATION runtime)
# libraries & examples
install(DIRECTORY libs/core/ DESTINATION libs/core)
install(DIRECTORY libs/external/ DESTINATION libs/external)
install(DIRECTORY libs/contrib/ DESTINATION libs/contrib)
install(DIRECTORY examples/ DESTINATION examples)
install(DIRECTORY extras/ DESTINATION extras)