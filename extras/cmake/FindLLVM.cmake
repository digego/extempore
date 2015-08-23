# - Find LLVM headers and libraries.
# This module locates LLVM and adapts the llvm-config output for use with
# CMake.
#
# The following variables are defined:
#  LLVM_FOUND          - true if LLVM was found
#  LLVM_CXXFLAGS       - C++ compiler flags for files that include LLVM headers.
#  LLVM_HOST_TARGET    - Target triple used to configure LLVM.
#  LLVM_INCLUDE_DIRS   - Directory containing LLVM include files.
#  LLVM_LDFLAGS        - Linker flags to add when linking against LLVM
#                        (includes -LLLVM_LIBRARY_DIRS).
#  LLVM_LIBRARIES      - Full paths to the library files to link against.
#  LLVM_LIBRARY_DIRS   - Directory containing LLVM libraries.
#  LLVM_ROOT_DIR       - The root directory of the LLVM installation.
#                        llvm-config is searched for in ${LLVM_ROOT_DIR}/bin.
#  LLVM_VERSION_MAJOR  - Major version of LLVM.
#  LLVM_VERSION_MINOR  - Minor version of LLVM.
#  LLVM_VERSION_STRING - Full LLVM version string (e.g. 2.9).
#
# Note: The variable names were chosen in conformance with the offical CMake
# guidelines, see ${CMAKE_ROOT}/Modules/readme.txt.

# Try suffixed versions to pick up the newest LLVM install available on Debian
# derivatives.
# We also want an user-specified LLVM_ROOT_DIR to take precedence over the
# system default locations such as /usr/local/bin. Executing find_program()
# multiples times is the approach recommended in the docs.
if(NOT EXISTS ${LLVM_ROOT_DIR}/include/llvm)
  message(FATAL_ERROR "LLVM_ROOT_DIR (${LLVM_ROOT_DIR}) is not a valid LLVM install")
endif()

set(llvm_config_names llvm-config-3.4 llvm-config34 llvm-config)
find_program(LLVM_CONFIG
    NAMES ${llvm_config_names}
    PATHS ${LLVM_ROOT_DIR}/bin NO_DEFAULT_PATH
    DOC "Path to llvm-config tool.")
find_program(LLVM_CONFIG NAMES ${llvm_config_names})

if (NOT LLVM_CONFIG)
  message(WARNING "Could not find llvm-config. Try manually setting LLVM_CONFIG to the llvm-config executable of the installation to use.")
endif()

execute_process(
  COMMAND ${LLVM_CONFIG} --cxxflags
  OUTPUT_VARIABLE LLVM_CXXFLAGS
  OUTPUT_STRIP_TRAILING_WHITESPACE)

execute_process(
  COMMAND ${LLVM_CONFIG} --host-target
  OUTPUT_VARIABLE LLVM_HOST_TARGET
  OUTPUT_STRIP_TRAILING_WHITESPACE)

execute_process(
  COMMAND ${LLVM_CONFIG} --includedir
  OUTPUT_VARIABLE LLVM_INCLUDE_DIRS
  OUTPUT_STRIP_TRAILING_WHITESPACE)

execute_process(
  COMMAND ${LLVM_CONFIG} --ldflags
  OUTPUT_VARIABLE LLVM_LDFLAGS
  OUTPUT_STRIP_TRAILING_WHITESPACE)

execute_process(
  COMMAND ${LLVM_CONFIG} --libfiles
  OUTPUT_VARIABLE LLVM_LIBRARIES
  OUTPUT_STRIP_TRAILING_WHITESPACE)
string(REPLACE " " ";" LLVM_LIBRARIES ${LLVM_LIBRARIES})

execute_process(
  COMMAND ${LLVM_CONFIG} --libdir
  OUTPUT_VARIABLE LLVM_LIBRARY_DIRS
  OUTPUT_STRIP_TRAILING_WHITESPACE)

execute_process(
  COMMAND ${LLVM_CONFIG} --version
  OUTPUT_VARIABLE LLVM_VERSION_STRING
  OUTPUT_STRIP_TRAILING_WHITESPACE)

# On CMake builds of LLVM, the output of llvm-config --cxxflags does not
# include -fno-rtti, leading to linker errors. Be sure to add it.
if(CMAKE_COMPILER_IS_GNUCXX OR (${CMAKE_CXX_COMPILER_ID} STREQUAL "Clang"))
    if(NOT ${LLVM_CXXFLAGS} MATCHES "-fno-rtti")
        set(LLVM_CXXFLAGS "${LLVM_CXXFLAGS} -fno-rtti")
    endif()
endif()

string(REGEX REPLACE "([0-9]+).*" "\\1" LLVM_VERSION_MAJOR "${LLVM_VERSION_STRING}" )
string(REGEX REPLACE "[0-9]+\\.([0-9]+).*[A-Za-z]*" "\\1" LLVM_VERSION_MINOR "${LLVM_VERSION_STRING}" )

# Use the default CMake facilities for handling QUIET/REQUIRED.
include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(LLVM
    REQUIRED_VARS LLVM_ROOT_DIR LLVM_HOST_TARGET
    VERSION_VAR LLVM_VERSION_STRING)
