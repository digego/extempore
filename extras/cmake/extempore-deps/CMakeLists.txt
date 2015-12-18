cmake_minimum_required(VERSION 3.0)

option(PACKAGE "set up install targets for packaging" ON)

set(EXT_DEPS_INSTALL_DIR ${CMAKE_SOURCE_DIR}/deps-install)

project(extempore-deps)

# Set a default build type if none was specified

# if(PACKAGE)
#   if(APPLE)
#     set(PACKAGING_C_FLAGS "-mcpu=sandybridge")
#   endif()
# endif()

if(NOT CMAKE_BUILD_TYPE)
  message(STATUS "Building 'Release' configuration")
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build." FORCE)
  # Set the possible values of build type for cmake-gui
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
endif()

include(ExternalProject)

ExternalProject_Add(portmidi
  PREFIX portmidi
  URL https://github.com/extemporelang/portmidi/archive/217.zip
  CMAKE_ARGS -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
             -DCMAKE_C_FLAGS=${PACKAGING_C_FLAGS}
             -DCMAKE_CXX_FLAGS=${PACKAGING_C_FLAGS}
             -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR})

ExternalProject_Add(nanovg
  PREFIX nanovg
  URL https://github.com/extemporelang/nanovg/archive/master.zip
  CMAKE_ARGS -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
             -DCMAKE_C_FLAGS=${PACKAGING_C_FLAGS}
             -DCMAKE_CXX_FLAGS=${PACKAGING_C_FLAGS}
             -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR})

ExternalProject_Add(kiss_fft
  PREFIX kiss_fft
  URL https://github.com/extemporelang/kiss_fft/archive/master.zip
  CMAKE_ARGS -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
             -DCMAKE_C_FLAGS=${PACKAGING_C_FLAGS}
             -DCMAKE_CXX_FLAGS=${PACKAGING_C_FLAGS}
             -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR})

ExternalProject_Add(stb_image
  PREFIX stb_image
  URL https://github.com/extemporelang/stb/archive/master.zip
  CMAKE_ARGS -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
             -DCMAKE_C_FLAGS=${PACKAGING_C_FLAGS}
             -DCMAKE_CXX_FLAGS=${PACKAGING_C_FLAGS}
             -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR})

ExternalProject_Add(glfw3
  PREFIX glfw3
  URL https://github.com/glfw/glfw/archive/3.1.2.zip
  CMAKE_ARGS -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
             -DCMAKE_C_FLAGS=${PACKAGING_C_FLAGS}
             -DCMAKE_CXX_FLAGS=${PACKAGING_C_FLAGS}
             -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR})

ExternalProject_Add(assimp
  PREFIX assimp
  URL https://github.com/assimp/assimp/archive/v3.1.1.zip
  CMAKE_ARGS -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
             -DCMAKE_C_FLAGS=${PACKAGING_C_FLAGS}
             -DCMAKE_CXX_FLAGS=${PACKAGING_C_FLAGS}
             -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR})

ExternalProject_Add(sndfile
  PREFIX libsndfile
  URL http://www.mega-nerd.com/libsndfile/files/libsndfile-1.0.26.tar.gz
  CONFIGURE_COMMAND sh configure --prefix=${EXT_DEPS_INSTALL_DIR}
  BUILD_COMMAND make
  INSTALL_COMMAND make install
  BUILD_IN_SOURCE ON)

# llvm (taken from main Extempore CMakeLists.txt)

ExternalProject_Add(LLVM
  PREFIX llvm
  URL http://www.llvm.org/releases/3.7.0/llvm-3.7.0.src.tar.xz
  URL_MD5 b98b9495e5655a672d6cb83e1a180f8e
  PATCH_COMMAND curl https://raw.githubusercontent.com/digego/extempore/master/extras/extempore-llvm-3.7.0.patch | patch -p0
  CMAKE_ARGS -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
             -DLLVM_ENABLE_TERMINFO=OFF
             -DLLVM_ENABLE_ZLIB=OFF
             -DCMAKE_INSTALL_PREFIX=${EXT_LLVM_DIR}
             -DCMAKE_C_FLAGS=${PACKAGING_C_FLAGS}
             -DCMAKE_CXX_FLAGS=${PACKAGING_C_FLAGS}
             -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR})
