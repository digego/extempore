cmake_minimum_required(VERSION 3.0)

option(PACKAGE "set up install targets for packaging" ON)

if(PACKAGE)
  # this needs to be set before project() is called
  set(CMAKE_OSX_SYSROOT macosx)
  set(CMAKE_OSX_DEPLOYMENT_TARGET 10.9)
  message(STATUS "Building Extempore dependencies for binary distribution")
endif()

set(EXT_DEPS_INSTALL_DIR ${CMAKE_SOURCE_DIR}/install)
set(CMAKE_BUILD_TYPE "Release")

project(extempore-deps)

# Set a default build type if none was specified

if(PACKAGE AND UNIX)
  set(PACKAGING_C_FLAGS "${CMAKE_C_FLAGS_RELEASE} -mtune=generic")
  set(PACKAGING_CXX_FLAGS "${CMAKE_CXX_FLAGS_RELEASE} -mtune=generic")
  message(STATUS "compiler flags for packaging:\nC    ${PACKAGING_C_FLAGS}\nCXX  ${PACKAGING_CXX_FLAGS}")
endif()

include(ExternalProject)

ExternalProject_Add(portmidi
  PREFIX portmidi
  URL https://github.com/extemporelang/portmidi/archive/217.zip
  CMAKE_ARGS -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
             -DCMAKE_C_FLAGS=${PACKAGING_C_FLAGS}
             -DCMAKE_CXX_FLAGS=${PACKAGING_CXX_FLAGS}
             -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR})

ExternalProject_Add(nanovg
  PREFIX nanovg
  URL https://github.com/extemporelang/nanovg/archive/master.zip
  CMAKE_ARGS -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
             -DCMAKE_C_FLAGS=${PACKAGING_C_FLAGS}
             -DCMAKE_CXX_FLAGS=${PACKAGING_CXX_FLAGS}
             -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR})

ExternalProject_Add(kiss_fft
  PREFIX kiss_fft
  URL https://github.com/extemporelang/kiss_fft/archive/master.zip
  CMAKE_ARGS -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
             -DCMAKE_C_FLAGS=${PACKAGING_C_FLAGS}
             -DCMAKE_CXX_FLAGS=${PACKAGING_CXX_FLAGS}
             -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR})

ExternalProject_Add(stb_image
  PREFIX stb_image
  URL https://github.com/extemporelang/stb/archive/master.zip
  CMAKE_ARGS -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
             -DCMAKE_C_FLAGS=${PACKAGING_C_FLAGS}
             -DCMAKE_CXX_FLAGS=${PACKAGING_CXX_FLAGS}
             -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR})

ExternalProject_Add(glfw3
  PREFIX glfw3
  URL https://github.com/glfw/glfw/archive/3.1.2.zip
  CMAKE_ARGS -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
             -DCMAKE_C_FLAGS=${PACKAGING_C_FLAGS}
             -DCMAKE_CXX_FLAGS=${PACKAGING_CXX_FLAGS}
             -DBUILD_SHARED_LIBS=ON
             -DGLFW_BUILD_EXAMPLES=OFF
             -DGLFW_BUILD_TESTS=OFF
             -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR})

ExternalProject_Add(assimp
  PREFIX assimp
  URL https://github.com/assimp/assimp/archive/v3.1.1.zip
  CMAKE_ARGS -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
             -DCMAKE_C_FLAGS=${PACKAGING_C_FLAGS}
             -DCMAKE_CXX_FLAGS=${PACKAGING_CXX_FLAGS}
             -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR})

if(WIN32)
  ExternalProject_Add(sndfile
    PREFIX libsndfile
    URL https://github.com/erikd/libsndfile/archive/master.zip
    CMAKE_ARGS -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
               -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR})
else()
  # build with as few deps as we can get away with
  ExternalProject_Add(sndfile
    PREFIX libsndfile
    URL http://www.mega-nerd.com/libsndfile/files/libsndfile-1.0.26.tar.gz
    CONFIGURE_COMMAND sh configure --prefix=${EXT_DEPS_INSTALL_DIR} --disable-static --disable-cpu-clip --disable-external-libs --disable-sqlite
    BUILD_COMMAND make CFLAGS=${PACKAGING_C_FLAGS}
    INSTALL_COMMAND make install
    BUILD_IN_SOURCE ON)
endif()

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
             -DCMAKE_CXX_FLAGS=${PACKAGING_CXX_FLAGS}
             -DCMAKE_INSTALL_PREFIX=${EXT_DEPS_INSTALL_DIR})

# to copy the relevant bits

# cp install/lib/libassimp.dylib install/lib/libkiss_fft.dylib install/lib/libnanovg.dylib install/lib/libportmidi.dylib install/lib/libsndfile.dylib install/lib/libstb_image.dylib ~/Code/extempore/libs/platform-shlibs
