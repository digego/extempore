cmake_minimum_required(VERSION 3.0)

option(PACKAGE "set up install targets for packaging" ON)

project(extempore-deps)

# Set a default build type if none was specified

if(PACKAGE)
  if(APPLE)
    set(PACKAGING_C_FLAGS "-mtune=generic")
  endif()
endif()

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
             -DCMAKE_CXX_FLAGS=${PACKAGING_C_FLAGS})

ExternalProject_Add(nanovg
  PREFIX nanovg
  URL https://github.com/extemporelang/nanovg/archive/master.zip
  CMAKE_ARGS -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
             -DCMAKE_C_FLAGS=${PACKAGING_C_FLAGS}
             -DCMAKE_CXX_FLAGS=${PACKAGING_C_FLAGS})

ExternalProject_Add(kiss_fft
  PREFIX kiss_fft
  URL https://github.com/extemporelang/kiss_fft/archive/master.zip
  CMAKE_ARGS -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
             -DCMAKE_C_FLAGS=${PACKAGING_C_FLAGS}
             -DCMAKE_CXX_FLAGS=${PACKAGING_C_FLAGS})

ExternalProject_Add(stb_image
  PREFIX stb_image
  URL https://github.com/extemporelang/stb/archive/master.zip
  CMAKE_ARGS -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
             -DCMAKE_C_FLAGS=${PACKAGING_C_FLAGS}
             -DCMAKE_CXX_FLAGS=${PACKAGING_C_FLAGS})

ExternalProject_Add(glfw3
  PREFIX glfw3
  URL https://github.com/glfw/glfw/archive/3.1.2.zip
  CMAKE_ARGS -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
             -DCMAKE_C_FLAGS=${PACKAGING_C_FLAGS}
             -DCMAKE_CXX_FLAGS=${PACKAGING_C_FLAGS})

ExternalProject_Add(assimp
  PREFIX assimp
  URL https://github.com/assimp/assimp/archive/v3.1.1.zip
  CMAKE_ARGS -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
             -DCMAKE_C_FLAGS=${PACKAGING_C_FLAGS}
             -DCMAKE_CXX_FLAGS=${PACKAGING_C_FLAGS})

# libsndfile
# portmidi
