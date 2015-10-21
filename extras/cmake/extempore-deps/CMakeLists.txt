cmake_minimum_required(VERSION 3.0)

project(extempore-deps)

# Set a default build type if none was specified

if(NOT CMAKE_BUILD_TYPE)
  message(STATUS "Building 'Release' configuration")
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build." FORCE)
  # Set the possible values of build type for cmake-gui
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
endif()

include(ExternalProject)

ExternalProject_Add(nanovg
  PREFIX nanovg
  URL https://github.com/extemporelang/nanovg/archive/master.zip
  CMAKE_ARGS -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE})

ExternalProject_Add(kiss_fft
  PREFIX kiss_fft
  URL https://github.com/extemporelang/kiss_fft/archive/master.zip
  CMAKE_ARGS -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE})

ExternalProject_Add(stb_image
  PREFIX stb_image
  URL https://github.com/extemporelang/stb/archive/master.zip
  CMAKE_ARGS -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE})
