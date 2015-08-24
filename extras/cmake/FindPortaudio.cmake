# - Try to find Portaudio
# Once done this will define
#
#  PORTAUDIO_FOUND - system has Portaudio
#  PORTAUDIO_INCLUDE_DIRS - the Portaudio include directory
#  PORTAUDIO_LIBRARIES - Link these to use Portaudio
#  PORTAUDIO_DEFINITIONS - Compiler switches required for using Portaudio
#  PORTAUDIO_VERSION - Portaudio version
#
#  Copyright (c) 2006 Andreas Schneider <mail@cynapses.org>
#
# Redistribution and use is allowed according to the terms of the New BSD license.
# For details see the accompanying COPYING-CMAKE-SCRIPTS file.
#

# to provide a hint for where to look, set the Portaudio_ROOT variable

if(NOT WIN32)
  include(FindPkgConfig)
  pkg_check_modules(PORTAUDIO portaudio-2.0)
endif()

find_path(PORTAUDIO_INCLUDE_DIR
  NAMES portaudio.h
  PATHS /usr/include /usr/local/include /opt/local/include /sw/include ${Portaudio_ROOT}/include
  )

find_library(PORTAUDIO_LIBRARY
  NAMES ${CMAKE_STATIC_LIBRARY_PREFIX}portaudio${CMAKE_STATIC_LIBRARY_SUFFIX}
  PATHS /usr/lib /usr/local/lib /opt/local/lib /sw/lib ${Portaudio_ROOT}/lib
  )

set(PORTAUDIO_INCLUDE_DIRS
  ${PORTAUDIO_INCLUDE_DIR}
  )
set(PORTAUDIO_LIBRARIES
  ${PORTAUDIO_LIBRARY}
  )

if(PORTAUDIO_INCLUDE_DIRS AND PORTAUDIO_LIBRARIES)
  set(PORTAUDIO_FOUND TRUE)
endif(PORTAUDIO_INCLUDE_DIRS AND PORTAUDIO_LIBRARIES)

if(PORTAUDIO_FOUND)
  if(NOT Portaudio_FIND_QUIETLY)
    message(STATUS "Found Portaudio: ${PORTAUDIO_LIBRARIES}")
  endif(NOT Portaudio_FIND_QUIETLY)
elseif(Portaudio_FIND_REQUIRED)
  message(FATAL_ERROR "Could not find Portaudio")
endif()

# show the PORTAUDIO_INCLUDE_DIRS and PORTAUDIO_LIBRARIES variables only in the advanced view
mark_as_advanced(PORTAUDIO_INCLUDE_DIRS PORTAUDIO_LIBRARIES)
