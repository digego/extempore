set(CTEST_PROJECT_NAME Extempore)
site_name(CTEST_SITE)
set(CTEST_DROP_METHOD "http")
set(CTEST_DROP_SITE "my.cdash.org")
set(CTEST_DROP_LOCATION "/submit.php?project=Extempore")
set(CTEST_DROP_SITE_CDASH TRUE)

# this is a hack - copied from Extempore's CMakeLists.txt

if(UNIX)
  find_program(UNAME_PROGRAM uname)
  execute_process(COMMAND ${UNAME_PROGRAM} -m
    OUTPUT_VARIABLE UNAME_MACHINE_NAME
    OUTPUT_STRIP_TRAILING_WHITESPACE)
  execute_process(COMMAND ${UNAME_PROGRAM} -r
    OUTPUT_VARIABLE UNAME_OS_RELEASE
    OUTPUT_STRIP_TRAILING_WHITESPACE)
  execute_process(COMMAND ${UNAME_PROGRAM} -s
    OUTPUT_VARIABLE UNAME_OS_NAME
    OUTPUT_STRIP_TRAILING_WHITESPACE)
endif(UNIX)

if(APPLE)
  set(EXTEMPORE_SYSTEM_NAME "osx")
  execute_process(COMMAND sw_vers -productVersion
    OUTPUT_VARIABLE EXTEMPORE_SYSTEM_VERSION
    OUTPUT_STRIP_TRAILING_WHITESPACE)
  string(REGEX MATCH "^10.[0-9]+" EXTEMPORE_SYSTEM_VERSION ${EXTEMPORE_SYSTEM_VERSION})
  set(EXTEMPORE_SYSTEM_ARCHITECTURE ${UNAME_MACHINE_NAME})
elseif(UNIX)
  # try lsb_release first - better at giving the distro name
  execute_process(COMMAND lsb_release -is
    OUTPUT_VARIABLE EXTEMPORE_SYSTEM_NAME
    OUTPUT_STRIP_TRAILING_WHITESPACE)
  if(NOT EXTEMPORE_SYSTEM_NAME)
    # otherwise use uname output
    set(EXTEMPORE_SYSTEM_NAME ${UNAME_OS_NAME})
  endif()
  set(EXTEMPORE_SYSTEM_VERSION ${UNAME_OS_RELEASE})
  set(EXTEMPORE_SYSTEM_ARCHITECTURE ${UNAME_MACHINE_NAME})
elseif(WIN32)
  set(EXTEMPORE_SYSTEM_NAME "Windows")
  execute_process(COMMAND wmic os get Caption /value
    OUTPUT_VARIABLE EXTEMPORE_SYSTEM_VERSION
    OUTPUT_STRIP_TRAILING_WHITESPACE)
  string(REGEX MATCH "[0-9]+" EXTEMPORE_SYSTEM_VERSION ${EXTEMPORE_SYSTEM_VERSION})
  set(EXTEMPORE_SYSTEM_ARCHITECTURE ${CMAKE_SYSTEM_PROCESSOR})
else()
  message(FATAL_ERROR "Sorry, Extempore isn't supported on this platform - OSX, Linux & Windows only.")
endif()

set(CTEST_BUILD_NAME "${EXTEMPORE_SYSTEM_NAME}-${EXTEMPORE_SYSTEM_VERSION}-${EXTEMPORE_SYSTEM_ARCHITECTURE}")

find_program(CTEST_GIT_COMMAND NAMES git)
set(CTEST_UPDATE_COMMAND "${CTEST_GIT_COMMAND}")

if(UNIX)
  set(CTEST_BASE_DIRECTORY "/tmp/extempore-ctest")
elseif(WIN32)
  set(CTEST_BASE_DIRECTORY "$ENV{HOMEPATH}/extempore-ctest")
endif()

set(CTEST_SOURCE_DIRECTORY "${CTEST_BASE_DIRECTORY}/source")
set(CTEST_BINARY_DIRECTORY "${CTEST_BASE_DIRECTORY}/build")

if(NOT EXISTS "${CTEST_BASE_DIRECTORY}/source")
  set(CTEST_CHECKOUT_COMMAND "${CTEST_GIT_COMMAND} clone https://github.com/digego/extempore.git source")
endif()

if(UNIX)
  set(CTEST_CMAKE_GENERATOR "Unix Makefiles")
elseif(WIN32)
  set(CTEST_CMAKE_GENERATOR "Visual Studio 14 2015 Win64")
endif()

ctest_start(Continuous)

ctest_update()

ctest_configure()

ctest_build(CONFIGURATION Release TARGET aot_extended)

ctest_test()

ctest_submit()
