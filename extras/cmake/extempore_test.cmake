set(CTEST_PROJECT_NAME Extempore)
set(CTEST_SITE "Extempore")
set(CTEST_DROP_METHOD "http")
set(CTEST_DROP_SITE "my.cdash.org")
set(CTEST_DROP_LOCATION "/submit.php?project=Extempore")
set(CTEST_DROP_SITE_CDASH TRUE)

find_program(CTEST_GIT_COMMAND NAMES git)

if(UNIX)
  set(CTEST_BASE_DIRECTORY "/tmp/extempore-ctest")
elseif(WIN32)
  set(CTEST_BASE_DIRECTORY "$ENV{TEMP}/extempore-ctest")
endif()

set(CTEST_SOURCE_DIRECTORY "${CTEST_BASE_DIRECTORY}/source")
set(CTEST_BINARY_DIRECTORY "${CTEST_BASE_DIRECTORY}/build")

file(MAKE_DIRECTORY ${CTEST_SOURCE_DIRECTORY})
file(MAKE_DIRECTORY ${CTEST_BINARY_DIRECTORY})

set(CTEST_BUILD_NAME "${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_VERSION}-${CMAKE_SYSTEM_PROCESSOR}")
set(CTEST_BUILD_CONFIGURATION "Release")

if(UNIX)
  set(CTEST_CMAKE_GENERATOR "Unix Makefiles")
elseif(WIN32)
  set(CTEST_CMAKE_GENERATOR "Visual Studio 14 2015 Win64")
endif()

if(NOT EXISTS "${CTEST_SOURCE_DIRECTORY}")
  set(CTEST_CHECKOUT_COMMAND "${CTEST_GIT_COMMAND} clone https://github.com/digego/extempore.git ${CTEST_SOURCE_DIRECTORY}")
endif()

set(CTEST_CONFIGURE_COMMAND "${CMAKE_COMMAND} \"-G${CTEST_CMAKE_GENERATOR}\" -DCMAKE_BUILD_TYPE=${CTEST_BUILD_CONFIGURATION} -DBUILD_TESTS=ON \"${CTEST_SOURCE_DIRECTORY}\"")
# set(CTEST_UPDATE_COMMAND "${CTEST_GIT_COMMAND} pull")

ctest_start(Continuous)
ctest_update(RETURN_VALUE count)

if(count GREATER 0)
  # if master has been updated, run test and upload results
  ctest_configure()
  ctest_build()
  ctest_test()
  ctest_submit()
else()
  message(STATUS "No changes detected, current test results are up-do-date")
endif()
