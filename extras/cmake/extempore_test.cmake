set(CTEST_PROJECT_NAME Extempore)
set(CTEST_SITE "Extempore")
set(CTEST_DROP_METHOD "http")
set(CTEST_DROP_SITE "my.cdash.org")
set(CTEST_DROP_LOCATION "/submit.php?project=Extempore")
set(CTEST_DROP_SITE_CDASH TRUE)

find_program(CTEST_GIT_COMMAND NAMES git)
set(CTEST_UPDATE_COMMAND "${CTEST_GIT_COMMAND}")

if(UNIX)
  set(CTEST_BASE_DIRECTORY "/tmp/extempore-ctest")
elseif(WIN32)
  set(CTEST_BASE_DIRECTORY "$ENV{TEMP}/extempore-ctest")
endif()

set(CTEST_SOURCE_DIRECTORY "${CTEST_BASE_DIRECTORY}/source")
set(CTEST_BINARY_DIRECTORY "${CTEST_BASE_DIRECTORY}/build")

if(NOT EXISTS "${CTEST_BASE_DIRECTORY}/source")
  set(CTEST_CHECKOUT_COMMAND "${CTEST_GIT_COMMAND} clone https://github.com/digego/extempore.git source")
endif()

set(CTEST_BUILD_NAME "${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_VERSION}-${CMAKE_SYSTEM_PROCESSOR}")

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
