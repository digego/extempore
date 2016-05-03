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

# file(MAKE_DIRECTORY ${CTEST_BASE_DIRECTORY}/source)
# file(MAKE_DIRECTORY ${CTEST_BASE_DIRECTORY}/build)

set(CTEST_BUILD_NAME "${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_VERSION}-${CMAKE_SYSTEM_PROCESSOR}")
# set(CTEST_BUILD_CONFIGURATION "Release")

if(UNIX)
  set(CTEST_CMAKE_GENERATOR "Unix Makefiles")
elseif(WIN32)
  set(CTEST_CMAKE_GENERATOR "Visual Studio 14 2015 Win64")
endif()

if(NOT EXISTS "${CTEST_BASE_DIRECTORY}/source")
  set(CTEST_CHECKOUT_COMMAND "${CTEST_GIT_COMMAND} clone https://github.com/digego/extempore.git ${CTEST_BASE_DIRECTORY}/source")
endif()

ctest_start(Continuous "${CTEST_BASE_DIRECTORY}/source" "${CTEST_BASE_DIRECTORY}/build")

ctest_update(SOURCE ${CTEST_BASE_DIRECTORY}/source)

ctest_configure(
  BUILD ${CTEST_BASE_DIRECTORY}/build
  SOURCE ${CTEST_BASE_DIRECTORY}/source
  OPTIONS "-DBUILD_TESTS=ON")

ctest_build(
  BUILD ${CTEST_BASE_DIRECTORY}/build
  FLAGS -j4
  CONFIGURATION Release)

ctest_test(BUILD ${CTEST_BASE_DIRECTORY}/build)

ctest_submit()
