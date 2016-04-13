include(CTest)

set(CTEST_PROJECT_NAME Extempore)
set(CTEST_SITE "http:://extempore.moso.com.au")

find_program(CTEST_GIT_COMMAND NAMES git)

if(UNIX)
  set(CTEST_BASE_DIRECTORY "$ENV{TMPDIR}/extempore-ctest/")
elseif(WIN32)
  set(CTEST_BASE_DIRECTORY "$ENV{TEMP}/extempore-ctest/")
endif()

set(CTEST_SOURCE_DIRECTORY "${CTEST_BASE_DIRECTORY}/ctest/source")
set(CTEST_BINARY_DIRECTORY "${CTEST_BASE_DIRECTORY}/ctest/build")

set(CTEST_BUILD_NAME ${CMAKE_SYSTEM_NAME})

# ctest_empty_binary_directory(${CTEST_BINARY_DIRECTORY})

set(CTEST_BUILD_CONFIGURATION "Release")

if(UNIX)
  set(CTEST_CMAKE_GENERATOR "Unix Makefiles")
elseif(WIN32)
  set(CTEST_CMAKE_GENERATOR "Visual Studio 14 2015 Win64")
endif()

if(NOT EXISTS "${CTEST_SOURCE_DIRECTORY}")
  set(CTEST_CHECKOUT_COMMAND "${CTEST_GIT_COMMAND} clone --depth 1 https://github.com/digego/extempore.git ${CTEST_SOURCE_DIRECTORY}")
endif()

set(CTEST_UPDATE_COMMAND "${CTEST_GIT_COMMAND}")

set(CTEST_CONFIGURE_COMMAND "${CMAKE_COMMAND} \"-G${CTEST_CMAKE_GENERATOR}\" -DCMAKE_BUILD_TYPE=${CTEST_BUILD_CONFIGURATION} -DWITH_TESTING=ON \"${CTEST_SOURCE_DIRECTORY}\"")

# Open CDash config

set(CTEST_NIGHTLY_START_TIME "00:00:00 EST")

set(CTEST_DROP_METHOD "http")
set(CTEST_DROP_SITE "my.cdash.org")
set(CTEST_DROP_LOCATION "/submit.php?project=Extempore")
set(CTEST_DROP_SITE_CDASH TRUE)

# do all the things
ctest_start("Nightly")
ctest_update()
ctest_configure()
ctest_build()
ctest_test()
ctest_submit()
