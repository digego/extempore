set(CTEST_PROJECT_NAME Extempore)
site_name(CTEST_SITE)
set(CTEST_DROP_METHOD "http")
set(CTEST_DROP_SITE "my.cdash.org")
set(CTEST_DROP_LOCATION "/submit.php?project=Extempore")
set(CTEST_DROP_SITE_CDASH TRUE)

include(${CMAKE_CURRENT_LIST_DIR}/platform.cmake)
extempore_detect_platform()

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
    # Let CMake auto-detect the available Visual Studio version
    # This avoids hardcoding an outdated generator
endif()

ctest_start(Continuous)
ctest_update()
ctest_configure()
ctest_build(CONFIGURATION Release TARGET aot_core)
ctest_test()
ctest_submit()
