add_executable(extempore_cpp_unit
    univ_test.cpp
    sharedir_test.cpp
    netutil_test.cpp
    osc_test.cpp
    zones_test.cpp
    scheduler_test.cpp
    ir_preamble_test.cpp
    bencode_test.cpp
    commandline_test.cpp
    formfinder_test.cpp
    ${CMAKE_SOURCE_DIR}/src/Bencode.cpp
    ${CMAKE_SOURCE_DIR}/src/CommandLine.cpp
    ${CMAKE_SOURCE_DIR}/src/FormFinder.cpp
    ${CMAKE_SOURCE_DIR}/src/EXTThread.cpp
    ${CMAKE_SOURCE_DIR}/src/EXTZones.cpp)

target_link_libraries(extempore_cpp_unit PRIVATE
    gtest_main)

if(WIN32)
    target_link_libraries(extempore_cpp_unit PRIVATE ws2_32)
endif()

target_include_directories(extempore_cpp_unit PRIVATE
    ${CMAKE_SOURCE_DIR}/include
    ${CMAKE_SOURCE_DIR}/src)

target_compile_features(extempore_cpp_unit PRIVATE cxx_std_23)

# Same warning set as the extempore target (defined in the top-level
# CMakeLists.txt), so test code is held to the same standard as what it tests.
target_compile_options(extempore_cpp_unit PRIVATE ${EXTEMPORE_WARNING_FLAGS})

find_package(Threads REQUIRED)
target_link_libraries(extempore_cpp_unit PRIVATE Threads::Threads)

include(GoogleTest)
gtest_discover_tests(extempore_cpp_unit
    PROPERTIES LABELS "cpp-unit")
