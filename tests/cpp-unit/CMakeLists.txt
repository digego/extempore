add_executable(extempore_cpp_unit
    univ_test.cpp)

target_link_libraries(extempore_cpp_unit PRIVATE
    gtest_main)

target_include_directories(extempore_cpp_unit PRIVATE
    ${CMAKE_SOURCE_DIR}/include
    ${CMAKE_SOURCE_DIR}/src)

target_compile_features(extempore_cpp_unit PRIVATE cxx_std_17)

if(UNIX AND NOT APPLE)
    target_link_libraries(extempore_cpp_unit PRIVATE pthread)
endif()

include(GoogleTest)
gtest_discover_tests(extempore_cpp_unit
    PROPERTIES LABELS "cpp-unit")
