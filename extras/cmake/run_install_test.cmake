# Installed-tree test: install the build to a temporary prefix, make it
# read-only, and check that the installed binary, run from outside the source
# tree, finds its files and AOT-compiles into the per-user cache dir. Invoked
# via cmake -P. Required variables:
#
#   BUILD_DIR    the build tree to install
#   SOURCE_DIR   the source tree, for the AOT test fixture
#   SCRATCH      a directory this test may delete and recreate

foreach(var BUILD_DIR SOURCE_DIR SCRATCH)
    if(NOT DEFINED ${var})
        message(FATAL_ERROR "run_install_test.cmake: required variable ${var} not set")
    endif()
endforeach()

set(prefix "${SCRATCH}/prefix")
set(cache "${SCRATCH}/cache")
set(work "${SCRATCH}/work")
if(EXISTS "${prefix}")
    file(CHMOD_RECURSE "${prefix}" DIRECTORY_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE)
endif()
file(REMOVE_RECURSE "${SCRATCH}")
file(MAKE_DIRECTORY "${work}/tests/core")
file(COPY "${SOURCE_DIR}/tests/core/aot-compilation-lib.xtm" "${SOURCE_DIR}/tests/core/aot-compilation.xtm"
    DESTINATION "${work}/tests/core")

# everything but the assets, which are large and which nothing here needs
execute_process(
    COMMAND "${CMAKE_COMMAND}" --install "${BUILD_DIR}" --prefix "${prefix}" --component Unspecified
    OUTPUT_QUIET
    RESULT_VARIABLE rc)
if(NOT rc EQUAL 0)
    message(FATAL_ERROR "cmake --install failed (rc=${rc})")
endif()
if(NOT CMAKE_HOST_WIN32)
    file(CHMOD_RECURSE "${prefix}"
        FILE_PERMISSIONS OWNER_READ OWNER_EXECUTE
        DIRECTORY_PERMISSIONS OWNER_READ OWNER_EXECUTE)
endif()

set(extempore "${prefix}/bin/extempore")
if(CMAKE_HOST_WIN32)
    string(APPEND extempore ".exe")
endif()

function(run_extempore what)
    execute_process(
        COMMAND "${CMAKE_COMMAND}" -E env "EXTEMPORE_CACHE_DIR=${cache}" "${extempore}" ${ARGN}
        WORKING_DIRECTORY "${work}"
        RESULT_VARIABLE rc
        OUTPUT_VARIABLE out
        ERROR_VARIABLE err
        TIMEOUT 300)
    if(NOT rc EQUAL 0)
        message(FATAL_ERROR "${what} failed (rc=${rc})\nstdout:\n${out}\nstderr:\n${err}")
    endif()
    set(out "${out}" PARENT_SCOPE)
endfunction()

# the directories, and a core library loaded from the installed AOT cache
run_extempore("the directory check"
    exec -e "(sys:load \"libs/core/math.xtm\") (println (sys:share-dir)) (println (sys:lib-dir)) (println (sys:cache-dir)) (println (sys:aot-cache-path \"math.xtm\"))")
string(REPLACE "\\" "/" out "${out}")
file(TO_CMAKE_PATH "${prefix}" prefix_path)
file(TO_CMAKE_PATH "${cache}" cache_path)
foreach(expected
        "\"${prefix_path}/share/extempore\""
        "\"${prefix_path}/lib/extempore\""
        "\"${cache_path}\""
        "\"${prefix_path}/lib/extempore/aot-cache/math.xtm\"")
    string(FIND "${out}" "${expected}" at)
    if(at EQUAL -1)
        message(FATAL_ERROR "expected ${expected} in the output:\n${out}")
    endif()
endforeach()

# AOT compilation writes to the cache dir, and a fresh process loads it there
run_extempore("the AOT compile" compile tests/core/aot-compilation-lib.xtm)
if(NOT EXISTS "${cache}/aot-cache/xtmaot-compilation-lib.ll")
    message(FATAL_ERROR "the AOT compile wrote nothing to ${cache}/aot-cache")
endif()
run_extempore("the AOT test" test tests/core/aot-compilation.xtm)

file(CHMOD_RECURSE "${prefix}" DIRECTORY_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE)
file(REMOVE_RECURSE "${SCRATCH}")
