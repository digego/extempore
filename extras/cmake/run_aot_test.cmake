# Two-phase AOT round-trip test: AOT-compile a fixture library, then load it
# from its precompiled cache in a fresh process and run xtmtest assertions
# against it. Invoked via cmake -P so it works on all platforms supported by
# CTest. Required variables:
#
#   EXTEMPORE        full path to the extempore binary under test
#   WORK_DIR         working directory (extempore source tree; for sys:load paths)
#   LIB_XTM          relative path to the fixture library to AOT-compile (phase 1)
#   TEST_XTM         relative path to the xtmtest test file to run (phase 2)
#   CACHE_LL         relative path to the AOT artifact phase 1 must produce
#   COMPILE_TIMEOUT  seconds allowed for the AOT-compile phase
#   TEST_TIMEOUT     seconds allowed for the test phase

foreach(var EXTEMPORE WORK_DIR LIB_XTM TEST_XTM CACHE_LL COMPILE_TIMEOUT TEST_TIMEOUT)
    if(NOT DEFINED ${var})
        message(FATAL_ERROR "run_aot_test.cmake: required variable ${var} not set")
    endif()
endforeach()

# Phase 1: AOT-compile the fixture (remove any stale artifact first so the
# EXISTS check below actually proves this phase produced it)
file(REMOVE "${WORK_DIR}/${CACHE_LL}")
execute_process(
    COMMAND "${EXTEMPORE}" --nobase --term nocolor
        --batch "(impc:aot:compile-xtm-file \"${LIB_XTM}\")"
    WORKING_DIRECTORY "${WORK_DIR}"
    RESULT_VARIABLE compile_rc
    TIMEOUT ${COMPILE_TIMEOUT})

if(NOT compile_rc EQUAL 0)
    message(FATAL_ERROR "AOT compile phase failed (rc=${compile_rc})")
endif()

if(NOT EXISTS "${WORK_DIR}/${CACHE_LL}")
    message(FATAL_ERROR "AOT compile phase produced no cache at ${CACHE_LL}")
endif()

# Phase 2: load the precompiled fixture in a fresh process and run the tests.
# xtmtest-run-tests quits with a non-zero exit code if any assertion fails.
execute_process(
    COMMAND "${EXTEMPORE}" --term nocolor
        --batch "(xtmtest-run-tests \"${TEST_XTM}\" #t #t)"
    WORKING_DIRECTORY "${WORK_DIR}"
    RESULT_VARIABLE test_rc
    TIMEOUT ${TEST_TIMEOUT})

if(NOT test_rc EQUAL 0)
    message(FATAL_ERROR "AOT test phase failed (rc=${test_rc})")
endif()
