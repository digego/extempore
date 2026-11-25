# Two-phase offline audio test: render a DSP to a float32 WAV, then load the
# audiotest.xtm assertion helpers and verify the output. Invoked via cmake -P
# so it works on all platforms supported by CTest. Required variables:
#
#   EXTEMPORE       full path to the extempore binary under test
#   WORK_DIR        working directory (extempore source tree; used for sys:load paths)
#   RENDER_XTM      relative path to the DSP file to render (loaded via sys:load)
#   WAV_PATH        absolute path to the WAV file to produce
#   DURATION        seconds of audio to render (float)
#   ASSERT_EXPR     xtlang assertion expression, with @WAV@ as a placeholder
#                   for the rendered WAV path. Example:
#                     "(audiotest_assert_sine \"@WAV@\" 440.0)"
#   RENDER_TIMEOUT  seconds of wall-clock time allowed for the render phase
#   VERIFY_TIMEOUT  seconds of wall-clock time allowed for the verify phase
#   PORT            TCP port for the render phase. Both phases start a full
#                   extempore, which binds its server port, so without this
#                   every offline test would sit on the default 7099 and
#                   collide under `ctest --parallel`. The verify phase gets
#                   PORT-1, which the caller leaves free for it.
#
# Optional: EXTRA_ARGS, a list of further arguments for the render phase. With
# EXTEMPORE_RENDER_TWICE set in the environment the file is rendered a second
# time and the two WAVs must be identical.

foreach(var EXTEMPORE WORK_DIR RENDER_XTM WAV_PATH DURATION ASSERT_EXPR RENDER_TIMEOUT VERIFY_TIMEOUT PORT)
    if(NOT DEFINED ${var})
        message(FATAL_ERROR "run_audio_offline_test.cmake: required variable ${var} not set")
    endif()
endforeach()

string(REPLACE "@WAV@" "${WAV_PATH}" ASSERT_EXPR_EXPANDED "${ASSERT_EXPR}")

# Phase 1: render
file(REMOVE "${WAV_PATH}")
execute_process(
    COMMAND "${EXTEMPORE}" --term nocolor --port=${PORT}
        --batch "(sys:load \"${RENDER_XTM}\")"
        --audio-outfile "${WAV_PATH}"
        --duration ${DURATION}
        ${EXTRA_ARGS}
    WORKING_DIRECTORY "${WORK_DIR}"
    RESULT_VARIABLE render_rc
    TIMEOUT ${RENDER_TIMEOUT})

if(NOT render_rc EQUAL 0)
    message(FATAL_ERROR "render phase failed (rc=${render_rc})")
endif()

if(NOT EXISTS "${WAV_PATH}")
    message(FATAL_ERROR "render phase produced no WAV at ${WAV_PATH}")
endif()

if(DEFINED ENV{EXTEMPORE_RENDER_TWICE})
    set(_again "${WAV_PATH}.again.wav")
    file(REMOVE "${_again}")
    execute_process(
        COMMAND "${EXTEMPORE}" --term nocolor --port=${PORT}
            --batch "(sys:load \"${RENDER_XTM}\")"
            --audio-outfile "${_again}"
            --duration ${DURATION}
            ${EXTRA_ARGS}
        WORKING_DIRECTORY "${WORK_DIR}"
        RESULT_VARIABLE again_rc
        TIMEOUT ${RENDER_TIMEOUT})
    if(NOT again_rc EQUAL 0)
        message(FATAL_ERROR "second render failed (rc=${again_rc})")
    endif()
    execute_process(
        COMMAND "${CMAKE_COMMAND}" -E compare_files "${WAV_PATH}" "${_again}"
        RESULT_VARIABLE same_rc)
    if(NOT same_rc EQUAL 0)
        message(FATAL_ERROR "two renders of ${RENDER_XTM} differ")
    endif()
endif()

# Phase 2: verify
math(EXPR _verify_port "${PORT} - 1")
execute_process(
    COMMAND "${EXTEMPORE}" --term nocolor --port=${_verify_port}
        --batch "(begin (sys:load \"libs/core/audiotest.xtm\") (quit ${ASSERT_EXPR_EXPANDED}))"
    WORKING_DIRECTORY "${WORK_DIR}"
    RESULT_VARIABLE verify_rc
    TIMEOUT ${VERIFY_TIMEOUT})

if(NOT verify_rc EQUAL 0)
    message(FATAL_ERROR "verify phase failed (rc=${verify_rc})")
endif()
