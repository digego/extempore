# Test registration for Extempore
# Requires: BUILD_TESTS option, extempore target

if(NOT BUILD_TESTS)
    return()
endif()

include(CTest)

add_subdirectory(tests/cpp-unit)

set(EXTEMPORE_TEST_PORT_COUNTER 17099 CACHE INTERNAL "")

# Per-assertion JUnit XML reports are written here, one file per test file, so
# ctest and CI can surface individual assertion failures (not just which .xtm
# file failed). Safe to upload as a CI test-report artifact.
set(EXTEMPORE_TEST_RESULTS_DIR ${CMAKE_CURRENT_BINARY_DIR}/test-results)
file(MAKE_DIRECTORY ${EXTEMPORE_TEST_RESULTS_DIR})

function(extempore_get_next_port OUT_VAR)
    set(${OUT_VAR} ${EXTEMPORE_TEST_PORT_COUNTER} PARENT_SCOPE)
    math(EXPR _new_port "${EXTEMPORE_TEST_PORT_COUNTER} - 2")
    set(EXTEMPORE_TEST_PORT_COUNTER ${_new_port} CACHE INTERNAL "" FORCE)
endfunction()

macro(extempore_add_test testfile label)
    extempore_get_next_port(_port)
    string(REPLACE "/" "_" _safe "${testfile}")
    add_test(NAME ${testfile}
        COMMAND extempore --term nocolor --port=${_port}
            --batch "(xtmtest-run-tests \"${testfile}\" #t #t \"${EXTEMPORE_TEST_RESULTS_DIR}/${_safe}.xml\")")
    set_tests_properties(${testfile} PROPERTIES
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        LABELS ${label})
endmacro()

macro(extempore_add_ipc_test testfile label)
    extempore_get_next_port(_port)
    string(REPLACE "/" "_" _safe "${testfile}")
    add_test(NAME ${testfile}
        COMMAND extempore --noaudio --term nocolor --port=${_port}
            --eval "(xtmtest-run-tests \"${testfile}\" #t #t \"${EXTEMPORE_TEST_RESULTS_DIR}/${_safe}.xml\")")
    set_tests_properties(${testfile} PROPERTIES
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        LABELS ${label})
endmacro()

macro(extempore_add_example_as_test examplefile timeout label)
    extempore_get_next_port(_port)
    add_test(NAME ${examplefile}
        COMMAND extempore --term nocolor --port=${_port}
            --batch "(sys:load-then-quit \"${examplefile}\" ${timeout})")
    set_tests_properties(${examplefile} PROPERTIES
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        TIMEOUT 300
        LABELS ${label})
endmacro()

# Render a DSP file offline via --audio-outfile, then assert on the output WAV
# using libs/core/audiotest.xtm. assert_expr is any xtlang expression returning
# i64 (0 pass, non-zero fail); use @WAV@ in the expression to reference the
# rendered file path, e.g.:
#
#   (audiotest_assert_sine "@WAV@" 440.0)
#   (audiotest_assert_stereo_sine "@WAV@" 330.0 550.0)
#   (audiotest_assert_three_sines "@WAV@" 330.0 550.0 770.0)
#
# duration is the length of audio to render (seconds). Two-phase: implemented
# as a cmake -P script so it works cross-platform.
macro(extempore_add_audio_offline_test testname renderfile duration assert_expr label)
    set(_wav "${CMAKE_CURRENT_BINARY_DIR}/audio_offline_${testname}.wav")
    add_test(NAME audio-offline/${testname}
        COMMAND ${CMAKE_COMMAND}
            -DEXTEMPORE=$<TARGET_FILE:extempore>
            -DWORK_DIR=${CMAKE_CURRENT_SOURCE_DIR}
            -DRENDER_XTM=${renderfile}
            -DWAV_PATH=${_wav}
            -DDURATION=${duration}
            -DASSERT_EXPR=${assert_expr}
            -DRENDER_TIMEOUT=60
            -DVERIFY_TIMEOUT=60
            -P ${CMAKE_CURRENT_SOURCE_DIR}/extras/cmake/run_audio_offline_test.cmake)
    set_tests_properties(audio-offline/${testname} PROPERTIES
        TIMEOUT 180
        LABELS ${label})
endmacro()

# Core and compiler xtlang tests are auto-discovered: every tests/core/*.xtm
# and tests/compiler/*.xtm runs as a batch test, labelled by its directory. The
# few that need a bespoke harness (or aren't standalone) are excluded here and
# registered explicitly below. CONFIGURE_DEPENDS re-globs when a test file is
# added or removed, so new tests are picked up without editing this file.
cmake_policy(SET CMP0057 NEW) # enable IN_LIST
file(GLOB _core_tests CONFIGURE_DEPENDS RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}
     ${CMAKE_CURRENT_SOURCE_DIR}/tests/core/*.xtm)
file(GLOB _compiler_tests CONFIGURE_DEPENDS RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}
     ${CMAKE_CURRENT_SOURCE_DIR}/tests/compiler/*.xtm)

# excluded from batch auto-registration (handled specially below, or fixtures)
set(_xtm_special_tests
    tests/core/system.xtm              # IPC test, driven with --eval
    tests/core/aot-compilation.xtm     # two-phase AOT round-trip
    tests/core/aot-compilation-lib.xtm) # fixture library for the AOT test

foreach(_t ${_core_tests})
    if(NOT _t IN_LIST _xtm_special_tests)
        extempore_add_test(${_t} libs-core)
    endif()
endforeach()
foreach(_t ${_compiler_tests})
    if(NOT _t IN_LIST _xtm_special_tests)
        extempore_add_test(${_t} compiler-unit)
    endif()
endforeach()

# system.xtm exercises inter-process eval, so it needs --eval not --batch
extempore_add_ipc_test(tests/core/system.xtm libs-core)

# failing.xtm is the quarantine for known-broken tests (see docs/reference/
# testing.md). Registered as an EXPECTED failure: ctest stays green while the
# bugs persist, and flips to failing the moment a fix makes the file pass --
# the cue to move that test into its proper home.
extempore_get_next_port(_failing_port)
add_test(NAME tests/failing.xtm
    COMMAND extempore --term nocolor --port=${_failing_port}
        --batch "(xtmtest-run-tests \"tests/failing.xtm\" #t #t)")
set_tests_properties(tests/failing.xtm PROPERTIES
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    LABELS libs-core
    TIMEOUT 120
    WILL_FAIL TRUE)

# AOT compilation round-trip test (two-phase): AOT-compile a fixture library,
# then in a fresh process load it from its precompiled cache and assert the
# AOT-compiled code executes and preserves type/global-var docstrings.
add_test(NAME tests/core/aot-compilation.xtm
    COMMAND ${CMAKE_COMMAND}
        -DEXTEMPORE=$<TARGET_FILE:extempore>
        -DWORK_DIR=${CMAKE_CURRENT_SOURCE_DIR}
        -DLIB_XTM=tests/core/aot-compilation-lib.xtm
        -DTEST_XTM=tests/core/aot-compilation.xtm
        -DCACHE_LL=libs/aot-cache/xtmaot-compilation-lib.ll
        -DCOMPILE_TIMEOUT=120
        -DTEST_TIMEOUT=60
        -P ${CMAKE_CURRENT_SOURCE_DIR}/extras/cmake/run_aot_test.cmake)
set_tests_properties(tests/core/aot-compilation.xtm PROPERTIES
    TIMEOUT 200
    LABELS compiler-unit)

# External library tests
extempore_add_test(tests/external/fft.xtm libs-external)

# Offline audio-rendering tests: render a DSP to a WAV with --audio-outfile,
# then verify it contains a sine at the expected frequency. Free-run driver
# means ~1s of audio renders in well under a second; 1.0s of content is
# plenty for a reliable Goertzel check.
extempore_add_audio_offline_test(hello-sine
    examples/core/hello-sine.xtm 1.0
    "(audiotest_assert_sine \"@WAV@\" 440.0)"
    audio-offline)
extempore_add_audio_offline_test(hello-stereo
    examples/core/hello-stereo.xtm 1.0
    "(audiotest_assert_stereo_sine \"@WAV@\" 330.0 550.0)"
    audio-offline)
extempore_add_audio_offline_test(hello-mt
    examples/core/hello-mt.xtm 1.0
    "(audiotest_assert_three_sines \"@WAV@\" 330.0 550.0 770.0)"
    audio-offline)
extempore_add_audio_offline_test(hello-sweep
    examples/core/hello-sweep.xtm 2.0
    "(audiotest_assert_sweep \"@WAV@\" 330.0 770.0 44100 8000)"
    audio-offline)

# Core examples
extempore_add_example_as_test(examples/core/audio_101.xtm 10 examples-audio)
extempore_add_example_as_test(examples/core/audio_streams.xtm 10 examples-audio)
extempore_add_example_as_test(examples/core/covers.xtm 30 examples-audio)
extempore_add_example_as_test(examples/core/expr_problem.xtm 10 examples-core)
extempore_add_example_as_test(examples/core/extempore_lang.xtm 60 examples-core)
extempore_add_example_as_test(examples/core/fmsynth.xtm 10 examples-audio)
extempore_add_example_as_test(examples/core/mtaudio.xtm 10 examples-audio)
extempore_add_example_as_test(examples/core/nbody_lang_shootout.xtm 10 examples-core)
extempore_add_example_as_test(examples/core/osc_101.xtm 10 examples-core)
extempore_add_example_as_test(examples/core/petri-net.xtm 10 examples-core)
extempore_add_example_as_test(examples/core/scheduler.xtm 10 examples-audio)
extempore_add_example_as_test(examples/core/synth.xtm 10 examples-audio)
extempore_add_example_as_test(examples/core/topclock_metro.xtm 15 examples-audio)
extempore_add_example_as_test(examples/core/typeclasses.xtm 10 examples-core)
extempore_add_example_as_test(examples/core/xthread.xtm 15 examples-core)

# External examples - audio
extempore_add_example_as_test(examples/external/audio_player.xtm 10 examples-audio)
extempore_add_example_as_test(examples/external/convolution_reverb.xtm 10 examples-audio)
extempore_add_example_as_test(examples/external/electrofunk.xtm 10 examples-audio)
extempore_add_example_as_test(examples/external/portmidi-output.xtm 10 examples-audio)
extempore_add_example_as_test(examples/external/portmidi.xtm 10 examples-audio)
extempore_add_example_as_test(examples/external/sampler.xtm 10 examples-audio)
extempore_add_example_as_test(examples/external/sing_a_song.xtm 10 examples-audio)

# WebGPU graphics examples
if(EXTERNAL_SHLIBS_GRAPHICS)
    extempore_add_example_as_test(examples/external/webgpu-triangle.xtm 10 examples-graphics)
    extempore_add_example_as_test(examples/external/shadertoy.xtm 10 examples-graphics)
endif()
