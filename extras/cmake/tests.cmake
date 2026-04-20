# Test registration for Extempore
# Requires: BUILD_TESTS option, extempore target

if(NOT BUILD_TESTS)
    return()
endif()

include(CTest)

add_subdirectory(tests/cpp-unit)

set(EXTEMPORE_TEST_PORT_COUNTER 17099 CACHE INTERNAL "")

function(extempore_get_next_port OUT_VAR)
    set(${OUT_VAR} ${EXTEMPORE_TEST_PORT_COUNTER} PARENT_SCOPE)
    math(EXPR _new_port "${EXTEMPORE_TEST_PORT_COUNTER} - 2")
    set(EXTEMPORE_TEST_PORT_COUNTER ${_new_port} CACHE INTERNAL "" FORCE)
endfunction()

macro(extempore_add_test testfile label)
    extempore_get_next_port(_port)
    add_test(NAME ${testfile}
        COMMAND extempore --term nocolor --port=${_port}
            --batch "(xtmtest-run-tests \"${testfile}\" #t #t)")
    set_tests_properties(${testfile} PROPERTIES
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        LABELS ${label})
endmacro()

macro(extempore_add_ipc_test testfile label)
    extempore_get_next_port(_port)
    add_test(NAME ${testfile}
        COMMAND extempore --noaudio --term nocolor --port=${_port}
            --eval "(xtmtest-run-tests \"${testfile}\" #t #t)")
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

# Core library tests
extempore_add_ipc_test(tests/core/system.xtm libs-core)
extempore_add_test(tests/core/adt.xtm libs-core)
extempore_add_test(tests/core/math.xtm libs-core)
extempore_add_test(tests/core/std.xtm libs-core)
extempore_add_test(tests/core/xtlang.xtm libs-core)
extempore_add_test(tests/core/generics.xtm libs-core)

# Compiler unit tests
extempore_add_test(tests/compiler/transforms.xtm compiler-unit)
extempore_add_test(tests/compiler/typeunify.xtm compiler-unit)
extempore_add_test(tests/compiler/typecheck.xtm compiler-unit)
extempore_add_test(tests/compiler/pipeline.xtm compiler-unit)
extempore_add_test(tests/compiler/constraints.xtm compiler-unit)

# External library tests
extempore_add_test(tests/external/fft.xtm libs-external)

# Offline audio-rendering tests: render a DSP to a WAV with --audio-outfile,
# then verify it contains a sine at the expected frequency. Free-run driver
# means ~1s of audio renders in well under a second; 1.0s of content is
# plenty for a reliable Goertzel check.
extempore_add_audio_offline_test(hello_sine
    examples/core/hello_sine.xtm 1.0
    "(audiotest_assert_sine \"@WAV@\" 440.0)"
    audio-offline)
extempore_add_audio_offline_test(hello_stereo
    examples/core/hello_stereo.xtm 1.0
    "(audiotest_assert_stereo_sine \"@WAV@\" 330.0 550.0)"
    audio-offline)
extempore_add_audio_offline_test(hello_mt
    examples/core/hello_mt.xtm 1.0
    "(audiotest_assert_three_sines \"@WAV@\" 330.0 550.0 770.0)"
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
