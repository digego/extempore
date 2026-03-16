# Test registration for Extempore
# Requires: BUILD_TESTS option, extempore target

if(NOT BUILD_TESTS)
    return()
endif()

include(CTest)

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

# Core library tests
extempore_add_ipc_test(tests/core/system.xtm libs-core)
extempore_add_test(tests/core/adt.xtm libs-core)
extempore_add_test(tests/core/math.xtm libs-core)
extempore_add_test(tests/core/std.xtm libs-core)
extempore_add_test(tests/core/xtlang.xtm libs-core)
extempore_add_test(tests/core/generics.xtm libs-core)

# External library tests
extempore_add_test(tests/external/fft.xtm libs-external)

# Core examples
extempore_add_example_as_test(examples/core/audio_101.xtm 10 examples-audio)
extempore_add_example_as_test(examples/core/fmsynth.xtm 10 examples-audio)
extempore_add_example_as_test(examples/core/mtaudio.xtm 10 examples-audio)
extempore_add_example_as_test(examples/core/nbody_lang_shootout.xtm 10 examples-core)
extempore_add_example_as_test(examples/core/scheduler.xtm 10 examples-audio)
extempore_add_example_as_test(examples/core/topclock_metro.xtm 10 examples-audio)
extempore_add_example_as_test(examples/core/typeclasses.xtm 10 examples-core)
extempore_add_example_as_test(examples/core/xthread.xtm 10 examples-core)

# External examples - audio
extempore_add_example_as_test(examples/external/audio_player.xtm 10 examples-audio)
extempore_add_example_as_test(examples/external/convolution_reverb.xtm 10 examples-audio)
extempore_add_example_as_test(examples/external/electrofunk.xtm 10 examples-audio)
extempore_add_example_as_test(examples/external/granulator.xtm 10 examples-audio)
extempore_add_example_as_test(examples/external/portmidi-output.xtm 10 examples-audio)
extempore_add_example_as_test(examples/external/portmidi.xtm 10 examples-audio)
extempore_add_example_as_test(examples/external/sampler.xtm 10 examples-audio)
extempore_add_example_as_test(examples/external/sing_a_song.xtm 10 examples-audio)

# External examples - graphics
extempore_add_example_as_test(examples/external/gl-compatibility.xtm 10 examples-graphics)
extempore_add_example_as_test(examples/external/openvg.xtm 10 examples-graphics)
extempore_add_example_as_test(examples/external/raymarcher.xtm 10 examples-graphics)
extempore_add_example_as_test(examples/external/spectrogram.xtm 10 examples-graphics)
extempore_add_example_as_test(examples/external/xtmrender1.xtm 10 examples-graphics)
extempore_add_example_as_test(examples/external/xtmrender2.xtm 10 examples-graphics)
extempore_add_example_as_test(examples/external/xtmrender3.xtm 10 examples-graphics)
extempore_add_example_as_test(examples/external/xtmrender4.xtm 10 examples-graphics)

# Shader tutorials
extempore_add_example_as_test(examples/external/shader-tutorials/arrows.xtm 10 examples-graphics)
extempore_add_example_as_test(examples/external/shader-tutorials/framebuffer.xtm 10 examples-graphics)
extempore_add_example_as_test(examples/external/shader-tutorials/heatmap.xtm 10 examples-graphics)
extempore_add_example_as_test(examples/external/shader-tutorials/particles.xtm 10 examples-graphics)
extempore_add_example_as_test(examples/external/shader-tutorials/points.xtm 10 examples-graphics)
extempore_add_example_as_test(examples/external/shader-tutorials/shadertoy.xtm 10 examples-graphics)
extempore_add_example_as_test(examples/external/shader-tutorials/simple-triangle.xtm 10 examples-graphics)
extempore_add_example_as_test(examples/external/shader-tutorials/texture.xtm 10 examples-graphics)
extempore_add_example_as_test(examples/external/shader-tutorials/triangle.xtm 10 examples-graphics)
