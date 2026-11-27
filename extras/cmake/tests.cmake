# Test registration for Extempore
# Requires: BUILD_TESTS option, extempore target

if(NOT BUILD_TESTS)
    return()
endif()

include(CTest)

add_subdirectory(tests/cpp-unit)

# Per-assertion JUnit XML reports are written here, one file per test file, so
# ctest and CI can surface individual assertion failures (not just which .xtm
# file failed). Safe to upload as a CI test-report artifact.
set(EXTEMPORE_TEST_RESULTS_DIR ${CMAKE_CURRENT_BINARY_DIR}/test-results)
file(MAKE_DIRECTORY ${EXTEMPORE_TEST_RESULTS_DIR})

# CTest has no default timeout, so a wedged extempore process would hang the
# run until the CI job's own limit. 120s is an order of magnitude more than the
# slowest .xtm test needs; instrumented builds run 5-15x slower and get a
# budget to match.
if(EXTEMPORE_SANITIZE)
    set(EXTEMPORE_TEST_TIMEOUT 900)
    set(EXTEMPORE_PYTHON_TEST_TIMEOUT 1800)
else()
    set(EXTEMPORE_TEST_TIMEOUT 120)
    # A Python test file starts extempore dozens of times; the slower CI
    # runners take two minutes over what takes one here.
    set(EXTEMPORE_PYTHON_TEST_TIMEOUT 600)
endif()

# On a timeout ctest first sends SIGUSR1, and extempore writes a hang report
# (include/HangReport.h) into the test's log before it is killed.
if(WIN32)
    set(EXTEMPORE_TIMEOUT_SIGNAL)
else()
    set(EXTEMPORE_TIMEOUT_SIGNAL TIMEOUT_SIGNAL_NAME SIGUSR1 TIMEOUT_SIGNAL_GRACE_PERIOD 5)
endif()

# Runs one .xtm test file with `extempore test`, which exits 1 if an assertion
# fails and writes the per-assertion report into the results directory.
macro(extempore_add_test testfile label)
    add_test(NAME ${testfile}
        COMMAND extempore test ${testfile} --junit ${EXTEMPORE_TEST_RESULTS_DIR})
    set_tests_properties(${testfile} PROPERTIES
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        TIMEOUT ${EXTEMPORE_TEST_TIMEOUT}
        ${EXTEMPORE_TIMEOUT_SIGNAL}
        LABELS ${label})
endmacro()

# A test which needs the eval servers runs in a session, and is the only kind
# to take a port (and the one below it, for the utility process).
macro(extempore_add_ipc_test testfile label port)
    string(REPLACE "/" "_" _safe "${testfile}")
    add_test(NAME ${testfile}
        COMMAND extempore --no-audio --no-nrepl --legacy-port=${port}
            -e "(xtmtest-run-tests \"${testfile}\" #t #t \"${EXTEMPORE_TEST_RESULTS_DIR}/${_safe}.xml\")")
    set_tests_properties(${testfile} PROPERTIES
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        TIMEOUT ${EXTEMPORE_TEST_TIMEOUT}
        ${EXTEMPORE_TIMEOUT_SIGNAL}
        LABELS ${label})
endmacro()

# Loads an example, lets it run for `timeout` seconds and quits: status 1 if a
# form in it raised an error.
macro(extempore_add_example_as_test examplefile timeout label)
    add_test(NAME ${examplefile}
        COMMAND extempore exec -e "(sys:load-then-quit \"${examplefile}\" ${timeout})")
    set_tests_properties(${examplefile} PROPERTIES
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        TIMEOUT 300
        ${EXTEMPORE_TIMEOUT_SIGNAL}
        LABELS ${label})
endmacro()

# Render a DSP file offline with `extempore render`, then assert on the output
# WAV using libs/core/audiotest.xtm. assert_expr is any xtlang expression
# returning i64 (0 pass, non-zero fail); use @WAV@ in the expression to
# reference the rendered file path, e.g.:
#
#   (audiotest_assert_sine "@WAV@" 440.0)
#   (audiotest_assert_stereo_sine "@WAV@" 330.0 550.0)
#   (audiotest_assert_three_sines "@WAV@" 330.0 550.0 770.0)
#
# duration is the length of audio to render (seconds). The file is rendered
# twice and the two WAVs must match byte for byte. Two-phase: implemented as a
# cmake -P script so it works cross-platform.
# Any further arguments are passed to extempore for the render phase.
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
            "-DEXTRA_ARGS=${ARGN}"
            -P ${CMAKE_CURRENT_SOURCE_DIR}/extras/cmake/run_audio_offline_test.cmake)
    set_tests_properties(audio-offline/${testname} PROPERTIES
        TIMEOUT 180
        LABELS ${label})
endmacro()

# Core, studio and compiler xtlang tests are auto-discovered: every
# tests/core/*.xtm, tests/studio/*.xtm and tests/compiler/*.xtm runs under
# `extempore test`, labelled by its directory. The
# few that need a bespoke harness (or aren't standalone) are excluded here and
# registered explicitly below. CONFIGURE_DEPENDS re-globs when a test file is
# added or removed, so new tests are picked up without editing this file.
file(GLOB _core_tests CONFIGURE_DEPENDS RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}
     ${CMAKE_CURRENT_SOURCE_DIR}/tests/core/*.xtm)
file(GLOB _compiler_tests CONFIGURE_DEPENDS RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}
     ${CMAKE_CURRENT_SOURCE_DIR}/tests/compiler/*.xtm)

file(GLOB _studio_tests CONFIGURE_DEPENDS RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}
     ${CMAKE_CURRENT_SOURCE_DIR}/tests/studio/*.xtm)

# excluded from auto-registration (handled specially below, or fixtures)
set(_xtm_special_tests
    tests/core/system.xtm              # IPC test, run in a session
    tests/core/aot-compilation.xtm     # two-phase AOT round-trip
    tests/core/aot-compilation-lib.xtm) # fixture library for the AOT test

foreach(_t ${_core_tests})
    if(NOT _t IN_LIST _xtm_special_tests)
        extempore_add_test(${_t} libs-core)
    endif()
endforeach()
foreach(_t ${_studio_tests})
    extempore_add_test(${_t} libs-studio)
endforeach()
foreach(_t ${_compiler_tests})
    if(NOT _t IN_LIST _xtm_special_tests)
        extempore_add_test(${_t} compiler-unit)
    endif()
endforeach()

# system.xtm exercises inter-process eval, so it needs a session's servers
extempore_add_ipc_test(tests/core/system.xtm libs-core 17099)

# failing.xtm is the quarantine for known-broken tests (see docs/reference/
# testing.md). Registered as an EXPECTED failure: ctest stays green while the
# bugs persist, and flips to failing the moment a fix makes the file pass --
# the cue to move that test into its proper home.
add_test(NAME tests/failing.xtm
    COMMAND extempore test tests/failing.xtm)
set_tests_properties(tests/failing.xtm PROPERTIES
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    LABELS libs-core
    TIMEOUT ${EXTEMPORE_TEST_TIMEOUT}
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
        -DCACHE_LL=$<TARGET_FILE_DIR:extempore>/libs/aot-cache/xtmaot-compilation-lib.ll
        -DCOMPILE_TIMEOUT=120
        -DTEST_TIMEOUT=60
        -P ${CMAKE_CURRENT_SOURCE_DIR}/extras/cmake/run_aot_test.cmake)
set_tests_properties(tests/core/aot-compilation.xtm PROPERTIES
    TIMEOUT 200
    LABELS compiler-unit
    RESOURCE_LOCK build-aot-cache)

# Installed-tree test: install to a temporary prefix, make it read-only, and
# check the installed binary finds its files and AOT-compiles into the per-user
# cache dir (extras/cmake/run_install_test.cmake). The install copies
# build/libs/aot-cache, which the AOT round-trip test above writes to, hence
# the lock.
add_test(NAME install/installed-tree
    COMMAND ${CMAKE_COMMAND}
        -DBUILD_DIR=${CMAKE_BINARY_DIR}
        -DSOURCE_DIR=${CMAKE_CURRENT_SOURCE_DIR}
        -DSCRATCH=${CMAKE_BINARY_DIR}/install-test
        -P ${CMAKE_CURRENT_SOURCE_DIR}/extras/cmake/run_install_test.cmake)
set_tests_properties(install/installed-tree PROPERTIES
    TIMEOUT 600
    LABELS install
    RESOURCE_LOCK build-aot-cache)

# Documentation example verifier: run every xtlang snippet in the docs through
# extempore exec and check its claimed output, so doc code that drifts from
# the compiler can be caught. Labelled `docs` (kept out of the libs-core
# runs). This is a LOCAL tool, deliberately excluded from CI (the
# build-and-test workflow's label-regex omits `docs`) --- run it on Linux with
# `ctest --test-dir build -L docs` before changing the docs.
find_package(Python3 COMPONENTS Interpreter)
if(Python3_Interpreter_FOUND)
    add_test(NAME docs/verify-examples
        COMMAND Python3::Interpreter ${CMAKE_CURRENT_SOURCE_DIR}/docs/verify-examples.py)
    set_tests_properties(docs/verify-examples PROPERTIES
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        ENVIRONMENT "EXTEMPORE_BIN=$<TARGET_FILE:extempore>"
        LABELS docs
        TIMEOUT 900)
else()
    message(STATUS "python3 not found --- skipping docs/verify-examples test")
endif()

# The nREPL server and its command-line client (backlog/docs/doc-002). Each
# test file starts its own extempore processes on free ports.
if(Python3_Interpreter_FOUND)
    foreach(_t test_server test_send)
        add_test(NAME nrepl/${_t}
            COMMAND Python3::Interpreter -m unittest -v ${_t})
        set_tests_properties(nrepl/${_t} PROPERTIES
            WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/tests/nrepl
            ENVIRONMENT "EXTEMPORE_BIN=$<TARGET_FILE:extempore>"
            TIMEOUT ${EXTEMPORE_PYTHON_TEST_TIMEOUT}
            LABELS nrepl)
    endforeach()
endif()

# The language server, driven as an editor drives it.
if(Python3_Interpreter_FOUND)
    add_test(NAME lsp/test_lsp
        COMMAND Python3::Interpreter -m unittest -v test_lsp)
    set_tests_properties(lsp/test_lsp PROPERTIES
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/tests/lsp
        ENVIRONMENT "EXTEMPORE_BIN=$<TARGET_FILE:extempore>"
        TIMEOUT ${EXTEMPORE_PYTHON_TEST_TIMEOUT}
        LABELS lsp)
endif()

# The .xtm sources are formatted: `build/extempore fmt libs examples tests
# runtime extras` fixes any this lists.
add_test(NAME cli/fmt-check
    COMMAND extempore fmt --check libs examples tests runtime extras)
set_tests_properties(cli/fmt-check PROPERTIES
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    LABELS cli)

# The command line itself: what each subcommand prints where, and how it exits.
if(Python3_Interpreter_FOUND)
    add_test(NAME cli/test_cli
        COMMAND Python3::Interpreter -m unittest -v test_cli)
    set_tests_properties(cli/test_cli PROPERTIES
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/tests/cli
        ENVIRONMENT "EXTEMPORE_BIN=$<TARGET_FILE:extempore>"
        TIMEOUT ${EXTEMPORE_PYTHON_TEST_TIMEOUT}
        LABELS cli)
endif()

# Offline audio-rendering tests: render a DSP to a WAV with extempore render,
# then verify it contains a sine at the expected frequency. 1.0s of content is
# plenty for a reliable Goertzel check.
extempore_add_audio_offline_test(hello-sine
    examples/core/hello-sine.xtm 1.0
    "(audiotest_assert_sine \"@WAV@\" 440.0)"
    audio-offline)
# A block of 16384 frames is a third of a second: a render which let the clock
# run while the base library loads would be past the end before the file began.
extempore_add_audio_offline_test(hello-sine-big-buffers
    examples/core/hello-sine.xtm 1.0
    "(audiotest_assert_sine \"@WAV@\" 440.0)"
    audio-offline
    --frames 16384)
extempore_add_audio_offline_test(hello-stereo
    examples/core/hello-stereo.xtm 1.0
    "(audiotest_assert_stereo_sine \"@WAV@\" 330.0 550.0)"
    audio-offline)
extempore_add_audio_offline_test(hello-mt
    examples/core/hello-mt.xtm 1.0
    "(audiotest_assert_three_sines \"@WAV@\" 330.0 550.0 770.0)"
    audio-offline)
# The callback which changes the frequency is due at frame 44100, so it runs
# before the block which holds that frame and the change is heard from that
# block's first frame (44032).
extempore_add_audio_offline_test(hello-sweep
    examples/core/hello-sweep.xtm 2.0
    "(audiotest_assert_sweep \"@WAV@\" 330.0 770.0 44032 0)"
    audio-offline)

# The studio's synthesised instruments each have a demo score which plays
# through their presets. Rendering the start of one compiles the instrument and
# pushes notes through the whole signal path; the check is only that sound
# came out. (The sampled instruments need the optional assets, so they aren't
# rendered here.)
set(_studio_demos acid chip epiano fm guitar kit808 kit909 macro organ padsynth polysynth resonator strings wavetable)
foreach(_demo ${_studio_demos})
    extempore_add_audio_offline_test(studio-${_demo}
        examples/studio/demos/${_demo}.xtm 3.0
        "(audiotest_assert_not_silent \"@WAV@\" 0.05)"
        audio-offline)
endforeach()

# Notes reaching an instrument through the pattern language. play-note asked
# sys:make-cptr for a zero-byte buffer when a note carried no extra arguments,
# got #f, and the xtlang wrapper refused the call without raising --- so
# patterns ran in silence through two releases with the whole suite green.
# The peak sits near 0.95 when notes sound and at exactly 0.0 when they do not.
# The analogue synth randomises oscillator phase and slop, and two patterns
# fire on the same beats, so this is also the render which shows that seeded
# random numbers and same-instant callbacks repeat.
#
# The pattern's first note lands 1-2s in.
extempore_add_audio_offline_test(pattern-notes
    examples/core/synth.xtm 5.0
    "(audiotest_assert_not_silent \"@WAV@\" 0.05)"
    audio-offline)

# Core examples
extempore_add_example_as_test(examples/core/audio_101.xtm 10 examples-audio)
extempore_add_example_as_test(examples/core/audio_streams.xtm 10 examples-audio)
extempore_add_example_as_test(examples/core/covers.xtm 30 examples-audio)
extempore_add_example_as_test(examples/core/expr_problem.xtm 10 examples-core)
extempore_add_example_as_test(examples/core/extempore_lang.xtm 60 examples-core)
extempore_add_example_as_test(examples/core/fmsynth.xtm 10 examples-audio)
extempore_add_example_as_test(examples/core/midi.xtm 10 examples-audio)
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
