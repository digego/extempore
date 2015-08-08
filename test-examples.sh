#!/bin/bash

: ${EXAMPLES:=
    "examples/core/audio_101.xtm \
    examples/core/fmsynth.xtm \
    examples/core/mtaudio.xtm \
    examples/core/nbody_lang_shootout.xtm \
    examples/core/polysynth.xtm \
    examples/core/temporal_recursion_101.xtm \
    examples/external/electrofunk.xtm \
    examples/external/fluid_dynamics.xtm \
    examples/external/gl-compatibility.xtm \
    examples/external/openvg.xtm \
    examples/external/pic.xtm \
    examples/external/raymarcher.xtm \
    examples/external/sampler.xtm \
    examples/external/xtmrender1.xtm \
    examples/external/xtmrender2.xtm \
    examples/external/xtmrender3.xtm \
    examples/external/xtmrender4.xtm"}

for f in $EXAMPLES
do
    extempore --eval "(sys:run-tests \"$f\" #t #t)"
    rc=$?
    if (($rc != 0)); then
        echo -e "\033[0;31mError in example $f\033[0;00m"
        echo
        exit $rc
    else
        echo -e "\033[0;32mSuccessfully ran example $f\033[0;00m"
        echo
    fi
done

exit 0
