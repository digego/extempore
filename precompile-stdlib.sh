#!/bin/bash
# pre-compile some standard Extempore libraries for faster loading

case $(uname) in
    Linux) SHLIB_EXT=so PLATFORM=linux ;;
    Darwin) SHLIB_EXT=dylib PLATFORM=osx ;;
    *) echo Cannot precompile modules for OS:  $(uname) >&2 ; exit 1 ;;
esac

PRECOMP_LIBS="\
core/math.xtm \
core/audio_dsp.xtm \
core/instruments.xtm \
external/fft.xtm \
external/sndfile.xtm \
external/audio_dsp_ext.xtm \
external/instruments_ext.xtm \
external/rtmidi.xtm \
external/soil.xtm \
external/opengl.xtm \
external/shaders.xtm \
external/assimp.xtm \
external/openvg.xtm"

PRECOMP_COMMAND_FILENAME="xtmprecomp-command-file.xtm"
PRECOMP_EXTEMPORE_RUN_COMMAND="./extempore --run "

# check all the required shared libs are there
for f in $PRECOMP_LIBS
do
    echo "(sys:precomp:compile-xtm-file \"libs/$f\" #t #t)" > $PRECOMP_COMMAND_FILENAME
    echo Precompiling $f with $PRECOMP_EXTEMPORE_RUN_COMMAND $PRECOMP_COMMAND_FILENAME
    $PRECOMP_EXTEMPORE_RUN_COMMAND $PRECOMP_COMMAND_FILENAME
done

rm $PRECOMP_COMMAND_FILENAME

exit 0
