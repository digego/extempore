#!/bin/bash
# pre-compile some standard Extempore libraries for faster loading

case $(uname) in
    Linux) SHLIB_EXT=so PLATFORM=linux ;;
    Darwin) SHLIB_EXT=dylib PLATFORM=osx ;;
    *) echo Cannot AOT-compile modules for OS:  $(uname) >&2 ; exit 1 ;;
esac

# this is the 'standard' library
# to override this list, call this script with:
# AOT_LIBS="libs/core/foo.xtm libs/external/bar.xtm" ./compile-stdlib.sh
: ${AOT_LIBS:="\
libs/base/base.xtm \
libs/core/math.xtm \
libs/core/audio_dsp.xtm \
libs/core/instruments.xtm \
libs/external/fft.xtm \
libs/external/sndfile.xtm \
libs/external/audio_dsp_ext.xtm \
libs/external/instruments_ext.xtm \
libs/external/rtmidi.xtm \
libs/external/glib.xtm \
libs/external/stb_image.xtm \
libs/external/gl.xtm \
libs/external/gl-compatibility.xtm \
libs/external/glext.xtm \
libs/external/glfw3.xtm \
libs/external/graphics-pipeline.xtm \
libs/external/nanovg.xtm \
libs/external/soil.xtm \
libs/external/assimp.xtm"}

: ${AOT_COMPILATION_COMMAND:="extempore --nobase $1 --eval "}

echo Ahead-of-time \(AOT\) compiling the Extempore standard library.  This may take several minutes...
echo

# check all the required shared libs are there
for f in $AOT_LIBS
do
    $AOT_COMPILATION_COMMAND "(impc:aot:compile-xtm-file \"$f\" #t)"
    rc=$?
    if (($rc != 0))  ; then
        echo -e "\033[0;31mError AOT-compiling $f\033[0;00m"
        echo
        exit $rc
    else
        echo -e "\033[0;32mSuccessfully AOT-compiled $f\033[0;00m"
        echo
    fi
done

exit 0
