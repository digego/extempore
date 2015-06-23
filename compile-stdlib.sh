#!/bin/bash
# pre-compile some standard Extempore libraries for faster loading

case $(uname) in
    Linux) SHLIB_EXT=so PLATFORM=linux ;;
    Darwin) SHLIB_EXT=dylib PLATFORM=osx ;;
    *) echo Cannot AOT-compile modules for OS:  $(uname) >&2 ; exit 1 ;;
esac

if [ -z "$EXT_LLVM_DIR" ] && [ ! -d "/usr/local/Cellar/extempore-llvm/3.4.1" ] ; then
    echo -e "\033[0;31mError\033[0;00m: You need to set the \033[0;32mEXT_LLVM_DIR\033[0;00m environment variable to point to your (Extempore) LLVM directory."
    exit 2
fi

# this is the 'standard' library
# to override this list, call this script with:
# AOT_LIBS="core/foo.xtm external/bar.xtm" ./compile-stdlib.sh
: ${AOT_LIBS:="\
core/std.xtm \
core/math.xtm \
core/audio_dsp.xtm \
core/instruments.xtm \
external/fft.xtm \
external/sndfile.xtm \
external/audio_dsp_ext.xtm \
external/instruments_ext.xtm \
external/rtmidi.xtm \
external/glib.xtm \
external/gl.xtm \
external/glext.xtm \
external/stb_image.xtm \
external/shaders-v2.xtm \
external/assimp.xtm \
external/openvg.xtm"}

EXTEMPORE_AOT_COMPILATION_COMMAND="./extempore --nostd $1 --eval "

echo Ahead-of-time \(AOT\) compiling the Extempore standard library.  This may take several minutes...
echo

# check all the required shared libs are there
for f in $AOT_LIBS
do
    $EXTEMPORE_AOT_COMPILATION_COMMAND "(impc:aot:compile-xtm-file \"libs/$f\" #t #t)"
    rc=$?
    if (($rc != 0))  ; then
        echo -e "\033[0;31mError AOT-compiling libs/$f\033[0;00m"
        echo
        exit $rc
    else
        echo -e "\033[0;32mSuccessfully AOT-compiled libs/$f\033[0;00m"
        echo
    fi
done

exit 0
