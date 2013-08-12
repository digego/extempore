#!/bin/bash
# create a binary extempore archive for distribution

case $(uname) in
    Linux) SHLIB_EXT=so ;;
    Darwin) SHLIB_EXT=dylib ;;
    *) echo Cannot package for OS:  $(uname) >&2 ; exit 1 ;;
esac

DIST_ARCHIVE=extempore-$(uname)-$(date "+%Y.%m.%d")
DIST_FILES="extempore libs examples extras README.md runtime"
DIST_SHLIBS="libassimp libcairo librtmidi libSOIL libsndfile libOpenVG libdrawtext"

mkdir -p $DIST_ARCHIVE/runtime/lib

cp -R $DIST_FILES $DIST_ARCHIVE
# check all the required shared libs are there
for f in $DIST_SHLIBS
do
    SHLIB_REL_PATH=runtime/lib/$f.$SHLIB_EXT
    if [ ! -f $SHLIB_REL_PATH ]
    then
        echo "Error: cannot find ${f}.  Add ${SHLIB_REL_PATH} to the runtime/lib directory and try again."
        exit 1
    fi
done
# tar up the archive
tar -czf ${DIST_ARCHIVE}.tar.gz $DIST_ARCHIVE
rm -rf $DIST_ARCHIVE

echo "Successfully created Extempore distribution archive."
echo "sha1: $(shasum ${DIST_ARCHIVE}.tar.gz)"

exit 0
