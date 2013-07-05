#!/bin/bash
# create a binary extempore archive for distribution

case $(uname) in
    Linux) SHLIB_EXT=so ;;
    Darwin) SHLIB_EXT=dylib ;;
    *) echo Cannot package for OS:  $(uname) >&2 ; exit 1 ;;
esac

DIST_ARCHIVE=extempore-$(uname)-$(date "+%Y.%m.%d")
DIST_FILES="extempore libs examples extras README.md"
DIST_SHLIBS="libassimp libcairo librtmidi"

echo "Creating extempore distribution archive ${DIST_ARCHIVE}"

mkdir -p $DIST_ARCHIVE/runtime/lib

cp -R $DIST_FILES $DIST_ARCHIVE
# need to handle runtime specially (because of shlibs)
cp runtime/* $DIST_ARCHIVE/runtime
# copy the shlibs into runtime/lib
for f in $DIST_SHLIBS
do
  cp -f runtime/lib/$f.$SHLIB_EXT $DIST_ARCHIVE/runtime/lib
done
# tar up the archive
tar -cvzf ${DIST_ARCHIVE}.tar.gz $DIST_ARCHIVE
rm -rf $DIST_ARCHIVE

echo "sha1: $(shasum ${DIST_ARCHIVE}.tar.gz)"
