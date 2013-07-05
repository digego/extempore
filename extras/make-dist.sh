#!/bin/bash
# package up the binary extempore executable

case $(uname) in
    Linux) SHLIB_EXT=so ;;
    Darwin) SHLIB_EXT=dylib ;;
    *) echo Cannot package for OS:  $(uname) >&2 ; exit 1 ;;
esac

DIST_DIR=extempore-$(uname)-$(date "+%Y.%m.%d")
DIST_SHLIBS="assimp cairo rtmidi"

echo "Creating extempore binary archive in ${DIST_DIR}"

mkdir $DIST_DIR

# list of files to go in the binary distribution
DIST_FILES="extempore runtime libs examples extras make-tags.sh README.md"

cp -R $DIST_FILES $DIST_DIR
tar -cvzf ${DIST_DIR}.tar.gz $DIST_DIR
rm -r $DIST_DIR

echo "sha1 $(shasum ${DIST_DIR}.tar.gz) (needed for homebrew formula)"
