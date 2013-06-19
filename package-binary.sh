#!/bin/bash
# package up the binary extempore executable

DIST_DIR=extempore-$(uname)-$(date "+%Y%m%d")

echo "Creating extempore binary archive in ${DIST_DIR}"

mkdir $DIST_DIR

# list of files to go in the binary distribution
DIST_FILES="extempore runtime libs examples extras make-tags.sh README.md"

cp -R $DIST_FILES $DIST_DIR
tar -cvzf ${DIST_DIR}.tar.gz $DIST_DIR
rm -r $DIST_DIR
