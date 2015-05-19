#!/bin/bash
ROOTDIR=~
EXTEMPORE_GH_BRANCH=singapore

cd $ROOTDIR
# get the extempore deps
# apt-get update # already done in update.sh
apt-get -y install git clang portaudio19-dev libpcre3-dev libgl1-mesa-dev

# download extempore
wget https://github.com/digego/extempore/archive/$EXTEMPORE_GH_BRANCH.zip && \
    unzip $EXTEMPORE_GH_BRANCH.zip && \
    mv extempore-$EXTEMPORE_GH_BRANCH extempore

# download & build LLVM 3.4.2
wget -qO- http://llvm.org/releases/3.4.2/llvm-3.4.2.src.tar.gz | tar xvz
cd llvm-3.4.2.src/lib/AsmParser && \
    patch < $ROOTDIR/extempore/extras/llparser.patch && \
    mkdir $ROOTDIR/extempore/llvm-build && \
    cd $ROOTDIR/llvm-3.4.2.src && \
    ./configure --prefix=$ROOTDIR/extempore/llvm-build --disable-shared --enable-optimized --enable-targets=host --disable-bindings --enable-curses=no --enable-terminfo=no && \
    make && make install

# build extempore
cd $ROOTDIR/extempore && \
    EXT_LLVM_DIR=llvm-build ./all.bash && \
    ./compile-stdlib.sh

cd $ROOTDIR
# clean up after ourselves
rm -r $EXTEMPORE_GH_BRANCH.zip
rm -r llvm-3.4.2.src
