#!/bin/bash
ROOTDIR="$(pwd)"
# sudo apt-get install git g++ portaudio19-dev libpcre3-dev libgl1-mesa-dev
wget -qO- http://llvm.org/releases/3.4.1/llvm-3.4.1.src.tar.gz | tar xvz && \
git clone git@github.com:digego/extempore && \
cd llvm-3.4.1.src/lib/AsmParser && \
patch < $ROOTDIR/extempore/extras/llparser.patch && \
mkdir $ROOTDIR/extempore/llvm-build && \
cd $ROOTDIR/llvm-3.4.1.src && \
./configure --prefix=$ROOTDIR/extempore/llvm-build --enable-optimized --enable-curses=no --enable-terminfo=no && \
make && make install && \
cd $ROOTDIR/extempore && \
EXT_LLVM_DIR=llvm-build ./all.bash
