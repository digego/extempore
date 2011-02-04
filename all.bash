#!/bin/bash

case $(uname) in
	Linux) EXTEMPORE_OS=linux ;;
	Darwin) EXTEMPORE_OS=darwin ;;
	*) echo Unsupported OS:  $(uname) >&2 ; exit 1 ;;
esac
export EXTEMPORE_OS

if [ ! -f config/llvm.bash ]; then
	echo Missing config/llvm.bash file.  See INSTALL. >&2
	exit 1
fi

. config/llvm.bash

make -f top.make extempore
