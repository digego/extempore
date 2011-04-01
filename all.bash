#!/bin/bash

case $(uname) in
	Linux) EXTEMPORE_OS=linux ;;
	Darwin) EXTEMPORE_OS=darwin ;;
	*) echo Unsupported OS:  $(uname) >&2 ; exit 1 ;;
esac
export EXTEMPORE_OS

if [ -z "$EXT_LLVM_DIR" ]; then 
	if [ ! -f config/llvm.bash ]; then
		echo Missing config/llvm.bash file.  See INSTALL. >&2
		exit 1	
	fi
	. config/llvm.bash
fi

EXT_USER_ARGS=$@
export EXT_USER_ARGS

make -f top.make extempore
