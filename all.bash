#!/bin/bash

case $(uname) in
        *NT*) EXTEMPORE_OS=windows ;;
	Linux) EXTEMPORE_OS=linux ;;
	Darwin) EXTEMPORE_OS=darwin ;;
	*) echo Unsupported OS:  $(uname) >&2 ; exit 1 ;;
esac
export EXTEMPORE_OS

# LLVM DEFS
EXT_LLVM_CONFIG_SCRIPT="$EXT_LLVM_DIR/bin/llvm-config"
EXT_LLVM_CXXFLAGS=`$EXT_LLVM_CONFIG_SCRIPT --cxxflags`
EXT_LLVM_LDFLAGS=`$EXT_LLVM_CONFIG_SCRIPT --ldflags`
EXT_LLVM_LIBS=`$EXT_LLVM_CONFIG_SCRIPT --libs`
export EXT_LLVM_CXXFLAGS EXT_LLVM_LDFLAGS EXT_LLVM_LIBS

# check for boost
if [[ "$@" =~ "-DEXT_BOOST" ]]
then
    EXT_BOOST=1
    export EXT_BOOST
else
    unset EXT_BOOST
fi

# build with mcjit by default
PLATFORM_DEFINES="-DEXT_MCJIT -DEXT_SHARE_DIR=\\\"${PWD}\\\""
export PLATFORM_DEFINES

EXT_USER_ARGS=$@
export EXT_USER_ARGS

make -f top.make extempore
