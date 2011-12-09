#!/bin/bash

case $(uname) in
        *NT*) EXTEMPORE_OS=windows ;;
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

# check for jack audio
if [[ "$@" =~ "-DJACK_AUDIO" ]]
then
    JACK_AUDIO=1
    export JACK_AUDIO
else
    unset JACK_AUDIO
fi

# check for boost
if [[ "$@" =~ "-DEXT_BOOST" ]]
then
    EXT_BOOST=1
    export EXT_BOOST
else
    unset EXT_BOOST
fi


EXT_USER_ARGS=$@
export EXT_USER_ARGS

make -f top.make extempore
