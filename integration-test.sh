#!/usr/bin/env bash

# without MCJIT

# remove Extempore, run the tests (with no aot compilation) for all
# the libraries (takes a while)
./clean.bash --precomp && ./all.bash && extempore --run tests/all.xtm

if (($? != 0))  ; then
    echo -e "\033[0;31mIntegration test failed (AOT:false, MCJIT:false) $f\033[0;00m"
    echo
    exit 1
fi

# aot-compile the stdlib, then run all the tests again
./compile-stdlib.sh && extempore --run tests/all.xtm

if (($? != 0))  ; then
    echo -e "\033[0;31mIntegration test failed (AOT:true, MCJIT:false) $f\033[0;00m"
    echo
    exit 1
fi

# repeat the above steps, this time with MCJIT

./clean.bash --precomp && ./all.bash -DEXT_MCJIT && extempore --run tests/all.xtm

if (($? != 0))  ; then
    echo -e "\033[0;31mIntegration test failed (AOT:false, MCJIT:true) $f\033[0;00m"
    echo
    exit 1
fi

./compile-stdlib.sh && extempore --run tests/all.xtm

if (($? != 0))  ; then
    echo -e "\033[0;31mIntegration test failed (AOT:true, MCJIT:true) $f\033[0;00m"
    echo
    exit 1
else
    echo -e "\033[0;32mSuccessfully AOT-compiled $f\033[0;00m"
    echo
    exit 0
fi
