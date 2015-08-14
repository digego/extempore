#!/usr/bin/env bash

TEST_DIR=$PWD/integration-test
# port to run the Extempore primary process on
TEST_PORT=17097

if [ ! -d $TEST_DIR ]; then
    mkdir $TEST_DIR
fi

cd $TEST_DIR

echo "Running tests in ${TEST_DIR}..."

# without MCJIT

cmake -DCMAKE_INSTALL_PREFIX=$TEST_DIR -DMCJIT=OFF .. && make clean && make && make install && ./bin/extempore --port=${TEST_PORT} --sharedir $TEST_DIR/share/extempore --run tests/all.xtm

if (($? != 0)); then
    echo -e "\033[0;31mIntegration test failed (AOT:false, MCJIT:false) $f\033[0;00m"
    echo
    exit 1
fi

# aot-compile the stdlib, then run all the tests again

AOT_COMPILATION_COMMAND="${TEST_DIR}/bin/extempore --port=${TEST_PORT} --nostd $1 --eval " ../compile-stdlib.sh && ./bin/extempore --port=${TEST_PORT} --sharedir $TEST_DIR/share/extempore --run tests/all.xtm

if (($? != 0)); then
    echo -e "\033[0;31mIntegration test failed (AOT:true, MCJIT:false) $f\033[0;00m"
    echo
    exit 1
fi

# repeat the above steps, this time with MCJIT

cmake -DCMAKE_INSTALL_PREFIX=$TEST_DIR -DMCJIT=ON .. && make clean && make && make install && ./bin/extempore --port=${TEST_PORT} --sharedir $TEST_DIR/share/extempore --run tests/all.xtm

if (($? != 0)); then
    echo -e "\033[0;31mIntegration test failed (AOT:false, MCJIT:true) $f\033[0;00m"
    echo
    exit 1
fi

AOT_COMPILATION_COMMAND="${TEST_DIR}/bin/extempore --port=${TEST_PORT} --nostd $1 --eval " ../compile-stdlib.sh && ./bin/extempore --port=${TEST_PORT} --sharedir $TEST_DIR/share/extempore --run tests/all.xtm

if (($? != 0)); then
    echo -e "\033[0;31mIntegration test failed (AOT:true, MCJIT:true) $f\033[0;00m"
    echo
    exit 1
else
    echo -e "\033[0;32mAll integration tests passed\033[0;00m"
    echo
    exit 0
fi
