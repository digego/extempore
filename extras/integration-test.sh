#!/usr/bin/env bash

# this script should be run from inside the extras/ subdirectory

cd .. # move up one level into top-level extempore directory
SRC_DIR=$PWD

if [ ! -f $SRC_DIR/extras/integration-test.sh ]; then
    echo -e "\033[0;31mError:\033[0;00m integration-test.sh must be run from inside the extras/ directory"
    exit 1
fi

TEST_DIR=/tmp/extempore-integration-test
# port to run the Extempore primary process on
TEST_PORT=17097

if [ -d $TEST_DIR ]; then
    rm -r $TEST_DIR
fi

mkdir $TEST_DIR && cd $TEST_DIR

echo "Running tests in ${TEST_DIR}..."

# repeat the above steps, this time with MCJIT

cmake -DCMAKE_INSTALL_PREFIX=$TEST_DIR -DCMAKE_BUILD_TYPE=Release -DIN_TREE=OFF $SRC_DIR && make clean && make install && $TEST_DIR/bin/extempore --port=${TEST_PORT} --sharedir $TEST_DIR/share/extempore --run tests/all.xtm

if (($? != 0)); then
    echo -e "\033[0;31mIntegration test failed (AOT:false, MCJIT:true) $f\033[0;00m"
    echo
    exit 1
fi

make aot_stdlib && $TEST_DIR/bin/extempore --port=${TEST_PORT} --sharedir $TEST_DIR/share/extempore --run tests/all.xtm

if (($? != 0)); then
    echo -e "\033[0;31mIntegration test failed (AOT:true, MCJIT:true) $f\033[0;00m"
    echo
    exit 1
else
    echo -e "\033[0;32mAll integration tests passed\033[0;00m"
    echo
    exit 0
fi
