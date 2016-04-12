#!/usr/bin/env bash

# this script should be run from inside the extras/ subdirectory

cd .. # move up one level into top-level extempore directory
SRC_DIR=$PWD

# uncomment this next line if you want to test the full "pull down LLVM" behaviour
# unset EXT_LLVM_DIR

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

cmake -DCMAKE_INSTALL_PREFIX=$TEST_DIR -DCMAKE_BUILD_TYPE=Release -DIN_TREE=OFF $SRC_DIR && make clean && make -j4 && make install && make clean_aot && $TEST_DIR/bin/extempore --noaudio --port=${TEST_PORT} --sharedir $TEST_DIR/share/extempore --run tests/all.xtm

if (($? != 0)); then
    echo -e "\033[0;31mIntegration test failed (AOT:false) $f\033[0;00m"
    echo
    exit 1
fi

make -j4 aot_extended && $TEST_DIR/bin/extempore --noaudio --port=${TEST_PORT} --sharedir $TEST_DIR/share/extempore --run tests/all.xtm

if (($? != 0)); then
    echo -e "\033[0;31mIntegration test failed (AOT:true) $f\033[0;00m"
    echo
    exit 1
else
    echo -e "\033[0;32mAll integration tests passed\033[0;00m"
    echo
    exit 0
fi
