#!/bin/bash

# remove extempore build files
rm -rf extempore build/obj/{.deps,*}

#!/bin/sh
if [ "$1" == "--precomp" ]; then
    rm -rf libs/*.{xtm,dylib,so} && echo removed AOT-compiled libs.
fi
