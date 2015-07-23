#!/bin/bash

# remove extempore build files
rm -rf extempore build/obj/{.deps,*}

#!/bin/sh
if [ "$1" == "--precomp" ]; then
    rm -rf libs/aot-cache && echo removed AOT-compiled libs.
fi
