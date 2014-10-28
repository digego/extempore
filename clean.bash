#!/bin/bash

# remove extempore build files
rm -rf extempore build/obj/{.deps,*}

# remove precompiled extempore libs (e.g. from the stdlib)
# rm -rf extempore libs/*.{xtm,dylib,so}
