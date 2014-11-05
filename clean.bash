#!/bin/bash

# remove extempore build files
rm -rf extempore build/obj/{.deps,*}

if [[ $- == *i* ]]
then
    read -p "Do you want to remove the precompiled xtlang libraries as well? [y/N] " -n 1 -r
else
    REPLY=n # keep the precompiled libs if not in an interactive terminal
fi    
if [[ $REPLY =~ ^[Yy]$ ]]
then
    # remove precompiled extempore libs (e.g. from the stdlib)
    echo removing precompiled libs...
    rm -rf extempore libs/*.{xtm,dylib,so}
fi
