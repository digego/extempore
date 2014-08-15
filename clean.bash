#!/bin/bash

# rm -rf extempore build/obj/{.deps,*}

# remove the precompiled libs from the stdlib

for file in libs/*
do
   case "$file" in
       libs/create-xtm.xtm ) continue;;
       libs/xtm.xtm ) continue;;
       libs/*.xtm ) rm -rf $file;;
       libs/xtm*.dylib) rm -rf $file;;
       libs/xtm*.so ) rm -rf $file;;
   esac 
done
