#!/usr/bin/env bash

# takes one argument: the path to write the json file

# e.g.
# shell> cd ~/path/to/extempore
# shell> ./extras/generate-xtmdoc.sh xtmdoc.json

./extempore --eval "(begin
  (sys:load \"libs/core/adt.xtm\")
  (sys:load \"libs/core/audio_dsp.xtm\")
  (sys:load \"libs/core/instruments.xtm\")
  (sys:load \"libs/core/math.xtm\")
  (sys:load \"libs/core/math_ext.xtm\")
  (sys:load \"libs/core/std.xtm\")
  (sys:load \"libs/external/fft.xtm\")
  (sys:load \"libs/external/gl.xtm\")
  (sys:load \"libs/external/glfw3.xtm\")
  (sys:load \"libs/external/instruments_ext.xtm\")
  (sys:load \"libs/external/nanovg.xtm\")
  (sys:load \"libs/external/sndfile.xtm\")
  (sys:load \"libs/external/stb_image.xtm\")
  (xtmdoc-export-caches-to-json \"$1\" #f)
  (quit 0))"

if (($? != 0))  ; then
    echo -e "\033[0;31mxtmdoc export failed\033[0;00m"
    echo
    exit 1
else
    echo -e "\033[0;32mSuccessfully\033[0;00m exported xtmdoc to $1"
    echo
    exit 0
fi
