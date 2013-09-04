#!/bin/bash

[ -f TAGS ] && rm TAGS

# make extempore/xtlang tags
find -E . -regex '.*/.*\.(cpp|h)$' -print | etags -
find -E . -regex '.*/.*\.xtm$' -print | etags --append --regex='/(bind-[a-z]* \([a-z-_!]+\)/\1/' --language=scheme -
find -E . -regex '.*/.*\.xtm$' -print | etags --append --regex='/(bind-lib [a-z-_!]+ \([a-z-_!]+\)/\2/' -
