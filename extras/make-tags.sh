#!/bin/bash

[ -f TAGS ] && rm TAGS

case $(uname) in
  'Linux')  FIND_CMD='find . -regextype posix-extended';;
  'Darwin') FIND_CMD='find -E .';;
esac

# make extempore/xtlang tags
$FIND_CMD -regex '.*/.*\.(cpp|h)$' -print | etags -
$FIND_CMD -regex '.*/.*\.xtm$' -print | etags --append --regex='/(bind-[a-z]* \([a-z-_!]+\)/\1/' --language=scheme -
$FIND_CMD -regex '.*/.*\.xtm$' -print | etags --append --regex='/(bind-lib [a-z-_!]+ \([a-z-_!]+\)/\2/' -
