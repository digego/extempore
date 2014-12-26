#!/bin/bash

[ -f TAGS ] && rm TAGS

if find -version 2>/dev/null | grep GNU > /dev/null; then
  FIND_CMD='find . -regextype posix-extended'
else
  FIND_CMD='find -E .'
fi

# non-hidden files with extension xtm
XTMFILE_REGEX='^.*/[^\.][^/]*\.xtm$'

# make extempore/xtlang tags

$FIND_CMD -regex '.*/[^\.].*\.(cpp|h)$' -print | etags -
$FIND_CMD -regex "$XTMFILE_REGEX" -print | etags --append --regex='/(bind-[a-z]* \([a-z-_!]+\)/\1/' --language=scheme -
$FIND_CMD -regex "$XTMFILE_REGEX" -print | etags --append --regex='/(bind-lib [a-z-_!]+ \([a-z-_!]+\)/\2/' -
