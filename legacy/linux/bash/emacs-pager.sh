#!/bin/sh
t=$(tempfile -s .emacs-pager) || exit 1
cat - >> $t
echo 'reading into emacs...'
emacsclient -c "$t"
rm -f -- $t