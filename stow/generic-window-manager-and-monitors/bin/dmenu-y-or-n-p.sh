#!/bin/bash

# This emulates the emacs function `y-or-n-p`; Meaning it will ask for "$1: no
# or yes?" and return an exit code of 1 on no and 0 on yes.

[ $(printf "no(n)\nyes(y)" | dmenu -p $1) = "yes(y)" ]

