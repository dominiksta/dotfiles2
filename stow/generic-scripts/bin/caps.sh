#!/bin/bash

# Maps capslock to control on hold and escape on touch.

setxkbmap -option ""
xkbset repeatkeys
xmodmap ~/Source/git/dotfiles/other/xmodmaprc

pgrep -x xcape && pkill -9 xcape
xcape -e Control_L=Escape
