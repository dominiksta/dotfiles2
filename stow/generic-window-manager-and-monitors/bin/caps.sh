#!/bin/bash

# Maps capslock to control on hold and escape on touch.

setxkbmap -option ""
xkbset repeatkeys
xmodmap ~/git/dotfiles/scripts/keys/xmodmaprc

pgrep -x xcape && pkill -9 xcape
xcape -e Control_L=Escape
