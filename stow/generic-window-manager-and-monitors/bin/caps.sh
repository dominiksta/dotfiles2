#!/bin/bash

function start-or-restart {
    if pgrep -x $1 > /dev/null
    then
        echo "restarting $1"
        pkill -x $1
        $2
    else
        echo "starting $1"
        $2
    fi
}


sleep 3
setxkbmap -option ""
xkbset repeatkeys
xmodmap ~/git/dotfiles/scripts/keys/xmodmaprc

start-or-restart xcape "xcape -e Control_L=Escape"
