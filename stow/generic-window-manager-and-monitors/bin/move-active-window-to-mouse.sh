#!/bin/bash
xmouse=$(xdotool getmouselocation --shell | awk 'NR==1' | sed 's/[XY]=//')
ymouse=$(xdotool getmouselocation --shell | awk 'NR==2' | sed 's/[XY]=//')
width=$(xwininfo -id $(xdotool getactivewindow) | grep 'Width' | awk '{print $2}')
height=$(xwininfo -id $(xdotool getactivewindow) | grep 'Height' | awk '{print $2}')

if [ $1 == "titlebar" ]; then
    xdotool getactivewindow windowmove -- $[xmouse-width/2] $[ymouse-10]
elif [ $1 == "middle" ]; then
    xdotool getactivewindow windowmove -- $[xmouse-width/2] $[ymouse-height/2]
fi
