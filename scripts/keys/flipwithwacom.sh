#!/bin/sh
# get these names with `xinput list`
stylus='Wacom ISDv4 90 Pen stylus'
eraser='Wacom ISDv4 90 Pen eraser'
trackpoint='TPPS/2 IBM TrackPoint'
touchpad='SynPS/2 Synaptics TouchPad'

areaNormal='227 32 28000 15713'
areaInverted='-285 75 27516 15742'

rotation="$(xrandr -q --verbose | grep 'connected' | egrep -o  '\) (normal|left|inverted|right) \(' | egrep -o '(normal|left|inverted|right)')"

case "$rotation" in
    normal)
        xinput set-prop "$trackpoint" "Device Enabled" 0
        xinput set-prop "$touchpad" "Device Enabled" 0
        xrandr -o inverted
        xsetwacom set "$stylus" rotate half
        xsetwacom set "$eraser" rotate half
        xsetwacom set "$stylus" Area $areaInverted
        ;;
    inverted)
        xinput set-prop "$trackpoint" "Device Enabled" 1
        xinput set-prop "$touchpad" "Device Enabled" 0
        xrandr -o normal
        xsetwacom set "$stylus" rotate none
        xsetwacom set "$eraser" rotate none
        xsetwacom set "$stylus" Area $areaNormal
        ;;
esac
