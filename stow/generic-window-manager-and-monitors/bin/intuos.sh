#!/bin/sh

stylus="Wacom Intuos PT M 2 Pen stylus"
pad="Wacom Intuos PT M 2 Pad pad"

# ------------------------------------------------------------
# set buttons
# ------------------------------------------------------------

# --- pen buttons ---

xsetwacom set "$stylus" Button 1 1 # write with pen
xsetwacom set "$stylus" Button 2 3 # lower pen-button
xsetwacom set "$stylus" Button 3 2 # upper pen-button

# --- face buttons ---

xsetwacom set "$pad" Button 1 \
          "key +ctrl z -ctrl"               # lower left
xsetwacom set "$pad" Button 3 \
          "key +ctrl +shift p -ctrl -shift" # upper left
xsetwacom set "$pad" Button 8 \
          "key +ctrl +shift l -ctrl -shift" # lower right
xsetwacom set "$pad" Button 9 \
          "key +ctrl +shift t -ctrl -shift" # upper right


# ------------------------------------------------------------
# limit to a given monitor
# ------------------------------------------------------------

if [ $1 ]; then
    # xrandr puts a '*' at the currently used resolutions
    resolution1=$(xrandr | grep \* | awk '{print $1}' | head -1)
    resolution2=$(xrandr | grep \* | awk '{print $1}' | tail -1)
    resolution2offsetx=$(echo $resolution1 | cut -d 'x' -f1)
    resolution2offsety=$(echo $resolution1 | cut -d 'x' -f2)

    case $1 in
        first) # limit to first screen
            xsetwacom set "$stylus" MapToOutput \
                      "$resolution1+0+0"
            break
            ;;
        second) # limit to second screen
            xsetwacom set "$stylus" MapToOutput \
                      "$resolution2+$resolution2offsetx+0"
            break
            ;;
        *)
    esac
fi
