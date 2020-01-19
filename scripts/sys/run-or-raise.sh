#!/bin/bash
# either raises $1 when it is running, or runs $2
wmctrl -x -a "$1" || $2 &
