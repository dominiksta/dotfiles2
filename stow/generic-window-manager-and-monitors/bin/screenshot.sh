#!/bin/bash

filename=/tmp/screenshot-$(date +%F_%H-%M-%S).png

if [ -z $1 ]; then
    screenshot_command=$(echo -e "selection\ncurrent-window\nall-monitors" | dmenu -i)
else
    screenshot_command=$1
fi

case $screenshot_command in
    "selection")
        scrot --select $filename
        ;;
    "current-window")
        scrot --focused $filename
        ;;
    "all-monitors")
        scrot $filename
        ;;
    *)
        exit 1
        ;;
esac

if [ -z $2 ]; then
    action_selection=$(echo -e "clipboard\ndired\nfilemanager" | dmenu -i)
else
    action_selection=$2
fi

case $action_selection in
    clipboard)
        xclip -selection c -t image/png < $filename
        ;;
    dired|filemanager)
        path="$HOME/Pictures/Screenshots/$(date +%Y)/$(date +%m)"
        mkdir -p $path
        mv $filename $path
        if [ $action_selection = "dired" ]; then
            em $path
        else
            xdg-open $path
        fi
        ;;
    *)
esac