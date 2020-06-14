#!/bin/bash

keepassxc # keepassxc is single instance by default
sleep 0.5
xdotool key "ctrl+l"
sleep 0.5

time_away=$(command time --output=/dev/stdout --format="%E" xsecurelock)
notify-send "Welcome back" "You were away for $time_away"