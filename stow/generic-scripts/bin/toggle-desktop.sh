#!/bin/sh

if wmctrl -m | grep -q "mode: OFF"; then
    wmctrl -k on
else
    wmctrl -k off
fi