#!/bin/bash

# Disable vsync in xfwm. This leaves composting enabled. I disable vsync because
# my 144 Hz panel gets vsync'ed to 60 Hz when i have my secondary 60 Hz monitor
# connected.
xfconf-query -c xfwm4 -p /general/vblank_mode -s xpresent

# Small thunar icons
xfconf-query --channel thunar --property /misc-small-toolbar-icons --create --type bool --set true