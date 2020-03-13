#!/bin/bash

sudo apt-get install -y lightdm openbox \
	xfce4 xfce4-whiskermenu-plugin xfce4-terminal caja \
	dmenu nitrogen


# get rid of some xfce components that i do not need
# sudo apt-get purge -y xiccd xfdesktop4 xfwm4 xfce4-panel

# Disable vsync in xfwm. This leaves composting enabled. I disable vsync because
# my 144 Hz panel gets vsync'ed to 60 Hz when i have my secondary 60 Hz monitor
# connected.
xfconf-query -c xfwm4 -p /general/vblank_mode -s xpresent

# Disable desktop icons
xfconf-query -c xfce4-desktop -v --create -p /desktop-icons/style -t int -s 0
