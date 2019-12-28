#!/bin/bash

sudo apt-get install -y lightdm openbox \
	xfce4 xfce4-whiskermenu-plugin xfce4-terminal caja \
	dmenu nitrogen


# get rid of some xfce components that i do not need
sudo apt-get purge -y xiccd xfdesktop4 xfwm4 xfce4-panel
