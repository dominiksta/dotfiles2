#!/bin/bash

# --- terminal ---
apt-get install -y rxvt-unicode \
        libcommon-sense-perl \
        liblinux-fd-perl \
        libanyevent-perl \
        tmux

# --- other ---
apt-get install -y stow \
        dmenu gsimplecal \
        redshift xdotool wmctrl xclip \
        xkbset xcape \
	ttf-dejavu

git config --global user.email "f1rstperson@gmx.net"
