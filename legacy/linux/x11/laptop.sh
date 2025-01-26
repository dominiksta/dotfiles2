#!/bin/sh

apt-get install -y xbacklight

mkdir -p /etc/X11/xorg.conf.d
cp -f 02-backlight.conf /etc/X11/xorg.conf.d/
