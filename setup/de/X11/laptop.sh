#!/bin/sh

apt-get install -y xbacklight

cp -f 02-backlight.sh /etc/X11/xorg.conf.d/
