#!/bin/sh

apt-get install -y nvidia-driver

cp -f 01-nvidia.conf /etc/X11/xorg.conf.d/
