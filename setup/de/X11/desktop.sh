#!/bin/sh

# apt-get install -y nvidia-driver

mkdir -p /etc/X11/xorg.conf.d
cp -f 01-nvidia.conf /etc/X11/xorg.conf.d/
