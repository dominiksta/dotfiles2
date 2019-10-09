#!/bin/bash

# backlight

sudo apt install xbacklight

sudo bash -c 'cat << EOF > /etc/X11/xorg.conf
Section "Device"
      Identifier  "Intel Graphics" 
      Driver      "intel"
      Option      "Backlight"  "intel_backlight"
EndSection
EOF'
