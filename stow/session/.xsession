#!/bin/sh

[ -f $HOME/.bash_profile ] && . $HOME/.bash_profile

[ -f $HOME/.xprofile ] && . $HOME/.xprofile

exec xfwm4 --compositor=on
