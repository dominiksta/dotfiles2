#!/bin/sh

case $1 in
    activate)
        xfconf-query -c xfce4-notifyd -p /do-not-disturb -s true
        ;;
    deactivate)
        xfconf-query -c xfce4-notifyd -p /do-not-disturb -s false
        ;;
    toggle)
        [ `xfconf-query -c xfce4-notifyd -p /do-not-disturb`  = "false" ] \
            && xfconf-query -c xfce4-notifyd -p /do-not-disturb -s true \
                || xfconf-query -c xfce4-notifyd -p /do-not-disturb -s false
        ;;
esac

[ `xfconf-query -c xfce4-notifyd -p /do-not-disturb` = "false" ] && echo "∀" || echo "¬"