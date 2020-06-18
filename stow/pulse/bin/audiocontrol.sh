#!/bin/bash

alsavolume() {
    amixer sget Master | grep 'Right:' | awk -F'[][]' '{ print $2 }' | head -1
}

alsamutestatus() {
    amixer sget Master | grep 'Right:' | awk -F'[][]' '{ print $4 }' | head -1
}

case $1 in
    up|down)
        case $1 in
            up)
                amixer set Master "5%+" > /dev/null
                ;;
            down)
                amixer set Master "5%-" > /dev/null
                ;;
        esac
        if which osd_cat; then
            killall osd_cat
            osd_cat -b percentage -p middle --delay 1 -P $(alsavolume) -T Volume:
        else
            notify-send " " -i stock_volume \
                        -h int:value:$(alsavolume) \
                        -h string:synchronous:volume
        fi
        ;;
    mute)
        amixer set Master toggle > /dev/null
        notify-send -u low --expire-time=1 "Volume Control" "audio toggled $(alsamutestatus)"
        ;;
    *)
        if [ "$(alsamutestatus)" = "on" ]; then
           echo $(alsavolume)
        else
           echo "---"
        fi
        ;;
esac
