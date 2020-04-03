#!/bin/bash

# Moves all pulse sink-inputs to a given sink identified by $1 and marks $1 as
# the default sink. If $1 is not provided, a dmenu prompt will be shown to
# select a sink.

if [ -z "$1" ]; then
    new_sink=$(
        pactl list short sinks |
            awk '{print $1, $2}' |
            dmenu -l 10 -i |
            awk '{print $1}'
        )
else
    new_sink="$1"
fi

pacmd set-default-sink $new_sink

pactl list short sink-inputs | while read stream; do
    stream_id=$(echo $stream | cut '-d ' -f1)
    echo "moving stream $stream_id"
    pactl move-sink-input "$stream_id" "$new_sink"
done
