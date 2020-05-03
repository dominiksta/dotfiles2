#!/bin/bash

TMUX_SESSION_NAME="main"

if ! tmux list-sessions | grep -q $TMUX_SESSION_NAME; then
    tmux new-session -d -s $TMUX_SESSION_NAME -c "$PWD"
else
    tmux new-window -c "$PWD"
fi 

wmctrl -x -a xterm || xterm -e tm