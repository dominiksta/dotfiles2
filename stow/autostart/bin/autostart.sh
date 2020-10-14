#!/bin/bash

# --- gpg-agent ---
# - start gpg-agent with `gpg-connect-agent /bye`
#   or `gpgconf --launch gpg-agent`
# - kill gpg-agent with `gpg-connect-agent killagent /bye`
if which gpg-agent > /dev/null; then
    gpgconf --launch gpg-agent
    export SSH_AUTH_SOCK="/run/user/$UID/gnupg/S.gpg-agent.ssh"
fi

# --- bindings ---
intuos.sh first &
caps.sh &
sxhkd &

# --- tray applications - need to start after the panels ---
(sleep 5 && (nextcloud & \
             nm-applet & \
             keepassxc &  \
             birdtray & \
             kdeconnect-indicator \
 )) &

# --- other autostart applications ---
running-on-laptop-p.sh && (
    echo "running laptop autostart"
) || (
    echo "running desktop autostart"
    autostart-desktop.sh
)

emacs --daemon