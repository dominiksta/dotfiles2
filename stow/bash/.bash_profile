#!/bin/bash
# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# source bashrc if available
[ -f "$HOME/.bashrc" ] && . "$HOME/.bashrc"

# source pc specific profile variables if available
[ -f "$HOME/.config/pc_specific_profile.sh" ] && source $HOME/.config/pc_specific_profile.sh

# set PATH so it includes user's private bin if it exists
[ -d "$HOME/bin" ] && PATH="$HOME/bin:$PATH"

# set PATH so it includes /snap/bin if it exists
[ -d "/snap/bin" ] && PATH="/snap/bin:$PATH"

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi
