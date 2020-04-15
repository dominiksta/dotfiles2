#!/bin/bash
# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# source bashrc if available
[ -f "$HOME/.bashrc" ] && . "$HOME/.bashrc"

# source pc specific profile variables if available
[ -f "$HOME/.config/pc_specific_profile.sh" ] && source $HOME/.config/pc_specific_profile.sh

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

# - start gpg-agent with `gpg-connect-agent /bye`
#   or `gpgconf --launch gpg-agent`
# - kill gpg-agent with `gpg-connect-agent killagent /bye`
if which gpg-agent > /dev/null; then
    gpgconf --launch gpg-agent
    export SSH_AUTH_SOCK="/run/user/$UID/gnupg/S.gpg-agent.ssh"
fi
