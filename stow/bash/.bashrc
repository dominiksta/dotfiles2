# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# cd
shopt -s autocd
shopt -s cdspell

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color|eterm-color) color_prompt=yes;;
esac

# I use a function to set the PS1, since I want to show the current exit code if
# it is non-zero. Therefore, we need to store the current exit code and use it
# later on, which makes a function a lot more convenient to use than simply
# setting a variable.
PROMPT_COMMAND=_prompt_command
_prompt_command() {
    local EXIT="$?"
    PS1=""

    local RCol='\[\e[0m\]'
    local Red='\[\e[0;31m\]'
    local Pur='\[\e[0;35m\]'
    local Cya='\[\e[36m\]'
    # local Gre='\[\e[0;32m\]'
    # local BYel='\[\e[1;33m\]'
    # local BBlu='\[\e[1;34m\]'

    if [ $EXIT != 0 ]; then
        if [ "$color_prompt" = yes ]; then
            PS1+="${Red}${EXIT} ${RCol}"
        else
            PS1+="${EXIT} "
        fi
    fi

    PS1+="${Cya}\W${Pur} % ${RCol}"

    # If this is an xterm set the title to user@host:dir
    case "$TERM" in
        xterm*|rxvt*)
            echo -ne "\033]0;${HOSTNAME}::${PWD/*\//}\007"
            PROMPT_COMMAND='echo -ne "\033]0;${HOSTNAME}::${PWD/*\//}\007"'
            ;;
        *)
            ;;
    esac
}

# unset color_prompt force_color_prompt

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# Alias definitions.
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then
        . /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
        . /etc/bash_completion
    fi
fi

export EDITOR='emacsclient'
export SUDO_ASKPASS="/home/$USER/git/dotfiles/other/sudo_askpass.sh"
