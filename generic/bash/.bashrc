# ----------------------------------------------------------------------
# general environment variables and settings
# ----------------------------------------------------------------------

export EDITOR=em

# --- history ---
HISTSIZE=1000
HISTFILESIZE=2000
HISTCONTROL=ignoreboth # don't put duplicate lines or lines starting with space
                       # in the history.
shopt -s histappend # append to the history file, don't overwrite it

# --- cd ---
shopt -s autocd # cd when directory name is typed
shopt -s cdspell # autocorrect some small spelling errors for cd

# ----------------------------------------------------------------------
# prompt and window title
# ----------------------------------------------------------------------

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

    # python virtual environment
    if [ "$VIRTUAL_ENV" != "" ]; then
        PS1+="[venv] "
    fi

    PS1+="${Cya}\W${Pur} % ${RCol}"

    # If this is an xterm set the title to user@host:dir
    # case "$TERM" in
    #     xterm*|rxvt*)
    #         echo -ne "\033]0;${HOSTNAME}::${PWD/*\//}\007"
    #         PROMPT_COMMAND='echo -ne "\033]0;${HOSTNAME}::${PWD/*\//}\007"'
    #         ;;
    #     *)
    #         ;;
    # esac
}

# unset color_prompt force_color_prompt

# ----------------------------------------------------------------------
# ensure emacs shell and terminal colors
# ----------------------------------------------------------------------

case ${INSIDE_EMACS/*,/} in
    comint)
        TERM=ansi
        ;;
    term*)
        TERM=eterm-color
        ;;
esac

# ----------------------------------------------------------------------
# ubuntu defaults
# ----------------------------------------------------------------------

# uncomment for colored gcc warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

[ -f ~/.bash_aliases ] && . ~/.bash_aliases # aliases

# enable programmable completion features (you don't need to enable this, if
# it's already enabled in /etc/bash.bashrc and /etc/profile sources
# /etc/bash.bashrc).
if ! shopt -oq posix; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then
        . /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
        . /etc/bash_completion
    fi
fi

# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return;;
esac

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


# wsl
# TODO detect if wsl
# ----------------------------------------------------------------------

# export DISPLAY=$(cat /etc/resolv.conf | grep nameserver | awk '{print $2; exit;}'):0.0
export DISPLAY=:0.0

# export PATH=$PATH:/mnt/c/Windows/system32:/mnt/c/Windows/System32/WindowsPowerShell/v1.0
alias cmd.exe=/mnt/c/Windows/system32/cmd.exe
alias powershell=/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe
function code() {
    command "/mnt/c/Program Files/Microsoft VS Code/bin/code" $@
}
function emw() {
    command "/mnt/c/Program Files/Emacs/28.1/emacs-28.1/bin/emacsclient.exe" $@
}

/usr/bin/keychain -q --nogui
source $HOME/.keychain/$(hostname)-sh
. "$HOME/.cargo/env"

# nvm
# ----------------------------------------------------------------------

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# ssh keys
# ----------------------------------------------------------------------

[ -h "$HOME/.local/bin/sk" ] && source ~/.local/bin/sk

# emacs interop
# ----------------------------------------------------------------------

function man() { 
    if [ "$TERM" == "eterm-color" ]; then
        emacsclient -e "(man \"$1\")";
    else
        command man "$@";
    fi
}

function less() { 
    if [ "$TERM" == "eterm-color" ]; then
        t=$(mktemp -p /tmp emacs-pager.XXXXX) || exit 1
        cat - >> $t
        echo 'Reading into emacs...'
        emacsclient "$t"
        rm -f -- $t
    else
        command less "$@";
    fi
}

# pnpm
export PNPM_HOME="/home/dominik/.local/share/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end
