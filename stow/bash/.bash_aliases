# -*- mode: shell-script; -*-
alias em='emacsclient -n --alternate-editor=emacs'
alias emn='emacsclient -nw --alternate-editor=emacs'
alias t='tmux'
alias o='xdg-open'

alias gd='gdbtui -q'

alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias lq='ll -h'
alias cl='cd "$@" && ls'

# enable color support of ls
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='TERM=ansi && ls --color=always'
    #alias dir='dir --color=always'
    #alias vdir='vdir --color=always'

    alias grep='grep --color=always'
    alias fgrep='fgrep --color=always'
    alias egrep='egrep --color=always'
fi

# enable colors in emacs shell too
if [ $TERM == 'emacs' ]; then
    alias ls='ls --color=always'

    alias grep='grep --color=always'
    alias fgrep='fgrep --color=always'
    alias egrep='egrep --color=always'
fi
