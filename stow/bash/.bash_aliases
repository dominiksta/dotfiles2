# -*- mode: shell-script; -*-

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


function screenshare-virtual-webcam {
    sudo rmmod v4l2loopback
    sudo modprobe v4l2loopback video_nr=7 'card_label=myFakeCam' 'exclusive_caps=1'

    ffmpeg -f x11grab -r 20 -s 1920x1080 \
           -i :0.0+0,0 -vcodec rawvideo \
           -pix_fmt yuv420p -threads 0 \
           -f v4l2 /dev/video7
}

# apt-get install -y vlc-plugin-access-extra
function screenshare-virtual-window {
    vlc --no-video-deco \
        --no-embedded-video \
        --screen-fps=20 \
        --screen-top=0 \
        --screen-left=0  \
        --screen-width=1920 \
        --screen-height=1080 \
        screen://
}
