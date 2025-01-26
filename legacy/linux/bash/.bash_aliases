# -*- mode: shell-script; -*-

alias em='emacsclient --no-wait'
alias t='tmux'
alias o='xdg-open'
alias lsblk='lsblk -o NAME,LABEL,SIZE,RO,TYPE,MOUNTPOINT'

alias gd='gdbtui -q'

alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

alias ll='ls -alhF'
alias la='ls -A'
alias l='ls -CF'
alias lq='ll -h'
alias cl='cd "$@" && ls'

# enable color support of ls
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# enable colors in emacs shell too
if [ $TERM == 'emacs' ]; then
    alias ls='ls --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
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

function remap-mic {
    echo "unloading module-remap-source"
    pactl unload-module module-remap-source
    echo "loading module-remap-source"
    pactl load-module module-remap-source \
          master=alsa_input.usb-Yamaha_Corporation_Steinberg_UR44-00.multichannel-input \
          source_name=mono_mic \
          channels=1 \
          master_channel_map=rear-right \
          channel_map=mono
    echo "setting default source to mono_mic"
    pactl set-default-source mono_mic
}
