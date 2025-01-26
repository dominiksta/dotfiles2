#!/bin/bash

# --- terminal ---
apt-get install -y xterm tmux

# --- gpg ---
apt-get install -y gnupg2 gnupg-agent scdaemon pcscd

# --- other ---
apt-get install -y stow \
        dmenu xosd-bin playerctl \
        redshift xdotool wmctrl xclip imagemagick zbar-tools \
        xkbset xcape sxhkd \
        kdeconnect \
        keepassxc xournal thunderbird birdtray

# --- cosmetic ---
apt-get install -y ttf-dejavu yaru-theme-icon

# --- software ---
apt-get install flatpak snapd \
        gnome-software gnome-software-plugin-snap gnome-software-plugin-flatpak

flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo

git config --global user.email "f1p@posteo.de"
git config --global user.name "f1p"
git config --global credential.helper 'store --file ~/.cache/git-credential-store'