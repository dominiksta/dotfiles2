#!/bin/bash

# --- terminal ---
apt-get install -y tmux

# --- other ---
apt-get install -y stow \
        dmenu j4-dmenu-desktop \
        redshift xdotool wmctrl xclip \
        xkbset xcape sxhkd \
        keepassxc

# --- cosmetic ---
apt-get install -y ttf-dejavu yaru-theme-icon

# --- software ---
apt-get install flatpak snapd \
        gnome-software gnome-software-plugin-snap gnome-software-plugin-flatpak

flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo

git config --global user.email "f1p@posteo.de"
git config --global user.name "f1p"
git config --global credential.helper 'store --file ~/.cache/git-credential-store'