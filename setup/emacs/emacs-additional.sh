#!/bin/bash
set -e

apt install -y ttf-dejavu

# shell autocompletion
apt install -y fish

# pdf
apt install -y elpa-pdf-tools-server

# music
apt install -y mediainfo mpv socat
pip3 install youtube-dl

# spellchecking
apt install -y aspell-de

# searching
apt install -y silversearcher-ag

# timetable
apt install -y gawk

# my eyyyes
apt install -y redshift

# latex in org-mode
apt install -y texlive-latex-* dvipng
