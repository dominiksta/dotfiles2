#!/bin/bash
set -e

apt-get install -y ttf-dejavu

# shell autocompletion
apt-get install -y fish

# pdf
apt-get install -y elpa-pdf-tools-server

# music
apt-get install -y mediainfo mpv socat
pip3 install youtube-dl

# spellchecking
apt-get install -y aspell-de

# searching
apt-get install -y silversearcher-ag

# timetable
apt-get install -y gawk

# latex in org-mode
apt-get install -y texlive-latex-* dvipng
