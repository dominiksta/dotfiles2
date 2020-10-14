#!/bin/bash
set -e

apt-get install -y ttf-dejavu

# shell autocompletion
apt-get install -y fish

# pdf
apt-get install -y elpa-pdf-tools-server

# spellchecking
apt-get install -y hunspell hunspell-de-de hunspell-en-us

# searching
apt-get install -y silversearcher-ag

# latex in org-mode
apt-get install -y texlive-full dvipng
