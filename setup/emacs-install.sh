#!/bin/bash

# stop on error
set -e

# configure apt for build-dep
sudo sed -Ei 's/^# deb-src/deb-src/' /etc/apt/sources.list
sudo apt update

# dependencies:
sudo apt build-dep -y emacs
sudo apt install -y \
	build-essential autoconf texinfo git \
	python python-pip \
	elpa-pdf-tools-server \
	xfonts-terminus \
	xkbset xcape libpoppler-dev libmagickwand-dev


# download emacs
cd ~ && mkdir -p git && cd git
git clone https://github.com/emacs-mirror/emacs
cd emacs
# wget https://mirror.clarkson.edu/gnu/emacs/emacs-26.1.tar.xz
# tar xvf emacs-26.1.tar.xz && cd emacs-26.1

# compile and install
./autogen.sh
./configure
make
sudo apt purge emacs
sudo make install
