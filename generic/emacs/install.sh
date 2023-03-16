#!/bin/bash

set -x

# configure apt for build-dep
sudo sed -Ei 's/^# deb-src/deb-src/' /etc/apt/sources.list
sudo apt update

pushd .
cd ~/Source/private

git clone git://git.sv.gnu.org/emacs.git
cd emacs
sudo apt build-dep -y emacs
sudo apt-get install -y build-essential libgtk-3-dev libgnutls28-dev \
     libtiff5-dev libgif-dev libjpeg-dev libpng-dev libxpm-dev \
     libncurses-dev texinfo
./autogen.sh
./configure --with-pgtk
# git checkout emacs-29
make -j8
sudo make install


popd