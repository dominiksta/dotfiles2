#!/bin/bash

pushd .

mkdir -p ~/.emacs.d/
mkdir -p ~/bin/

cd ~/git/dotfiles/stow
stow -v --target=$HOME --restow *

popd
