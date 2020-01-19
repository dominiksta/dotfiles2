#!/bin/bash
pushd .
mkdir -p ~/.emacs.d/
cd ~/git/dotfiles/stow
stow -v --target=$HOME --restow *

mkdir -p ~/bin/
ln -sf ~/git/dotfiles/scripts/sys ~/bin/sys

popd
