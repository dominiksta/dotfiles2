#!/bin/bash
pushd .
mkdir -p ~/.emacs.d/
cd ~/git/dotfiles/stow
stow -v --target=$HOME --restow *
popd
