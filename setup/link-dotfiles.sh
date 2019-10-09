#!/bin/bash
pushd
cd ~/git/dotfiles/stow
stow -v --target=$HOME --restow *
popd
