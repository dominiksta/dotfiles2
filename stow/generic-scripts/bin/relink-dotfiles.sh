#!/bin/bash

pushd . > /dev/null

DEBUG=false

# ----------------------------------------------------------------------
# copy
# ----------------------------------------------------------------------

cd ~/git/dotfiles/stow-copy/

for group in */ ; do
    cd $group
    $DEBUG && echo "pwd: $(pwd)"

    for file in $(find . -type f); do
        dest=$(echo $file | sed "s/\./\$HOME/")
        eval dest=$dest
        $DEBUG && echo "running mkdir -p $(dirname $dest)"
        mkdir -p "$(dirname $dest)"
        $DEBUG && echo "running cp -f $file $dest"
        cp -f $file $dest
    done

    cd ..
done

# ----------------------------------------------------------------------
# stow
# ----------------------------------------------------------------------

mkdir -p ~/.config/autostart/
mkdir -p ~/.emacs.d/
mkdir -p ~/bin/

cd ~/git/dotfiles/stow
stow -v --target=$HOME --restow *

popd > /dev/null
