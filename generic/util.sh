#!/bin/bash

function ln_overwrite() {
    source="$1"
    dest="${2/#\~/$HOME}"

    if [ -f "$dest" ] || [ -d "$dest" ]; then
        if [ -L "$dest" ]; then
            echo "[INFO] overwriting symlink: ln -s $PWD/$source $dest"
            unlink "$dest"
            ln -s "$PWD/$source" "$dest"
        else
            echo "[WARN] $dest is not a symbolic link, skipping"
            return 1
        fi
    else
        echo "[INFO] writing symlink: ln -s $PWD/$source $dest"
        ln -s "$PWD/$source" "$dest"
    fi
}