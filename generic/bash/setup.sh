#!/bin/bash

source ../util.sh

ln_overwrite .bash_aliases ~/.bash_aliases
ln_overwrite .bash_profile ~/.bash_profile
ln_overwrite .bashrc ~/.bashrc

ln_overwrite sk.sh ~/.local/bin/sk
ln_overwrite emacs-pager.sh ~/.local/bin/emacs-pager