#!/bin/bash

source ../../_util_linux.sh

mkdir -p ~/.emacs.d/straight
ln_overwrite .emacs.d/config ~/.emacs.d/config
ln_overwrite .emacs.d/straight/versions ~/.emacs.d/straight/versions
ln_overwrite .emacs.d/init.el ~/.emacs.d/init.el