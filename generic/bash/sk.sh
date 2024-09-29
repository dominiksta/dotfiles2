#!/bin/bash

(return 0 2>/dev/null) && sourced=1 || sourced=0

if [ $sourced -eq 1 ]; then
    _sk_completion() {
        available=''
        for d in ~/.ssh/id_rsa* ; do
            available+=$(basename $d)
            available+=' '
        done
        COMPREPLY=($(compgen -W "$available" "${COMP_WORDS[1]}"))
    }

    complete -F _sk_completion sk
else
    ssh-add ~/.ssh/$1
fi