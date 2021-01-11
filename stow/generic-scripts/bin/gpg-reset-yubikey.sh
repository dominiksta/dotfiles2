#!/bin/bash

EMAILADDRESS="dominik.stahmer@posteo.de"

killall -9 ssh-agent gpg-agent

for keystub in $(gpg --with-keygrip --list-secret-keys $EMAILADDRESS \
                     | grep Keygrip \
                     | awk '{print $3}');
do
    rm $HOME/.gnupg/private-keys-v1.d/$keystub.key;
done;

gpg --card-status
gpgconf --launch gpg-agent