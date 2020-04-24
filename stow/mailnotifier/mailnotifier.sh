#!/bin/bash

netrc_password() {
    gpg --quiet --decrypt $1 |
        grep $2 |
        head -1 |
        awk -F'password ' '{print $2}'
}

netrc_password ~/.authinfo.gpg wrk


