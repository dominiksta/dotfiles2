#!/bin/bash

set -e

case $1 in
    setup)
        echo "setting up"
        operation="setup"
        ;;
    teardown)
        echo "tearing down"
        operation="teardown"
        ;;
    *)
        echo "invalid arguments"
        exit 1
esac

for d in */ ; do
    echo "considering $d"
    cd $d
    [ -f "$operation.sh" ] && echo "running $d/$operation.sh" \
        && bash "$operation.sh"
    cd ..
done