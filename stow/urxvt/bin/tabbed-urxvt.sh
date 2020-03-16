#!/bin/bash

# Starts up an instance of urxvt with suckless's tabbed. This seems a lot more
# elegant, straightforward and adaptable to other terminals than using the
# tabbed perl extension for urxvt. Further instances of urxvt will be spawned in
# the same instance of tabbed.

XID_FILE=/tmp/tabbed-urxvt.xid

# If there is no file containing the xid AND there is no running instance of
# tabbed matching that xid, only then start a new instance of tabbed and write
# to xid file.
if [ ! -f $XID_FILE ] || ! wmctrl -l | grep $(cat $XID_FILE | cut -c 3-) ; then
    tabbed -n tabbed-urxvt -d > $XID_FILE
fi

# Raise the tabbed window
wmctrl -i -a $(<$XID_FILE)

# Start urxvt in the instance of tabbed identified by the xid.
urxvt -tn xterm-256color -embed $(<$XID_FILE) $*

