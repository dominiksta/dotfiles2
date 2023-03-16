@echo off

REM ### Start X410 in Windowed Apps Mode. If X410 is already running in Desktop Mode, 
REM ### it'll be terminated first without any warning message box.

start /B x410.exe /wm

REM ### Start Emacs

ubuntu2204.exe run "bash --login -c 'nohup emacsclient -c >/dev/null 2>&1 & sleep 1'"