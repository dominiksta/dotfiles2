<# 

This will set the default version of WSL to wsl1 and install ubuntu1804 for wsl1
and ubuntu2004 for wsl2. I do this because currently, the performance of
accessing files on ntfs in wsl1 is good, while wsl2 is almost unusable in this
regard. On the other hand, docker is only supported on wsl2.

wsl2-base.ps1 needs to be run before this script runs.

#>

wsl --set-default-version 2

choco install -y wsl-ubuntu-2004
ubuntu2004.exe

wsl --set-default-version 1

# Chocolatey package seems broken. I needed to install through the store.
# choco install -y wsl-ubuntu-1804
ubuntu1804.exe

wsl --set-default Ubuntu-18.04

# Final check
wsl --list --verbose