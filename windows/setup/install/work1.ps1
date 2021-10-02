# ----------------------------------------------------------------------
# --- development ---
# ----------------------------------------------------------------------

.\docker-desktop.ps1

cinst -y dbeaver nodejs composer rdm firefox googlechrome

RefreshEnv.cmd

npm install -g @angular/cli

pip3 install sqlparse

# ----------------------------------------------------------------------
# --- other ---
# ----------------------------------------------------------------------

cinst -y microsoft-teams office365proplus

net use H: \\ares\home /persistent:yes
net use B: \\ares\Filesharing-Baar /persistent:yes
net use K: \\wotan\filesharing-kassel /persistent:yes