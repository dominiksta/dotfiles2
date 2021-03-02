# ----------------------------------------------------------------------
# --- development ---
# ----------------------------------------------------------------------

.\docker-desktop.ps1

cinst -y dbeaver nodejs composer rdm

RefreshEnv.cmd

npm install -g @angular/cli

# ----------------------------------------------------------------------
# --- other ---
# ----------------------------------------------------------------------

cinst -y microsoft-teams office365proplus

net use H: \\ares\home /persistent:yes
net use B: \\ares\Filesharing-Baar /persistent:yes