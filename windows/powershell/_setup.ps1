. "../../_util_windows.ps1"

mkdir -Force ~\Documents\WindowsPowerShell | Out-Null
mkdir -Force ~\Documents\PowerShell | Out-Null

New-Symlink -Target $PWD\Microsoft.PowerShell_profile.ps1 ~\Documents\WindowsPowerShell\Microsoft.PowerShell_profile.ps1
New-Symlink -Target $PWD\Profile.ps1 ~\Documents\PowerShell\Profile.ps1
