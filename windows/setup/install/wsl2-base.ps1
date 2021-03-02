<#
Windows Subsystem for Linux (Version 1+2)
--------------------------------------------------------------------------------

I contain this in a seperate file, since I may not want to have this on every
system I run.

From the chocolatey docs: 

"/Retry:true" should only be set when installing WSL 2 on systems that didn't
have WSL 1. Setting this param to true will schedule a self deleting task if
script didn't detect WSL. That task if created will run "choco install wsl2 -y
-f" in a powershell window after the computer is restarted.

#>

choco install -y wsl2 --params "/Version:2 /Retry:true"

$confirmation = Read-Host `
  "A reboot is needed for the installation to complete. Reboot now? (y/N)"
if ($confirmation -eq 'y') {
    Restart-Computer
}