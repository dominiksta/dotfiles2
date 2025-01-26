# Install scoop
# --------------------------------------------------------------------------------

Set-ExecutionPolicy -ExecutionPolicy Unrestricted -Scope CurrentUser
Invoke-RestMethod -Uri https://get.scoop.sh | Invoke-Expression

scoop bucket add extras
scoop bucket add versions
scoop bucket add nerd-fonts

# Install basic apps
# --------------------------------------------------------------------------------

scoop install `
  vivaldi firefox `
  IosevkaTerm-NF `
  ripgrep python `
  altsnap `
  systeminformer-nightly gsudo nircmd shmnview shellmenunew powertoys `
  keepassxc gpg4win `
  xournalpp sumatrapdf naps2 `
  7zip nomacs vlc spotify notepadplusplus audacity filezilla `
  syncthingtray

rm "$env:LocalAppData\Microsoft\WindowsApps\python3.exe"
rm "$env:LocalAppData\Microsoft\WindowsApps\python.exe"

# ----------------------------------------------------------------------
# --- registry tweaks ---
# ----------------------------------------------------------------------
reg import .\windows\reg\networkdrivetimeout.reg
reg import .\windows\reg\webdavfilesizelimit.reg
reg import .\windows\reg\stop-cursor-blink.reg
reg import .\windows\reg\disable-web-search.reg
reg import .\windows\reg\no-alt-shift-for-language-change.reg

# Finish
# --------------------------------------------------------------------------------
Write-Output "Some next (manual) Steps:"
Write-Output "- Set up syncthing tray"
Write-Output "- Add to PATH: Git\usr\bin"
Write-Output "- Enable SSH Agent Service & Set up KeepassXC to talk to it"