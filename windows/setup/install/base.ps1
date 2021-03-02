# ----------------------------------------------------------------------
# --- packages ---
# ----------------------------------------------------------------------
# download chocolatey
Set-ExecutionPolicy Bypass -Scope Process -Force
iex ((New-Object System.Net.WebClient).DownloadString(
         'https://chocolatey.org/install.ps1'))

cinst -y `
  waterfox chromium `
  git emacs dejavufonts ag ripgrep python3 `
  autohotkey nircmd altdrag `
  xournal sumatrapdf.install `
  keepassxc gpg4win `
  7zip.install procexp zoomit irfanview vlc notepadplusplus audacity filezilla `
  veracrypt colora `
  thunderbird birdtray `
  doublecmd microsoft-windows-terminal `
  synctrayzor plex

[Environment]::SetEnvironmentVariable('HOME', $Env:Userprofile,
                                      [EnvironmentVariableTarget]::Machine)

RefreshEnv.cmd

pip3 install youtube-dl

# ----------------------------------------------------------------------
# --- git config & dotfiles ---
# ----------------------------------------------------------------------

git config --global core.autocrlf false

mkdir -Force ~/Source/git; cd ~/Source/git
git clone https://github.com/f1rstperson/dotfiles2 dotfiles

# ----------------------------------------------------------------------
# --- tasks/services ---
# ----------------------------------------------------------------------
schtasks /create /tn AltDrag `
  /tr %USERPROFILE%\AppData\Roaming\AltDrag\AltDrag.exe `
  /sc ONLOGON /rl HIGHEST
schtasks /create /tn ahk `
  /tr %USERPROFILE%\Source\git\dotfiles\windows\ahk\AllTheHotKeys.ahk `
  /sc ONLOGON
schtasks /create /tn birdtray `
  /tr %ProgramFiles%\Birdtray\birdtray.exe `
  /sc ONLOGON


# ----------------------------------------------------------------------
# --- symlinks ---
# ----------------------------------------------------------------------

# --- emacs
mkdir -Force $env:userprofile\.emacs.d\straight\versions
cmd /c mklink /d %USERPROFILE%\.emacs.d\config `
  %USERPROFILE%\Source\git\dotfiles\stow\emacs\.emacs.d\config
cmd /c mklink %USERPROFILE%\.emacs.d\init.el `
  %USERPROFILE%\Source\git\dotfiles\stow\emacs\.emacs.d\init.el
cmd /c mklink %USERPROFILE%\.emacs.d\straight\versions\default.el `
  %USERPROFILE%\Source\git\dotfiles\emacs\.emacs.d\straight\default.el
  
# --- windows terminal
rm $env:localappdata\Packages\Microsoft.WindowsTerminal_8wekyb3d8bbwe\LocalState\settings.json
cmd /c mklink %LOCALAPPDATA%\Packages\Microsoft.WindowsTerminal_8wekyb3d8bbwe\LocalState\settings.json `
  %USERPROFILE%\Source\git\dotfiles\windows\windows-terminal\settings.json
  
# --- powershell
cmd /c mklink %USERPROFILE%\Documents\WindowsPowerShell\Microsoft.PowerShell_profile.ps1 `
  %USERPROFILE%\Source\git\dotfiles\windows\powershell\Microsoft.PowerShell_profile.ps1

# ----------------------------------------------------------------------
# --- network drives ---
# ----------------------------------------------------------------------

net use Z: \\nasenbaer\public /persistent:yes
net use N: \\icyou.icu@SSL\DavWWWRoot\remote.php\dav\files\dominik.stahmer@posteo.de\ /persistent:yes

# ----------------------------------------------------------------------
# --- gpg ---
# ----------------------------------------------------------------------

gpg --import N:\external\documents\00-critical\public-yubi.txt
Write-Output "reader-port Yubico Yubi" > $env:AppData\gnupg\scdaemon.conf
gpg --edit-key dominik.stahmer@posteo.de trust # ultimately
gpg --card-status