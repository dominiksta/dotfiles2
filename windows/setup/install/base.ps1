# ----------------------------------------------------------------------
# --- helpers ---
# ----------------------------------------------------------------------

Function Push-Path {
    param ( [string] $Add, [string] $Target )
    [Environment]::SetEnvironmentVariable(
        'Path', [Environment]::GetEnvironmentVariable('Path', $Target) + $Add + ';',
        $target
    )
}

# ----------------------------------------------------------------------
# --- packages ---
# ----------------------------------------------------------------------
# download chocolatey
Set-ExecutionPolicy Bypass -Scope Process -Force
iex ((New-Object System.Net.WebClient).DownloadString(
         'https://chocolatey.org/install.ps1'))

cinst -y `
  firefox chromium `
  emacs git dejavufonts ubuntu.font ripgrep python3 `
  totalcmd frhed microsoft-windows-terminal `
  autohotkey altdrag open-shell `
  procexp gsudo nircmd shmnview shellmenunew zoomit colora `
  keepassxc veracrypt gpg4win `
  xournal pinta sumatrapdf.install naps2 `
  thunderbird birdtray `
  7zip.install nomacs greenshot vlc spotify notepadplusplus audacity filezilla `
  synctrayzor plex

rm "$env:LocalAppData\Microsoft\WindowsApps\python3.exe"
rm "$env:LocalAppData\Microsoft\WindowsApps\python.exe"

Push-Path -Add "$env:LocalAppData\SumatraPDF" -Target 'Machine'
Push-Path -Add "$env:ProgramFiles\Waterfox" -Target 'Machine'
Push-Path -Add "$env:ProgramFiles\Git\usr\bin" -Target 'Machine'

# Set 'HOME' for Emacs
[Environment]::SetEnvironmentVariable(
    'HOME', $env:Userprofile, [EnvironmentVariableTarget]::Machine
)

RefreshEnv.cmd

pip3 install yt-dlp

# ----------------------------------------------------------------------
# --- git config & dotfiles ---
# ----------------------------------------------------------------------

git config --global core.autocrlf false

mkdir -Force ~/Source/git; cd ~/Source/git
git clone https://github.com/f1rstperson/dotfiles2 dotfiles

# ----------------------------------------------------------------------
# --- tasks/services ---
# ----------------------------------------------------------------------
$onlogon = New-ScheduledTaskTrigger -AtLogon
$realtime = New-ScheduledTaskSettingsSet -Priority 0

$run_ahk = New-ScheduledTaskAction -Execute `
  "%USERPROFILE%\Source\git\dotfiles\windows\ahk\AllTheHotKeys.ahk"
$run_altdrag = New-ScheduledTaskAction -Execute `
  "%USERPROFILE%\AppData\Roaming\AltDrag\AltDrag.exe"
$run_birdtray = New-ScheduledTaskAction -Execute `
  "%ProgramFiles%\Birdtray\birdtray.exe"

Register-ScheduledTask -TaskName "ahk" -Action $run_ahk -Settings $realtime -Trigger $onlogon 
Register-ScheduledTask -TaskName "altdrag" -Trigger $onlogon -Action $run_altdrag
Register-ScheduledTask -TaskName "birdtray" -Trigger $onlogon -Action $run_birdtray

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
  %USERPROFILE%\Source\git\dotfiles\stow\emacs\.emacs.d\straight\versions\default.el

# --- windows terminal
rm $env:localappdata\Packages\Microsoft.WindowsTerminal_8wekyb3d8bbwe\LocalState\settings.json
cmd /c mklink %LOCALAPPDATA%\Packages\Microsoft.WindowsTerminal_8wekyb3d8bbwe\LocalState\settings.json `
  %USERPROFILE%\Source\git\dotfiles\windows\windows-terminal\settings.json

# --- powershell
cmd /c mklink %USERPROFILE%\Documents\WindowsPowerShell\Microsoft.PowerShell_profile.ps1 `
  %USERPROFILE%\Source\git\dotfiles\windows\powershell\Microsoft.PowerShell_profile.ps1

# --- totalcmd
cmd /c mklink /d %APPDATA%\GHISLER `
  %USERPROFILE%\Source\git\dotfiles\windows\totalcmd

# ----------------------------------------------------------------------
# --- file associations ---
# ----------------------------------------------------------------------
cmd /c ftype emacsclient=C:\ProgramData\chocolatey\bin\emacsclientw.exe "%1"

cmd /c assoc .ps1=emacsclient
cmd /c assoc .sql=emacsclient
cmd /c assoc .c=emacsclient
cmd /c assoc .h=emacsclient
cmd /c assoc .cpp=emacsclient
cmd /c assoc .hpp=emacsclient
cmd /c assoc .txt=emacsclient
cmd /c assoc .js=emacsclient
cmd /c assoc .ts=emacsclient
cmd /c assoc .json=emacsclient
cmd /c assoc .py=emacsclient
cmd /c assoc .md=emacsclient
cmd /c assoc .java=emacsclient

# ----------------------------------------------------------------------
# --- registry tweaks ---
# ----------------------------------------------------------------------
reg import ..\networkdrivetimeout.reg
reg import ..\webdavfilesizelimit.reg
reg import ..\stop-cursor-blink.reg
reg import ..\disable-web-search.reg
reg import ..\no-alt-shift-for-language-change.reg
reg import ..\emacs-context-menu.reg
reg import ..\totalcmd-as-default.reg
reg import ..\keepasssxc-waterfox.reg

# ----------------------------------------------------------------------
# --- network drives ---
# ----------------------------------------------------------------------

net use Z: \\nasenbaer\public /persistent:yes
net use N: \\icyou.icu@SSL\DavWWWRoot\remote.php\dav\files\dominik.stahmer@posteo.de\ /persistent:yes
start Z:\dominik # Initializes the drive
Start-Sleep 5

# ----------------------------------------------------------------------
# --- gpg ---
# ----------------------------------------------------------------------

$gpg_program = "C:/Program Files (x86)/GnuPG/bin/gpg.exe"

git config --global gpg.program $gpg_program

&$gpg_program --import Z:\dominik\documents\00-critical\public-yubi.txt
&$gpg_program --edit-key dominik.stahmer@posteo.de trust # ultimately
&$gpg_program --card-status