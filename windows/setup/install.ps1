# ----------------------------------------------------------------------
# --- packages ---
# ----------------------------------------------------------------------
# download chocolatey
Set-ExecutionPolicy Bypass -Scope Process -Force
iex ((New-Object System.Net.WebClient).DownloadString(
         'https://chocolatey.org/install.ps1'))

# install packages
cinst -y `
  cmdermini `
  autohotkey `
  git `
  altdrag `
  mpv `
  ag `
  python3 `
  nextcloud `
  keepassxc `
  xournal `
  sumatrapdf.install `
  7zip.install

[Environment]::SetEnvironmentVariable("HOME", $Env:Userprofile,
                                      [EnvironmentVariableTarget]::Machine)

# ----------------------------------------------------------------------
# --- tasks/services ---
# ----------------------------------------------------------------------
schtasks /create /tn AltDrag `
  /tr C:\Users\user\AppData\Roaming\AltDrag\AltDrag.exe `
  /sc ONLOGON /rl HIGHEST
schtasks /create /tn AllTheHotkeys `
  /tr %USERPROFILE%\git\dotfiles\ahk\AllTheHotKeys.ahk `
  /sc ONLOGON /rl HIGHEST


# ----------------------------------------------------------------------
# --- symlinks ---
# ----------------------------------------------------------------------

# --- emacs
mkdir -Force $env:userprofile\.emacs.d
cmd /c mklink /d %USERPROFILE%\.emacs.d\config %USERPROFILE%\git\dotfiles\emacs\config
cmd /c mklink %USERPROFILE%\.emacs.d\init.el %USERPROFILE%\git\dotfiles\emacs\init.el

# ----------------------------------------------------------------------
# --- addtional packages ---
# ----------------------------------------------------------------------
pip3 install youtube-dl
