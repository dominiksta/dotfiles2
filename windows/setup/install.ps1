# ----------------------------------------------------------------------
# --- packages ---
# ----------------------------------------------------------------------
# download chocolatey
Set-ExecutionPolicy Bypass -Scope Process -Force
iex ((New-Object System.Net.WebClient).DownloadString(
         'https://chocolatey.org/install.ps1'))

# install packages
cinst -y `
  git ag python3 `
  autohotkey altdrag `
  xournal sumatrapdf.install `
  keepassxc `
  7zip.install

[Environment]::SetEnvironmentVariable("HOME", $Env:Userprofile,
                                      [EnvironmentVariableTarget]::Machine)

# ----------------------------------------------------------------------
# --- tasks/services ---
# ----------------------------------------------------------------------
schtasks /create /tn AltDrag `
  /tr C:\Users\user\AppData\Roaming\AltDrag\AltDrag.exe `
  /sc ONLOGON /rl HIGHEST
schtasks /create /tn ahk `
  /tr %USERPROFILE%\git\dotfiles\windows\ahk\AllTheHotKeys.ahk `
  /sc ONLOGON


# ----------------------------------------------------------------------
# --- symlinks ---
# ----------------------------------------------------------------------

# --- emacs
mkdir -Force $env:userprofile\.emacs.d\straight\versions
cmd /c mklink /d %USERPROFILE%\.emacs.d\config `
  %USERPROFILE%\git\dotfiles\stow\emacs\.emacs.d\config
cmd /c mklink %USERPROFILE%\.emacs.d\init.el `
  %USERPROFILE%\git\dotfiles\stow\emacs\.emacs.d\init.el
cmd /c mklink %USERPROFILE%\.emacs.d\straight\versions\default.el `
  %USERPROFILE%\git\dotfiles\emacs\.emacs.d\straight\default.el

# ----------------------------------------------------------------------
# --- addtional packages ---
# ----------------------------------------------------------------------
pip3 install youtube-dl
