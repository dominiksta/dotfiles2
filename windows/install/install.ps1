if (!(Test-Path ($env:userprofile + "programme\emacs"))) {
    Write-Output "EMACS IS NOT YET INSTALLED"
    Write-Output "download from https://github.com/m-parashar/emax64/releases"
    Start-Sleep 5
}

# ----------------------------------------------------------------------
# --- packages ---
# ----------------------------------------------------------------------
# download chocolatey
Set-ExecutionPolicy Bypass -Scope Process -Force
iex ((New-Object System.Net.WebClient).DownloadString(
         'https://chocolatey.org/install.ps1'))

# install packages
cinst -y claws-mail `
  cmdermini `
  autohotkey `
  git `
  cygwin `
  altdrag `
  mpv `
  ag `
  python3 `
  dropbox `
  xournal `
  qtpass `
  wget `
  nircmd `
  sumatrapdf.install `
  7zip.install `
  nodejs `
  miktex

# TODO syncthing

# ----------------------------------------------------------------------
# --- environment variables ---
# ----------------------------------------------------------------------
[Environment]::SetEnvironmentVariable(
    "Path",
    [Environment]::GetEnvironmentVariable("Path", [EnvironmentVariableTarget]::Machine) `
      + ";" + $Env:Userprofile + "\programme\emacs\bin\" `
      + ";C:\tools\cygwin\bin",
    [EnvironmentVariableTarget]::Machine)

[Environment]::SetEnvironmentVariable("EMACS_SERVER_FILE",
                                      "%userprofile%\.emacs.d\server\server",
                                      [EnvironmentVariableTarget]::User)

[Environment]::SetEnvironmentVariable("EDITOR", "emacsclientw.exe",
                                      [EnvironmentVariableTarget]::User)

[Environment]::SetEnvironmentVariable("HOME", $Env:Userprofile,
                                      [EnvironmentVariableTarget]::Machine)

# reload path
$env:Path = [System.Environment]::GetEnvironmentVariable("Path","Machine") + `
  ";" + [System.Environment]::GetEnvironmentVariable("Path","User")

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
# --- download dotfiles and symlink as needed ---
# ----------------------------------------------------------------------
mkdir -Force $env:userprofile\git
cd $env:userprofile\git
git clone https://github.com/f1rstperson/dotfiles

# --- cmder
cd ($env:userprofile + "\git\dotfiles\windows\cmder\")
# this needs to be removed, because if it exists cmder copies it to
# user-conemu.xml on startup. Also, copies are used instead of
# symlinks because conemu changes the xml even on small things like
# resizing the window.
rm -ErrorAction Ignore C:\tools\cmdermini\vendor\conemu-maximus5\ConEmu.xml
Get-ChildItem ".\" |
  Foreach-Object {
      cp $_.fullname ("C:\tools\cmdermini\config\" + ($_.basename) + $_.extension)
  }

# --- emacs
mkdir -Force $env:userprofile\.emacs.d
cmd /c mklink /d %USERPROFILE%\.emacs.d\config %USERPROFILE%\git\dotfiles\emacs\config
cmd /c mklink %USERPROFILE%\.emacs.d\init.el %USERPROFILE%\git\dotfiles\emacs\init.el


# ----------------------------------------------------------------------
# --- addtional emacs packages ---
# ----------------------------------------------------------------------
C:\tools\cygwin\bin\bash.exe -c 'wget rawgit.com/transcode-open/apt-cyg/master/apt-cyg'
rm 'apt-cyg'
cd $env:userprofile\git\dotfiles\windows\install
C:\tools\cygwin\bin\bash.exe 'cygwin-install.sh'

pip3 install youtube-dl sqlint
npm install -g jsonlint
