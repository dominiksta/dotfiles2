[Environment]::SetEnvironmentVariable("ChocolateyInstall",
                                      ($Env:Userprofile + "\chocolatey"),
                                      [EnvironmentVariableTarget]::User)


# ----------------------------------------------------------------------
# --- packages ---
# ----------------------------------------------------------------------
# download chocolatey
Set-ExecutionPolicy Bypass -Scope Process -Force
iex ((New-Object System.Net.WebClient).DownloadString(
         'https://chocolatey.org/install.ps1'))

# install packages
cinst -y autohotkey.portable `
  git `
  altdrag `
  cmdermini.portable `
  mpv.portable `
  mediainfo-cli `
  dejavufonts `
  ag `
  wget `
  nircmd `
  sumatrapdf.commandline `
  7zip.portable `

mkdir ~/programme

[Environment]::SetEnvironmentVariable(
    "Path",
    [Environment]::GetEnvironmentVariable("Path", [EnvironmentVariableTarget]::User) `
      + ";" + $Env:Userprofile + "\programme\emacs\bin\" `
      + ";" + $Env:Userprofile + "\programme\nodejs\" `
      [EnvironmentVariableTarget]::User)

# reload path
$env:Path = [System.Environment]::GetEnvironmentVariable("Path","Machine") + `
  ";" + [System.Environment]::GetEnvironmentVariable("Path","User")

npm install -g jsonlint

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
