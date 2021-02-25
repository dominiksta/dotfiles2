# Will be automatically executed by powershell if saved as the filename in
# `$profile`. By default, this is
# `$env:Userprofile\Documents\WindowsPowerShell\`.

# If I hand only known this was possible earlier.
Set-PSReadLineOption -EditMode Emacs


function prompt {
  $p = Split-Path -Leaf -Path (Get-Location)
  "PS $p> "
}