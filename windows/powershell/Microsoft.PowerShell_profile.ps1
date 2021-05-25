# Will be automatically executed by powershell if saved as the filename in
# `$profile`. By default, this is
# `$env:Userprofile\Documents\WindowsPowerShell\Microsoft.PowerShell_profile.ps1`.

# If I hand only known this was possible earlier.
Set-PSReadLineOption -EditMode Emacs


function prompt {
    $p = Split-Path -Leaf -Path (Get-Location)
    "PS $env:my__loaded_environment$p> "
}

Function la { Get-ChildItem -Hidden $args }

Set-Alias -Name em -Value emacsclientw.exe
Set-Alias -Name gfind -Value "C:\Program Files\Git\usr\bin\find.exe"

Function admin { nircmd.exe elevate $args }
Function admincmd { nircmd.exe elevate cmd /k "cd $pwd && $args" }
Function adminpsh { nircmd.exe elevate powershell /k $args }

Function Load-EnvFromBat {
    param( [string]$BatFile )
    $dir = (Split-Path -Path $BatFile)
    $file = (Split-Path -Leaf $BatFile)
    pushd "$dir"
    echo "Loading $file ..."
    cmd /c "$file&set" |
      foreach {
          if ($_ -match "=") {
              $v = $_.split("="); set-item -force -path "ENV:\$($v[0])"  -value "$($v[1])"
          }
      }
    popd
    Write-Host "`n$file variables set." -ForegroundColor Yellow
}

Function vcvars2019x64 {
    Load-EnvFromBat -BatFile "C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Auxiliary\Build\vcvars64.bat"
    $env:my__loaded_environment = "vs2019x64 "
    Write-Host "Environment for 'x64 Native Tools Command Prompt for VS 2019'"
}