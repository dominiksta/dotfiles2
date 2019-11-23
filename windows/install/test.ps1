
cd ($env:userprofile + "\git\dotfiles\windows\cmder\")
Get-ChildItem ".\" | 
  Foreach-Object {
      cmd /c mklink ("C:\tools\cmder\config\" + ($_.basename)) $_.fullname 
  }
