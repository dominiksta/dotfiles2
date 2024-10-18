rm $env:localappdata\lazygit\config.yml
New-Item -Path $env:localappdata\lazygit\config.yml -ItemType SymbolicLink -Value .\config.yml