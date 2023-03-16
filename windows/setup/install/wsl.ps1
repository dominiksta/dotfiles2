
# wsl --install -d Ubuntu-22.04

choco install -y freefilesync
Register-ScheduledTask -Xml (Get-Content .\ffs_mvtn.xml | Out-String) -TaskName "ffs_mvtn"


