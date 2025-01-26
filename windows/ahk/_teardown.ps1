$exists = [bool](Get-ScheduledTask | Where-Object {$_.TaskName -like "ahk" })

if ($exists) {
    Write-Warning "Please delete the ahk task manually by running"
    Write-Warning 'Unregister-ScheduledTask -Confirm -TaskName "ahk"'
}
