$exists = [bool](Get-ScheduledTask | Where-Object {$_.TaskName -like "ahk" })

if (!$exists) {
    # $onlogon = New-ScheduledTaskTrigger -AtLogon
    # $run_ahk = New-ScheduledTaskAction -Execute `
    #   "%USERPROFILE%\Source\private\dotfiles\windows\ahk\AllTheHotKeys.ahk"

    # Register-ScheduledTask -User $env:USERNAME -TaskName "ahk" -Action $run_ahk -Trigger $onlogon 
    Write-Warning "AHK Scheduled Task does not exist. You need to do this with manually"
}
