# This script starts or stops a series of "background" apps. This is mainly
# useful saving some battery.

class BackgroundAppsManager {
    static $apps = @(
        @('ferdi', 'Ferdi'),
        @('birdtray', 'Birdtray'),
        @('spotify', 'Spotify')
    )

    static StopBackgroundApps() {
        foreach ($app in [BackgroundAppsManager]::apps){
            Stop-Process -Name $app[0]
        }   
    }

    static StartBackgroundApps() {
        foreach ($app in [BackgroundAppsManager]::apps){
            $AppID = (Get-StartApps | Where 'Name' -Eq $app[1]).AppID
            Write-Output "Starting $AppID"
            Start-Process "explorer.exe" -ArgumentList ("shell:AppsFolder\" + $AppID)
        }   
    }
    
}

function Stop-Backgroundapps { [BackgroundAppsManager]::StopBackgroundApps() }
function Start-Backgroundapps { [BackgroundAppsManager]::StartBackgroundApps() }