# Will be automatically executed by powershell if saved as the filename in
# `$profile`. By default, this is
# `$env:Userprofile\Documents\WindowsPowerShell\Microsoft.PowerShell_profile.ps1`.

# If I hand only known this was possible earlier.
Set-PSReadLineOption -EditMode Emacs

# color settings
# ----------------------------------------------------------------------


if ($env:WT_SESSION) {
    function Get-WindowsAppTheme() {
        return (
            Get-ItemProperty -Path `
              "HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\Themes\Personalize"
        ).AppsUseLightTheme;
    }

    function Set-WTTheme($isLight) {
        $bg = if ($isLight) {'White'} else {'Black'};
        $fg = if ($isLight) {'Black'} else {'White'};

        Set-PSReadLineOption -Colors @{
            Command            = $fg
            Number             = $fg
            Member             = $fg
            Operator           = $fg
            Type               = $fg
            Variable           = $fg
            Parameter          = $fg
            ContinuationPrompt = $fg
            Default            = $fg
        }
    }

    Set-WTTheme(Get-WindowsAppTheme);

    Set-PSReadLineKeyHandler -Chord Ctrl+Shift+I -ScriptBlock {
        Set-WTTheme(Get-WindowsAppTheme);
    }
}

# prompt
# ----------------------------------------------------------------------

if ($env:WT_SESSION) {
    function Prompt {
        $p = Split-Path -Leaf -Path (Get-Location)
        $host.UI.RawUI.WindowTitle = $p

        # see https://superuser.com/a/1259916
        $fgGreen = "$([char]27)[32m"
        $fgCyan = "$([char]27)[36m"
        $fgMagenta = "$([char]27)[35m"
        $fgDefault = "$([char]27)[0m"

        $fgGreen + "$env:my__loaded_environment" + `
          $fgCyan + "$p" + `
          $fgMagenta + " % " + `
          $fgDefault
    }
} else {
    function Prompt {
        $p = Split-Path -Leaf -Path (Get-Location)
        "PS $env:my__loaded_environment$p> "
    }
}

Function la { Get-ChildItem -Hidden $args }

Set-Alias -Name sysjava -Value "$env:JAVA_HOME\bin\java.exe"

Set-Alias -Name em -Value emacsclientw.exe
Set-Alias -Name gfind -Value "C:\Program Files\Git\usr\bin\find.exe"

Set-Alias -Name firefox-dev -Value "C:\Program Files\Firefox Developer Edition\firefox.exe"
Function firefox-debug {
    &"C:\Program Files\Firefox Developer Edition\firefox.exe" `
      -start-debugger-server $args
} 

Function admin { nircmd.exe elevate $args }
Function admincmd { nircmd.exe elevate cmd /k "cd $pwd && $args" }
Function adminpsh { nircmd.exe elevate powershell /k $args }

Function time {
    Measure-Command { "$args" | Out-Default }
}

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

function Get-IrfanViewQR { zbarimg $env:temp\iviewscrot.png }

function Switch-YubiKey {
    $email = "dominik.stahmer@posteo.de"

    $keygrips = gpg --with-keygrip --list-secret-keys $email | select-string -pattern "Keygrip"

    foreach ($match in $keygrips) {
        $keygrip = $match.Line.Split('=')[1].Substring(1)
        # $keydir = "$env:UserProfile\.gnupg\private-keys-v1.d" # git for windows
        $keydir = "$env:AppData\gnupg\private-keys-v1.d"
        rm -Force "$keydir\$keygrip.key"
    }

    gpg --card-status
}

Function Restart-ScheduledTask {
    Param ([parameter(Position=0)][String]$task)

    Stop-ScheduledTask $task
    Start-ScheduledTask $task
}

function Get-YoutubePlaylistAsPodcast {

    Param ([parameter(Position=0)][String]$url)

    yt-dlp.exe `
      --yes-playlist `
      -x --audio-format=mp3 `
      -o "%(upload_date)s_%(playlist_index)s - %(title)s.%(ext)s" `
      --sponsorblock-remove sponsor `
      $url
}

# . "~/Source/git/dotfiles/windows/powershell/Background-Apps.ps1"
