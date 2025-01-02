# Set-Location $HOME
Set-PSReadLineOption -EditMode Emacs

# disable quiche-eater suggestions
Set-PSReadLineOption -PredictionSource None

# color settings
# ----------------------------------------------------------------------

if ($env:WT_SESSION) {
    $fgDefault = "$([char]27)[0m"

    $PSStyle.FileInfo.Directory = "$([char]27)[36m" # cyan 

    # See Get-PSReadLineOption
    Set-PSReadLineOption -Colors @{
        Command                 = $PSStyle.Foreground.Green
        Comment                 = $PSStyle.Foreground.BrightBlack
        ContinuationPrompt      = $fgDefault
        Emphasis                = $PSStyle.Foreground.BrightCyan
        InlinePrediction        = $PSStyle.Foreground.BrightCyan
        Keyword                 = $PSStyle.Foreground.BrightGreen
        ListPrediction          = "`e[33m"
        ListPredictionSelected  = "`e[48;5;238m"
        ListPredictionTooltip   = "`e[97;2;3m"
        Member                  = "`e[37m"
        Number                  = "`e[97m"
        Operator                = "`e[90m"
        Parameter               = "`e[90m"
        Selection               = "`e[30;47m"
        String                  = "`e[36m"
        Type                    = "`e[37m"
        Variable                = "`e[92m"
        Default                 = $fgDefault
    }

    # function Get-WindowsAppTheme() {
    #     return (
    #         Get-ItemProperty -Path `
    #           "HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\Themes\Personalize"
    #     ).AppsUseLightTheme;
    # }

    # function Set-WTTheme($isLight) {
    #     $bg = if ($isLight) {'White'} else {'Black'};
    #     $fg = if ($isLight) {'Black'} else {'White'};

    #     $fg= "$([char]27)[0m"

    #     Set-PSReadLineOption -Colors @{
    #         Command            = $fg
    #         Number             = $fg
    #         Member             = $fg
    #         Operator           = $fg
    #         Type               = $fg
    #         Variable           = $fg
    #         Parameter          = $fg
    #         ContinuationPrompt = $fg
    #         Default            = $fg
    #     }
    # }

    # Set-WTTheme(Get-WindowsAppTheme);

    # Set-PSReadLineKeyHandler -Chord Ctrl+Shift+I -ScriptBlock {
    #     Set-WTTheme(Get-WindowsAppTheme);
    # }
}

# prompt
# ----------------------------------------------------------------------

if ($env:WT_SESSION) {
    function Prompt {
        $hasErr = $?
        $realLastExitCode = $global:LASTEXITCODE

        # see https://superuser.com/a/1259916
        $fgGreen = "$([char]27)[32m"
        $fgRed = "$([char]27)[31m"
        $fgCyan = "$([char]27)[36m"
        $fgMagenta = "$([char]27)[35m"
        $fgYellow = "$([char]27)[33m"
        $fgDefault = "$([char]27)[0m"

        $err = ""
        if (!$hasErr) {
            if ($realLastExitCode -ne 0) {
                $err = $fgRed + $realLastExitCode + " " + $fgDefault
            } else {
                $err = $fgRed + "E " + $fgDefault
            }
        }

        $global:LASTEXITCODE = $realLastExitCode
        Remove-Variable realLastExitCode

        $p = Split-Path -Leaf -Path (Get-Location)
        $host.UI.RawUI.WindowTitle = $p

        $LastCmd = Get-History -Count 1
        $timeLastCmd = ""

        if ($LastCmd) {
            $time = [math]::Round(
                $LastCmd.EndExecutionTime.Subtract(
                    $LastCmd.StartExecutionTime
                ).TotalSeconds,
                2
            )
            if ($time -gt 1) {
                $timeLastCmd = "[" + $time + "s] " + $fgDefault    
            }
        }

        $err + $timeLastCmd + `
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

# aliases
# ----------------------------------------------------------------------

Function la { Get-ChildItem -Hidden $args }

Set-Alias -Name sysjava -Value "$env:JAVA_HOME\bin\java.exe"

Set-Alias -Name em -Value emacsclientw.exe
Set-Alias -Name lg -Value lazygit

Set-Alias -Name gfind -Value "C:\tools\msys64\usr\bin\find.exe"

Function time {
    Measure-Command { "$args" | Out-Default }
}

Function Restart-ScheduledTask {
    Param ([parameter(Position=0)][String]$task)

    Stop-ScheduledTask $task
    Start-ScheduledTask $task
}

# visual studio build tools
# ----------------------------------------------------------------------

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

Function vcvars2022x64 {
    Load-EnvFromBat -BatFile "c:\Program Files (x86)\Microsoft Visual Studio\2022\BuildTools\VC\Auxiliary\Build\vcvars64.bat"
    $env:my__loaded_environment = "vs2019x64 "
    Write-Host "Environment for 'x64 Native Tools Command Prompt for VS 2019'"
}