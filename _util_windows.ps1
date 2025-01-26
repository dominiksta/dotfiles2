function New-Symlink {
    param(
        [string]$Target,
        [string]$Destination
    )

    $exists = Test-Path $Destination
    if ($exists) {
        Write-Output "  [LN]: Skipping $Target"
        return
    }
    New-Item -Path $Destination -ItemType SymbolicLink -Value $Target | Out-Null
    Write-Output "  [LN]: $Destination -> $Target"
}

function Remove-Symlink {
    param(
        [string]$LinkFile
    )
    $exists = Test-Path $LinkFile
    if (!$exists) {
        Write-Output "  [RM]: Skipping $LinkFile"
        return
    }
    $file = Get-Item $LinkFile
    $is_symlink = [bool]($file.Attributes -band [IO.FileAttributes]::ReparsePoint)
    if (!$is_symlink) {
        Write-Output "  [RM]: WARNING: NOT A SYMLINK $LinkFile"
    }
    Remove-Item $file | Out-Null
    Write-Output "  [RM]: $LinkFile"
}