param(
    [string]$Action,
    [string]$Target = "__all__"
)

Write-Output "[INFO]: Running $Action on $Target"

$script_name = switch($Action) {
    "setup" { "_setup.ps1" }
    "teardown" { "_teardown.ps1" }
    "install" { "_install.ps1" }
    "uninstall" { "_uninstall.ps1" }
    default { throw "Invalid action: $Action" }
}

$script_files = switch($Target) {
    "__all__" {
        Get-ChildItem -Path . -Filter $script_name -Recurse -ErrorAction SilentlyContinue -Force
    }
    default {
        $exists = Test-Path $Target -PathType Container
        if (!$exists) { throw "Could not find target directory: $Target" }
        Get-ChildItem -Path $Target -Filter $script_name -Recurse -ErrorAction SilentlyContinue -Force
    }
}

foreach ($script_file in $script_files) {
    $name = $script_file.FullName
    Write-Output "[INFO]: Running $name"
    Write-Output "[INFO]: ----------------------------------------------------------------------"
    Push-Location .
    cd $script_file.DirectoryName
    try {
        & $name
    } finally {
        Pop-Location
    }
}