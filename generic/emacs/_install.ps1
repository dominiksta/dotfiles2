scoop bucket add extras
scoop install emacs

[Environment]::SetEnvironmentVariable(
    'HOME', $env:Userprofile, [EnvironmentVariableTarget]::User
)