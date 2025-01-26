. "../../_util_windows.ps1"

mkdir -Force ~/.emacs.d/straight | Out-Null

New-Symlink -Target $PWD\.emacs.d\config ~\.emacs.d\config
New-Symlink -Target $PWD\.emacs.d\straight/versions ~\.emacs.d\straight/versions
New-Symlink -Target $PWD\.emacs.d\init.el ~\.emacs.d\init.el

Write-Warning "Please manually set up the file type associations"

<#
cmd /c ftype emacsclient=emacsclientw.exe "%1"

cmd /c assoc .ps1=emacsclient
cmd /c assoc .sql=emacsclient
cmd /c assoc .c=emacsclient
cmd /c assoc .h=emacsclient
cmd /c assoc .cpp=emacsclient
cmd /c assoc .hpp=emacsclient
cmd /c assoc .txt=emacsclient
cmd /c assoc .js=emacsclient
cmd /c assoc .ts=emacsclient
cmd /c assoc .json=emacsclient
cmd /c assoc .py=emacsclient
cmd /c assoc .md=emacsclient
cmd /c assoc .java=emacsclient
#>