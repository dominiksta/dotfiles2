
if not exist %USERPROFILE%\.emacs.d md %USERPROFILE%\.emacs.d
mklink /d %USERPROFILE%\.emacs.d\config %USERPROFILE%\git\dotfiles\stow\emacs\.emacs.d\config
mklink %USERPROFILE%\.emacs.d\init.el %USERPROFILE%\git\dotfiles\stow\emacs\.emacs.d\init.el
