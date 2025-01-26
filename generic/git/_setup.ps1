git config --global core.autocrlf false

$gpg_program = (Resolve-Path (get-command gpg).Source).Path.Replace('\', '/')
git config --global gpg.program $gpg_program
