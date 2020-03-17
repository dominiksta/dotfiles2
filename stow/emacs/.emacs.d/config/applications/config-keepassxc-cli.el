(require-and-log 'keepassxc-cli)

(config-add-external-dependency
 'keepassxc-cli 'config-keepassxc-cli "access passwords"
 (lambda () (executable-find "keepassxc-cli"))
 "sudo apt install keepassxc" "None")

(setq auth-sources '("~/.authinfo.gpg")
      kcli-std-database-file (concat sync-directory "general/passwords.kdbx"))

(evil-leader/set-key "ak" 'kcli-interactive-clip-password)

(provide 'config-keepassxc-cli)

