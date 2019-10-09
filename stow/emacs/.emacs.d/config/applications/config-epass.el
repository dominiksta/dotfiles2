(config-add-external-dependency 'gpg 'config-epass "encryption"
                                (lambda () (executable-find "gpg"))
                                "sudo apt install gpg" "cinst -y gpg4win-vanilla")

(epass-set-store-location "~/sync/general/.password-store/")

(evil-define-key 'normal dired-mode-map "P" 'epass-clipboard-dired-silent)
(evil-define-key 'normal epa-key-list-mode-map
  "m" 'epa-mark-key
  "u" 'epa-unmark-key
  "cc" 'exit-recursive-edit)
(evil-leader/set-key-for-mode 'epass-edit-mode
  "mp" 'epass-edit-hide-show-password
  "mc" 'epass-clipboard-current-file)

(provide 'config-epass)
