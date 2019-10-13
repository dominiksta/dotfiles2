(config-add-external-dependency 'gpg 'config-epass "encryption"
                                (lambda () (executable-find "gpg"))
                                "sudo apt install gpg" "cinst -y gpg4win-vanilla")

(epass-set-store-location (concat sync-directory "general/.password-store/"))

;; So apparently this is a bug where the change only applies when you set this
;; through custom-set-variables. Also on windows it just does not work with the
;; default gpg2, there is probably some reason for that.
(custom-set-variables '(epg-gpg-program  "gpg"))

(evil-define-key 'normal dired-mode-map "P" 'epass-clipboard-dired-silent)
(evil-define-key 'normal epa-key-list-mode-map
  "m" 'epa-mark-key
  "u" 'epa-unmark-key
  "cc" 'exit-recursive-edit)
(evil-leader/set-key-for-mode 'epass-edit-mode
  "mp" 'epass-edit-hide-show-password
  "mc" 'epass-clipboard-current-file)

(provide 'config-epass)
