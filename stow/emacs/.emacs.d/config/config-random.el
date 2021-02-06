(setq vc-follow-symlinks t)

;; --- help mode ---
(evil-set-initial-state 'help-mode 'normal)
(evil-define-key 'normal help-mode-map
  (kbd "q") 'quit-window
  (kbd "r") 'revert-buffer)


;; --- saving, auto-revert and backups ---
(global-auto-revert-mode 1)

(setq create-lockfiles nil)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;; --- man-mode ---
(evil-define-key 'normal global-map "K" 'man)
(evil-define-key 'motion Man-mode-map (kbd "<escape>") 'evil-force-normal-state)
(evil-define-key 'visual Man-mode-map (kbd "<escape>") 'evil-force-normal-state)

;; --- only use encrypted authinfo ---
(setq auth-sources '("~/sync/documents/code/emacs/.authinfo.gpg" "~/.authinfo.gpg"))


;; --- no more yes ---
(defalias 'yes-or-no-p 'y-or-n-p)

(provide 'config-random)
