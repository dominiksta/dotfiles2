(require-and-log 'config-shell)

(straight-use-package 'vterm)
(evil-set-initial-state 'vterm-mode 'emacs)

(defun fp/vterm-enter-emacs-state ()
  (interactive)
  (evil-emacs-state)
  (vterm-reset-cursor-point))

(defun fp/vterm-mode-hook ()
  (setq-local evil-emacs-state-cursor 'hbar)

  (add-hook 'evil-insert-state-entry-hook 'fp/vterm-enter-emacs-state nil t))

(add-hook 'vterm-mode-hook 'fp/vterm-mode-hook)

(evil-define-key 'normal vterm-mode-map
  "i" 'fp/vterm-enter-emacs-state)

(define-key vterm-mode-map (kbd "<escape>")
            (lambda nil (interactive)
              (evil-exit-emacs-state) (evil-normal-state)))

(provide 'config-vterm)