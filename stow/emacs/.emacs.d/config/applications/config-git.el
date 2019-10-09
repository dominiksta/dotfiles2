;; --- magit ---
(require-and-log 'config-editor)

(use-package magit
  :ensure t
  :defer t
  :config
  (define-key magit-status-mode-map (kbd "M-1") nil)
  (define-key magit-status-mode-map (kbd "M-2") nil)
  (define-key magit-status-mode-map (kbd "M-3") nil)
  (define-key magit-status-mode-map (kbd "M-4") nil)
  (use-package evil-magit
    :ensure t
    :config
    (evil-define-key 'normal magit-log-mode-map
      (kbd "M-j") 'magit-section-forward
      (kbd "M-k") 'magit-section-backward
      (kbd "C-j") nil
      (kbd "C-k") nil)
    (evil-define-key 'normal magit-status-mode-map
      (kbd "M-j") 'magit-section-forward
      (kbd "M-k") 'magit-section-backward
      (kbd "C-j") nil
      (kbd "C-k") nil)))


;; --- ediff ---
(with-eval-after-load "ediff"
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function (if (> (frame-width) 170) 'split-window-horizontally
                                      'split-window-vertically)))

(provide 'config-git)
