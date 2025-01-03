;; --- magit ---
(require-and-log 'config-editor)
(require-and-log 'config-programming-general)

(straight-use-package 'magit)
(straight-use-package 'evil-magit)

(defun fp/run-fork (directory)
  (start-process
   "git-fork" nil
   (format "c:/Users/%s/AppData/Local/Fork/Fork.exe" user-login-name)
   directory))

(defun fp/run-fork-projectile ()
  (interactive)
  (fp/run-fork (projectile-project-root)))

(with-eval-after-load "magit"
  (require 'evil-magit)
  (require 'mvtn)

  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

  (add-hook 'magit-process-find-password-functions
            'magit-process-password-auth-source)

  (define-key magit-status-mode-map (kbd "M-1") nil)
  (define-key magit-status-mode-map (kbd "M-2") nil)
  (define-key magit-status-mode-map (kbd "M-3") nil)
  (define-key magit-status-mode-map (kbd "M-4") nil)

  (dolist (mode (list magit-log-mode-map magit-diff-mode-map))
    (evil-define-key 'normal mode
      (kbd "M-j") 'magit-section-forward
      (kbd "M-k") 'magit-section-backward
      (kbd "C-j") nil
      (kbd "C-k") nil))
  
  (evil-define-key 'normal magit-status-mode-map
    "Z" 'magit-stash
    "gP" 'magit-process-buffer
    (kbd "M-j") 'magit-section-forward
    (kbd "M-k") 'magit-section-backward
    (kbd "C-j") nil
    (kbd "C-k") nil))


;; --- ediff ---
(with-eval-after-load "ediff"
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function (if (> (frame-width) 170) 'split-window-horizontally
                                      'split-window-vertically)))

(provide 'config-git)
