
;; --- startup ---
(setq inhibit-startup-screen t
      initial-buffer-choice nil
      initial-scratch-message nil
      initial-major-mode 'org-mode)

;; --- server ---
(server-start)

;; --- restart ---
(use-package restart-emacs
  :ensure t
  :commands restart-emacs)

;; --- kill ---
(setq confirm-kill-emacs 'y-or-n-p)

(defun fp/save-some-buffers-and-shutdown ()
  (interactive)
  (save-some-buffers)
  (if (yes-or-no-p "Really SHUTDOWN?")
      (if (eq system-type 'windows-nt)
          (shell-command "shutdown -s -t 1")
        (shell-command "shutdown now"))))


(provide 'config-session)
