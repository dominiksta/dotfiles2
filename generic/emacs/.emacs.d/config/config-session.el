(defvar fp/running-on-wsl-p
  (and (not (eq system-type 'windows-nt))
       (string-match-p "microsoft" (get-string-from-file "/proc/version")))
  "Wether we are running on WSL (Windows Subsystem for Linux) or not")

(defvar fp/powershell-executable
  (concat (if fp/running-on-wsl-p "/mnt/c/" "c:/")
          "Windows/System32/WindowsPowerShell/v1.0/powershell.exe"))

(defun fp/powershell-command (cmd)
  (let ((run (concat fp/powershell-executable " -Command "
                     (shell-quote-argument cmd))))
    (message run)
    (shell-command run)))

(setq frame-title-format (if fp/running-on-wsl-p "emacs@wsl" "emacs@native"))


(save-place-mode 1)

;; --- startup ---
(setq inhibit-startup-screen t
      initial-buffer-choice nil
      initial-scratch-message nil
      initial-major-mode 'org-mode)

;; --- server ---
(server-start)

;; --- restart ---
(straight-use-package 'restart-emacs)

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
