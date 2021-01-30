(require-and-log 'config-programming-general)

(straight-use-package 'ahk-mode)

(with-eval-after-load "ahk-mode"
  (defun fp/reload-ahk ()
    (interactive)
    (shell-command "tskill AutoHotkey")
    (w32-shell-execute "open" (buffer-file-name))
    (message "Restarted AutoHotKey"))
  (evil-leader/set-key-for-mode 'ahk-mode "eb" 'fp/reload-ahk))

(provide 'config-language-ahk)

