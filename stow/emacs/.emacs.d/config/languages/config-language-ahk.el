
(use-package ahk-mode :ensure t :defer t
  :config
  (defun fp/reload-ahk ()
    (interactive)
    (shell-command "tskill AutoHotkey")
    (w32-shell-execute "open" (buffer-file-name))
    (message "Restarted AutoHotKey"))
  (evil-leader/set-key-for-mode 'ahk-mode "eb" 'fp/reload-ahk))

(provide 'config-language-ahk)

