(require-and-log 'config-programming-general)


(defun fp/sh-mode-hook ()
  (add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p
            nil t)
  (company-mode 1)
  (flycheck-mode 1))

(add-hook 'sh-mode-hook 'fp/sh-mode-hook)

(provide 'config-language-shell-script)
