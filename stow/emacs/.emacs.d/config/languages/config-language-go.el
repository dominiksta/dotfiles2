(require-and-log 'config-programming-general)

(use-package go-mode :ensure t :config
  (add-hook 'go-mode-hook
            (lambda () (add-hook 'before-save-hook 'gofmt-before-save nil t)))
  (evil-define-key 'normal go-mode "gd" 'godef-jump)
  (evil-leader/set-key-for-mode 'go-mode "md" 'godoc-at-point))

(provide 'config-language-go)
