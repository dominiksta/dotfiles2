(require-and-log 'config-language-web-general)
(require-and-log 'config-language-web-js)

(use-package typescript-mode
  :ensure t
  :config
  (add-hook 'typescript-mode-hook 'tide-setup)
  (add-hook 'typescript-mode-hook 'fp/toggle-show-too-long-lines)
  (add-hook 'tide-mode-hook 'tide-hl-identifier-mode)

  (evil-leader/set-key-for-mode 'typescript-mode
    "md" 'tide-documentation-at-point)
  (define-key typescript-mode-map (kbd "M-R") 'tide-rename-symbol))

(provide 'config-language-web-ts)
