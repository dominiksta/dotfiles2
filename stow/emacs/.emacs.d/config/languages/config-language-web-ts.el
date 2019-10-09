(require-and-log 'config-language-web-general)
(require-and-log 'config-language-web-js)

(use-package typescript-mode
  :ensure t
  :config
  (add-hook 'typescript-mode-hook 'tide-setup)

  (evil-leader/set-key-for-mode 'typescript-mode
    "md" 'tide-documentation-at-point))

(provide 'config-language-web-ts)
