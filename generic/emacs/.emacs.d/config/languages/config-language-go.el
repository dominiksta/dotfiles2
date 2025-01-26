(require-and-log 'config-programming-general)

(straight-use-package 'go-mode)

(evil-leader/set-key-for-mode 'go-mode
  "md" 'lsp-ui-doc-glance
  "mD" 'godoc-at-point)

(provide 'config-language-go)
