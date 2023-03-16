(require-and-log 'config-programming-general)

(straight-use-package 'go-mode)

(add-hook 'go-mode-hook
          (lambda () (add-hook 'before-save-hook 'gofmt-before-save nil t)))

(evil-leader/set-key-for-mode 'go-mode
  "md" 'lsp-ui-doc-glance
  "mD" 'godoc-at-point)

(config-add-external-dependency
 'gopls 'config-language-go "lsp" (lambda () (executable-find "gopls"))
 "go get golang.org/x/tools/gopls@latest" "go get golang.org/x/tools/gopls@latest")

(when (config-external-check-list '(gopls))
  (add-hook 'go-mode-hook 'lsp))


(provide 'config-language-go)
