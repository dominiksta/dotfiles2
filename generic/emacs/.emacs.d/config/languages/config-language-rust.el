(require-and-log 'config-programming-general)

(straight-use-package 'rust-mode)
(require 'rust-mode)

(setq rust-format-on-save t
      lsp-rust-analyzer-server-display-inlay-hints t
      )

(remove-hook 'rust-mode-hook 'lsp-rust-analyzer-inlay-hints-mode)

;; https://rust-analyzer.github.io/manual.html#rust-analyzer-language-server-binary
(add-hook 'rust-mode-hook #'lsp-deferred)

(provide 'config-language-rust)