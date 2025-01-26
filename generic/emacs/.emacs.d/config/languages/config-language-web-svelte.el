(require-and-log 'config-language-web-general)

(define-derived-mode web-svelte-mode web-mode "WebSvelte"
  "Web mode for svelte"
  ;; (setq-local lsp-enable-indentation t)
  ;; (add-hook 'before-save-hook 'lsp-format-buffer nil t)
  (setq-local
   web-mode-code-indent-offset 2
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-style-padding 2
   web-mode-script-padding 2)
  ;; (lsp)
  )

(provide 'config-language-web-svelte)
