(require-and-log 'config-language-web-general)

(use-package php-mode :ensure t
  :config
  ;; ----------------------------------------------------------------------
  ;; lsp
  ;; ----------------------------------------------------------------------
  (defun fp/php-mode-hook ()
    (interactive)
    (fp/toggle-show-too-long-lines)
    (setq-local lsp-enable-indentation nil)
    (lsp))
  (add-hook 'php-mode-hook 'fp/php-mode-hook)

  ;; ----------------------------------------------------------------------
  ;; Binds
  ;; ----------------------------------------------------------------------
  (evil-define-key 'normal php-mode-map "gd" 'fp/evil-dumb-jump-go)
  (evil-leader/set-key-for-mode 'php-mode
    "md" 'lsp-ui-doc-show
    "mD" 'lsp-ui-doc-hide))

(provide 'config-language-web-php)
