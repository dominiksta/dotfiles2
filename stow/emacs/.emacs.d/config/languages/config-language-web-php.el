(require-and-log 'config-language-web-general)

(straight-use-package 'php-mode) (require 'php-mode)
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
(evil-leader/set-key-for-mode 'php-mode
  "md" 'lsp-ui-doc-show
  "mD" 'lsp-ui-doc-hide)

(provide 'config-language-web-php)
