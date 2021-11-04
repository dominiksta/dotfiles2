(require-and-log 'config-language-web-general)

;; ----------------------------------------------------------------------
;; lsp
;; ----------------------------------------------------------------------

;; (straight-use-package 'php-mode) (require 'php-mode)

;; (defun fp/php-mode-hook ()
;;   (interactive)
;;   (fp/toggle-show-too-long-lines)
;;   (setq-local lsp-enable-indentation nil)
;;   (lsp))

;; (add-hook 'php-mode-hook 'fp/php-mode-hook)

(defun fp/web-mode-php-hook ()
  (when (string= web-mode-engine "php")
    (fp/toggle-show-too-long-lines)
    (lsp)))

(add-hook 'web-mode-hook 'fp/web-mode-php-hook)

;; ----------------------------------------------------------------------
;; Binds
;; ----------------------------------------------------------------------
(evil-leader/set-key-for-mode 'php-mode
  "md" 'lsp-ui-doc-show
  "mD" 'lsp-ui-doc-hide)

(provide 'config-language-web-php)
