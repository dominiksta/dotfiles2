(require-and-log 'config-programming-general)

(use-package lua-mode :ensure t :config
  (setq lua-indent-nested-block-content-align nil))

(provide 'config-language-lua)