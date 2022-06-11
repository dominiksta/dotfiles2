(require-and-log 'config-language-web-css)

(straight-use-package 'sws-mode)
(straight-use-package 'stylus-mode)

(add-hook 'stylus-mode-hook 'company-mode)

(provide 'config-language-web-stylus)