(require-and-log 'config-language-web-css)

(straight-use-package 'sws-mode)
(straight-use-package 'stylus-mode)

(add-hook 'stylus-mode-hook 'company-mode)
(add-hook 'stylus-mode-hook 'highlight-indent-guides-mode)

(provide 'config-language-web-stylus)