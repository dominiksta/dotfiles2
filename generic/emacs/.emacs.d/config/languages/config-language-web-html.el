(require-and-log 'config-language-web-general)

(add-hook 'sgml-mode-hook 'sgml-electric-tag-pair-mode)
(setq sgml-basic-offset 4)

;; (add-hook 'sgml-mode-hook 'highlight-indent-guides-mode)

(provide 'config-language-web-html)
