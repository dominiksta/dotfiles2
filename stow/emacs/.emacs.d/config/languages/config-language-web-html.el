(require-and-log 'config-language-web-general)

(add-hook 'sgml-mode-hook 'sgml-electric-tag-pair-mode)
(setq sgml-basic-offset 4)

;; (add-hook 'sgml-mode-hook 'highlight-indent-guides-mode)

(straight-use-package 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)
(evil-define-key 'insert emmet-mode-keymap (kbd "C-,") 'emmet-expand-yas)

(provide 'config-language-web-html)
