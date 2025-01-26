(require 'config-programming-general)

(add-hook 'emacs-lisp-mode-hook 'company-mode)
(add-hook 'emacs-lisp-mode-hook (lambda () (add-hook 'before-save-hook 'check-parens nil t)))
(add-hook 'ielm-mode-hook 'company-mode)

(provide 'config-language-elisp)
