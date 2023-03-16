(require 'config-programming-general)

(add-hook 'emacs-lisp-mode-hook 'company-mode)
(add-hook 'emacs-lisp-mode-hook (lambda () (add-hook 'before-save-hook 'check-parens nil t)))
(add-hook 'ielm-mode-hook 'company-mode)

;; --- fancy appearance ---
(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)

(provide 'config-language-elisp)
