(require 'config-programming-general)

(add-hook 'emacs-lisp-mode-hook 'company-mode)
(add-hook 'emacs-lisp-mode-hook (lambda () (add-hook 'before-save-hook 'check-parens nil t)))
(add-hook 'ielm-mode-hook 'company-mode)

;; --------------------------------------------------------------------------------
;; fancy appearance
;; --------------------------------------------------------------------------------
(defun config--emacs-lisp-pretty-symbols ()
  (mapc (lambda (pair) (push pair prettify-symbols-alist))
        '(("and" . #x2227)
          ("or"  . #x2228)))
  (prettify-symbols-mode 1))

(add-hook 'emacs-lisp-mode-hook 'config--emacs-lisp-pretty-symbols)

(provide 'config-language-elisp)
