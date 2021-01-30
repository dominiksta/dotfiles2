(require-and-log 'config-language-web-general)

;; lsp currently sucks for js/ts (completions are very slow). tide is
;; still much better

(config-add-external-dependency 'npm 'config-language-web-ts "tide"
                                (lambda () (executable-find "npm"))
                                "apt install npm" "None")

(straight-use-package 'tide) (require 'tide)

(defun fp/tide-mode-hook ()
  (setq-local flycheck-check-syntax-automatically
              '(save idle-change new-line mode-enabled))

  (defun fp/tide-jump-to-definition ()
    (interactive)
    (evil-set-jump)
    (tide-jump-to-definition))

  (company-mode 1)
  (flycheck-mode 1)
  (setq-local company-tooltip-align-annotations t))
(add-hook 'tide-mode-hook 'fp/tide-mode-hook)

(evil-define-key 'normal tide-mode-map
  "gd" 'fp/tide-jump-to-definition)

(straight-use-package 'js2-mode)

(flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
(add-hook 'js2-mode-hook 'tide-setup)

(evil-leader/set-key-for-mode 'js2-mode
  "md" 'tide-documentation-at-point)

(provide 'config-language-web-js)
