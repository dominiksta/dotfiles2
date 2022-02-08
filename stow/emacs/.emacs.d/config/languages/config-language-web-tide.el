(require-and-log 'config-language-web-general)

;; ----------------------------------------------------------------------
;; tide
;; ----------------------------------------------------------------------
;; lsp currently sucks for js/ts (completions are very slow). tide is
;; still much better

(config-add-external-dependency 'npm 'config-language-web-tide "tide"
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
  "gd" 'fp/tide-jump-to-definition
  "gf" 'tide-references)

(evil-define-key 'normal tide-references-mode-map
  "q" 'quit-window
  (kbd "RET") 'tide-goto-line-reference
  "a" (lambda () (interactive)
        (tide-goto-line-reference)
        (pulse-momentary-highlight-one-line (point))
        (select-window (previous-window))))

(flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)

;; ----------------------------------------------------------------------
;; (old) typescript-mode
;; ----------------------------------------------------------------------

;; (straight-use-package 'typescript-mode)

;; (add-hook 'typescript-mode-hook 'tide-setup)
;; (add-hook 'typescript-mode-hook 'fp/toggle-show-too-long-lines)

;; (add-hook 'tide-mode-hook 'tide-hl-identifier-mode)

;; (evil-leader/set-key-for-mode 'typescript-mode
;;   "md" 'tide-documentation-at-point)
;; (define-key typescript-mode-map (kbd "M-R") 'tide-rename-symbol)

;; ----------------------------------------------------------------------
;; web-tide-mode
;; ----------------------------------------------------------------------

(evil-define-operator evil-tide-format (beg end)
  "Format text with tide. See `evil-indent' for reference."
  :move-point nil
  :type line
  ;; these two movements mimic the behaviour of `evil-indent`. not sure if they
  ;; are useful, but consistency is always nice
  (goto-char beg)
  (evil-first-non-blank)
  (tide-format-region beg end))

(define-derived-mode web-tide-mode web-mode "WebTide"
  "Web mode with tide"
  (tide-setup))

(flycheck-add-mode 'javascript-tide 'web-tide-mode)

(evil-define-key '(normal visual) web-tide-mode-map
  "=" 'evil-tide-format)

;; This has to be set again here despite already being configured in
;; init-default.el because tide requires typescript-mode, which inserts itself
;; at the top of auto-mode alist.
(add-to-list 'auto-mode-alist '("\\.\\(tsx?\\)\\|\\(jsx?\\)\\'" . web-tide-mode))

(provide 'config-language-web-tide)
