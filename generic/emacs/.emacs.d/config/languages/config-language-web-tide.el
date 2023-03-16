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

(setq tide-save-buffer-after-code-edit nil)

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
  "gd" 'fp/tide-jump-to-definition ;; TODO evil-add-command-properties
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

(straight-use-package 'typescript-mode)

(add-hook 'typescript-mode-hook 'tree-sitter-hl-mode)
(add-hook 'typescript-mode-hook 'tide-setup)
;; (add-hook 'typescript-mode-hook 'fp/toggle-show-too-long-lines)

;; (add-hook 'tide-mode-hook 'tide-hl-identifier-mode)

(evil-leader/set-key-for-mode 'typescript-mode
  "md" 'tide-documentation-at-point)
(define-key typescript-mode-map (kbd "M-R") 'tide-rename-symbol)

;; (define-derived-mode typescript-tsx-mode typescript-mode "TSX"
;;   "Major mode for editing TSX files.")

;; (tree-sitter-require 'tsx)
;; (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx))

;; (setq typescript-indent-offset 2)

;; (straight-use-package '(tsi :type git :host github :repo "orzechowskid/tsi.el"))

;; ----------------------------------------------------------------------
;; experimental tsx-mode
;; ----------------------------------------------------------------------
;; (straight-use-package '(tsi :type git :host github :repo "orzechowskid/tsi.el"))
;; (straight-use-package '(tsx-mode :type git :host github :repo "orzechowskid/tsx-mode.el"))

;; ----------------------------------------------------------------------
;; jsx-mode
;; ----------------------------------------------------------------------

(require 'js)
(add-to-list 'tree-sitter-major-mode-language-alist '(js-jsx-mode . tsx))

(defun fp/js-jsx-mode-setup ()
  (tide-setup)
  (emmet-mode)
  (tree-sitter-mode 1)
  (tree-sitter-hl-mode 1))

(add-hook 'js-jsx-mode-hook 'fp/js-jsx-mode-setup)

(define-key js-jsx-mode-map (kbd "M-R") 'tide-rename-symbol)

(evil-leader/set-key-for-mode 'js-jsx-mode
  "mo" 'tide-organize-imports
  "md" 'tide-documentation-at-point)

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
  ;; (setq-local indent-line-function (lambda () (tide-format-region
  ;;                                         (point-at-bol) (point-at-eol))))
  (tide-setup))

(add-to-list 'tree-sitter-major-mode-language-alist '(web-tide-mode . tsx))

(flycheck-add-mode 'javascript-tide 'web-tide-mode)

(define-key web-tide-mode-map (kbd "M-R") 'tide-rename-symbol)
(evil-define-key '(normal visual) web-tide-mode-map
  "=" 'evil-tide-format)

(evil-leader/set-key-for-mode 'web-tide-mode
  "mo" 'tide-organize-imports
  "md" 'tide-documentation-at-point)

;; This has to be set again here despite already being configured in
;; init-default.el because tide requires typescript-mode, which inserts itself
;; at the top of auto-mode alist.
(add-to-list 'auto-mode-alist '("\\.\\(tsx?\\)\\'" . web-tide-mode))

;; ----------------------------------------------------------------------
;; prettier
;; ----------------------------------------------------------------------

;; requires 'diff' and 'prettier' to be installed (npm i -g prettier)
(straight-use-package 'prettier-js)

(setq-default fp/enable-prettier-js nil)
(defun fp/maybe-enable-prettier-js ()
  (run-at-time 1 nil (lambda () (when fp/enable-prettier-js
                             (prettier-js-mode 1)))))
(add-hook 'web-tide-mode-hook 'fp/maybe-enable-prettier-js)
(add-hook 'js-jsx-mode-hook 'fp/maybe-enable-prettier-js)


(provide 'config-language-web-tide)
