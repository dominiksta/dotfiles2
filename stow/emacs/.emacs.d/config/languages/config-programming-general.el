(require-and-log 'config-search)
(require-and-log 'config-helm-minibuffer)

;; --------------------------------------------------------------------------------
;; random
;; --------------------------------------------------------------------------------
;; --- indentation ---
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(defun fp/indent-all()
  (interactive)
  (indent-region (point-min) (point-max)))

(use-package highlight-indent-guides
  :init (setq highlight-indent-guides-method 'character)
  :ensure t :defer t)

;; --- parens ---
(setq show-paren-delay 0) (show-paren-mode 1)
(add-hook 'prog-mode-hook 'electric-pair-mode)

;; --- todos ---
(use-package hl-todo
  :demand t
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'hl-todo-mode))

(setq project-todo-regexp "TODO|FIXME|NEXT|DONE|HACK")
(defun project-todo-search ()
  (interactive)
  (cond
   (config-ag-available (ag-project-regexp project-todo-regexp))
   (config-grep-available (let ((default-directory (projectile-project-root)))
                            (grep (concat "grep --exclude-dir=.git -nHIrE \""
                                          project-todo-regexp "\" ." ))))
   (t (message "No program for searching available"))))

(evil-leader/set-key "st" 'project-todo-search)

;; --- imenu ---
(evil-leader/set-key
  "hi" 'helm-imenu
  "hI" 'helm-imenu-in-all-buffers)

(use-package imenu-anywhere :ensure t :init
  (evil-leader/set-key "pi" 'helm-imenu-anywhere))

(use-package editorconfig :ensure t :demand t
  :config (editorconfig-mode 1))

;; --------------------------------------------------------------------------------
;; company
;; --------------------------------------------------------------------------------
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 3)
  ;; (use-package company-quickhelp
  ;;   :ensure t
  ;;   :config
  ;;   (setq company-quickhelp-max-lines 10
  ;;         company-quickhelp-use-propertized-text t))
  ;; (company-quickhelp-mode 1)
  :bind (:map company-active-map
              ("C-j" . company-select-next)
              ("C-k" . company-select-previous)
              ("M-j" . company-select-next)
              ("M-k" . company-select-previous)
              ("M-l" . company-complete-common)
              ("C-SPC" . company-abort)))

;; --------------------------------------------------------------------------------
;; dumb-jump - jump to definition
;; --------------------------------------------------------------------------------
(use-package dumb-jump :ensure t
  :init (dumb-jump-mode 1)
  :config (setq dumb-jump-selector 'helm))

;; --------------------------------------------------------------------------------
;; debugging
;; --------------------------------------------------------------------------------

(use-package realgud
  :ensure t
  :defer t
  :config
  (evil-define-key 'normal realgud:shortkey-mode-map
    "n" 'realgud:cmd-next
    "s" 'realgud:cmd-step
    "f" 'realgud:cmd-finish
    "c" 'realgud:cmd-continue
    "b" 'realgud:cmd-break

    "r" 'realgud:cmd-restart

    "1" 'realgud-goto-arrow1
    "2" 'realgud-goto-arrow2
    "3" 'realgud-goto-arrow3
    "4" 'realgud:goto-loc-hist-4
    "5" 'realgud:goto-loc-hist-5
    "6" 'realgud:goto-loc-hist-6
    "7" 'realgud:goto-loc-hist-7
    "8" 'realgud:goto-loc-hist-8
    "9" 'realgud:goto-loc-hist-9))

;; --------------------------------------------------------------------------------
;; flycheck
;; --------------------------------------------------------------------------------
(use-package flycheck
  :defer 5
  :ensure t
  :config
  ;; (custom-set-faces '(flycheck-error ((t (:background nil :underline "red")))))
  (setq flycheck-highlighting-mode 'symbols
        ;; flycheck-indication-mode nil
        ;; only check when saving or loading file
        ;; flycheck-check-syntax-automatically '(save mode-enabled)
        )
  (define-key prog-mode-map (kbd "C-M-n") 'flycheck-next-error)
  (define-key prog-mode-map (kbd "C-M-p") 'flycheck-previous-error))

;; --------------------------------------------------------------------------------
;; compilation
;; --------------------------------------------------------------------------------
(with-eval-after-load "compile"
  ;; --- color compilation buffers ---
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (save-excursion
        (ansi-color-apply-on-region compilation-filter-start (point)))))
  (ansi-color-for-comint-mode-on)
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer)


  ;; --- bindings ---
  (evil-set-initial-state 'compilation-mode 'normal)
  (evil-define-key 'normal compilation-mode-map
    "gj" 'compilation-next-file
    "gk" 'compilation-previous-file
    "a" 'compilation-display-error
    "f" 'next-error-follow-minor-mode
    "n" 'next-error
    "p" 'previous-error
    "q" 'delete-window
    "r" 'recompile)
  (define-key compilation-mode-map (kbd "g") nil)
  (evil-leader/set-key "se" (defhydra project-todo-hydra ()
                              ("j" next-error "search")
                              ("k" previous-error "search")
                              ("q" nil "quit" :color blue))))


;; --------------------------------------------------------------------------------
;; project management
;; --------------------------------------------------------------------------------
(use-package projectile
  :ensure t
  :init (projectile-mode 1)
  :config
  ;; dont display anything in modeline, since this can slow down tramp
  (setq projectile-dynamic-mode-line nil
        projectile-mode-line-prefix " proj"
        projectile-mode-line " proj"
        ;; on windows it's native which is way to slow for me
        projectile-indexing-method 'alien)
  (use-package helm-projectile
    :ensure t
    :after helm projectile))


;; --------------------------------------------------------------------------------
;; snippets
;; --------------------------------------------------------------------------------
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  (global-set-key (kbd "C-.") 'yas-expand)
  (use-package yasnippet-snippets :ensure t))


;; --------------------------------------------------------------------------------
;; lsp
;; --------------------------------------------------------------------------------
(use-package lsp-mode
  :defer t
  :commands lsp
  :ensure t
  :config
  (remove-hook 'lsp-eldoc-hook 'lsp-document-highlight)
  (setq lsp-enable-indentation nil)
  ;; i only use this to make lsp use flycheck instead of flymake
  (use-package lsp-ui
    :ensure t
    :config
    (require 'lsp-ui-flycheck)
    (setq lsp-ui-doc-include-signature t
          lsp-ui-doc-enable nil
          lsp-enable-symbol-highlighting t
          lsp-ui-peek-enable nil
          lsp-ui-flycheck-enable t
          lsp-prefer-flymake nil
          lsp-ui-imenu-enable t
          lsp-ui-sideline-enable nil
          read-process-output-max 1048576
          lsp-prefer-capf t
          lsp-eldoc-render-all nil)
    ;; The lsp-prefer-capf variable did not work anymore and probably another company backend
    ;; was the source of the weird flycheck/make errors which arised with any lsp-backend for c++
    ;; This fixes the issue by changing the company backend to the one recommended by the lsp team
    (add-hook 'lsp-managed-mode-hook (lambda () (setq-local company-backends '(company-capf))))
    (set-face-background 'lsp-ui-doc-background nil)))


;; --------------------------------------------------------------------------------
;; regexps
;; --------------------------------------------------------------------------------
(use-package pcre2el
  :ensure t
  :config
  (setq reb-re-syntax 'pcre)
  (setq reb-auto-match-limit 500))

(provide 'config-programming-general)
