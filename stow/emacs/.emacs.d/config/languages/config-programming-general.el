(require-and-log 'config-search)
(require-and-log 'config-helm-minibuffer)

;; --------------------------------------------------------------------------------
;; random
;; --------------------------------------------------------------------------------
;; --- indentation ---
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(defun fp/indent-all()
  (interactive)
  (indent-region (point-min) (point-max)))

(straight-use-package 'highlight-indent-guides)
(setq highlight-indent-guides-method 'character)

;; --- parens ---
(setq show-paren-delay 0) (show-paren-mode 1)
(add-hook 'prog-mode-hook 'electric-pair-mode)

;; --- todos ---
(straight-use-package 'hl-todo)
(add-hook 'prog-mode-hook 'hl-todo-mode)

(defun fp/project-search (regexp)
  (let ((default-directory (projectile-project-root)))
    (cond
     (config-ag-available (ag-project-regexp regexp))
     (config-grep-available (grep (concat "grep --exclude-dir=.git -nHIrE \""
                                          regexp "\" ." )))
     (t (message "No program for searching available")))))

(defun fp/project-todo-search-todo () (interactive) (fp/project-search "TODO|FIXME|NEXT"))
(defun fp/project-todo-search-hack () (interactive) (fp/project-search "HACK"))
(defun fp/project-todo-search-all  () (interactive) (fp/project-search "TODO|FIXME|NEXT|DONE|HACK"))

(evil-leader/set-key
  "stt" 'fp/project-todo-search-todo
  "sta" 'fp/project-todo-search-all
  "sth" 'fp/project-todo-search-hack)

;; --- imenu ---
(evil-leader/set-key
  "hi" 'helm-imenu
  "hI" 'helm-imenu-in-all-buffers)

(straight-use-package 'imenu-anywhere)
(evil-leader/set-key "pi" 'helm-imenu-anywhere)

(straight-use-package 'editorconfig)
(editorconfig-mode 1)

;; --------------------------------------------------------------------------------
;; company
;; --------------------------------------------------------------------------------
(straight-use-package 'company) (require 'company)
(setq company-idle-delay 0
      company-minimum-prefix-length 3)
(define-key company-mode-map (kbd "C-j") 'company-select-next)
(define-key company-mode-map (kbd "C-k") 'company-select-previous)
(define-key company-mode-map (kbd "M-j") 'company-select-next)
(define-key company-mode-map (kbd "M-k") 'company-select-previous)
(define-key company-mode-map (kbd "M-l") 'company-complete-common)


;; --------------------------------------------------------------------------------
;; dumb-jump - jump to definition
;; --------------------------------------------------------------------------------
(straight-use-package 'dumb-jump)
(with-eval-after-load "dumb-jump"
  (setq dumb-jump-selector 'helm))
(defun fp/evil-dumb-jump-go ()
  (interactive)
  (evil-set-jump)
  (call-interactively 'dumb-jump-go))


;; --------------------------------------------------------------------------------
;; debugging
;; --------------------------------------------------------------------------------
(straight-use-package 'realgud)
(with-eval-after-load "realgud"
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
(straight-use-package 'flycheck)
(setq flycheck-highlighting-mode 'symbols
      ;; flycheck-indication-mode nil
      ;; only check when saving or loading file
      ;; flycheck-check-syntax-automatically '(save mode-enabled)
      )
(define-key prog-mode-map (kbd "C-M-n") 'flycheck-next-error)
(define-key prog-mode-map (kbd "C-M-p") 'flycheck-previous-error)

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
  (define-key compilation-mode-map (kbd "g") nil))


;; --------------------------------------------------------------------------------
;; project management
;; --------------------------------------------------------------------------------
(straight-use-package 'projectile) (require 'projectile)
(straight-use-package 'helm-projectile) (require 'helm-projectile)

(projectile-mode 1)
(with-eval-after-load "projectile"
  ;; dont display anything in modeline, since this can slow down tramp
  (setq projectile-dynamic-mode-line nil
        projectile-mode-line-prefix " proj"
        projectile-mode-line " proj"
        projectile-indexing-method (if (eq system-type 'windows-nt)
                                       'native 'alien)))

;; --------------------------------------------------------------------------------
;; snippets
;; --------------------------------------------------------------------------------
(straight-use-package 'yasnippet)
(straight-use-package 'yasnippet-snippets)

(yas-global-mode 1)
(global-set-key (kbd "C-.") 'yas-expand)

;; --------------------------------------------------------------------------------
;; lsp
;; --------------------------------------------------------------------------------
(straight-use-package 'lsp-mode)
(with-eval-after-load "lsp-mode"
  ;; --- bindings ---
  (define-key lsp-mode-map (kbd "M-R") 'lsp-rename)
  (evil-define-key 'normal lsp-mode-map "gd" 'lsp-find-definition)

  ;; --- Fight against very insane defaults ---
  (remove-hook 'lsp-eldoc-hook 'lsp-document-highlight)
  (setq lsp-enable-indentation nil
        lsp-enable-snippet nil
        lsp-modeline-code-actions-enable nil
        lsp-headerline-breadcrumb-enable nil
        lsp-modeline-diagnostics-enable nil)

  ;; This was recommended on the internet.
  (add-hook 'lsp-managed-mode-hook (lambda () (setq-local company-backends '(company-capf)))))

;; I only want lsp-ui for `lsp-ui-doc-*'
(straight-use-package 'lsp-ui)
(setq lsp-ui-doc-enable nil
      lsp-ui-peek-enable nil
      lsp-ui-sideline-enable nil)


;; --------------------------------------------------------------------------------
;; regexps
;; --------------------------------------------------------------------------------
(straight-use-package 'pcre2el)
(with-eval-after-load "re-builder"
  (require 'pcre2el)
  (setq reb-re-syntax 'pcre)
  (setq reb-auto-match-limit 500))

(provide 'config-programming-general)
