(require-and-log 'config-search)
(require-and-log 'config-helm-minibuffer)

(global-eldoc-mode 0)

(evil-define-key 'normal prog-mode-map "gf" 'xref-find-references)

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
(setq highlight-indent-guides-method 'character
      highlight-indent-guides-auto-enabled nil)
(custom-set-faces
 '(highlight-indent-guides-character-face
   ((t (:foreground nil :inherit font-lock-comment-face)))))

;; --- parens ---
(setq show-paren-delay 0) (show-paren-mode 1)
(add-hook 'prog-mode-hook 'electric-pair-mode)

;; --- bug references ---
(add-hook 'prog-mode-hook 'bug-reference-prog-mode)

(defvar-local fp/bug-reference-trackers '())

(defun fp/bug-reference-dispatch (&optional pos)
  (interactive
   (list (if (integerp last-command-event) (point) last-command-event)))
  (dolist (o (overlays-at pos))
    (when (overlay-get o 'bug-reference-url)
      (let* ((text (buffer-substring-no-properties
                    (overlay-start o) (overlay-end o)))
             (num (substring text (string-match-p "[[:digit:]]" text)))
             (tracker (completing-read "Tracker: " (mapcar 'car fp/bug-reference-trackers)))
             (tracker-url (cdar (seq-filter (lambda (el) (string-equal (car el) tracker))
                                           fp/bug-reference-trackers))))
        (browse-url (format tracker-url num)))
      )))
(evil-define-key 'normal prog-mode-map "gb" 'fp/bug-reference-dispatch)

;; --- todos ---
(straight-use-package 'hl-todo)
(add-hook 'prog-mode-hook 'hl-todo-mode)
(add-hook 'text-mode-hook 'hl-todo-mode)

(defun fp/project-todo-search-todo () (interactive)
       (fp/rg-project-everything "TODO|FIXME|NEXT"))
(defun fp/project-todo-search-hack () (interactive)
       (fp/rg-project-everything "HACK"))
(defun fp/project-todo-search-all  () (interactive)
       (fp/rg-project-everything "TODO|FIXME|NEXT|DONE|HACK"))

(evil-leader/set-key
  "stt" 'fp/project-todo-search-todo
  "sta" 'fp/project-todo-search-all
  "sth" 'fp/project-todo-search-hack)

;; --- imenu ---
(evil-leader/set-key
  "hi" 'helm-imenu
  "hI" 'helm-imenu-in-all-buffers)

(evil-add-command-properties #'helm-imenu :jump t)

(straight-use-package 'imenu-anywhere)
(evil-leader/set-key "pi" 'helm-imenu-anywhere)

;; --- editorconfig ---
(straight-use-package 'editorconfig)
(editorconfig-mode 1)

;; --- automatically highlight symbols ---
(straight-use-package 'symbol-overlay)
(add-hook 'prog-mode-hook 'symbol-overlay-mode)
(with-eval-after-load "symbol-overlay"
  (global-set-key (kbd "M-R") 'symbol-overlay-rename)
  (evil-define-key 'normal symbol-overlay-mode-map
    "gn" 'symbol-overlay-jump-next
    "gp" 'symbol-overlay-jump-prev))

;; --------------------------------------------------------------------------------
;; project actions
;; --------------------------------------------------------------------------------

(defvar fp/project-actions
  '(("compile" compile)
    ("make" (lambda () (interactive) (compile "make"))))
  "An alist of commands/actions to execute and their names. See
`fp/project-actions-run'")

(defvar fp/project-actions-default-directory nil
  "Set this in .dir-locals.el for `fp/project-actions-run' to set
its `default-directory'.")

;; Example .dir-locals.el:
;; ((nil . ((fp/project-actions-default-directory . "~/Source/git/dotfiles")
;;          (fp/project-actions . (("mycompile" (lambda () (interactive)
;;                                                (compile "make"))))))))

(defun fp/project-actions-run ()
  "Prompt for an action defined in `fp/project-actions' and
execute it, setting `default-directory' to
`fp/project-actions-default-directory' when non-nil or
`projectile-project-root'."
  (interactive)
  (let ((default-directory (if fp/project-actions-default-directory
                               fp/project-actions-default-directory
                             (projectile-project-root)))
        (selection (completing-read "Run action: "
                                    (mapcar 'car fp/project-actions))))
    (call-interactively (cadr (assoc selection fp/project-actions)))))

;; --------------------------------------------------------------------------------
;; company
;; --------------------------------------------------------------------------------
(straight-use-package 'company) (require 'company)
(setq company-idle-delay 0
      company-minimum-prefix-length 3)
(global-set-key (kbd "C-:") 'company-complete)
(define-key company-mode-map (kbd "C-j") 'company-select-next)
(define-key company-mode-map (kbd "C-k") 'company-select-previous)
(define-key company-mode-map (kbd "M-j") 'company-select-next)
(define-key company-mode-map (kbd "M-k") 'company-select-previous)
(define-key company-mode-map (kbd "M-l") 'company-complete-common)


;; --------------------------------------------------------------------------------
;; dumb-jump - jump to definition
;; --------------------------------------------------------------------------------
(straight-use-package 'dumb-jump)

(defun fp/maybe-activate-dump-jump ()
  (unless (bound-and-true-p lsp-mode)
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate nil t)))

(add-hook 'prog-mode-hook 'fp/maybe-activate-dump-jump)

(defvar fp/project-find-regexp-files nil)
;; (evil-define-key 'normal "gf" (lambda () (interactive) (project-find-regexp )))

;; --------------------------------------------------------------------------------
;; diff-hl (highlighting git/svn/vc changes in fringe)
;; --------------------------------------------------------------------------------
(straight-use-package 'diff-hl)
(global-diff-hl-mode 1)
(run-with-idle-timer 2 10 'diff-hl-update)

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
    "q" 'quit-window
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
        projectile-indexing-method 'hybrid
        projectile-switch-project-action (lambda () (dired default-directory))))

;; --------------------------------------------------------------------------------
;; snippets
;; --------------------------------------------------------------------------------
(straight-use-package 'yasnippet)
(straight-use-package 'yasnippet-snippets)

(yas-global-mode 1)
(global-set-key (kbd "C-.") 'yas-expand)

;; --------------------------------------------------------------------------------
;; tree-sitter
;; --------------------------------------------------------------------------------
(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)
(require 'tree-sitter)
(require 'tree-sitter-langs)

(global-tree-sitter-mode)

;; --------------------------------------------------------------------------------
;; lsp
;; --------------------------------------------------------------------------------
(straight-use-package 'lsp-mode)
(with-eval-after-load "lsp-mode"
  ;; --- bindings ---
  (define-key lsp-mode-map (kbd "M-R") 'lsp-rename)
  (evil-define-key 'normal lsp-mode-map
    "gd" 'lsp-find-definition
    "gh" 'lsp-ui-doc-glance
    "gf" 'lsp-find-references)

  ;; --- Fight against very insane defaults ---
  (remove-hook 'lsp-eldoc-hook 'lsp-document-highlight)
  (setq lsp-enable-indentation nil
        lsp-enable-snippet nil

        lsp-signature-render-documentation nil
        lsp-eldoc-enable-hover nil
        lsp-eldoc-render-all nil

        ;; Not insane, but i want to try symbol-overlay
        lsp-enable-symbol-highlighting nil
        lsp-modeline-code-actions-enable nil
        lsp-headerline-breadcrumb-enable nil
        lsp-modeline-diagnostics-enable nil)

  ;; This was recommended on the internet.
  (add-hook 'lsp-managed-mode-hook (lambda () (setq-local company-backends '(company-capf))))

  )

;; I only want lsp-ui for `lsp-ui-doc-*'
(straight-use-package 'lsp-ui)
(setq lsp-ui-doc-enable nil
      lsp-ui-peek-enable nil
      lsp-ui-sideline-enable nil)

(defvar-local fp/evil-lsp-format-enable nil)

(evil-define-operator evil-lsp-format (beg end)
  "Format text with lsp. See `evil-indent' for reference."
  :move-point nil
  :type line
  (if fp/evil-lsp-format-enable
      (progn
        ;; these two movements mimic the behaviour of `evil-indent`. not sure if they
        ;; are useful, but consistency is always nice
        (goto-char beg)
        (evil-first-non-blank)
        (lsp-format-region beg end))
    (evil-indent beg end)))

(evil-define-key '(normal visual) lsp-mode-map "=" 'evil-lsp-format)

;; ----------------------------------------------------------------------
;; xref (for lsp, tide, maybe others)
;; ----------------------------------------------------------------------
(with-eval-after-load "xref"
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  (evil-define-key 'normal xref--xref-buffer-mode-map
    "q" 'quit-window
    "r" 'xref-revert-buffer
    (kbd "RET") 'xref-goto-xref
    "a" 'xref-show-location-at-point
    (kbd "C-c C-f") 'next-error-follow-minor-mode))

;; ----------------------------------------------------------------------
;; diff-mode
;; ----------------------------------------------------------------------
(evil-set-initial-state 'diff-mode 'motion)


(with-eval-after-load "diff-mode"
  ;; (setq diff-refine 'font-lock)
  (evil-define-key 'motion diff-mode-map
    "q" 'quit-window
    "gj" 'diff-file-next
    "gk" 'diff-file-prev
    (kbd "M-j") 'diff-hunk-next
    (kbd "M-k") 'diff-hunk-prev
    "gx" 'diff-hunk-kill
    (kbd "RET") 'diff-goto-source)
  ;; (set-face-attribute 'diff-refine-removed nil :background 'unspecified
  ;;                     :inherit 'highlight)
  ;; (set-face-attribute 'diff-refine-added nil :background 'unspecified
  ;;                     :inherit 'highlight)
  ;; (set-face-attribute 'diff-refine-changed nil :background 'unspecified
  ;;                     :inherit 'highlight)
  )

;; --------------------------------------------------------------------------------
;; regexps
;; --------------------------------------------------------------------------------
(straight-use-package 'pcre2el)
(with-eval-after-load "re-builder"
  (require 'pcre2el)
  (setq reb-re-syntax 'pcre)
  (setq reb-auto-match-limit 500))

(provide 'config-programming-general)
