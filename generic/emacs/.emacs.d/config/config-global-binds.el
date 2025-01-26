(require-and-log 'config-editor)

(global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs)

;; --------------------------------------------------------------------------------
;; window management
;; --------------------------------------------------------------------------------

(setq resize-window-amount 3)
(bind-key* (kbd "M-Z") (lambda () (interactive) (shrink-window-horizontally resize-window-amount)))
(bind-key* (kbd "M-U") (lambda () (interactive) (shrink-window resize-window-amount)))
(bind-key* (kbd "M-I") (lambda () (interactive) (enlarge-window resize-window-amount)))
(bind-key* (kbd "M-O") (lambda () (interactive) (enlarge-window-horizontally resize-window-amount)))

(straight-use-package 'buffer-move)
(require 'buffer-move)

(bind-key* (kbd "M-C-z") 'buf-move-left)
(bind-key* (kbd "M-C-u") 'buf-move-down)
(bind-key* (kbd "M-C-i") 'buf-move-up)
(bind-key* (kbd "M-C-o") 'buf-move-right)

(bind-key* "M-v" (lambda () (interactive) (split-window-right) (windmove-right)))
(bind-key* "M-c" (lambda () (interactive) (split-window-below) (windmove-down)))

(bind-key* "M-Ö" 'make-frame)
(bind-key* "M-ö" 'other-frame)
(bind-key* "M-q" 'delete-window)
(bind-key* "M-d" 'kill-this-buffer)
(bind-key* "M-s" 'switch-to-buffer)
(bind-key* "M-y" 'windmove-left)
(bind-key* "M-u" 'windmove-down)
(bind-key* "M-i" 'windmove-up)
(bind-key* "M-o" 'windmove-right)
(bind-key* "M-s" 'switch-to-buffer)

(evil-leader/set-key
  "q"  'delete-window
  "wd" 'delete-window

  "wu" 'winner-undo
  "wr" 'winner-redo

  "wo" 'other-frame
  "wn" 'make-frame
  "wb" 'split-window-vertically
  "wv" 'split-window-horizontally

  "wl" 'windmove-right
  "wh" 'windmove-left
  "wj" 'windmove-down
  "wk" 'windmove-up

  "wH" 'evil-window-move-far-left
  "wJ" 'evil-window-move-very-top
  "wK" 'evil-window-move-very-bottom
  "wL" 'evil-window-move-far-right
  "wm" 'fp/toggle-window-maximized
  "w=" 'balance-windows)

;; --- buffers ---
(evil-leader/set-key "bb" 'switch-to-buffer)
(evil-leader/set-key
  "bi" 'ibuffer
  "bb" (lambda () (interactive) (call-interactively (if (featurep 'helm) 'helm-buffers-list 'switch-to-buffer)))
  "bs" (lambda () (interactive) (switch-to-buffer "*scratch*"))
  "bS" (lambda () (interactive) (find-file (concat sync-directory "documents/notes/mvtn/static/20210623-203826 Scratch.txt")))
  "bn" 'next-buffer
  "bp" 'previous-buffer
  "d" 'kill-this-buffer)

(global-set-key fp/mouse-back 'previous-buffer)
(global-set-key fp/mouse-forward 'next-buffer)


;; -- evaluating ---
(evil-leader/set-key
  "eb" 'eval-buffer
  "er" 'eval-region
  "ef" 'eval-defun
  "ee" 'eval-last-sexp
  "em" 'emacs-lisp-macroexpand
  "eE" 'eval-print-last-sexp)

;; --- eyebrowse ---
(dotimes (i 10)
  (bind-key* (kbd (format "M-%d" i))
                  `(lambda ()
                     (interactive)
                     (fp/eyebrowse-switch-to-window-config-and-run-defaults ,i)))
  (evil-leader/set-key (format "%d" i)
    `(lambda ()
       (interactive)
       (fp/eyebrowse-switch-to-window-config-and-run-defaults ,i))))

(dotimes (i 10)
  (evil-leader/set-key (format "w %d" i)
    `(lambda ()
       (interactive)
       (eyebrowse-switch-to-window-config ,i))))

(bind-key* "M-H" 'eyebrowse-prev-window-config)
(bind-key* "M-L" 'eyebrowse-next-window-config)

;; --------------------------------------------------------------------------------
;; searching
;; --------------------------------------------------------------------------------

(global-set-key (kbd "C-s") 'helm-occur)
(global-set-key (kbd "C-S-s") 'occur)

(autoload 'fp/rg-project-everything "rg")
(evil-leader/set-key
  "ss"  'helm-occur
  "sr"  'rg
  "sh"  'fp/helm-projectile-rg
  "sp"  'fp/rg-project-everything
  "sP"  'rg-project
  "sgr" 'rgrep
  "sgp" 'projectile-grep
  "sgP" 'helm-projectile-grep
  ;; pgdfgrep
  "sd" 'pdfgrep
  "sD" 'fp/pdfgrep-todos)

;; --------------------------------------------------------------------------------
;; other
;; --------------------------------------------------------------------------------

(evil-leader/set-key
  "SPC" (lambda () (interactive) (call-interactively (if (featurep 'helm) 'helm-M-x
                                                  'execute-extended-command))))

(evil-define-key 'normal global-map
  "go" 'browse-url-at-point
  "gx" nil)

;; --- olivetti ---
(evil-leader/set-key "Rr" 'olivetti-mode)

;; --- bookmarks ---
(evil-leader/set-key
  "bh"  'helm-bookmarks
  "ba"  'bookmark-set
  "bl"  'bookmark-bmenu-list)


;; --- files ---
(evil-leader/set-key
  "fa" 'save-some-buffers
  "fs" 'save-buffer
  "ff" (lambda () (interactive) (call-interactively (if (featurep 'helm) 'helm-find-files 'find-file)))
  "fF" 'find-file-other-window
  "fp" 'project-find-file ;; TODO why is projectile not working?
  "fk" (lambda () (interactive)
         (let ((cb (current-buffer))) (dired-jump)
              (kill-buffer cb)))
  "fj" 'dired-jump
  "fJ" 'dired-jump-other-window)

;; --- spell-checking ---
(evil-define-key 'normal global-map
  "zd" 'ispell-toggle-dictionary
  "zg" 'helm-flyspell-correct
  "zn" 'evil-next-flyspell-error
  "zp" 'evil-prev-flyspell-error)

(evil-leader/set-key
  "add" 'dictcc
  "adp" 'dictcc-at-point
  "adb" 'ispell-buffer
  "adf" 'fp/toggle-flyspell-check-buffer)

(autoload-list '(ispell-toggle-dictionary
                 flyspell-correct-word
                 ispell-buffer
                 evil-next-flyspell-error
                 evil-prev-flyspell-error
                 fp/toggle-flyspell-check-buffer) "config-language-natural")


;; theme
(evil-leader/set-key
  "T" 'fp/theme-toggle
  ;; A lot of themes do not work correctly in a terminal session. The default
  ;; theme (aka no theme) handles terminal colors quite well though.
  (kbd "C-t") (lambda () (interactive) (mapc #'disable-theme custom-enabled-themes)))

(defhydra fp/font-size-hydra ()
  "searching"
  ("+" (lambda () (interactive) (fp/theme-adjust-global-font-size 20)) "increase")
  ("-" (lambda () (interactive) (fp/theme-adjust-global-font-size -20)) "decrease")
  ("0" (lambda () (interactive) (fp/theme-adjust-global-font-size 0)) "default")
  ("t" fp/toggle-large-font "toggle large font")
  ("q" nil "quit" :color blue))

(global-set-key (kbd "C-c C-+") 'fp/font-size-hydra/body)
(global-set-key (kbd "<f5>") 'window-show-cursor)

(global-set-key (kbd "C-M-ö") (lambda () (interactive) (insert "→")))

;; --------------------------------------------------------------------------------
;; "applications"
;; --------------------------------------------------------------------------------
;; --- bibliography ---
(evil-leader/set-key "ab" 'helm-bibtex)

;; --- clocking ---
(evil-leader/set-key
  "aoci" 'org-clock-in
  "aocl" 'org-clock-in-last
  "aoco" 'org-clock-out
  "aocd" 'org-clock-display
  "aocr" 'org-clock-remove-overlays
  "aocj" 'org-clock-goto)

;; --- calendar ---
(evil-leader/set-key "aC" 'calendar)

;; --- agenda ---
(evil-leader/set-key
  "aa" (lambda () (interactive) (mvtn-org-agenda nil "a")))

;; --- calc ---
(autoload 'fp/calc-eval-region "config-calc")
(evil-leader/set-key
  "acc" 'fp/run-python-calculator
  "ace" 'fp/calc-eval-region)

;; --- colors ---
(evil-leader/set-key "ahc" 'helm-colors)

;; --------------------------------------------------------------------------------
;; programming
;; --------------------------------------------------------------------------------
(evil-leader/set-key
  "ps" 'projectile-switch-project
  "pR" 'projectile-run-project
  "pT" 'projectile-test-project
  "pC" 'projectile-compile-project
  "pa" 'fp/project-actions-run
  "pr" (lambda () (interactive) (let ((compilation-scroll-output t)) (recompile)))
  "pc" (lambda () (interactive) (let ((compilation-scroll-output t)) (compile)))
  "pI" 'projectile-discover-projects-in-directory
  "pf" 'helm-projectile-find-file
  "pe" 'projectile-run-eshell
  "pv" (lambda () (interactive) (vc-dir (projectile-project-root)))
  "pP" 'helm-projectile-switch-project)

;; --------------------------------------------------------------------------------
;; shells
;; --------------------------------------------------------------------------------
(autoload 'fp/terminal-here "config-shell.el")
(autoload 'fp/open-directory-with-system-default "config-dired.el")
(evil-leader/set-key
  "-s" 'shell
  "-r" 'shell-command-on-region
  "-c" 'shell-command
  "-a" 'async-shell-command
  "-t" 'fp/terminal-here
  "-d" 'fp/open-directory-with-system-default)

(define-key comint-mode-map (kbd "C-l") 'comint-clear-buffer)

;; --------------------------------------------------------------------------------
;; finishing
;; --------------------------------------------------------------------------------
(straight-use-package 'which-key)
(which-key-mode 1)

(provide 'config-global-binds)
