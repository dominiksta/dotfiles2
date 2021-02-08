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
(bind-key* "M-z" 'windmove-left)
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
  "bN" (lambda () (interactive) (find-file (concat sync-directory "general/notes/notes.org")))
  "bn" 'next-buffer
  "bp" 'previous-buffer
  "d" 'kill-this-buffer)


;; -- evaluating ---
(evil-leader/set-key
  "eb" 'eval-buffer
  "er" 'eval-region
  "ef" 'eval-defun
  "ee" 'eval-last-sexp
  "em" 'emacs-lisp-macroexpand
  "eE" 'eval-print-last-sexp
  "ec" (lambda () (interactive) (byte-recompile-directory
                            (concat user-emacs-directory "config/") 0))
  "eC" (lambda () (interactive) (byte-recompile-directory
                            (concat user-emacs-directory "config/") 0 t)))

;; --- eyebrowse ---
(dotimes (i 10)
  (global-set-key (kbd (format "M-%d" i))
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

;; --------------------------------------------------------------------------------
;; searching
;; --------------------------------------------------------------------------------

(global-set-key (kbd "C-s") 'helm-swoop-no-prefix)

(evil-leader/set-key
  "ss"  'helm-swoop
  "sr"  'ag
  "sR"  '(lambda () (interactive) (helm-do-ag default-directory))
  "sP"  'helm-projectile-ag
  "sp"  'projectile-ag
  "sgr" 'rgrep
  "sgp" 'projectile-grep
  "sgP" 'helm-projectile-grep
  ;; emacs lisp fallbacks
  "sep" 'projectile-multi-occur
  "seP" 'helm-multi-swoop-projectile
  ;; pgdfgrep
  "sd" 'pdfgrep
  "sD" 'fp/pdfgrep-todos)

(evil-leader/set-key "se" (defhydra project-todo-hydra ()
                            ("j" next-error "search")
                            ("k" previous-error "search")
                            ("q" nil "quit" :color blue)))

;; --------------------------------------------------------------------------------
;; other
;; --------------------------------------------------------------------------------

(evil-leader/set-key
  "SPC" (lambda () (interactive) (call-interactively (if (featurep 'helm) 'helm-M-x
                                                  'execute-extended-command))))

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

;; --------------------------------------------------------------------------------
;; "applications"
;; --------------------------------------------------------------------------------
;; --- bibliography ---
(evil-leader/set-key "ab" 'helm-bibtex)

;; --- mail ---
(evil-leader/set-key "am" 'gnus)

;; --- magit ----
(evil-leader/set-key "ag" 'magit-status)

;; --- sql connections ----
(evil-leader/set-key "aS" 'fp/sql-connect)

;; --- processes ---
(autoload 'fp/proced-startup "config-proced.el")
(evil-leader/set-key
  "app" 'fp/proced-startup
  "apd" 'proced
  "apt" 'helm-timers
  "ape" 'helm-list-emacs-process)

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
  "aa" (lambda () (interactive) (org-agenda nil "a")))

;; --- calc ---
(autoload 'fp/calc-eval-region "config-calc")
(evil-leader/set-key
  "acc" 'fp/run-python-calculator
  "ace" 'fp/calc-eval-region)

;; --- colors ---
(evil-leader/set-key "ahc" 'helm-colors)

;; --- external applications ---
(evil-leader/set-key  "ahe" 'helm-run-external-command)

;; --------------------------------------------------------------------------------
;; programming
;; --------------------------------------------------------------------------------
(evil-leader/set-key
  "pR" 'projectile-run-project
  "pT" 'projectile-test-project
  "pC" 'projectile-compile-project
  "pr" 'recompile
  "pc" 'compile
  "pI" 'projectile-discover-projects-in-directory
  "pf" 'helm-projectile-find-file
  "pe" 'projectile-run-eshell
  "pP" 'helm-projectile-switch-project)

;; --------------------------------------------------------------------------------
;; shells
;; --------------------------------------------------------------------------------
(autoload 'fp/terminal-here "config-shell.el")
(autoload 'fp/open-directory-with-system-default "config-dired.el")
(evil-leader/set-key
  "-s" 'shell
  "-b" 'fp/new-bash-here-xterm
  "-B" 'fp/new-bash-here
  "-r" 'shell-command-on-region
  "-c" 'shell-command
  "-a" 'async-shell-command
  ;; insert shell-command into current buffer
  "-i" (lambda () (interactive) (let ((current-prefix-arg 4))
                                  (call-interactively 'shell-command)))
  "-t" 'fp/terminal-here
  "-d" 'fp/open-directory-with-system-default

  "-e" 'eshell
  "-E" 'fp/eshell-other-window
  "-h" 'fp/eshell-here
  "-H" 'fp/eshell-here-other-window
  "<" 'shell-pop)

(autoload-list '(fp/eshell-other-window fp/eshell-here fp/eshell-here-other-window)
               "config-eshell")

(define-key comint-mode-map (kbd "C-l") 'comint-clear-buffer)

(bind-key* (kbd "C-'") 'ss/dispatch-bash-here)
(bind-key* (kbd "C-#") 'ss/dispatch-bash)

;; --------------------------------------------------------------------------------
;; finishing
;; --------------------------------------------------------------------------------
(straight-use-package 'which-key)
(which-key-mode 1)

(provide 'config-global-binds)
