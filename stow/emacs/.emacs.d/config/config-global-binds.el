(require-and-log 'config-editor)

(use-package hydra :ensure t :demand t)

;; --------------------------------------------------------------------------------
;; window management
;; --------------------------------------------------------------------------------

(setq resize-window-amount 3)
(bind-key* (kbd "M-Z") (lambda () (interactive) (shrink-window-horizontally resize-window-amount)))
(bind-key* (kbd "M-U") (lambda () (interactive) (shrink-window resize-window-amount)))
(bind-key* (kbd "M-I") (lambda () (interactive) (enlarge-window resize-window-amount)))
(bind-key* (kbd "M-O") (lambda () (interactive) (enlarge-window-horizontally resize-window-amount)))

(use-package buffer-move :ensure t
  :demand t
  :config
  (bind-key* (kbd "M-C-z") 'buf-move-left)
  (bind-key* (kbd "M-C-u") 'buf-move-down)
  (bind-key* (kbd "M-C-i") 'buf-move-up)
  (bind-key* (kbd "M-C-o") 'buf-move-right))

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
  "bb" 'switch-to-buffer
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
                     (eyebrowse-switch-to-window-config ,i)))
  (evil-leader/set-key (format "%d" i)
    `(lambda ()
       (interactive)
       (eyebrowse-switch-to-window-config ,i))))

;; --------------------------------------------------------------------------------
;; other
;; --------------------------------------------------------------------------------
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
  "ff" 'find-file
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
(evil-leader/set-key "T" 'fp/theme-toggle)
(global-set-key (kbd "S-<f12>") 'fp/toggle-large-font)
(global-set-key (kbd "<f5>") 'window-show-cursor)

;; --------------------------------------------------------------------------------
;; "applications"
;; --------------------------------------------------------------------------------
;; --- mail ---
(evil-leader/set-key "am" 'gnus)

;; --- emms / music ----
(autoload 'emms "emms-playlist-mode.el")

(defhydra emms-control (:color pink :hint nil)
  "
  ^Views^             ^Control^            ^Add^         
  ^^^^^^^^-----------------------------------------------
  _m_: playlist view   _+_:   volume up     _f_: find    
  _b_: browser view    _-_:   volume down   _r_: dirtree 
  _d_: music dir       _SPC_: play/pause    _y_: youtube 
  ^ ^                  _c_: toggle cache    _p_: playlist
"
  ("m" emms)
  ("b" emms-browser)
  ("d" (find-file "~/Music"))
  ("f" emms-add-find)
  ("r" emms-add-directory-tree)
  ("y" emms-add-youtube-url)
  ("p" emms-add-playlist)
  ("c" emms-cache-toggle)
  ("t" emms-player-simple-ipc-mpv-cycle-video)
  ("+" emms-volume-raise)
  ("-" emms-volume-lower)
  ("SPC" emms-pause)
  ("q" nil "quit" :color blue))

(evil-leader/set-key "," 'emms-control/body)

(global-set-key (kbd "C-S-<up>") 'emms-volume-raise)
(global-set-key (kbd "C-S-<down>") 'emms-volume-lower)
(global-set-key (kbd "C-S-<right>") 'emms-next)
(global-set-key (kbd "C-S-<left>")  'emms-previous)
(global-set-key (kbd "C-S-SPC")     'emms-pause)

;; --- elfeed ---
(autoload 'elfeed-load-db-and-open "config-elfeed.el")
(evil-leader/set-key "aE" 'elfeed-load-db-and-open)

;; --- magit ----
(evil-leader/set-key "ag" 'magit-status)

;; --- sql connections ----
(evil-leader/set-key "aS" 'fp/sql-connect)

;; --- epass ---
(evil-leader/set-key "as" 'epass-store-helm)

;; --- proced ---
(autoload 'fp/proced-startup "config-proced.el")
(evil-leader/set-key
  "app" 'fp/proced-startup
  "apd" 'proced)

;; --- clocking ---
(evil-leader/set-key
  "aoci" 'org-clock-in
  "aocl" 'org-clock-in-last
  "aoco" 'org-clock-out
  "aocd" 'org-clock-display
  "aocr" 'org-clock-remove-overlays
  "aocj" 'org-clock-jump-to-current-clock)

;; --- calendar ---
(evil-leader/set-key "aC" 'calendar)

;; --- agenda ---
(evil-leader/set-key
  "aa" 'org-agenda
  "al" 'org-agenda-list)

;; --- calc ---
(autoload 'fp/calc-eval-region "config-calc")
(evil-leader/set-key
  ;; For turning Calc on and off:
  "acc"  'calc
  "acF"  'full-calc
  "aco"  'calc-other-window
  "acb"  'calc-big-or-small
  "acq"  'quick-calc
  "ack"  'calc-keypad
  "ace"  'calc-embedded
  "acj"  'calc-embedded-select
  "acw"  'calc-embedded-word
  "acz"  'calc-user-invocation
  "acx"  'calc-quit
  "acE" 'fp/calc-eval-region

  ;; For moving data into and out of Calc:
  "acg"  'calc-grab-region
  "acr"  'calc-grab-rectangle
  "ac:"  'calc-grab-sum-down
  "ac_"  'calc-grab-sum-across
  "acy"  'calc-copy-to-buffer

  ;; For use with Embedded mode:
  "aca"  'calc-embedded-activate
  "acd"  'calc-embedded-duplicate
  "acf"  'calc-embedded-new-formula
  "acn"  'calc-embedded-next
  "acp"  'calc-embedded-previous
  "acu"  'calc-embedded-update-formula
  "ac`"  'calc-embedded-edit

  ;; Documentation:
  "aci"  'calc-info
  "act"  'calc-tutorial
  "acs"  'calc-summary

  ;; Miscellaneous:
  "acl"  'calc-load-everything
  "acm"  'read-kbd-macro
  "ac0'" 'calc-reset)

;; --------------------------------------------------------------------------------
;; programming
;; --------------------------------------------------------------------------------
(evil-leader/set-key
  "pI" 'projectile-discover-projects-in-directory
  "pf" 'helm-projectile-find-file
  "pe" 'projectile-run-eshell
  "pR" 'projectile-run-project
  "pr" 'recompile
  "pC" 'compile
  "pc" 'projectile-compile-project
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

(global-set-key (kbd "M-RET") 'fp/eshell-here)
(global-set-key (kbd "<C-s-return>") 'multi-term)

(global-set-key (kbd "<C-return>") 'ss/dispatch-bash)
;; --------------------------------------------------------------------------------
;; finishing
;; --------------------------------------------------------------------------------
(use-package which-key :ensure t :config (which-key-mode 1))
(provide 'config-global-binds)
