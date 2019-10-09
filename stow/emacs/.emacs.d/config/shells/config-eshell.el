(require-and-log 'config-shell)

(setq eshell-aliases-file "~/sync/emacs/random/eshell-aliases-linux")
(defalias 'vis 'eshell-exec-visual)

;; --- open these in a terminal emulator ---
(setq eshell-visual-commands '("htop" "top" "less" "vi" "vim" "more")
      ;; eshell-visual-subcommands '(("git" "log" "diff" "show"))
      eshell-destroy-buffer-when-process-dies t)

;; --- open with system default ---
(defun eshell/o (file)
  (interactive)
  (if (eq system-type 'windows-nt) (w32-browser file) (call-process "xdg-open" nil 0 nil file)))
(defun eshell/start (file) (interactive) (eshell/o file))

;; --- clearing ---
(defun fp/eshell-clear () (interactive)
       (let ((inhibit-read-only t))
         (erase-buffer)
         (insert " ") ;; prompt-colors are bugged without this
         (eshell-send-input)))
(add-hook 'eshell-mode-hook 'fp/eshell-clear)

;; --- colors ---
(add-hook 'eshell-mode-hook 'fp/shell-add-colors)

;; --------------------------------------------------------------------------------
;; prompt
;; --------------------------------------------------------------------------------
(setq eshell-banner-message ""
      eshell-prompt-function
      (lambda ()
        (concat
         (propertize "┌─[" 'face `(:inherit eshell-prompt))
         (propertize (user-login-name) 'face `(:inherit warning))
         (propertize "@" 'face `(:inherit eshell-prompt))
         (propertize (system-name) 'face `(:inherit font-lock-function-name-face))
         (propertize "]──[" 'face `(:inherit eshell-prompt))
         (propertize (format-time-string "%H:%M" (current-time)) 'face `(:inherit warning))
         (propertize "]──[" 'face `(:inherit eshell-prompt))
         (propertize (fp/eshell-prompt-pwd) 'face `(:inherit font-lock-keyword-face))
         (propertize "]\n" 'face `(:inherit eshell-prompt))
         (propertize "└─>" 'face `(:inherit eshell-prompt))
         (propertize (if (= (user-uid) 0) " # " " $ ") 'face `(:inherit eshell-prompt)))))

(defun fp/eshell-prompt-pwd ()
  (let* ((path default-directory)
         (bigger (- (length path) 50)))
    (if (> bigger 0)
        (concat ".../"
                (mapconcat
                 'identity
                 (cdr (split-string (substring path bigger) "/"))
                 "/"))
      path)))


(setq eshell-highlight-prompt nil) ; this is apparently required
(defun spacemacs//protect-eshell-prompt ()
  "Protect Eshell's prompt like Comint's prompts.
  E.g. `evil-change-whole-line' won't wipe the prompt. This
  is achieved by adding the relevant text properties."
  (let ((inhibit-field-text-motion t))
    (add-text-properties
     (point-at-bol)
     (point)
     '(rear-nonsticky t
                      inhibit-line-move-field-capture t
                      field output
                      read-only t
                      front-sticky (field inhibit-line-move-field-capture)))))
(add-hook 'eshell-after-prompt-hook 'spacemacs//protect-eshell-prompt)

;; --------------------------------------------------------------------------------
;; eshell-here
;; --------------------------------------------------------------------------------
(defun fp/eshell-here (arg)
  (interactive "p")
  (let ((path (file-name-directory (or  (buffer-file-name) default-directory)))
        (buf (eshell (if (not (eq 1 arg)) arg nil))))
    (if (eshell-process-interact 'process-live-p)
        (message "Won't change CWD because of running process.")
      (setq default-directory path)
      (eshell-reset))
    buf))

;; --------------------------------------------------------------------------------
;; other-window
;; --------------------------------------------------------------------------------
(defun fp/eshell-here-other-window (arg)
  (interactive "p")
  (let ((prev (current-buffer))
        (buf (fp/eshell-here arg)))
    (switch-to-buffer prev)
    (switch-to-buffer-other-window buf)))

(defun fp/eshell-other-window (arg)
  (interactive "p")
  (let ((prev (current-buffer))
        (buf (eshell arg)))
    (switch-to-buffer prev)
    (switch-to-buffer-other-window buf)))

;; --------------------------------------------------------------------------------
;; history
;; --------------------------------------------------------------------------------
(defun fp/helm-eshell-history ()
  "This only exists because the default one seems to be bugged for me for some reason"
  (interactive)
  (eshell-bol)
  (ignore-errors (kill-line))
  (insert (completing-read "Eshell history: " (delete-dups (ring-elements eshell-history-ring)))))

(defun eshell-previous-input-with-clear ()
  (interactive)
  (ignore-errors (eshell-bol) (kill-line nil))
  (eshell-previous-input 1))

(defun eshell-next-input-with-clear ()
  (interactive)
  (ignore-errors (eshell-bol) (kill-line nil))
  (eshell-next-input 1))

;; --------------------------------------------------------------------------------
;; sudo
;; --------------------------------------------------------------------------------
(require 'em-tramp)

(setq eshell-prefer-lisp-functions nil)
(setq eshell-prefer-lisp-variables nil)

(setq password-cache t)
(setq password-cache-expiry 3600)

;; --------------------------------------------------------------------------------
;; environment variables
;; --------------------------------------------------------------------------------
;; git config --global core.pager '$`test "$TERM" = "dumb-emacs-ansi" && echo cat || echo less`'
;; (if (not (string-equal
;;           (car (split-string (shell-command-to-string "git config --global core.pager") "\n"))
;;           "`test \"$TERM\" = \"dumb-emacs-ansi\" && echo cat || echo less`"))
;;     (shell-command
;;      "git config --global core.pager `test \"$TERM\" = \"dumb-emacs-ansi\" && echo cat || echo less`"))

(defun fp/eshell-set-environment ()
  (make-local-variable 'process-environment)
  ;; disable paging
  (setenv "PAGER" "cat") (setenv "GIT_PAGER" "cat")
  ;; set editor
  (setenv "EDITOR" "emacsclient"))

(add-hook 'eshell-mode-hook 'fp/eshell-set-environment)

;; --------------------------------------------------------------------------------
;; bindings
;; --------------------------------------------------------------------------------
(defun fp/eshell-hook ()
  "Apparently eshell-mode-map is buffer-local, so i set it to simply be configured in a hook."
  (define-key eshell-mode-map (kbd "C-l") 'fp/eshell-clear)
  (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)
  (define-key eshell-mode-map (kbd "M-P") 'fp/helm-eshell-history)
  (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)
  (define-key eshell-mode-map (kbd "s-+") 'delete-window)
  (define-key eshell-mode-map (kbd "M-p") 'eshell-previous-input-with-clear)
  (define-key eshell-mode-map (kbd "M-n") 'eshell-next-input-with-clear)
  (evil-define-key 'normal eshell-mode-map
    "gk" (lambda (n) (interactive "p") (eshell-next-prompt -2))
    "gj" 'eshell-next-prompt
    "0" 'eshell-bol
    (kbd "G") 'end-of-buffer
    (kbd "RET") 'eshell-send-input)
  (evil-define-key 'visual 'eshell-mode-map
    "gk" (lambda (n) (interactive "p") (eshell-next-prompt -2))
    "gj" 'eshell-next-prompt
    "0" 'eshell-bol))

(add-hook 'eshell-mode-hook 'fp/eshell-hook)


(provide 'config-eshell)
