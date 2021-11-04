;; ----------------------------------------------------------------------
;; binds in general
;; ----------------------------------------------------------------------

(straight-use-package 'bind-key) (require 'bind-key)
(straight-use-package 'hydra)(require 'hydra)

;; --------------------------------------------------------------------------------
;; evil
;; --------------------------------------------------------------------------------

(straight-use-package 'evil)
(require 'evil)

(setq evil-respect-visual-line-mode t)
(evil-define-key '(normal visual motion) visual-line-mode-map
  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line
  "$" 'evil-end-of-visual-line
  "0" 'evil-beginning-of-visual-line)
(evil-mode 1)
(setq evil-insert-state-message nil
      evil-insert-state-modes nil)

(straight-use-package 'evil-leader)
(require 'evil-leader)

(setq evil-leader/in-all-states 1)
(evil-leader/set-leader "SPC")
(evil-mode 0)
(global-evil-leader-mode 1)
(evil-mode 1)

;; --------------------------------------------------------------------------------
;; global bindings
;; --------------------------------------------------------------------------------
;; --- folding ---
(straight-use-package 'yafolding)
(require 'yafolding)

(add-hook 'prog-mode-hook 'yafolding-mode)
(with-eval-after-load "yafolding"
  (define-key yafolding-mode-map (kbd "<C-S-return>") nil)
  (define-key yafolding-mode-map (kbd "<C-M-return>") nil)
  (define-key yafolding-mode-map (kbd "<C-return>") nil))
;; I don't use the normal folding commands at all, so i just overwrite them.
(evil-define-key 'normal global-map
  "za" 'yafolding-toggle-element
  "zA" 'yafolding-hide-all
  (kbd "z M-a") 'yafolding-show-all)


;; --- avy ---
(straight-use-package 'avy)
(evil-define-key 'normal global-map
  "gl" 'evil-avy-goto-line
  "gw" 'evil-avy-goto-word-1
  "g-" 'evil-avy-goto-char-2
  "ö" 'evil-avy-goto-char-2
  "Ö" 'evil-avy-goto-char)

(dolist (state (list 'normal 'motion))
  (evil-define-key state global-map
    (kbd "C-j") (lambda () (interactive) (evil-scroll-down nil))
    (kbd "C-k") (lambda () (interactive) (evil-scroll-up nil))
    "-" 'evil-search-forward))

(evil-leader/set-key
  "ia" 'align
  "ir" 'align-regexp
  "if" 'fill-paragraph
  "iR" 'fill-region)

;; --- searching ---
(defun fp/evil-search-region (start end)
  "Call `evil-search' on current word or text selection."
  (interactive "r")
  (evil-normal-state)
  (evil-search (buffer-substring-no-properties start end) t t nil))

(define-key evil-visual-state-map (kbd "-") 'fp/evil-search-region)
(define-key evil-visual-state-map (kbd "*") 'fp/evil-search-region)

;; --- whitespace ---
(require 'whitespace)
(setq whitespace-style '(face))

(defun fp/toggle-show-too-long-lines ()
  (interactive)
  (if (member 'lines-tail whitespace-style)
      (progn (setq-local whitespace-style (delq 'lines-tail whitespace-style))
             (message "not highlighting long lines"))
    (progn (setq-local whitespace-style (append whitespace-style '(lines-tail)))
           (message "highlighting long lines")))
  (setq-local whitespace-line-column fill-column)
  ;; yes you need to do this
  (whitespace-mode 0) (whitespace-mode 1)
  (font-lock-mode 0) (font-lock-mode 1))

;; Don't add a newline on the end of files on saving
(setq mode-require-final-newline nil)

(defun fp/toggle-show-trailing-whitespace ()
  (interactive)
  (setq show-trailing-whitespace (if (eq show-trailing-whitespace t) nil t))
  (redraw-display))

(evil-leader/set-key
  "uw" 'whitespace-mode
  "ud" 'delete-trailing-whitespace
  "ut" 'fp/toggle-show-trailing-whitespace
  "ul" 'fp/toggle-show-too-long-lines)

;; --- strings ---
(defun fp/string-new-line ()
  (interactive)
  (insert "\" + \"")
  (backward-char 2)
  (newline)
  (indent-for-tab-command))

(defun fp/string-new-line-auto ()
  (interactive)
  (move-to-column (- fill-column 3)) ;; the insert above is 3 characters
  (evil-find-char-backward 1 (string-to-char " "))
  (insert "\" + \"")
  (backward-char 2)
  (newline)
  (indent-for-tab-command))

(global-set-key (kbd "C-<down>") 'fp/string-new-line-auto)


(setq-default fill-column 80)

;; (add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'text-mode-hook 'electric-pair-mode)

;; --- kill ring ---
(setq save-interprogram-paste-before-kill t
      yank-pop-change-selection nil)

;; --- surround ---
(straight-use-package 'evil-surround) (require 'evil-surround)
(global-evil-surround-mode 1)

(straight-use-package 'evil-nerd-commenter)
(evil-leader/set-key "kk" 'evilnc-comment-or-uncomment-lines)

;; --- tample sext ---
(straight-use-package 'lorem-ipsum)

;; --------------------------------------------------------------------------------
;; key translations and remappings
;; --------------------------------------------------------------------------------

;; (evil-define-key 'insert global-map
;;   (kbd "C-q") 'backward-delete-char
;;   (kbd "<left>") 'backward-delete-char)

(define-key key-translation-map (kbd "C-M-7") (kbd "{"))
(define-key key-translation-map (kbd "C-M-8") (kbd "["))
(define-key key-translation-map (kbd "C-M-9") (kbd "]"))
(define-key key-translation-map (kbd "C-M-0") (kbd "}"))

;; --------------------------------------------------------------------------------
;; undo-tree
;; --------------------------------------------------------------------------------

;; I have no idea why it did, but this worked ootb until now. So i guess i have
;; to configure it explicitly now. Kind of annoying when your editor all of the
;; sudden tells you that you can't redo anything anymore. EDIT: An evil update
;; made undo-tree an optional dependency, so thats why.
(straight-use-package 'undo-tree) (require 'undo-tree)
(evil-set-undo-system 'undo-tree)
(global-undo-tree-mode)
(evil-leader/set-key "uu" 'undo-tree-visualize)

;; --------------------------------------------------------------------------------
;; query replace
;; --------------------------------------------------------------------------------

(defun query-replace-region (beg end)
  (interactive "r")
  (if (region-active-p)
      (query-replace (read-from-minibuffer
                      "replace: "
                      (buffer-substring-no-properties beg end))
                     (read-from-minibuffer "replace with: ")
                     nil (point-min) (point-max))
    (query-replace (read-from-minibuffer
                    "replace: "
                    (thing-at-point 'word))
                   (read-from-minibuffer "replace with: ")
                   nil (point-min) (point-max))))

(defun query-replace-regexp-region (beg end)
  (interactive "r")
  (if (region-active-p)
      (query-replace-regexp (read-from-minibuffer
                             "replace: "
                             (buffer-substring-no-properties beg end))
                            (read-from-minibuffer "replace with: ")
                            nil (point-min) (point-max))
    (query-replace-regexp (read-from-minibuffer
                           "replace: "
                           (thing-at-point 'word))
                          (read-from-minibuffer "replace with: ")
                          nil (point-min) (point-max))))

(evil-leader/set-key
  "ho" 'helm-occur
  "rr" 'query-replace-region
  "rk" 'helm-show-kill-ring
  "rR" 'query-replace
  "re" 'query-replace-regexp-region
  "rE" 'query-replace)


;; --------------------------------------------------------------------------------
;; mouse-scrolling
;; --------------------------------------------------------------------------------
(setq scroll-conservatively 100)
(setq scroll-margin 0)


;; scroll two lines at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(2 ((shift) . 2)) ;; one line at a time
      mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
      mouse-wheel-follow-mouse 't ;; scroll window under mouse
      scroll-step 2) ;; keyboard scroll two lines at a time


;; --------------------------------------------------------------------------------
;; sudo
;; --------------------------------------------------------------------------------
(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

  With a prefix ARG prompt for a file to visit.
  Will also prompt for a file to visit if current
  buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (if (eq major-mode 'dired-mode)
          (find-alternate-file (concat "/sudo:root@localhost:" (file-truename default-directory)))
        (find-alternate-file (concat "/sudo:root@localhost:" (helm-read-file-name "SUDO: "))))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(evil-leader/set-key "fR" 'sudo-edit)

;; --------------------------------------------------------------------------------
;; coding systems
;; --------------------------------------------------------------------------------

;; Set the default to unix line endings.
(prefer-coding-system 'utf-8-unix)

(setq fp/coding-system-current (car coding-system-list))
(setq fp/coding-system-current-number 0)


(defun fp/coding-system--set-nth (n)
  (setq fp/coding-system-current-number n
        fp/coding-system-current (nth n coding-system-list))
  (let ((revert-without-query '(".*")))
    (revert-buffer-with-coding-system fp/coding-system-current))
  (message (concat "Setting coding system to " (symbol-name fp/coding-system-current)
                   " (" (number-to-string n) " out of "
                   (number-to-string (length coding-system-list)) ")")))


(defun fp/coding-system-revert-with-next-coding-system ()
  (interactive)
  (incf fp/coding-system-current-number)
  (fp/coding-system--set-nth fp/coding-system-current-number))

(defun fp/coding-system-revert-with-previous-coding-system ()
  (interactive)
  (decf fp/coding-system-current-number)
  (fp/coding-system--set-nth fp/coding-system-current-number))

(defun fp/coding-system-revert-with-first-coding-system ()
  (interactive)
  (setq fp/coding-system-current-number 0)
  (fp/coding-system--set-nth fp/coding-system-current-number))

(evil-leader/set-key "<end>"
  (defhydra coding-systems-hydra ()
    "searching"
    ("n" (fp/coding-system-revert-with-next-coding-system) "next")
    ("p" (fp/coding-system-revert-with-previous-coding-system) "previous")
    ("f" (fp/coding-system-revert-with-first-coding-system) "first")
    ("q" nil "quit" :color blue)))

(provide 'config-editor)

;; --------------------------------------------------------------------------------
;; edit from browser
;; --------------------------------------------------------------------------------

;; Install "Edit with Emacs" from AMO and run M-x edit-server-start
(straight-use-package 'edit-server)
(with-eval-after-load "edit-server"
  (setq edit-server-default-major-mode 'markdown-mode
        edit-server-new-frame nil))

