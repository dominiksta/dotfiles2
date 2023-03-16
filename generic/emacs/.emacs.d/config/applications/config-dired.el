(require 'dired-x)
(require-and-log 'config-editor)

;; --------------------------------------------------------------------------------
(put 'dired-find-alternate-file 'disabled nil)
(defun dired-find-alternate-up ()
  (interactive)
  (find-alternate-file ".."))

(defun fp/dired-copy-path-at-point ()
  (interactive)
  (dired-copy-filename-as-kill 0))

(setq ls-lisp-emulation t
      ls-lisp-dirs-first t
      dired-listing-switches
      (if (eq system-type 'windows-nt)
          "-a -l -h"
        "-a -l -h --group-directories-first"))

(defun fp/dired-beginning-of-buffer ()
  (interactive) (evil-goto-line) (dired-previous-line 1))

(defun fp/dired-end-of-buffer ()
  (interactive)
  (evil-goto-first-line)
  (if (eq fp/dired-hide-details-globally nil) (dired-next-line 3) (dired-next-line 2)))

;; --------------------------------------------------------------------------------
;; tail
;; --------------------------------------------------------------------------------
(defun dired-tail ()
  (interactive)
  (let ((file (dired-get-filename))
        (name (dired-get-filename t)))
    (switch-to-buffer (process-buffer
                       (start-process-shell-command
                        "tail" (concat "*tail " name "*")
                        (concat "tail -f " file))))
    (run-with-timer 0.2 nil (lambda () (beginning-of-buffer)))))

(evil-leader/set-key-for-mode 'dired-mode "mt" 'dired-tail)

;; --------------------------------------------------------------------------------
;; diffs
;; --------------------------------------------------------------------------------
(defun dired-ediff-marked ()
  (interactive)
  (let* ((files (dired-get-marked-files))
         (len (length files)))
    (cond
     ((> len 3) (message "Too many files marked"))
     ((= len 3) (ediff3 (car files) (nth 1 files) (nth 2 files)))
     ((= len 2) (ediff (car files) (cadr files)))
     ((< len 2) (message "Not enough files marked")))))

;; --------------------------------------------------------------------------------
;; file previews
;; --------------------------------------------------------------------------------
(straight-use-package 'peep-dired)

(evil-leader/set-key-for-mode 'dired-mode "mp" 'peep-dired)

(with-eval-after-load "peep-dired"
  (add-hook 'peep-dired-hook (lambda () (evil-emacs-state) (evil-normal-state)))
  (evil-define-key 'normal peep-dired-mode-map
    "j" 'peep-dired-next-file
    "k" 'peep-dired-prev-file
    "J" 'peep-dired-scroll-page-down
    "K" 'peep-dired-scroll-page-up))

;; --------------------------------------------------------------------------------
;; file-sizes
;; --------------------------------------------------------------------------------
(defun fp/dired-file-size-under-point ()
  (interactive)
  (shell-command (format "du -h %s | tail -1" (dired-file-name-at-point))))

(evil-leader/set-key-for-mode 'dired-mode "mS" 'fp/dired-file-size-under-point)

;; This is supposed to mimic how i use gui file managers
(define-key dired-mode-map (kbd "M-RET") 'fp/dired-file-size-under-point)


;; --------------------------------------------------------------------------------
;; subtrees
;; --------------------------------------------------------------------------------
;; TODO light face
(straight-use-package 'dired-subtree)
(define-key dired-mode-map (kbd "C-x n s") 'dired-subtree-narrow)
(evil-define-key 'normal dired-mode-map
  (kbd "TAB") 'dired-subtree-toggle
  (kbd "M-j") 'dired-subtree-next-sibling
  (kbd "M-k") 'dired-subtree-previous-sibling
  "gj" 'dired-subtree-next-sibling
  "gk" 'dired-subtree-previous-sibling
  "gh" 'dired-subtree-up
  "i" 'dired-subtree-insert
  "I" 'dired-subtree-remove)

;; --------------------------------------------------------------------------------
;; open with default application
;; --------------------------------------------------------------------------------
(defun fp/dired-open-with-system-default ()
  (interactive)
  (if (eq system-type 'windows-nt)
      (progn
        (w32-browser (dired-replace-in-string "/" "\\" (dired-get-filename))))
    (progn
      (let* ((file (dired-get-filename nil t)))
        (call-process "xdg-open" nil 0 nil file)))))

(defun fp/dired-mouse-open-with-system-default (event)
  (interactive "e")
  (fp/point-to-mouse event)
  (fp/dired-open-with-system-default))

(defun fp/dired-open-directory-with-system-default ()
  (interactive)
  (if (eq system-type 'windows-nt) (shell-command "start .")
    (start-process-shell-command "thunar" nil "thunar .")))

;; --------------------------------------------------------------------------------
;; appearance
;; --------------------------------------------------------------------------------
;; --- hiding details ---
(defvar fp/dired-hide-details-globally t "Wether to hide details in dired globally")

(defun fp/dired-toggle-hide-details-globally ()
  (interactive)
  (if (eq fp/dired-hide-details-globally nil)
      (progn
        (setq fp/dired-hide-details-globally t)
        (add-hook 'dired-after-readin-hook 'dired-hide-details-mode)
        (dired-hide-details-mode 1))
    (progn
      (setq fp/dired-hide-details-globally nil)
      (remove-hook 'dired-after-readin-hook 'dired-hide-details-mode)
      (dired-hide-details-mode 0))))

(define-key dired-mode-map (kbd "(") 'fp/dired-toggle-hide-details-globally)
(add-hook 'dired-after-readin-hook 'dired-hide-details-mode)

;; --- hiding the dot and others ---
(setq dired-omit-files
      (concat "^\\.$" "\\|"
              "^desktop\\.ini$" "\\|"
              "^NTUSER\\.dat$" "\\|"
              "^ntuser\\.ini$" "\\|"
              "^__pycache__$"))
(setq dired-omit-extensions nil)
(setq dired-omit-verbose nil)
(add-hook 'dired-after-readin-hook 'dired-omit-mode)


;; --------------------------------------------------------------------------------
;; async
;; --------------------------------------------------------------------------------
(defun dired-async-system-notify (format-string &rest args)
  (generic-notification-notify "Async File Operation" (apply 'format format-string (cdr args)) t))
(setq dired-async-message-function 'dired-async-system-notify)

(define-key dired-mode-map (kbd "A") 'dired-async-mode)

;; --------------------------------------------------------------------------------
;; rsync - this might just make me get rid of async
;; --------------------------------------------------------------------------------
;; For this package to work fully as intended, an ssh-agent (such as the
;; gpg-agent that i use) and an ssh config is necessary specifying what user to
;; log in to on the remote machine.
(straight-use-package 'dired-rsync)
(evil-define-key 'normal dired-mode-map
  "R" 'dired-rsync
  "bR" (lambda () (interactive) (let ((dired-dwim-target t))
                             (call-interactively 'dired-rsync))))

;; --------------------------------------------------------------------------------
;; bindings
;; --------------------------------------------------------------------------------
(evil-leader/set-key-for-mode 'dired-mode
  "ma" 'gnus-dired-attach
  "mo" 'fp/dired-open-with-system-default
  "md" 'fp/dired-open-directory-with-system-default
  "mi" 'image-dired-no-window-config
  "mI" 'image-dired-my-window-config)
(evil-define-key 'normal dired-mode-map
  "j" 'dired-next-line
  "k" 'dired-previous-line
  "l" 'dired-find-alternate-file
  "h" 'dired-find-alternate-up
  "n" 'evil-search-next
  "N" 'evil-search-previous

  "E" 'dired-ediff-marked
  "e" 'dired-toggle-read-only
  "c" 'dired-do-copy
  "bc" (lambda () (interactive) (let ((dired-dwim-target t)) (dired-do-copy)))
  "v" 'dired-do-rename
  "bv" (lambda () (interactive) (let ((dired-dwim-target t)) (dired-do-rename)))
  "S" 'dired-do-symlink
  "bS" (lambda () (interactive) (let ((dired-dwim-target t)) (dired-do-symlink)))

  "W" 'fp/dired-copy-path-at-point

  "r" (lambda () (interactive)
        (if (bound-and-true-p dired-du-mode) (dired-du-mode 0)) (revert-buffer))
  "q"   'kill-this-buffer
  "gg" 'fp/dired-end-of-buffer
  "G"   'fp/dired-beginning-of-buffer

  "q" (lambda () (interactive) (kill-buffer (current-buffer)))
  "0" 'evil-beginning-of-line-or-digit-argument
  "$" 'evil-end-of-line)


(evil-define-key 'normal wdired-mode-map
  (kbd "_") (lambda () (interactive) (dired-next-line 1) (dired-previous-line 1)))


;; --- mouse binds ---

(defun fp/dired-mouse-find-alternate-file (event)
  (interactive "e")
  (fp/point-to-mouse event)
  (let ((file (dired-get-file-for-visit)))
    (if (file-directory-p file)
        (dired-find-alternate-file)
      (find-alternate-file file))))

(define-key dired-mode-map [mouse-2] 'fp/dired-mouse-find-alternate-file)
(define-key dired-mode-map [(double-mouse-2)] 'fp/dired-mouse-find-alternate-file)
(define-key dired-mode-map [(triple-mouse-2)] 'fp/dired-mouse-find-alternate-file)

(define-key dired-mode-map [mouse-3] 'fp/dired-mouse-open-with-system-default)
(define-key dired-mode-map [(double-mouse-3)] 'fp/dired-mouse-open-with-system-default)
(define-key dired-mode-map [(triple-mouse-3)] 'fp/dired-mouse-open-with-system-default)

;; --------------------------------------------------------------------------------
;; kdeconnect
;; --------------------------------------------------------------------------------

(defvar fp/kdeconnect-active-device "Pixel 3a"
  "The device used for kdeconnect. Only one is supported. You can
  find the names with 'kdeconnect-cli -a --names-only'")

(setq fp/kdeconnect-program "kdeconnect-cli")

(defun fp/kdeconnect-base-command ()
  (format "%s -n \"%s\"" fp/kdeconnect-program fp/kdeconnect-active-device))

(defun fp/kdeconnect-send-files (files)
  "Send a list of files to `fp/kdeconnect-active-device'."
  (when (not (listp files)) (error "invalid type"))
  (message "Sending files %s to %s" files fp/kdeconnect-active-device)
  (call-process-shell-command
   (format "%s %s %s" (fp/kdeconnect-base-command) "--share"
           (mapconcat 'identity files " "))))

(defun fp/kdeconnect-send-files-dired ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (when (y-or-n-p (format "Send %s to %s?" files fp/kdeconnect-active-device))
      (fp/kdeconnect-send-files files))))

(evil-leader/set-key-for-mode
  'dired-mode "mk" 'fp/kdeconnect-send-files-dired)

;; --------------------------------------------------------------------------------
;; archives
;; --------------------------------------------------------------------------------
;; i know that this is not technically dired, but i thought it still fits here nicely
(evil-set-initial-state 'archive-mode 'normal)

(evil-define-key 'normal archive-mode-map
  "h" 'dired-jump
  "j" 'archive-next-line
  "k" 'archive-previous-line
  "l" 'archive-extract
  "o" 'archive-extract-other-window
  "m" 'archive-mark
  "d" 'archive-flag-deleted
  "q" (lambda () (interactive)
        (let ((wb (current-buffer)))
          (dired ".")(kill-buffer wb)))

  "u" 'archive-unflag
  "U" 'archive-unmark-all-files
  "R" 'archive-rename-entry)

;; --------------------------------------------------------------------------------
;; filename normalization
;; --------------------------------------------------------------------------------
(straight-use-package 'unidecode)
(evil-leader/set-key-for-mode 'dired-mode "ms" 'fp/dired-sanitize-marked-filenames)

(autoload 'fp/dired-sanitize-marked-filenames "unidecode")
(with-eval-after-load "unidecode"
  (defun fp/dired-sanitize-marked-filenames ()
    (interactive)
    (let ((files (dired-get-marked-files nil current-prefix-arg)))
      (dolist (file files nil)
        (let ((conversions '(("ä" . "ae") ("ö" . "oe") ("ü" . "ue")
                             (" \+- \+" . "-") ("_" . "-")))
              (newfile file))

          (dolist (conversion conversions)
            (setq newfile
                  (replace-regexp-in-string (car conversion) (cdr conversion) newfile)))

          (dired-rename-file file
                             (concat
                              (unidecode-sanitize
                               (file-name-sans-extension
                                (file-name-nondirectory newfile)))
                              "."
                              (file-name-extension
                               (file-name-nondirectory newfile)))
                             nil))
        (revert-buffer)))))

(provide 'config-dired)
