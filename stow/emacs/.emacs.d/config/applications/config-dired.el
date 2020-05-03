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
      dired-listing-switches "-a -l -h --group-directories-first")

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
(use-package peep-dired
  :defer t
  :ensure t
  :init (evil-leader/set-key-for-mode 'dired-mode "mp" 'peep-dired)
  :config
  (evil-define-key 'normal peep-dired-mode-map
    "j" 'peep-dired-next-file
    "k" 'peep-dired-prev-file
    "J" 'peep-dired-scroll-page-down
    "K" 'peep-dired-scroll-page-up)
  ;; fix the evil bug by just forcing a state switch
  (add-hook 'peep-dired-hook (lambda () (evil-emacs-state) (evil-normal-state))))


;; --------------------------------------------------------------------------------
;; file-sizes
;; --------------------------------------------------------------------------------
(use-package dired-du
  :defer t
  :ensure t
  :init (evil-leader/set-key-for-mode 'dired-mode (kbd "mS")
          (lambda () (interactive) (dired-du-mode 'toggle)))
  :config
  (setq dired-du-size-format t
        dired-du--user-warned nil)
  (defun fp/disable-du-in-new-dired-buffers ()
    (interactive)
    (if (bound-and-true-p dired-du-mode)
        (dired-du-mode 0)))
  (add-hook 'dired-before-readin-hook 'fp/disable-du-in-new-dired-buffers))


;; --------------------------------------------------------------------------------
;; subtrees
;; --------------------------------------------------------------------------------
;; TODO light face
(use-package dired-subtree
  :ensure t
  :defer t
  :init
  (define-key dired-mode-map (kbd "C-x n s") 'dired-subtree-narrow)
  (evil-define-key 'normal dired-mode-map
    (kbd "TAB") 'dired-subtree-cycle
    (kbd "M-j") 'dired-subtree-next-sibling
    (kbd "M-k") 'dired-subtree-previous-sibling
    "gj" 'dired-subtree-next-sibling
    "gk" 'dired-subtree-previous-sibling
    (kbd "M-n") 'dired-subtree-down
    (kbd "M-p") 'dired-subtree-up
    "i" 'dired-subtree-insert
    "I" 'dired-subtree-remove))

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
    (start-process-shell-command "pcmanfm" nil "pcmanfm .")))

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
(setq dired-omit-files "^\\.$\\|desktop.ini\\|^NTUSER.DAT\\|ntuser.ini")
(setq dired-omit-extensions nil)
(setq dired-omit-verbose nil)
(add-hook 'dired-after-readin-hook 'dired-omit-mode)

;; --- different colors for files ---
(use-package dired-rainbow :ensure t
  :config
  (dired-rainbow-define-chmod executable-unix (:inherit success) "-.*x.*")
  (dired-rainbow-define compressed (:inherit font-lock-string-face)
                        ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z"
                         "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
  (dired-rainbow-define document (:inherit font-lock-doc-face)
                        ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf"
                         "djvu" "epub" "odp" "ppt" "pptx" "xoj" "xopp"))
  ;; (dired-rainbow-define code (:inherit Info-quoted)
  ;;                       ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql"
  ;;                        "r" "clj" "cljs" "scala" "js" "asm" "cl" "lisp" "el"
  ;;                        "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp"
  ;;                        "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s"
  ;;                        "rs" "hi" "hs" "pyc" ".java"))
  )

;; --------------------------------------------------------------------------------
;; async
;; --------------------------------------------------------------------------------
(defun dired-async-system-notify (format-string &rest args)
  (generic-notification-notify "Async File Operation" (apply 'format format-string (cdr args))))
(setq dired-async-message-function 'dired-async-system-notify)

(define-key dired-mode-map (kbd "A") 'dired-async-mode)


;; --------------------------------------------------------------------------------
;; bindings
;; --------------------------------------------------------------------------------
(evil-leader/set-key-for-mode 'dired-mode
  "mo" 'fp/dired-open-with-system-default
  "md" 'fp/dired-open-directory-with-system-default
  "mq" 'dired-toggle-read-only
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

(define-key dired-mode-map fp/mouse-back 'dired-find-alternate-up)
(define-key dired-mode-map fp/double-mouse-back 'dired-find-alternate-up)
(define-key dired-mode-map fp/triple-mouse-back 'dired-find-alternate-up)

(define-key dired-mode-map [mouse-3] 'fp/dired-mouse-open-with-system-default)
(define-key dired-mode-map [(double-mouse-3)] 'fp/dired-mouse-open-with-system-default)
(define-key dired-mode-map [(triple-mouse-3)] 'fp/dired-mouse-open-with-system-default)


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
;; music filenames
;; --------------------------------------------------------------------------------
(use-package unidecode
  :ensure t
  :defer t
  :commands fp/dired-sanitize-marked-filenames
  :init (evil-leader/set-key-for-mode 'dired-mode "ms" 'fp/dired-sanitize-marked-filenames)
  :config
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
