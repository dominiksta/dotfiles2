
(require 'seq)
(require 'cl-macs)

(defvar scratchy-dir (concat user-emacs-directory "scratchy/"))
(defvar scratchy-archive-dir (concat scratchy-dir "archive/"))

(defvar scratchy-modes
  '(("typescript" . (:mode typescript-mode :ext "ts" :template nil))
    ("javascript" . (:mode js-mode :ext "js" :template nil))
    ("python" . (:mode python-mode :ext "py" :template nil))
    ("elisp" . (:mode emacs-lisp-mode :ext "el" :template nil))
    ("basic" . (:mode fundamental-mode :ext "txt" :template nil))
    ("org" . (:mode org-mode :ext "org" :template nil))
    ))

(defvar scratchy-date-format  "%Y-%m-%d %H_%M_%S")

(defvar scratchy-disabled-extras '(lsp lsp-mode tide-setup))

(defun scratchy-default-template (scratchy-mode-name)
  (with-temp-buffer
    (funcall (plist-get (cdr (assoc scratchy-mode-name scratchy-modes)) :mode))
    (insert (format (concat
             "scratchy buffer for mode '%s'\n"
             "timestamp: %s\n\n")
            scratchy-mode-name
            (format-time-string  "%Y-%m-%d %H:%M:%S")))
    (if (not comment-start) (setq-local comment-start "-- "))
    (comment-region (point-min) (point-max))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun scratchy--get-mode-details (scratchy-mode-name key)
  (plist-get (cdr (assoc scratchy-mode-name scratchy-modes)) key))

(defmacro scratchy--disable-extras (&rest body)
  `(let ((backups (mapcar 'symbol-function scratchy-disabled-extras)))
     (dolist (f scratchy-disabled-extras)
       (fset f (lambda (&optional o &rest r) nil)))
     (progn ,@body)
     (dotimes (i (length scratchy-disabled-extras))
       (fset (nth i scratchy-disabled-extras) (nth i backups)))))

(defun scratchy--buffer-name (scratchy-mode-name)
   (format "*scratchy:%s*" scratchy-mode-name))

(defun scratchy--get-buffer (scratchy-mode-name)
  (let ((buf (get-buffer (scratchy--buffer-name scratchy-mode-name))))
    (if (buffer-live-p buf) buf nil)))

(defun scratchy-make-file-name (scratchy-mode-name)
  (let ((ext (scratchy--get-mode-details scratchy-mode-name :ext)))
    (concat scratchy-dir "scratchy_" scratchy-mode-name "." ext)))

(defun scratchy-open (scratchy-mode-name)
  (interactive
   (list (completing-read
          "Open scratchy for mode: " (mapcar 'car scratchy-modes))))
  (if (not (file-exists-p scratchy-dir)) (mkdir scratchy-dir))
  (let* ((mode-details (cdr (assoc scratchy-mode-name scratchy-modes)))
         (mode (plist-get mode-details :mode))
         (template (plist-get mode-details :template))
         (ext (plist-get mode-details :ext))
         (template-string (if template (apply template scratchy-mode-name)
                            (scratchy-default-template scratchy-mode-name)))
         (buf-exists (scratchy--get-buffer scratchy-mode-name))
         (buf (or buf-exists
                  (find-file-noselect
                   (scratchy-make-file-name scratchy-mode-name)
                   nil t))))
    (display-buffer-same-window buf nil)
    (unless buf-exists
      (with-current-buffer buf
        (scratchy--disable-extras (funcall-interactively mode))
        (rename-buffer (format "*scratchy:%s*" scratchy-mode-name))
        (if (eq (point-max) 1) (insert template-string))
        (goto-char (point-max))
        (setq-local scratchy--is-scratchy-buffer t)))
    (setq scratchy--last-open scratchy-mode-name)
    buf))

(defvar scratchy--last-open nil)
(defun scratchy--is-scratchy-buffer ()
  (and buffer-file-name
       (string-prefix-p (expand-file-name scratchy-dir)
                        (expand-file-name (buffer-file-name)))))

(defun scratchy-open-dwim (scratchy-mode-name)
  (interactive
   (list (or (and (not (scratchy--is-scratchy-buffer)) scratchy--last-open)
             (completing-read
              "Open scratchy for mode: " (mapcar 'car scratchy-modes)))))
  (scratchy-open scratchy-mode-name))

(defun scratchy-get-archive-filename (scratchy-mode-name)
  (format "%s %s.%s"
          (format-time-string scratchy-date-format)
          scratchy-mode-name
          (scratchy--get-mode-details scratchy-mode-name :ext)
          ))

(defun scratchy-archive ()
  (interactive)
  (if (not (file-exists-p scratchy-archive-dir)) (mkdir scratchy-archive-dir))
  (if (not (scratchy--is-scratchy-buffer)) (error "Not in a scratchy buffer"))
  (let* ((scratchy-mode (caar (seq-filter
                               (lambda (m) (eq (plist-get (cdr m) :mode)
                                          major-mode))
                               scratchy-modes)))
         (archive-name (scratchy-get-archive-filename scratchy-mode)))
    (setq buffer-file-name (concat scratchy-archive-dir archive-name))
    (save-buffer)
    (kill-buffer)
    (delete-file (scratchy-make-file-name scratchy-mode))
    (scratchy-open scratchy-mode)
    (message "Archived scratchy buffer to %s" archive-name)))

(defun scratchy-open-archived ()
  (interactive)
  (if (not (file-exists-p scratchy-archive-dir)) (mkdir scratchy-archive-dir))
  (let ((file (completing-read "Open archived scratchy file: "
                               (cddr (directory-files scratchy-archive-dir)))))
    (find-file (concat scratchy-archive-dir file))))

(provide 'scratchy)