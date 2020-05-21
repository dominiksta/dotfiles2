(require-and-log 'config-programming-general)
(require-and-log 'config-shell)

;; ----------------------------------------------------------------------
;; making connections
;; ----------------------------------------------------------------------

(when (featurep 'epass)
  (defvar fp/sql-connection-alist nil
    "Equivalent to `sql-connection-alist', but sql-password is a
  file-path to a password-file. Used in `fp/sql-connect'")

  (let ((sql-connections-file (concat sync-directory "emacs/random/sql-connections.el")))
    (when (file-exists-p sql-connections-file)
      (load sql-connections-file)))

  (defun fp/sql-connect ()
    (interactive)
    "Interactively prompts for a connection as defined in
`fp/sql-connection-alist' and starts a connection with
`sql-connect'. It works by temporarily setting
`sql-connection-alist' with sql-password set in plain texts and
calling (setq sql-connection-alist nil) afterwards."
    (let* ((connection-name
            (intern
             (completing-read "name: " (mapcar (lambda (con) (car con))
                                               fp/sql-connection-alist))))
           (connection-without-password
            (cdr (assoc connection-name fp/sql-connection-alist)))
           (password-location
            (cdr (assoc 'sql-password connection-without-password)))
           ;; copy the the connection so it can be restored later
           (connection-with-password (copy-tree connection-without-password)))
      ;; put password in `connection-with-password'
      (if (cdr (assoc 'sql-password-plain connection-without-password))
          (setcar (cdr (assoc 'sql-password connection-with-password))
                  (cadr (assoc 'sql-password connection-without-password)))
        (when (assoc 'sql-password connection-without-password)
          (setcar (cdr (assoc 'sql-password connection-with-password))
                  (epass-from-file
                   (cadr (assoc 'sql-password connection-without-password))))))



      ;; `sql-product' and `sql-connection-alist' have to be set for
      ;; `sql-connect' to work
      (setq sql-product (cdr (assoc 'sql-product connection-with-password))
            sql-connection-alist (list (cons connection-name
                                             connection-with-password)))
      (print sql-connection-alist)
      (sql-connect connection-name)
      (setq sql-connection-alist nil))))

;; ----------------------------------------------------------------------
;; indentation and pretty-printing
;; ----------------------------------------------------------------------
(use-package sql-indent :ensure t)

(config-add-external-dependency
 'sqlparse 'config-language-sql "sql indentation"
 (lambda () (string=
             (shell-command-to-string "python -c 'import sqlparse'") ""))
 "pip2 install sqlparse" "pip2 install sqlparse")

(defun sqlparse-pretty-print-region (beg end)
  "Indents the active region with sqlparse (python)."
  (interactive "r")
  (shell-command-on-region
   beg end
   (concat "python -c \"import sys, sqlparse; "
           "print(sqlparse.format(sys.stdin.read(), reindent=True))\"")
   t t))

(defun sqlparse-pretty-print-string ()
  "Indents the string under the cursor as SQL with
`sql-parse-region' and repects the previous indentation
level. Tested with `php-mode'."
  (interactive)
  (save-excursion
    (message "%d %d" (region-beginning) (region-end))
    (let* ((inhibit-message t)
           (beg (car (evil-inner-double-quote)))
           (end (cadr (evil-inner-double-quote)))
           (text (buffer-substring-no-properties beg end))
           (column (progn (goto-char beg) (current-column)))
           (formatted-text
            (with-temp-buffer
              (erase-buffer)
              (insert text)
              (beginning-of-buffer) (delete-horizontal-space)
              (sqlparse-pretty-print-region (point-min) (point-max))
              (replace-string
               "\n" (concat "\n" (make-string column (string-to-char " ")))
               nil (point-min) (point-max))
              (buffer-string))))
      (delete-region beg end)
      (goto-char beg)
      (insert formatted-text))))

;; ----------------------------------------------------------------------
;; fix formatting of the first line of tables
;; modified from https://www.emacswiki.org/emacs/SqlMode
;; ----------------------------------------------------------------------
(defvar sql-last-prompt-pos 1
  "position of last prompt when added recording started")
(make-variable-buffer-local 'sql-last-prompt-pos)
(put 'sql-last-prompt-pos 'permanent-local t)

;; (defun sql-add-newline-first (output)
;;   "Add newline to beginning of OUTPUT for
;; `comint-preoutput-filter-functions' This fixes up the display of
;; queries sent to the inferior buffer programatically."
;;   (let ((begin-of-prompt
;;          (if comint-last-prompt (marker-position (car comint-last-prompt)) 1)))
;;     (if (> begin-of-prompt sql-last-prompt-pos)
;;         (progn
;;           (setq sql-last-prompt-pos begin-of-prompt)
;;           (concat "\n" output))
;;       output)))

(defun sql-add-newline-first (output) (concat "\n" output))

(add-hook 'sql-interactive-mode-hook
          (lambda () (add-hook 'comint-preoutput-filter-functions
                               'sql-add-newline-first)))

(add-hook 'sql-interactive-mode-hook (lambda ()
                                       (font-lock-mode 0)
                                       (setq truncate-lines t)))

;; ----------------------------------------------------------------------
;; upcase
;; ----------------------------------------------------------------------
(use-package sqlup-mode :ensure t
  :init (add-hook 'sql-mode-hook 'sqlup-mode))

;; ----------------------------------------------------------------------
;; bindings
;; ----------------------------------------------------------------------
(evil-leader/set-key-for-mode 'php-mode "is" 'sqlparse-pretty-print-string)
(evil-leader/set-key-for-mode 'web-mode "is" 'sqlparse-pretty-print-string)
(evil-leader/set-key-for-mode 'sql-mode
  "is" 'sqlparse-pretty-print-string
  "ir" 'sqlparse-pretty-print-region
  "ef" 'sql-send-paragraph
  "eb" 'sql-send-buffer
  "er" 'sql-send-region
  "ee" 'sql-send-line-and-next)

(provide 'config-language-sql)
