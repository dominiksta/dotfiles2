(require-and-log 'config-programming-general)
(require-and-log 'config-shell)

;; ----------------------------------------------------------------------
;; indentation and pretty-printing
;; ----------------------------------------------------------------------
(straight-use-package 'sql-indent)

(config-add-external-dependency
 'sqlparse 'config-language-sql "sql indentation"
 (lambda () (string=
             (shell-command-to-string "python3 -c 'import sqlparse'") ""))
 "pip install sqlparse" "pip install sqlparse")

(defun sqlparse-pretty-print-region (beg end)
  "Indents the active region with sqlparse (python)."
  (interactive "r")
  (shell-command-on-region
   beg end
   (concat "python3 -c \"import sys, sqlparse; "
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
;; upcase
;; ----------------------------------------------------------------------
(straight-use-package 'sqlup-mode)
(add-hook 'sql-mode-hook 'sqlup-mode)

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
