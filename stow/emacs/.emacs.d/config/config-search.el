(require-and-log 'config-helm-minibuffer)
(require 'grep)


;; --------------------------------------------------------------------------------
;; options
;; --------------------------------------------------------------------------------
;; recenter even if the line was already visible
(setq next-error-recenter '(4))

;; TODO start using autoload properly for this and others

;; --------------------------------------------------------------------------------
;; internet
;; --------------------------------------------------------------------------------
(defun fp/documentation-with-browser (start end url)
  (interactive "r")
  (start-process-shell-command
   "documentation search" nil
   (concat browse-url-generic-program " \"" url
           (if (use-region-p)
               (buffer-substring-no-properties start end)
             (current-word)) "\"")))

(defun fp/search-ddg (start end)
  (interactive "r")
  (fp/documentation-with-browser
   start end "https://duckduckgo.com/?q="))

;; --------------------------------------------------------------------------------
;; grep
;; --------------------------------------------------------------------------------

(define-key grep-mode-map "g g" 'beginning-of-buffer)
(define-key grep-mode-map "r" 'recompile)

;; --------------------------------------------------------------------------------
;; pdfgrep
;; --------------------------------------------------------------------------------
(straight-use-package 'pdfgrep)

(with-eval-after-load "pdfgrep"
  (config-add-external-dependency 'pdfgrep 'config-search "grep through pdfs"
                                  (lambda () (executable-find "pdfgrep"))
                                  "apt-get install -y pdfgrep" "None")

  ;; Conform to emacs's internal `exec-path' rather than the environment
  ;; variable.
  (setq pdfgrep-program (executable-find "pdfgrep"))

  (setq pdfgrep-context-length nil)

  (defun pdfgrep-default-command ()
    "REDEFINED: Compute the default pdfgrep command for `pdfgrep'."
    (let ((cmd (concat pdfgrep-program " -H -n -r "
		               (when pdfgrep-ignore-case "-i ")
		               (when pdfgrep-context-length
		                 (format "-C %d " pdfgrep-context-length)))))
      (if pdfgrep-ignore-errors
	      (cons (concat cmd " 2>/dev/null") (1+ (length cmd))) cmd)))

  (defun fp/pdfgrep-todos ()
    "Searches for every keyword in `hl-todo-keyword-faces'
recursively from the current directory using `pdfgrep'."
    (interactive)
    (pdfgrep (concat "pdfgrep -nHrF \""
                     (mapconcat (lambda (elt) (car elt))
                                hl-todo-keyword-faces "\n")
                     "\"")))
  (pdfgrep-mode))

;; --------------------------------------------------------------------------------
;; ripgrep
;; --------------------------------------------------------------------------------
(straight-use-package 'rg)
(straight-use-package 'wgrep)
(with-eval-after-load "rg"
  (setq rg-executable "rg"
        helm-grep-ag-command (concat "rg"
                                     " --color=never"
                                     " --smart-case"
                                     " --no-heading"
                                     " --line-number %s %s %s")
        helm-grep-file-path-style 'relative)

  (defun fp/helm-projectile-rg (arg)
    "Run `helm-do-grep-ag' in project root according to
`rg-project-root'."
    (interactive "P")
    (let ((default-directory (rg-project-root (buffer-file-name
                                               (current-buffer)))))
      (helm-do-grep-ag arg)))

  (defun fp/rg-search-full-command (default-directory command)
    (let ((default-directory default-directory))
      (compilation-start command 'rg-mode #'rg-buffer-name)))

  (defun fp/rg-search-multi-directory (base-dir dirs search)
    (fp/rg-search-full-command base-dir (format "rg --color=always --colors=match:fg:red \
--colors=path:fg:magenta --colors=line:fg:green --colors=column:none -n \
--column --heading --no-config -e \"%s\" %s" search (mapconcat 'shell-quote-argument
                                                               dirs " "))))

  (defun fp/rg-search-multi-directory-thing-at-point (base-dir dirs)
    (let* ((default (word-at-point t))
           (in (read-string (format "Search (default %s): " default)))
           (search (if (eq (length in) 0) default in)))
      (fp/rg-search-multi-directory base-dir dirs search)))

  (rg-define-search fp/rg-project-everything
    :files "everything"
    :dir project)

  (add-hook 'rg-mode-hook 'wgrep-rg-setup)
  (evil-define-key 'normal rg-mode-map
    "q" 'quit-window
    "r" 'recompile
    "gj" 'rg-next-file
    "gk" 'rg-prev-file
    "a" 'compilation-display-error
    "e" 'wgrep-change-to-wgrep-mode)
  (evil-set-initial-state 'rg-mode 'normal))

;; --------------------------------------------------------------------------------
;; dependencies
;; --------------------------------------------------------------------------------
(evil-leader/set-key "si" 'fp/search-ddg)

(config-add-external-dependency 'rg 'config-search "searching"
                                (lambda () (executable-find "rg"))
                                "apt install ripgrep" "cinst -y ripgrep")

(config-add-external-dependency 'grep 'config-search "searching"
                                (lambda () (executable-find "grep"))
                                "None" "None")

(provide 'config-search)
