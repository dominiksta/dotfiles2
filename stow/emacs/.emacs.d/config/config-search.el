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
(use-package pdfgrep :ensure t
  :config
  (config-add-external-dependency 'pdfgrep 'config-search "grep through pdfs"
                                  (lambda () (executable-find "pdfgrep"))
                                  "apt-get install -y pdfgrep" "None")

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
;; silver searcher
;; --------------------------------------------------------------------------------
(use-package helm-ag :defer t :ensure t)
(use-package ag :defer t :ensure t
  :config
  (use-package wgrep-ag :ensure t)
  (evil-define-key 'normal ag-mode-map
    "q" 'quit-window
    "e" 'wgrep-change-to-wgrep-mode
    "k" 'evil-previous-line)
  (setq ag-highlight-search t)
  (evil-set-initial-state 'ag-mode 'normal))

;; --------------------------------------------------------------------------------
;; dependencies
;; --------------------------------------------------------------------------------
(evil-leader/set-key "si" 'fp/search-ddg)

(config-add-external-dependency 'ag 'config-search "searching"
                                (lambda () (executable-find "ag"))
                                "apt install silversearcher-ag" "cinst -y ag")

(config-add-external-dependency 'grep 'config-search "searching"
                                (lambda () (executable-find "grep"))
                                "None" "None")

(setq config-ag-available (config-external-check-list '(ag))
      config-grep-available (config-external-check-list '(grep)))


(provide 'config-search)
