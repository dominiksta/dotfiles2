(require-and-log 'config-helm-minibuffer)

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

(require 'grep)
(defun fp/rgrep (rstart rend)
  (interactive "r")
  (grep (concat "grep -i --color=always -nH -r \""
                (read-from-minibuffer "grep recursively for: "
                                      (if (use-region-p)
                                          (buffer-substring-no-properties rstart rend) (current-word))
                                      ) "\" .")))

(define-key grep-mode-map "g g" 'beginning-of-buffer)
(define-key grep-mode-map "r" 'recompile)

;; --------------------------------------------------------------------------------
;; pdfgrep
;; --------------------------------------------------------------------------------
(use-package pdfgrep :ensure t
  :config
  (config-add-external-dependency 'pdfgrep 'config-pdf-tools "grep through pdfs"
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

(evil-leader/set-key
  "sd" 'pdfgrep
  "sD" 'fp/pdfgrep-todos)

;; --------------------------------------------------------------------------------
;; bindings TODO use config-add-external
;; --------------------------------------------------------------------------------
(evil-leader/set-key "si" 'fp/search-ddg)

(setq config-ag-available (executable-find "ag")
      config-grep-available (executable-find "grep"))

(use-package helm-ag :defer t :ensure t)
(use-package ag :defer t :ensure t
  :config
  (use-package wgrep-ag :ensure t)
  (evil-define-key 'normal ag-mode-map
    "e" 'wgrep-change-to-wgrep-mode
    "k" 'evil-previous-line)
  (evil-set-initial-state 'ag-mode 'normal))

(setq projectile-use-git-grep t)
(cond
 (config-ag-available (evil-leader/set-key
                        "sr" (lambda () (interactive) (helm-do-ag default-directory))
                        "sR" 'helm-ag
                        "sp" 'helm-projectile-ag
                        "sP" 'helm-multi-swoop-projectile
                        "sgg" 'grep
                        "sgr" 'rgrep
                        "sgr" 'fp/rgrep
                        "sgR" 'rgrep
                        "sgp" 'helm-projectile-grep))
 (config-grep-available (evil-leader/set-key
                          "sR"  'rgrep
                          "sr"  'fp/rgrep
                          "sg"  'grep
                          "sP" 'helm-multi-swoop-projectile
                          "sp"  'helm-projectile-grep))
 (t (evil-leader/set-key
      "sR"  'helm-multi-swoop
      "sr"  'helm-multi-swoop-all
      "sP" 'helm-multi-swoop-projectile
      "sp"  'helm-multi-swoop-projectile)))


(provide 'config-search)
