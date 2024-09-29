(require-and-log 'config-programming-general)

(config-add-external-dependency 'texlive-full 'config-language-latex "latex"
                                (lambda () (executable-find "pdflatex"))
                                "apt install texlive-full" "None")

(straight-use-package 'auctex)

;; ----------------------------------------------------------------------
;; Visuals
;; ----------------------------------------------------------------------
(add-hook 'TeX-mode-hook 'olivetti-mode)
(add-hook 'TeX-mode-hook 'hl-todo-mode)
(add-hook 'TeX-mode-hook 'symbol-overlay-mode)

;; ----------------------------------------------------------------------
;; Random
;; ----------------------------------------------------------------------

(setq
 ;; \item should be indented by 2 spaces
 LaTeX-item-indent 0
 ;; use pdf-tools as a viewer
 TeX-view-program-selection '((output-pdf "PDF Tools")))

(add-hook 'TeX-mode-hook 'TeX-source-correlate-mode)

(evil-leader/set-key-for-mode 'latex-mode
  "ma" 'TeX-command-run-all
  "mC" 'TeX-command-master)

(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
(setq-default TeX-master nil)

;; ----------------------------------------------------------------------
;; Math
;; ----------------------------------------------------------------------

(add-hook 'latex-mode-hook 'prettify-symbols-mode)
(evil-leader/set-key-for-mode 'latex-mode "mp" 'prettify-symbols-mode)

;; ----------------------------------------------------------------------
;; References
;; ----------------------------------------------------------------------

(setq bibtex-dialect 'biblatex
      bibtex-completion-library-path (concat sync-directory "documents/studium/00-academic/")
      reftex-default-bibliography (concat sync-directory "documents/zotero/library.bib"))

;; I use `zotero` with the `Better BibTeX` plugin for library management.
(straight-use-package 'helm-bibtex)
(with-eval-after-load "helm-bibtex"
  (setq bibtex-completion-bibliography (list reftex-default-bibliography)
        bibtex-completion-pdf-open-function 'find-file
        bibtex-completion-pdf-field "file"))

;; If i need to edit bibtex entries manually, this has proven useful.
(evil-leader/set-key-for-mode 'bibtex-mode "if" 'bibtex-fill-entry)

(provide 'config-language-latex)
