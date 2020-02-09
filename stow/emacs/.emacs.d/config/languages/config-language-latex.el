(require-and-log 'config-programming-general)

(config-add-external-dependency 'texlive-full 'config-language-latex "latex"
                                (lambda () (executable-find "pdflatex"))
                                "apt install texlive-full" "None")

(use-package auctex :ensure t :defer t)

(setq LaTeX-item-indent 0 ;; \item should be indented by 2 spaces
      TeX-view-program-selection '((output-pdf "PDF Tools")))

(defun fp/tex-mode-hook ()
  (TeX-source-correlate-mode 1) ;; `TeX-view' should jump to the location under
  ;; point, not just the relevant file. (Uses synctex)
  )

(add-hook 'TeX-mode-hook 'fp/tex-mode-hook)

(evil-leader/set-key-for-mode 'latex-mode
  "ma" 'TeX-command-run-all
  "mc" 'TeX-command-master)

(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
(setq-default TeX-master nil)

(evil-leader/set-key-for-mode 'bibtex-mode
  "if" 'bibtex-fill-entry)

(use-package helm-bibtex :ensure t)


(provide 'config-language-latex)
