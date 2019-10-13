(require-and-log 'config-programming-general)

(use-package auctex :ensure t :defer t)

(setq LaTeX-item-indent 0 ;; \item should be indented by 2 spaces
      TeX-view-program-selection '((output-pdf "PDF Tools"))
      )

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

(provide 'config-language-latex)
