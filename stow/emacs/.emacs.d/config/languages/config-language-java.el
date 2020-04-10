(require-and-log 'config-language-cc)


(config-add-external-dependency 'java 'config-language-java "java lsp"
                                (lambda () (executable-find "java"))
                                "apt install default-jdk" "choco install openjdk")

;; TODO lsp support
;; (use-package lsp-java
;;   :ensure t
;;   :config
;;   (add-hook 'java-mode-hook 'lsp)
;;   (evil-leader/set-key-for-mode 'java-mode "mo" 'lsp-java-organize-imports))

(defun fp/java-indentation-setup ()
  (setq-local c-default-style "java")
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-cont-nonempty '+)
  (c-set-offset 'arglist-close '0)
  (c-set-offset 'case-label '+))

(add-hook 'java-mode-hook 'fp/java-indentation-setup)

(provide 'config-language-java)


