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

(provide 'config-language-java)


