(require-and-log 'config-programming-general)

(config-add-external-dependency 'yamllint 'config-language-yaml "flycheck"
                                (lambda () (executable-find "yamllint"))
                                "pip install yamllint"
                                "pip install yamllint")

(use-package yaml-mode :ensure t :config
  (use-package flycheck-yamllint :ensure t :demand t)
  (defun fp/yaml-mode-hook ()
    (flycheck-mode 1))
  (add-hook 'yaml-mode-hook 'fp/yaml-mode-hook))


(provide 'config-language-yaml)
