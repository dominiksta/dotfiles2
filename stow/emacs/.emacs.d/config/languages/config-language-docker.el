(require-and-log 'config-programming-general)
(require-and-log 'config-language-yaml)

(use-package dockerfile-mode :ensure t)
(use-package docker-compose-mode :ensure t :config
  (flycheck-add-mode 'yaml-yamllint 'docker-compose-mode)
  (defun fp/docker-compose-mode-hook ()
    (company-mode 1)
    (flycheck-mode 1))
  (add-hook 'docker-compose-mode-hook 'fp/docker-compose-mode-hook))

(provide 'config-language-docker)
