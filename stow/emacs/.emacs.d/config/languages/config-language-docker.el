(require-and-log 'config-programming-general)
(require-and-log 'config-language-yaml)

(straight-use-package 'dockerfile-mode)
(straight-use-package 'docker-compose-mode)

(flycheck-add-mode 'yaml-yamllint 'docker-compose-mode)
(defun fp/docker-compose-mode-hook ()
  (company-mode 1)
  (flycheck-mode 1))
(add-hook 'docker-compose-mode-hook 'fp/docker-compose-mode-hook)

(provide 'config-language-docker)
