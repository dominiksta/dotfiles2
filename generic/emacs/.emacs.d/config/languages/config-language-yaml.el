(require-and-log 'config-programming-general)

(config-add-external-dependency 'yamllint 'config-language-yaml "flycheck"
                                (lambda () (executable-find "yamllint"))
                                "sudo pip install yamllint"
                                "pip install yamllint")

(straight-use-package 'yaml-mode) (require 'yaml-mode)
(straight-use-package 'flycheck-yamllint) (require 'flycheck-yamllint)
(defun fp/yaml-mode-hook ()
  (flycheck-mode 1))
(add-hook 'yaml-mode-hook 'fp/yaml-mode-hook)
(add-hook 'yaml-mode-hook 'highlight-indent-guides-mode)


(provide 'config-language-yaml)
