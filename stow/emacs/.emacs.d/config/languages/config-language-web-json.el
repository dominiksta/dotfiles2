(require-and-log 'config-language-web-general)

(config-add-external-dependency 'jsonlint 'config-language-json "flymake json"
                                (lambda () (executable-find "jsonlint"))
                                "npm install -g jsonlint" "npm install -g jsonlint")

(defun fp/json-mode-hook ()
  (setq-local js-indent-level 2))

(straight-use-package 'json-mode) (require 'json-mode)
(add-hook 'json-mode-hook 'flycheck-mode)
(add-hook 'json-mode-hook 'fp/json-mode-hook)

(provide 'config-language-web-json)
