(require-and-log 'config-language-web-general)

(config-add-external-dependency 'jsonlint 'config-language-json "flymake json"
                                (lambda () (executable-find "jsonlint"))
                                "npm install -g jsonlint" "npm install -g jsonlint")

(straight-use-package 'json-mode) (require 'json-mode)
(add-hook 'json-mode-hook 'flycheck-mode)

(provide 'config-language-web-json)
