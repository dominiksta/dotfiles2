(require-and-log 'config-editor)

(straight-use-package 'csv-mode)
(add-hook 'csv-mode-hook 'csv-align-mode)
(add-hook 'csv-mode-hook 'csv-header-line)

(evil-define-key 'normal csv-mode-map
  (kbd "<tab>") 'csv-tab-command)

(provide 'config-language-csv)