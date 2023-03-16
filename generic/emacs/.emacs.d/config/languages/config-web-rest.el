(require-and-log 'config-language-web-general)

(straight-use-package 'restclient)
(with-eval-after-load "restclient"
  (evil-define-key 'normal restclient-mode-map
    (kbd "TAB") 'restclient-toggle-body-visibility
    (kbd "C-c C-c") 'restclient-http-send-current-stay-in-window))

(provide 'config-web-rest)
