(require-and-log 'config-language-web-general)
(add-hook 'css-mode-hook 'company-mode)

(evil-leader/set-key-for-mode 'css-mode
  "msc" 'fp/search-caniuse
  "ed" 'fp/refresh-browser)

;;--------------------------------------------------------------------------------
;; sassy css
;;--------------------------------------------------------------------------------

(defun fp/scss-mode-hook ()
  (interactive)
  (auto-save-mode 0)
  (company-mode 1))
(add-hook 'scss-mode-hook 'fp/scss-mode-hook)

(provide 'config-language-web-css)
