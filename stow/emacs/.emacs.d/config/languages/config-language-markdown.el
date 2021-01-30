(require 'config-programming-general)

(straight-use-package 'markdown-mode)

(defun fp/markdown-mode-hook ()
  (interactive)
  (outline-minor-mode 1))
(add-hook 'markdown-mode-hook 'fp/markdown-mode-hook)

(evil-define-key 'normal markdown-mode-map
  (kbd "<tab>") 'markdown-cycle
  "gj" 'markdown-outline-next-same-level
  "gk" 'markdown-outline-previous-same-level)

(straight-use-package 'gh-md)
(evil-leader/set-key-for-mode 'markdown-mode
  "mp" 'gh-md-render-buffer
  "me" 'gh-md-export-buffer)


(provide 'config-language-markdown)
