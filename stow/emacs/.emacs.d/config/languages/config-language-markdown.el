(require 'config-programming-general)

;; (config-add-external-dependency 'markdown 'config-language-markdown "previewing and exporting"
;;                                 (lambda () (executable-find "markdown"))
;;                                 "sudo apt-get install -y markdown" "None")

(use-package markdown-mode :ensure t :defer t
  :config

  (defun fp/markdown-mode-hook ()
    (interactive)
    (outline-minor-mode 1))
  (add-hook 'markdown-mode-hook 'fp/markdown-mode-hook)

  (evil-define-key 'normal markdown-mode-map
    (kbd "<tab>") 'markdown-cycle
    "gj" 'markdown-outline-next-same-level
    "gk" 'markdown-outline-previous-same-level)

  (use-package gh-md :ensure t
    :config
    (evil-leader/set-key-for-mode 'markdown-mode
      "mp" 'gh-md-render-buffer
      "me" 'gh-md-export-buffer)))


(provide 'config-language-markdown)
