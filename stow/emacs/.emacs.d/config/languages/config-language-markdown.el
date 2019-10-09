(require 'config-programming-general)

(config-add-external-dependency 'markdown 'config-language-markdown "previewing and exporting"
                                (lambda () (executable-find "markdown"))
                                "sudo apt-get install -y markdown" "None")

(use-package markdown-mode :ensure t :defer t
  :config
  (use-package markdown-preview-mode :ensure t))


(provide 'config-language-markdown)
