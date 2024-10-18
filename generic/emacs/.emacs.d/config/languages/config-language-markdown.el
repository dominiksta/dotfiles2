(require 'config-programming-general)

(straight-use-package 'markdown-mode)
(straight-use-package 'edit-indirect)

(with-eval-after-load "markdown-mode"
  (setq markdown-fontify-code-blocks-natively t)
  ;; heading sizes (`markdown-header-scaling' doesn't work for me)
  (custom-set-faces
   '(markdown-code-face ((t (:foreground nil))))
   '(markdown-header-face-1 ((t (:height 1.5))))
   '(markdown-header-face-2 ((t (:height 1.2))))
   '(markdown-header-face-3 ((t (:height 1.0))))))


(defun fp/markdown-mode-hook ()
  (interactive)
  (outline-minor-mode 1))
(add-hook 'markdown-mode-hook 'fp/markdown-mode-hook)

(evil-define-key 'normal markdown-mode-map
  (kbd "<tab>") 'markdown-cycle
  "gj" 'markdown-outline-next-same-level
  "gk" 'markdown-outline-previous-same-level)

(evil-leader/set-key-for-mode 'markdown-mode
  "mo" 'markdown-edit-code-block)

;; ----------------------------------------------------------------------
;; manual previews without external dependencies
;; ----------------------------------------------------------------------

(straight-use-package 'gh-md)
(evil-leader/set-key-for-mode 'markdown-mode
  "mp" 'gh-md-render-buffer
  "me" 'gh-md-export-buffer)

;; ----------------------------------------------------------------------
;; live previews
;; ----------------------------------------------------------------------
(straight-use-package 'grip-mode) ;; pip install grip
(evil-leader/set-key-for-mode 'markdown-mode "mg" 'grip-mode)

;; ----------------------------------------------------------------------
;; mermaid graphs
;; ----------------------------------------------------------------------
(straight-use-package 'mermaid-mode)
(config-add-external-dependency
 'mermaid-cli 'config-language-markdown "compile/preview mermaid graphs"
 (lambda () (executable-find "mmdc"))
 "npm i -g @mermaid-js/mermaid-cli" "npm i -g @mermaid-js/mermaid-cli")


(provide 'config-language-markdown)
