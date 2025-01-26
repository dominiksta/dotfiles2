(require-and-log 'config-editor)
(require-and-log 'config-programming-general)

;; --------------------------------------------------------------------------------
;; web-mode
;; --------------------------------------------------------------------------------
(straight-use-package 'web-mode) (require 'web-mode)

(setq web-mode-code-indent-offset 2
      web-mode-markup-indent-offset 2
      web-mode-css-indent-offset 2
      web-mode-enable-auto-pairing nil
      web-mode-enable-auto-quoting nil
      web-mode-enable-current-element-highlight t
      web-mode-smart-quotes '("&bdquo;" . "&ldquo;"))

(straight-use-package 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)
(evil-define-key 'insert emmet-mode-keymap (kbd "C-,") 'emmet-expand-yas)

(setq-default web-mode-comment-formats
              '(("java" . "//")
                ("javascript" . "//")
                ("typescript" . "//")
                ("php" . "//")
                ("css" . "//")))

;; (setq-default web-mode-comment-style 2)
;; (setq-default web-mode-comment-prefixing nil)

(custom-set-faces
 '(web-mode-current-element-highlight-face ((t (:inherit highlight))))
 '(web-mode-html-tag-face ((t nil))))
(evil-leader/set-key-for-mode 'web-mode
  "ed" 'fp/refresh-browser
  "mf" 'web-mode-fold-or-unfold)
(evil-define-key 'normal web-mode-map "gt" 'web-mode-tag-match)
(define-key web-mode-map (kbd "M-R") 'web-mode-element-rename)


(straight-use-package 'apache-mode)
(straight-use-package 'nginx-mode)

(provide 'config-language-web-general)
