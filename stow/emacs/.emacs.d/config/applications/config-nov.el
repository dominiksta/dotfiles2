(require-and-log 'config-ui)

(use-package nov :ensure t :config
  ;; ----------------------------------------------------------------------
  ;; Visuals
  ;; ----------------------------------------------------------------------
  (add-hook 'nov-pre-html-render-hook 'olivetti-mode)

  ;; ----------------------------------------------------------------------
  ;; Binds
  ;; ----------------------------------------------------------------------
  ;; (define-key nov-mode-map (kbd "M-h") 'nov-history-back)
  ;; (define-key nov-mode-map (kbd "M-l") 'nov-history-forward)

  (evil-define-key 'normal nov-mode-map
    "gr"                  'nov-render-document
    "s"                   'nov-view-source
    "S"                   'nov-view-content-source
    "g?"                  'nov-display-metadata
    "gj"                  'nov-next-document
    (kbd "M-h") 'nov-history-back
    (kbd "M-l") 'nov-history-forward
    (kbd "M-j")           'nov-next-document
    "gk"                  'nov-previous-document
    (kbd "M-k")           'nov-previous-document
    "o"                   'nov-goto-toc
    (kbd "RET")           'nov-browse-url
    (kbd "<follow-link>") 'mouse-face
    (kbd "<mouse-2>")     'nov-browse-url
    (kbd "TAB")           'shr-next-link
    (kbd "<backtab>")     'shr-previous-link))

(provide 'config-nov)