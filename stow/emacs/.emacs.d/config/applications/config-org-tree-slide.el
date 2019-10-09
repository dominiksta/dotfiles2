(require-and-log 'config-org)
(require-and-log 'config-ui)

(use-package org-tree-slide :ensure t :demand t)

(setq org-tree-slide-activate-message nil
      org-tree-slide-deactivate-message nil)
(org-tree-slide-simple-profile)

(set-face-attribute 'org-tree-slide-header-overlay-face nil
                    :foreground nil :background nil)

(defun org-tree-slide-enter ()
  (interactive)
  (org-tree-slide-mode 1)
  (olivetti-mode 1)
  (evil-emacs-state) (evil-normal-state) ; evil bug
  (fp/org-toggle-hide-block-delimiters))

(defun org-tree-slide-exit ()
  (interactive)
  (setq-local mode-line-format fp/mode-line-format)
  (org-tree-slide-mode 0)
  (evil-emacs-state) (evil-normal-state) ; evil bug
  (fp/org-toggle-hide-block-delimiters))

(define-key org-tree-slide-mode-map (kbd "<f12>") 'org-tree-slide-exit)
(define-key org-tree-slide-mode-map (kbd "<f8>") 'org-tree-slide-content)

(define-key org-tree-slide-mode-map (kbd "<next>") 'org-tree-slide-move-next-tree)
(define-key org-tree-slide-mode-map (kbd "<prior>") 'org-tree-slide-move-previous-tree)
(evil-define-key 'normal org-tree-slide-mode-map
  (kbd "<right>") 'org-tree-slide-move-next-tree
  (kbd "<left>") 'org-tree-slide-move-previous-tree)



;; TODO make this a minor mode
(setq fp/org-toggle-hide-block-delimiters-state nil
      fp/org-toggle-hide-block-delimiters-backup nil)

(defun fp/org-toggle-hide-block-delimiters ()
  (interactive)
  (if (not fp/org-toggle-hide-block-delimiters-state)
      (progn
        (setq fp/org-toggle-hide-block-delimiters-backup
              (list (face-attribute 'org-block-begin-line :foreground)
                    (face-attribute 'org-block-begin-line :background))
              fp/org-toggle-hide-block-delimiters-state t)
        (set-face-attribute 'org-block-begin-line nil
                            :foreground (face-attribute 'default :background))
        (set-face-attribute 'org-block-begin-line nil
                            :background (face-attribute 'default :background))
        (set-face-attribute 'org-block-end-line nil
                            :foreground (face-attribute 'default :background))
        (set-face-attribute 'org-block-end-line nil
                            :background (face-attribute 'default :background)))
    (progn
      (setq fp/org-toggle-hide-block-delimiters-state nil)
      (set-face-attribute 'org-block-begin-line nil
                          :foreground (car fp/org-toggle-hide-block-delimiters-backup))
      (set-face-attribute 'org-block-begin-line nil
                          :background (cadr fp/org-toggle-hide-block-delimiters-backup))
      (set-face-attribute 'org-block-end-line nil
                          :foreground (car fp/org-toggle-hide-block-delimiters-backup))
      (set-face-attribute 'org-block-end-line nil
                          :background (cadr fp/org-toggle-hide-block-delimiters-backup)))))

(provide 'config-org-tree-slide)
