(require-and-log 'config-org)
(require-and-log 'config-ui)
(require-and-log 'config-modeline)

(straight-use-package 'org-tree-slide)

;; ----------------------------------------------------------------------
;; basics
;; ----------------------------------------------------------------------
(setq org-tree-slide-activate-message nil
      org-tree-slide-deactivate-message nil)
(org-tree-slide-simple-profile)

(set-face-attribute 'org-tree-slide-header-overlay-face nil
                    :foreground nil :background nil)

;; ----------------------------------------------------------------------
;; enter and exit
;; ----------------------------------------------------------------------
(defun org-tree-slide-enter ()
  (interactive)
  (org-tree-slide-mode 1)
  (olivetti-mode 1)
  (evil-emacs-state) (evil-normal-state) ; evil bug
  (fp/hide-mode-line-mode 1)
  (fp/org-hide-block-delimiters-mode 1))

(defun org-tree-slide-exit ()
  (interactive)
  (org-tree-slide-mode 0)
  (evil-emacs-state) (evil-normal-state) ; evil bug
  (fp/hide-mode-line-mode 0)
  (fp/org-hide-block-delimiters-mode 0))

;; ----------------------------------------------------------------------
;; bindings
;; ----------------------------------------------------------------------
(define-key org-tree-slide-mode-map (kbd "<f12>") 'org-tree-slide-exit)
(define-key org-tree-slide-mode-map (kbd "<f8>") 'org-tree-slide-content)

(define-key org-tree-slide-mode-map (kbd "<next>") 'org-tree-slide-move-next-tree)
(define-key org-tree-slide-mode-map (kbd "<prior>") 'org-tree-slide-move-previous-tree)
(evil-define-key 'normal org-tree-slide-mode-map
  (kbd "<right>") 'org-tree-slide-move-next-tree
  (kbd "<left>") 'org-tree-slide-move-previous-tree)

;; ----------------------------------------------------------------------
;; toggle block delimiters
;; ----------------------------------------------------------------------

(defvar fp/org-hide-block-delimiters-backup-faces nil
  "Hold the appearance of org delimiters before setting them to
nil.")

(define-minor-mode fp/org-hide-block-delimiters-mode
  "Hide block delimiters for source/example/quote/etc
blocks. Based on the `org-block-*' faces."
  nil " hb" nil
  (if fp/org-hide-block-delimiters-mode
      (progn
        (setq fp/org-hide-block-delimiters-backup-faces
              (list (face-attribute 'org-block-begin-line :foreground)
                    (face-attribute 'org-block-begin-line :background)))
        (set-face-attribute 'org-block-begin-line nil
                            :foreground (face-attribute 'default :background))
        (set-face-attribute 'org-block-begin-line nil
                            :background (face-attribute 'default :background))
        (set-face-attribute 'org-block-end-line nil
                            :foreground (face-attribute 'default :background))
        (set-face-attribute 'org-block-end-line nil
                            :background (face-attribute 'default :background)))
    (progn
      (set-face-attribute 'org-block-begin-line nil
                          :foreground (car fp/org-hide-block-delimiters-backup-faces))
      (set-face-attribute 'org-block-begin-line nil
                          :background (cadr fp/org-hide-block-delimiters-backup-faces))
      (set-face-attribute 'org-block-end-line nil
                          :foreground (car fp/org-hide-block-delimiters-backup-faces))
      (set-face-attribute 'org-block-end-line nil
                          :background (cadr fp/org-hide-block-delimiters-backup-faces)))))


(provide 'config-org-tree-slide)
