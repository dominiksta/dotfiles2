;; ----------------------------------------------------------------------
;; workspaces
;; ----------------------------------------------------------------------

(use-package eyebrowse :ensure t :demand t)
(eyebrowse-mode 1)

(setq eyebrowse-mode-line-style nil
      eyebrowse-new-workspace t)

(setq fp/workspace-defaults
      '((lambda ()
          (when (not (member t (mapcar
                                (lambda (w) (eq (with-current-buffer
                                               (window-buffer w) major-mode)
                                           'org-agenda-mode))
                                (window-list))))
            (delete-other-windows)
            (split-window-right) (other-window 1)
            (find-file (concat sync-directory  "general/org/meinleben/"))
            (other-window 1)
            (org-agenda nil "a"))) ; 0
        (lambda () nil) ; 1
        (lambda () nil) ; 2
        (lambda () nil) ; 3
        (lambda () nil) ; 4
        (lambda () nil) ; 5
        (lambda () nil) ; 6
        (lambda () nil) ; 7
        (lambda () nil) ; 8
        (lambda ()
          (if (not (or (string-match "\\`gnus-" (symbol-name major-mode))
                      (eq major-mode 'message-mode)))
              (gnus))) ; 9
        ))

(defun fp/eyebrowse-switch-to-window-config-and-run-defaults (i)
  (eyebrowse-switch-to-window-config i)
  (other-window 0) ;; hack - for some reason the selected buffer stays on the
                   ;; old workspace if this is not called
  (funcall (nth i fp/workspace-defaults)))


(winner-mode 1)

;; ----------------------------------------------------------------------
;; maximizing
;; ----------------------------------------------------------------------
;; TODO play nicer with eyebrowse

(setq fp/maximized-windows '()
      fp/maximized-mode-line-indicator (propertize " M" 'face 'error))

(defun fp/toggle-window-maximized ()
  (interactive)
  (let* ((win (get-buffer-window (current-buffer)))
         (config (current-window-configuration))
         (comb (cons win config))
         (restore (seq-find (lambda (elt) (equal (car elt) win)) fp/maximized-windows)))
    (if (and (stringp (nth 0 mode-line-format)) (string-equal (nth 0 mode-line-format) " M"))
        (progn (set-window-configuration (cdr restore))
               (setq fp/maximized-windows (remove restore fp/maximized-windows))
               (setq mode-line-format (remove fp/maximized-mode-line-indicator mode-line-format))
               (force-mode-line-update))
      (progn (delete-other-windows)
             (add-to-list 'fp/maximized-windows comb)
             (add-to-list 'mode-line-format fp/maximized-mode-line-indicator)
             (force-mode-line-update)))))

;; ----------------------------------------------------------------------
;; make some windows play a little nicer
;; ----------------------------------------------------------------------

(use-package shackle :ensure t
  :config
  (setq shackle-rules '((realgud-mode :same t)
                        (gud-mode :same t))))


(provide 'config-window-management)
