;; ----------------------------------------------------------------------
;; workspaces
;; ----------------------------------------------------------------------

(use-package eyebrowse
  :after evil evil-leader
  :ensure t
  :commands 'eyebrowse-switch-to-window-config
  :config
  (eyebrowse-mode 1)
  (setq eyebrowse-mode-line-style nil
        eyebrowse-new-workspace t))

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
